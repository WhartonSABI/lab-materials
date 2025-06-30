"""
bayes_cv_softmax.py

Bayesian hierarchical soft-max with hyper-prior grid-search.

• Loads and preprocesses the Spotify playlist via load.py
• Runs a 5-fold stratified CV over a small prior-scale grid:
      σβ ∈ {0.3, 0.5, 1.0}
      σᵅ ∈ {1.0, 2.0}
• Reports mean / stdev log-loss for each combo
• Re-fits the best prior setting on the whole data set and
  saves the posterior for future prediction.
"""

import numpy as np
import pandas as pd
from pathlib import Path
import pymc as pm
import arviz as az
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import log_loss
import joblib
from data import load as data_loader

# ---------------------------------------------------------------------
OUT_POSTERIOR = Path("bayes_softmax_best.nc")
KFOLDS = 5
DRAWS = 800  # keep runtime sane in CV
TUNE = 800
TARGET_ACCEPT = 0.95
SEED = 42

# prior scale grids
SIGMA_B_GRID = [0.3, 0.5, 1.0]
SIGMA_A_GRID = [1.0, 2.0]

# ---------------------------------------------------------------------
# prepare data once
df_all = data_loader.load_df()
X_all, y_all, class_names, _ = data_loader.prepare_data(df_all, fit_transform=True)
if hasattr(X_all, "toarray"):
    X_all = X_all.toarray().astype("float32")

K = len(class_names)
P = X_all.shape[1]


def build_model(X, y, sigma_beta_scale, sigma_alpha_scale, coords):
    """Return an un-sampled PyMC model instance."""
    with pm.Model(coords=coords) as m:
        mu_beta = pm.Normal("mu_beta", 0.0, 1.0, dims=("feature",))
        sigma_beta = pm.HalfNormal("sigma_beta", sigma_beta_scale, dims=("feature",))

        mu_alpha = pm.Normal("mu_alpha", 0.0, 2.0)
        sigma_alpha = pm.HalfNormal("sigma_alpha", sigma_alpha_scale)

        beta_std = pm.Normal("beta_std", 0.0, 1.0, dims=("class", "feature"))
        beta = pm.Deterministic("beta",
                                mu_beta + beta_std * sigma_beta,
                                dims=("class", "feature"))

        alpha_std = pm.Normal("alpha_std", 0.0, 1.0, dims=("class",))
        alpha = pm.Deterministic("alpha",
                                 mu_alpha + alpha_std * sigma_alpha,
                                 dims=("class",))

        eta = alpha + pm.math.dot(X, beta.T)
        y_obs = pm.Categorical("y_obs", logit_p=eta, observed=y, dims=("obs_id",))
    return m


def posterior_mean_probs(trace, X):
    alpha = (
        trace.posterior["alpha"]
        .stack(sample=("chain", "draw"))
        .transpose("sample", "class")
        .values
    )  # (S, K)
    beta = (
        trace.posterior["beta"]
        .stack(sample=("chain", "draw"))
        .transpose("sample", "class", "feature")
        .values
    )  # (S, K, P)
    eta = alpha[:, None, :] + np.einsum("skp,np->snk", beta, X)
    p = np.exp(eta - eta.max(axis=-1, keepdims=True))
    p = p / p.sum(axis=-1, keepdims=True)
    return p.mean(axis=0)  # (n_obs, K)


# ---------------------------------------------------------------------
print(f"Running {KFOLDS}-fold CV over {len(SIGMA_B_GRID) * len(SIGMA_A_GRID)} prior settings…")
skf = StratifiedKFold(n_splits=KFOLDS, shuffle=True, random_state=SEED)
results = []

for s_b in SIGMA_B_GRID:
    for s_a in SIGMA_A_GRID:
        losses = []
        for fold, (tr_idx, va_idx) in enumerate(skf.split(X_all, y_all), 1):
            X_tr, y_tr = X_all[tr_idx], y_all[tr_idx]
            X_va, y_va = X_all[va_idx], y_all[va_idx]

            coords = {
                "obs_id": np.arange(X_tr.shape[0]),
                "feature": np.arange(P),
                "class": class_names,
            }
            model = build_model(X_tr, y_tr, s_b, s_a, coords)
            with model:
                tr = pm.sample(
                    draws=DRAWS,
                    tune=TUNE,
                    chains=2,
                    cores=2,
                    target_accept=TARGET_ACCEPT,
                    progressbar=False,
                    random_seed=fold + SEED,
                )
            p_va = posterior_mean_probs(tr, X_va)
            loss = log_loss(y_va, p_va, labels=np.arange(K))
            losses.append(loss)
            print(f"  σβ={s_b:<3}  σα={s_a:<3}  fold {fold}  loss={loss:.4f}")

        mean_loss = np.mean(losses)
        std_loss = np.std(losses)
        results.append((mean_loss, std_loss, s_b, s_a))
        print(f"→ σβ={s_b}  σα={s_a}  mean={mean_loss:.4f} ± {std_loss:.4f}")

# pick best prior setting
best = min(results, key=lambda t: t[0])
best_mean, best_std, best_sb, best_sa = best
print(f"\nBEST prior: σβ={best_sb}, σα={best_sa}  mean CV log-loss={best_mean:.4f}")

# ---------------------------------------------------------------------
# fit final model on ALL data with best priors
coords_full = {
    "obs_id": np.arange(X_all.shape[0]),
    "feature": np.arange(P),
    "class": class_names,
}
final_model = build_model(X_all, y_all, best_sb, best_sa, coords_full)
with final_model:
    final_trace = pm.sample(
        draws=1200,
        tune=1200,
        chains=4,
        cores=4,
        target_accept=TARGET_ACCEPT,
        progressbar=True,
        random_seed=SEED,
    )
    az.to_netcdf(final_trace, OUT_POSTERIOR)
print(f"\n✅ Final posterior saved → {OUT_POSTERIOR.resolve()}")

# ---------------------------------------------------------------------
# quick sanity check: loo
loo = az.loo(final_trace, scale="deviance")
print(f"Final PSIS-LOO estimate: {loo.loo:.2f} deviance (± {loo.loo_se:.2f})")

if __name__ == "__main__":
    print(f"Running {KFOLDS}-fold CV over {len(SIGMA_B_GRID)*len(SIGMA_A_GRID)} prior settings…")
