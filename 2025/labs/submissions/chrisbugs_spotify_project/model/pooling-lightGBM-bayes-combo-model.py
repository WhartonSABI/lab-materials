"""
pooling-lightGBM-bayes-combo-model.py

End-to-end script that

1. loads & preprocesses the Spotify competition data via load.py,
2. fits *Bayesian hierarchical soft-max* (PyMC) **and**
   a calibrated LightGBM multiclass model,
3. learns a simple multinomial logistic blender (“stacker”)
   on the validation fold to combine the two sets of probabilities,
4. prints validation log-loss for each stage and the final ensemble,
5. saves the PyMC posterior, calibrated LightGBM, and stacker
   so you can reuse them without re-training.

Designed for the 321-song / 10-user dataset and
aimed at minimising out-of-sample cross-entropy.
"""

# ──────────────────────────────────────────────────────────────────────────────
# Imports
import numpy as np
import pandas as pd
from pathlib import Path
import joblib
from sklearn.model_selection import train_test_split
from sklearn.metrics import log_loss
from sklearn.linear_model import LogisticRegression
from sklearn.calibration import CalibratedClassifierCV
from lightgbm import LGBMClassifier, early_stopping, log_evaluation
import pymc as pm
import arviz as az

from data import load as data_loader  # <-- your load.py module

# ──────────────────────────────────────────────────────────────────────────────
# Global paths
POSTERIOR_PATH = Path("bayes_softmax_posterior.nc")
LGBM_MODEL_PATH = Path("lgbm.joblib")
CALIBRATED_LGBM_MODEL_PATH = Path("calibrated_lgbm.joblib")
STACKER_PATH = Path("stacker.joblib")

# ──────────────────────────────────────────────────────────────────────────────
# 1. Load and split data (stratified by user)
df = data_loader.load_df()
df_train, df_val = train_test_split(
    df, test_size=0.20, stratify=df["Added by"], random_state=42
)

X_train, y_train, class_names, _ = data_loader.prepare_data(df_train, fit_transform=True)
X_val, y_val, _, _ = data_loader.prepare_data(df_val, fit_transform=False)

if hasattr(X_train, "toarray"):  # densify if still sparse
    X_train = X_train.toarray().astype("float32")
    X_val = X_val.toarray().astype("float32")

N, P = X_train.shape
K = len(class_names)
print(f"Training   shape: {X_train.shape}")
print(f"Validation shape: {X_val.shape}")

# ──────────────────────────────────────────────────────────────────────────────
# 2. Bayesian hierarchical soft-max (same priors as before)

coords = {"obs_id": np.arange(N), "feature": np.arange(P), "class": class_names}

with pm.Model(coords=coords) as bayes_model:
    # Global
    mu_beta = pm.Normal("mu_beta", 0.0, 1.0, dims=("feature",))
    sigma_beta = pm.HalfNormal("sigma_beta", 1.0, dims=("feature",))

    mu_alpha = pm.Normal("mu_alpha", 0.0, 2.0)
    sigma_alpha = pm.HalfNormal("sigma_alpha", 2.0)

    # Non-centred
    beta_std = pm.Normal("beta_std", 0.0, 1.0, dims=("class", "feature"))
    beta = pm.Deterministic("beta",
                            mu_beta + beta_std * sigma_beta,
                            dims=("class", "feature"))

    alpha_std = pm.Normal("alpha_std", 0.0, 1.0, dims=("class",))
    alpha = pm.Deterministic("alpha",
                             mu_alpha + alpha_std * sigma_alpha,
                             dims=("class",))

    eta = alpha + pm.math.dot(X_train, beta.T)
    y_obs = pm.Categorical("y_obs", logit_p=eta, observed=y_train, dims=("obs_id",))

    trace = pm.sample(
        draws=1000,
        tune=1000,
        chains=4,
        cores=4,
        target_accept=0.95,
        max_treedepth=15,
        progressbar=True,
        random_seed=42,
    )
    az.to_netcdf(trace, POSTERIOR_PATH)
print(f"✅ PyMC posterior saved → {POSTERIOR_PATH.resolve()}")

# Bayesian validation probabilities
alpha_samp = (
    trace.posterior["alpha"]
    .stack(sample=("chain", "draw"))
    .transpose("sample", "class")
    .values
)  # (S, K)
beta_samp = (
    trace.posterior["beta"]
    .stack(sample=("chain", "draw"))
    .transpose("sample", "class", "feature")
    .values
)  # (S, K, P)

eta_val = alpha_samp[:, None, :] + np.einsum("skp,np->snk", beta_samp, X_val)
p_bayes = np.exp(eta_val - eta_val.max(axis=-1, keepdims=True))
p_bayes /= p_bayes.sum(axis=-1, keepdims=True)
p_bayes_mean = p_bayes.mean(axis=0)  # (n_val, K)

logloss_bayes = log_loss(y_val, p_bayes_mean, labels=np.arange(K))
print(f"Bayesian model val log-loss: {logloss_bayes:.4f}")

# ──────────────────────────────────────────────────────────────────────────────
# 3. Calibrated LightGBM
lgbm_base = LGBMClassifier(
    objective="multiclass",
    num_class=K,
    learning_rate=0.05,
    n_estimators=300,
    max_depth=2,            # very shallow
    num_leaves=7,           # 2^depth – 1
    min_child_samples=1,    # allow splits even with a single row
    min_gain_to_split=0.0,  # disable gain threshold
    subsample=1.0,
    colsample_bytree=1.0,
    class_weight="balanced",
    random_state=42,
)

# Use the validation split for early stopping
lgbm_base.fit(
    X_train, y_train,
    eval_set=[(X_val, y_val)],
    eval_metric="multi_logloss",
    callbacks=[
        early_stopping(stopping_rounds=20),
        log_evaluation(period=0),
    ],
)
joblib.dump(lgbm_base, LGBM_MODEL_PATH)

cal_lgbm = CalibratedClassifierCV(
    estimator=lgbm_base,
    method="sigmoid",
    cv=5,
)
cal_lgbm.fit(X_train, y_train)
joblib.dump(cal_lgbm, CALIBRATED_LGBM_MODEL_PATH)
print(f"✅ Calibrated LightGBM saved → {CALIBRATED_LGBM_MODEL_PATH.resolve()}")

p_lgbm_val = cal_lgbm.predict_proba(X_val)  # (n_val, K)
logloss_lgbm = log_loss(y_val, p_lgbm_val, labels=np.arange(K))
print(f"Calibrated LightGBM val log-loss: {logloss_lgbm:.4f}")

# ──────────────────────────────────────────────────────────────────────────────
# 4. Simple logistic stacker (Bayes + LGBM probabilities)

stack_X_val = np.hstack([p_bayes_mean, p_lgbm_val])  # (n_val, 2K)

stacker = LogisticRegression(
    multi_class="multinomial",
    penalty="l2",
    C=1.0,
    max_iter=1000,
    solver="lbfgs",
    random_state=42,
)
stacker.fit(stack_X_val, y_val)
joblib.dump(stacker, STACKER_PATH)
print(f"✅ Stacker saved → {STACKER_PATH.resolve()}")

p_stack_val = stacker.predict_proba(stack_X_val)
logloss_stack = log_loss(y_val, p_stack_val, labels=np.arange(K))
print(f"Ensemble val log-loss: {logloss_stack:.4f}")


# ──────────────────────────────────────────────────────────────────────────────
# Helper for future inference
def predict_proba(new_df: pd.DataFrame):
    """Return ensemble probabilities for new songs DataFrame."""
    # 1. Preprocess features
    preproc = joblib.load(data_loader.PREPROCESSOR_PATH)
    X_new = preproc.transform(new_df)
    if hasattr(X_new, "toarray"):
        X_new = X_new.toarray().astype("float32")

    # 2. Bayesian probs
    alpha_samp = (
        trace.posterior["alpha"]
        .stack(sample=("chain", "draw"))
        .transpose("sample", "class")
        .values
    )
    beta_samp = (
        trace.posterior["beta"]
        .stack(sample=("chain", "draw"))
        .transpose("sample", "class", "feature")
        .values
    )
    eta_new = alpha_samp[:, None, :] + np.einsum("skp,np->snk", beta_samp, X_new)
    p_bayes_new = np.exp(eta_new - eta_new.max(axis=-1, keepdims=True))
    p_bayes_new /= p_bayes_new.sum(axis=-1, keepdims=True)
    p_bayes_mean_new = p_bayes_new.mean(axis=0)  # (n_obs, K)

    # 3. LightGBM probs
    cal_lgbm = joblib.load(LGBM_MODEL_PATH)
    p_lgbm_new = cal_lgbm.predict_proba(X_new)

    # 4. Stack
    stacker = joblib.load(STACKER_PATH)
    stack_feats = np.hstack([p_bayes_mean_new, p_lgbm_new])
    return stacker.predict_proba(stack_feats)  # (n_obs, K)


if __name__ == "__main__":
    print("🏁 Pipeline finished.")
