"""
Hey JP -> instructions are in load.py to load the data and run the model
"""

import sys
from pathlib import Path

# ensure the repo root is on the import path
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

import numpy as np
import pymc as pm
import arviz as az
import joblib
import pandas as pd

from data import load as data_loader  # <- your load.py module

# ---------------------------------------------------------------------
POSTERIOR_PATH = Path("model") / "final_model.nc"
SIGMA_BETA = 1.0  # best σβ  (feature-level scale)
SIGMA_ALPHA = 2.0  # best σα  (intercept scale)
DRAWS = 1500  # per chain
TUNE = 1500
CHAINS = 4
CORES = 4
TARGET_ACCEPT = 0.95
SEED = 42


# ---------------------------------------------------------------------


def fit_and_save():
    """Fit the hierarchical soft-max on the full data set and save posterior."""
    # 1. Load & preprocess (fit_transform=True writes preprocessor.joblib)
    df = data_loader.load_df()
    X, y, class_names, _ = data_loader.prepare_data(df, fit_transform=True)

    if hasattr(X, "toarray"):  # densify just in case
        X = X.toarray().astype("float32")

    N, P = X.shape
    K = len(class_names)
    coords = {
        "obs_id": np.arange(N),
        "feature": np.arange(P),
        "class": class_names,
    }

    # 2. Build & sample model
    with pm.Model(coords=coords) as model:
        mu_beta = pm.Normal("mu_beta", 0.0, 1.0, dims=("feature",))
        sigma_beta = pm.HalfNormal("sigma_beta", SIGMA_BETA, dims=("feature",))

        mu_alpha = pm.Normal("mu_alpha", 0.0, 2.0)
        sigma_alpha = pm.HalfNormal("sigma_alpha", SIGMA_ALPHA)

        # non-centred class-specific effects
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

        trace = pm.sample(
            draws=DRAWS,
            tune=TUNE,
            chains=CHAINS,
            cores=CORES,
            target_accept=TARGET_ACCEPT,
            max_treedepth=15,
            random_seed=SEED,
            progressbar=True,
            idata_kwargs={"log_likelihood": False},
        )

        az.to_netcdf(trace, POSTERIOR_PATH)
    print(f"✅ Posterior saved to {POSTERIOR_PATH.resolve()}")


# ---------------------------------------------------------------------
def _load_posterior():
    if not POSTERIOR_PATH.exists():
        raise FileNotFoundError(
            "final_model.nc not found. Run `python final_model.py` first."
        )
    return az.from_netcdf(POSTERIOR_PATH)


def _posterior_mean_probs(trace, X: np.ndarray) -> np.ndarray:
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


def predict_proba(new_df: pd.DataFrame) -> np.ndarray:
    """
    Return posterior-averaged class probabilities for each row in `new_df`.
    """
    preproc = joblib.load(data_loader.PREPROCESSOR_PATH)
    X_new = preproc.transform(new_df)
    if hasattr(X_new, "toarray"):
        X_new = X_new.toarray().astype("float32")

    trace = _load_posterior()
    return _posterior_mean_probs(trace, X_new)


# ---------------------------------------------------------------------
if __name__ == "__main__":
    fit_and_save()
