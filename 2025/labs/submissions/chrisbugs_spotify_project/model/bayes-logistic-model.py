"""
Hey JP - this is the file for building and saving the Bayesian logistic regression model.

It imports the saved data from the `preprocessor.joblib` file, trains the model, and saves the posterior.
- Chris
"""

# TODO: NEED TO TEST THIS MODEL USING CROSS VALIDATION (similar to in bayes-v2)

import numpy as np
import pymc as pm
import arviz as az
from sklearn.model_selection import train_test_split
from sklearn.metrics import log_loss
import joblib
import pandas as pd
from pathlib import Path
from data import load as data_loader

# Posterior path
POSTERIOR_PATH = Path("bayes_softmax_posterior.nc")

# Load in data from load.py
df = data_loader.load_df()

# TTS Split: 80-20
df_train, df_val = train_test_split(
    df, test_size=0.20, stratify=df["Added by"], random_state=42
)

X_train, y_train, class_names, _ = data_loader.prepare_data(df_train, fit_transform=True)
X_val, y_val, _, _ = data_loader.prepare_data(df_val, fit_transform=False)

if hasattr(X_train, "toarray"):
    X_train = X_train.toarray().astype("float32")
    X_val = X_val.toarray().astype("float32")

N, P = X_train.shape
K = len(class_names)

coords = {
    "obs_id": np.arange(X_train.shape[0]),
    "feature": np.arange(P),
    "class": class_names,
}

# Train the model
with pm.Model(coords=coords) as model:
    # Global hyper-priors
    mu_beta = pm.Normal("mu_beta", 0.0, 1.0, dims=("feature",))  # shared mean
    sigma_beta = pm.HalfNormal("sigma_beta", 1.0, dims=("feature",))  # ridge shrinkage

    mu_alpha = pm.Normal("mu_alpha", 0.0, 2.0)  # intercept mean
    sigma_alpha = pm.HalfNormal("sigma_alpha", 2.0)  # intercept SD

    # Class-specific coefficients (partial pooling, non-centred)
    beta_std = pm.Normal("beta_std", 0.0, 1.0, dims=("class", "feature"))
    beta = pm.Deterministic("beta",
                            mu_beta + beta_std * sigma_beta,
                            dims=("class", "feature"))

    alpha_std = pm.Normal("alpha_std", 0.0, 1.0, dims=("class",))
    alpha = pm.Deterministic("alpha",
                             mu_alpha + alpha_std * sigma_alpha,
                             dims=("class",))

    # Linear predictor and soft-max
    eta = alpha + pm.math.dot(X_train, beta.T)  # shape (N, K)

    # Likelihood
    y_obs = pm.Categorical("y_obs", logit_p=eta, observed=y_train, dims=("obs_id",))

    # Sample posterior with NUTS - MCMC :)
    trace = pm.sample(
        draws=1000,
        tune=1000,
        chains=4,
        cores=4,
        target_accept=0.95,
        max_treedepth=15,
        progressbar=True,
        random_seed=42
    )

    # Save posterior to NetCDF for re-use
    az.to_netcdf(trace, POSTERIOR_PATH)

print(f"Posterior saved to {POSTERIOR_PATH.resolve()}")

# Validation log-loss
alpha_samples = (
    trace.posterior["alpha"]
    .stack(sample=("chain", "draw"))
    .transpose("sample", "class")
    .values
)  # (samples, K)

beta_samples = (
    trace.posterior["beta"]
    .stack(sample=("chain", "draw"))
    .transpose("sample", "class", "feature")
    .values
)  # (samples, K, P)

eta_val = (
    alpha_samples[:, None, :]
    + np.einsum("skp,np->snk", beta_samples, X_val)
)
p_val = np.exp(eta_val - eta_val.max(axis=-1, keepdims=True))
p_val /= p_val.sum(axis=-1, keepdims=True)

p_val_mean = p_val.mean(axis=0)   # Bayesian model averaging over draws
val_logloss = log_loss(y_val, p_val_mean, labels=np.arange(K))
print(f"Validation log-loss: {val_logloss:.4f}")

# Helper function to predict probabilities for new data
def predict_proba(new_df: pd.DataFrame):
    preproc = joblib.load(data_loader.PREPROCESSOR_PATH)
    X_new = preproc.transform(new_df)

    if hasattr(X_new, "toarray"):
        X_new = X_new.toarray().astype("float32")

    alpha_samples = (
        trace.posterior["alpha"]
        .stack(sample=("chain", "draw"))
        .transpose("sample", "class")
        .values
    )
    beta_samples = (
        trace.posterior["beta"]
        .stack(sample=("chain", "draw"))
        .transpose("sample", "class", "feature")
        .values
    )

    eta_new = (
        alpha_samples[:, None, :]
        + np.einsum("skp,np->snk", beta_samples, X_new)
    )
    p_new = np.exp(eta_new - eta_new.max(axis=-1, keepdims=True))
    p_new /= p_new.sum(axis=-1, keepdims=True)
    return p_new.mean(axis=0)      # (n_obs, K)

if __name__ == "__main__":
    print("Modelling pipeline complete – ready for predictions.")
