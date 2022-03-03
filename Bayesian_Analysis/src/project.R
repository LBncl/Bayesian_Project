# Logistic regression for Bayesian analysis scaled/not

# Libraries used
library(tidyverse)
library(rjags)
library(coda)
library(ggplot2)

model_string = "model{
  # Defining Prior Hyperparameters:
  mu_0 <- 0
  tau_0 <- 1E-2
  
  # Priors (M is the number predictors used):
  for (j in 1:M) {
    beta[j] ~ dnorm(mu_0, tau_0)
  }

  # Probability Model (N is number of data):
  for (i in 1:N) {
    mean[i] <- inprod(X[i, ], beta) # X is design matrix
    Y[i] ~ dnorm(mean[i], tau)
  }
  
  tau ~ dgamma(0.1, 0.1)
  
  # Prediction (L is the number of predictions to make):
  for (k in 1:L) {
    mean_pred[k] <- inprod(X_pred[k, ], beta)
    Y_pred[k] ~ dnorm(mean_pred[k], tau)
  }
  
}"

jags_data <- list(Y = reisby.train$hd, X = reisby.train[,3:7], X_pred = reisby.test[,3:7], 
                  N = length(reisby.train$hd), M = ncol(reisby.train[,3:7]), L = nrow(reisby.test[,3:7]))
model <- jags.model(textConnection(model_string), data = jags_data, n.chains = 4)
update(model, n.iter = 10000)
samples <- coda.samples(model = model, variable.names = c("Y_pred"), n.iter = 1000, thin = 5)

# Note // the following section is mcmc diagnostics, these have been completed and the tuning parameters above
# Have been changed accordingly

# MCMC DIAG

# mcmc as matrix
mcmc_mat = as.matrix(samples)

# ACF
par(mfrow=c(3,3))
for (i in 1:9) {
  acf(mcmc_mat[,i], lag.max=60, main=colnames(mcmc_mat)[i])
}

# Gelman
gelman.diag(samples, multivariate = FALSE)

# Cross plot
crosscorr.plot(samples)

# Trace 
par(mfrow=c(2,2))

# Extract posterior mean from MCMC output
mcmc_mean = summary(samples)$statistics[,1]
mcmc_mean[1:50] = as.integer(round(mcmc_mean[1:50], 0))


jags_data <- list(Y = reisby.train$hd, X = reisby.train[,3:7], X_pred = reisby.test[,3:7], 
                  N = length(reisby.train$hd), M = ncol(reisby.train[,3:7]), L = nrow(reisby.test[,3:7]))
model <- jags.model(textConnection(model_string), data = jags_data, n.chains = 4)
update(model, n.iter = 10000)
samples <- coda.samples(model = model, variable.names = c("beta", "tau", "Y_pred"), n.iter = 1000, thin = 5)

###############################################################
modelstring="
  model {
    k = 10^3
    b0 ~ dnorm(0, k)
    
    for (j in 1:p) {
      b[j] ~ dnorm(0, k)
    }
    
    tau ~ dgamma(0.001, 0.001)
    sd = pow(tau, -0.5)
    
    for (i in 1:N) {
      y[i] ~ dnorm(mu[i], tau)
      mu[i] = b0 + inprod(b, x[i,])
    }
}"

y = reisby$lndmi
x = reisby[,-4]
x = x[,-1]

jags_data = list(y=y, x=x, N=nrow(x), p=ncol(x))
model = jags.model(textConnection(modelstring), data=jags_data, n.chains=4)
update(model, n.iter=1000)
th = 32
samples = coda.samples(model,
                       variable.names=c("b0", "sd", "b"), thin=th,
                       n.iter=th*1000)

gelman.diag(samples, multivariate = FALSE)
crosscorr.plot(samples)
effectiveSize(samples)
plot(samples, auto.layout=FALSE, density=FALSE)
summary(samples)
HPDinterval(samples[[1]])
mcmc_mean = summary(samples)$statistics[,1]
# Trace 
par(mfrow=c(3,3))
plot(samples, auto.layout=FALSE, density=FALSE)
dic = dic.samples(model, n.iter=th*1000, thin=th)

# ACF
mcmc_mat = as.matrix(samples[[1]])
par(mfrow=c(3,3))
for (i in 1:7) {
  acf(mcmc_mat[,i], lag.max=60, main=colnames(mcmc_mat)[i])
}