



jus_model <- "model {

  # Likelihood
  for (n in 1:N) {
    y[n] ~ dbern(p[n])
    logit(p[n]) <- a[case[n]] + b[just[n]] + gamma[term[n]] * R[just[n]]
  }

  # Case effects (case details enter through X %*% beta)
  for (i in 1:nCase) {
    mu.a[i] <- inprod(beta[], X[i, ])
    a[i] ~ dnorm(mu.a[i], tau.a)
  }

  # Justice baseline lean
  for (j in 1:nJust) {
    b[j] ~ dnorm(0, tau.b)
  }

  # Time-varying partisanship effect (random walk across terms)
  gamma[1] ~ dnorm(0, 1)
  for (t in 2:nTerm) {
    gamma[t] ~ dnorm(gamma[t - 1], tau.gamma)
  }

  # Priors
  for (k in 1:K) {
    beta[k] ~ dnorm(0, 0.4)
  }
  sd.a ~ dunif(0, 3)
  sd.b ~ dunif(0, 3)
  sd.gamma ~ dunif(0, 1)
  tau.a <- pow(sd.a, -2)
  tau.b <- pow(sd.b, -2)
  tau.gamma <- pow(sd.gamma, -2)
}
"



