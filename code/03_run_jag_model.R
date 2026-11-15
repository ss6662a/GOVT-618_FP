

set.seed(111426)

# Params ----

nChains = 1
burnIn = 1000
thin = 2
nDraws = 2000

# Model ----

jm <- jags.model(
  textConnection(jus_model),
  data = dat,
  n.chains = nChains
  )

update(jm, n.iter = burnIn) # burn-in

post <- coda.samples( # draws
  jm,
  variable.names = c("beta", "sd.a", "sd.b", "sd.gamma", "gamma", "b"),
  n.iter = nDraws,
  thin = thin
)

# Convergence Diagnostics ----
gelman.diag(post[, c("sd.a", "sd.b", "sd.gamma")], multivariate = FALSE)
effectiveSize(post[, c("sd.a", "sd.b", "sd.gamma")])
plot(post[, c("sd.gamma", "sd.a", "sd.b")]) # trace plots


