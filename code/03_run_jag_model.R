

set.seed(618)

# Params ----

nChains = 3
burnIn = 1000
thin = 1
nDraws = 5000

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


# to save MCMC draws ----

# Save the posterior draws
#dir.create("output", showWarnings = FALSE)
#saveRDS(post, "output/post_prelim.rds")

# Later, in a new session
#library(coda)                      # load coda first so the mcmc.list class is recognized
#post <- readRDS("output/post_prelim.rds")






