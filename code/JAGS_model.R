
# WORK IN PROGRESS

jus_mod <- "model {

# Likelihood

  for (n in 1:N){
    y[n] ~ dbern(p[n])
    logit(p[n]) <- a[case[n]] + b[just[n]] + gamma[term[n]] * R[just[n]]
    
    y.rep[n] ~ dbern(p[n])
  }

# case

  for (i in 1:ncase) {
    mu.a[i] <- inprod(beta[], X[i,])
    a[i] ~ dnorm(0, tau.b)
    
  }

# justice

  for(j in 1:njust){
    b[j] ~ dnorm(0, tau.b)
  }

# time partisanship


# priors

  for (k in 1:K){
    beta[k] ~ dnorm(0, 0.04)
  
  }


}"










