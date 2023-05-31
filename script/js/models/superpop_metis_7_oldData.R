library(nimble)
library(tidyverse)
library(coda)
library(boot)

load("~/projects/def-pelleti2/renl2702/phoques/20210608_cmr_pup35.RData")

# model with constant survival, time dependent entry and capture heterogeneity
# with T occasions, T entry probabilities must be defined and they have to sum to 1. 

js.pop<-nimbleCode({
# Priors and constraints
for (i in 1:M){
  for (t in 1:(n.occasions-1)){
    phi[i,t] <- mean.phi } #t
  
  for (t in 1:n.occasions){ 
    logit(p[i,t]) <- mean.lp + epsilon[i]
  } #t
} #i
  
mean.phi ~ dunif(0, 1) # Prior for mean survival
mean.lp <- log(mean.p/(1-mean.p))
mean.p ~ dunif(0, 1) # Prior for mean capture
for (i in 1:M){
  epsilon[i]~T(dnorm(0, tau),-15,15)
}
tau<-1/(sd*sd) # prior for survival
sd~dunif(0,10)
sd2 <- (sd*sd) # l'utilise pas dans son modèle - pour comparer aux données simulées
psi ~ dunif(0, 1) # Prior for inclusion probability


# Dirichlet prior for entry probabilities
for (t in 1:n.occasions){
  beta[t] ~ dgamma(1, 1)
  b[t] <- beta[t] / sum(beta[1:n.occasions]) 
}

# Convert entry probs to conditional entry probs # a time effect on entry prob
nu[1] <- b[1]
for (t in 2:n.occasions){
  nu[t] <- b[t] / (1-sum(b[1:(t-1)])) 
  } #t


# Likelihood
for (i in 1:M){
  # First occasion
  # State process
  w[i] ~ dbern(psi)   # Draw latent inclusion
  z[i,1] ~ dbern(nu[1])
  # Observation process
  mu1[i] <- z[i,1] * p[i,1] * w[i] 
  y[i,1] ~ dbern(mu1[i])
  
  # Subsequent occasions
  for (t in 2:n.occasions){
    # State process
    q[i,t-1] <- 1-z[i,t-1]
    mu2[i,t] <- phi[i,t-1] * z[i,t-1] + nu[t] * prod(q[i,1:(t-1)]) 
    z[i,t] ~ dbern(mu2[i,t])
    # Observation process
    mu3[i,t] <- z[i,t] * p[i,t] * w[i]
    y[i,t] ~ dbern(mu3[i,t])
  } #t
} #i


# Calculate derived population parameters
for (i in 1:M){
  for (t in 1:n.occasions){
    u[i,t] <- z[i,t]*w[i]     # Deflated latent state (u)
  } 
}
for (i in 1:M){
  recruit[i,1] <- u[i,1] 
  for (t in 2:n.occasions){
    recruit[i,t] <- (1-u[i,t-1]) * u[i,t]
  } #t
} #i
  for (t in 1:n.occasions){
    N[t] <- sum(u[1:M,t]) # Actual population size 
    B[t] <- sum(recruit[1:M,t]) # Number of entries
  } #t
  for (i in 1:M){
    Nind[i] <- sum(u[i,1:n.occasions]) 
    Nalive[i] <- 1-equals(Nind[i], 0)
  } #i
  Nsuper <- sum(Nalive[1:M]) # Superpopulation size
})


# Parameters monitored
parameters <- c("psi", "mean.p", "mean.phi", "b", "Nsuper", "N", "B", "nu", "w", "z", "epsilon", "sd", "sd2") # added w and z to WAIC


# data select years or not 
#years<-years[years=="2009"]

dflist<-list()
for(i in 1:length(years)){ # treat each year separately
  dflist[[i]]<-get(paste0("metisData35.",years[i]))
}


# dflist<-list()
# for(i in 1:length(years)){ # treat each year separately
#   now <- as.character(years[i])
#   dflist[[i]]<-list_ch_metis[[now]]
# }



# start model 

start <- Sys.time()
for(i in 1:length(dflist)){
  data <- list(y = as.matrix(dflist[[i]])) # make list of constants <- list(y=metisData1998)
  const <- list(M = nrow(as.matrix(dflist[[i]])), 
                n.occasions = ncol(as.matrix(dflist[[i]])))# make list of data
  z.init=data$y # added this 
  z.init[z.init==0] <- 1 # added this 
  w.init=rep(1,nrow(data$y)) # added this after example in Kery 
  inits <- function(){list(mean.phi = runif(1, 0.7, 0.9),  
                           beta = runif(const$n.occasions,0,1),
                           mean.p = runif(1, 0.1, 0.2),
                          # sd=runif(1,0,1),
                           psi = runif(1, 0, 1), w=w.init,z = z.init)}
  
  newOut<-nimbleMCMC(
    code = js.pop,
    constants = const,
    data = data,
    WAIC=TRUE, # needs to be changed
    summary = TRUE,
    samplesAsCodaMCMC = TRUE,
    inits = inits,
    monitors = parameters,
    niter = 50000,
    nburnin = 10000,
    thin = 40, #  at 3 = keeps to much mem ??
    nchains = 3)                                             # Run nimble model
    newOut$samples <- lapply(newOut$samples,function(x) x[,!grepl('z',colnames(x))])
    write_rds(newOut,file=paste0('outputs/metis_superpop7_oldData',years[i],'.rds'))
  rm(newOut,data)
}
dur=Sys.time()-start

