library(nimble)
library(tidyverse)
library(coda)
library(boot)

# code fitting fixed time effects on phi and mean capture probability. 

#load("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Data/Mine/20210302_dataAugBySiteSuperpop.RData")
load("~/projects/def-pelleti2/renl2702/Phoques/20210302_dataAugBySiteSuperpop.RData")


js.pop<-nimbleCode({
  # Priors and constraints
  for (t in 1:(n.occasions-1)){
    phi[t] ~ dunif(0, 1)
  } #t
  
  for (t in 1:n.occasions){ 
    p[t] <- mean.p
  } #t
  
  mean.p ~ dunif(0, 1) # Prior for mean capture
  psi ~ dunif(0, 1) # Prior for inclusion probability
  
  
  # Dirichlet prior for entry probabilities
  for (t in 1:n.occasions){
    beta[t] ~ dgamma(1, 1)
    b[t] <- beta[t] / sum(beta[1:n.occasions]) 
  }
  
  # Convert entry probs to conditional entry probs
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
    mu1[i] <- z[i,1] * p[1] * w[i] 
    y[i,1] ~ dbern(mu1[i])
    
    # Subsequent occasions
    for (t in 2:n.occasions){
      # State process
      q[i,t-1] <- 1-z[i,t-1]
      mu2[i,t] <- phi[t-1] * z[i,t-1] + nu[t] * prod(q[i,1:(t-1)]) 
      z[i,t] ~ dbern(mu2[i,t])
      # Observation process
      mu3[i,t] <- z[i,t] * p[t] * w[i]
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
parameters <- c("psi", "mean.p", "phi", "b", "Nsuper", "N", "B", "nu", "w", "z") # added w and z to WAIC


# data select years or not 
#years<-years[years=="2019"]

dflist<-list()
for(i in 1:length(years)){ # treat each year separately
  dflist[[i]]<-get(paste0("bicData",years[i]))
}


# start model 

start <- Sys.time()
for(i in 1:length(dflist)){
  data <- list(y = as.matrix(dflist[[i]])) # make list of constants <- list(y=bicData1998)
  const <- list(M = nrow(as.matrix(dflist[[i]])), 
                n.occasions = ncol(as.matrix(dflist[[i]])))# make list of data
  z.init=data$y # added this 
  z.init[z.init==0] <- 1 # added this 
  w.init=rep(1,nrow(data$y)) # added this after example in Kery 
  inits <- function(){list(phi = runif(const$n.occasions-1, 0.7, 0.9), 
                           beta = runif(const$n.occasions,0,1),
                           mean.p = runif(1, 0.1, 0.2),
                           psi = runif(1, 0, 1), w=w.init,z = z.init)}
  
  newOut<-nimbleMCMC(
    code = js.pop,
    constants = const,
    data = data,
    WAIC=TRUE,#
    inits = inits,
    monitors = parameters,
    niter = 50000,
    nburnin = 10000,
    thin = 40, 
    nchains = 3) 
  newOut$samples <- lapply(newOut$samples,function(x) x[,!grepl('z',colnames(x))])
  write_rds(newOut,file=paste0('bic_superpop4_',years[i],'.rds'))
  rm(newOut,data)
}
dur=Sys.time()-start

