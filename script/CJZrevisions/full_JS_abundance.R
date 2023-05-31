# script for estimating preweaning survival in harbour seal using a cjs model 
# code adapted from Kéry and Schaub 2012.
# model fitting a constant phi and a constant p (mean.p)(m1 in model selection table)

library(dplyr)
library(magrittr)
library(nimble)
library(coda)
library(boot)
library(nimbleEcology)
library(lubridate)# function now to calculate time to run model

# mydat for all yrs -------------------------------------------------------
load("~/projects/def-pelleti2/renl2702/phoques/2023-05-12_dataJS_pup35.RData")

# nimble model
js <- nimbleCode({
    
    # Priors and constraints
    for (i in 1:M) { # individuals
        for (t in 1:(n.occasions - 1)) { # time
            logit(phi[i,t]) <- phi.betaYear[site_int[i]+1,year_int[i]]+
                phi.occ[year_int[i],t]+
                phi.sex*sex_int[i]+
                phi.betaWeaned*weaned[i,t]
            
        }
        for(t in 1:n.occasions){
            logit(p[i,t]) <- p.betaYear[site_int[i]+1,year_int[i]]+ # year effect, as factor
                weaned[i, t] * p.betaWeaned 
        }
        
        # age # vector of 10 dates # weanedAge=constant, specified below
        for (t in 1:n.occasions) {
            weaned[i, t] <- (captureJJ[t] - bDate[i]) > weanedAge 
            born[i,t] <- (captureJJ[t] - bDate[i]) > (-1)
        } #t
    } #i
    
    # add linear growth curve
    for (j in 1:Nw) {
        wt.hat[j] <- 5.9 + beta.wt * (julianDay[j] - bDate[nimbleID[j]]) 
        mass[j] ~ dnorm(wt.hat[j], sd = sd.mass)
    }
    
    # truncated distn between minimal bdate and first entry
    for (i in 1:M) {
        bDate[i] ~ T(dnorm(mu.bd[year_int[i]], sd = sd.bd), 100, first.bd[i]) 
        sex[i] ~ dbern(0.5)
    }
    
    # prior wt-BD model
    beta.wt ~ dnorm(0.5, 0.001)
    sd.mass ~ dunif(0, 5)
    sd.bd ~ dunif(1, 15)     # at least a 4 day range in BD , max 60
    sd.birthmass~ dunif(0, 3)
    # prior P
    p.betaWeaned ~ dnorm(0, 0.001)
    p.mean ~ dlogis(0, 1)
    
    p.betaSite~dlogis(0,1) # with * 0 will be reference level - additive effect of yr and site
    sd.p~dunif(0, 1.5)
    for(y in 1:16) {
        mu.bd[y]~ dnorm(130, 0.001)
        p.betaYear[1,y]~dlogis(0,1)
        p.betaYear[2,y]~dlogis(0,1)
    }
    for(y in 1:16) {
        phi.betaYear[1,y]~dlogis(0, 1)
        phi.betaYear[2,y]~dlogis(0, 1)
        for(t in 1:n.occasions){
            phi.occ[y,t] <- 0
        }
    }
    phi.betaWeaned~ dnorm(0, sd=4)
    phi.sex <- 0
    sd.phi.yr~dunif(0, 1.5)
    sd.occ~dunif(0, 1.5)
    
    # Dirichlet prior for entry probabilities
    for (t in 1:n.occasions){
        beta[t] ~ dgamma(1, 1)
        b[t] <- beta[t] / sum(beta[1:n.occasions]) 
    }
    
    # Convert entry probs to conditional entry probs
    nu[1] <- b[1]
    for (t in 2:n.occasions){
        nu[t] <- b[t] / (1 - sum(b[1:(t-1)])) 
    } #t
    
    # Likelihood
    for (i in 1:M){ # first
        # First occasion
        # State process
        w[i] ~ dbern(psi)   # Draw latent inclusion
        z[i,1] ~ dbern(nu[1]) # conditional entry prob
        # Observation process
        mu1[i] <- z[i,1] * p[i,1] * w[i] 
        y[i,1] ~ dbern(mu1[i])
        
        # Subsequent occasions
        for (t in 2:n.occasions){ # after first
            # State process
            q[i,t-1] <- 1 - (z[i,t-1]*born[i,t])
            mu2[i,t] <- (phi[i,t-1] * z[i,t-1] + nu[t] * prod(q[i,1:(t-1)]) )* born[i,t]
            z[i,t] ~ dbern(mu2[i,t])
            # Observation process
            mu3[i,t] <- z[i,t] * p[i,t] * w[i]*trueOcc[i,t]
            y[i,t] ~ dbern(mu3[i,t])
        } #t
    } #i
    
    # Calculate derived population parameters
    for (i in 1:M){
        for (t in 1:n.occasions){
            u[i,t] <- z[i,t] * w[i]     # Deflated latent state (u)
        } 
    }
    for (i in 1:M){
        recruit[i,1] <- u[i,1] 
        for (t in 2:n.occasions){
            recruit[i,t] <- (1 - u[i,t-1]) * u[i,t]
        } #t
    } #i
    for (t in 1:n.occasions){
        N[t] <- sum(u[1:M,t]) # Actual population size 
        for(year in 1:16){
        N_YerSite[year,1,t] <- sum(u[1:M,t]*(1-site_int[1:M])*(year_int[1:M]==year))
        N_YerSite[year,2,t] <- sum(u[1:M,t]*site_int[1:M]*(year_int[1:M]==year))
        }
        B[t] <- sum(recruit[1:M,t]) # Number of entries
    } #t
    for (i in 1:M){
        Nind[i] <- sum(u[i,1:n.occasions]) 
        Nalive[i] <- 1 - equals(Nind[i], 0)
    } #i
    Nsuper <- sum(Nalive[1:M]) # Superpopulation size
    for(year in 1:16){
        Nsuper_site0[year] <- sum(Nalive[1:M]*(1-site_int[1:12572])*(year_int[1:12572]==year)) # Superpopulation size
        Nsuper_site1[year] <- sum(Nalive[1:M]*site_int[1:12572]*(year_int[1:12572]==year)) # Superpopulation size
    }
    
    # derived survival from unequal occasions
    logit(dailySurv) <- mean.phi 
    weanSurv <- dailySurv^weanedAge
})




# inits -------------------------------------------------------------------


# provide other initial values for computing efficiency
z.init=df.js$data$y # added this 
z.init[z.init==0] <- 1 # added this 
w.init=rep(1,nrow(df.js$data$y)) # added this after example in Kery 

inits <- function() {
    list(
        bDate=sample(138:142,size = nrow(df.js$data$y),replace = T),
        sd.bd=runif(1,3,6),
        sd.mass=runif(1,0,1),
        mu.bd=round(rnorm(1,140,sd = 2)),
        beta.wt=rnorm(1,0.6,0.02),
        p.mean = runif(1, -1, 0.5),
        phi.betaYear=matrix(rnorm(16*2,0,0.5),nrow = 2),
        p.betaWeaned = runif(1, 0, 1),
        sd.p=runif(1,0.3,0.8),
        phi.mean=rnorm(2,0,1),
        phi.betaYear=matrix(rnorm(16*2,0,0.5),nrow = 2),
        phi.occ=matrix(rnorm(16*55,0,0.5),nrow = 16),
        phi.betaWeaned= rnorm(1, 0, 1.5),
        phi.sex= rnorm(1, 0, 1),
        sd.phi.yr=runif(1,0,0.8),
        sd.occ=runif(1,0,0.8),
        w=w.init,z = z.init,psi = runif(1, 0, 1)
    )
    
}

# parameters monitored
parameters <-
    c(
        "mean.p",
        "mean.phi",
      #  "z",
        "betaWeaned",
        "beta.wt",
        "bDate",
        "mu.bd",
        "sd.bd",
        "sd.mass",
        "weanSurv",
      #  "ranef.t",
        'dailySurv',
      'N','Nsuper','Nsuper_site0','Nsuper_site1','N_site0','N_site1','B'
        # "delta.occ",
        # "phi"
    ) # added w and z to WAIC - here z is shitty


# run model   ######
# nmodel <- nimbleModel(
#     code = js,
#     constants = df.js$const,
#     data = df.js$data,
#     inits = inits())
# nmcmc <- buildMCMC(monitors = parameters)


newOut <- nimbleMCMC(
    code = js,
    constants = df.js$const,
    data = df.js$data,
    inits = inits(),
    monitors = parameters,
    nchains = 1,
    niter = 100000, thin = 80,nburnin = 20000, # 1000 iterations left to estimate parameters
    # niter = 1100, thin = 1,nburnin = 10, # 1000 iterations left to estimate parameters
    WAIC=TRUE,
    summary = TRUE,
    samplesAsCodaMCMC = TRUE
)
 
# change model name in rds object
saveRDS(newOut,file=paste0('~/projects/def-pelleti2/renl2702/phoques/outputs/20230512_2311_js.rds'),compress = 'xz')
