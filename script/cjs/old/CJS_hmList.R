# script for estimating preweaning survival in harbour seal using a cjs model
# modified 2022-02-15


cjs <- nimbleCode({
    # Priors and constraints
    for (i in 1:nind) {
        for (t in f[i]:(n.occasions - 1)) {
            phi[i, t] <- pho[i,t] ^ (delta.occ[t]/30) # preweaning survival is derived from daily survival, and unequal occasions
            logit(pho[i,t]) <- mean.phi+sbw*weaned[i,t]+ranef.yr[t] # survival depends on a growth curve and age at weaning
        }
        
        for (t in 1:n.occasions) {
            weaned[i, t] <- (captureJJ[t] - bDate[i]) > weanedAge # weaned age is estimated from a vector of 10 dates # weanedAge=constant
            logit(p[i, t]) <- mean.p + weaned[i, t] * betaWeaned+ranef.id[i]
        } #t
        ranef.id[i]~ dnorm(0, sd=sd.ip)
    } #i
    for(t in 1:(n.occasions-1)){
        ranef.yr[t]~ dnorm(0, sd=sd.yr) 
    }
    betaWeaned ~ dnorm(0, 0.001)
    mean.phi ~dlogis(0, 1)# Prior for mean survival
    mean.p ~ dlogis(0, 1) # Prior for mean capture
    sbw ~ dnorm(0, 0.001)
    sd.yr~ dunif(0, 5)
    sd.ip~ dunif(0, 5)
    
    logit(wean.surv)<-mean.phi

    # add a linear growth curve for pups - linear growth as a function of birth date 
    for (j in 1:Nw) {
        wt.hat[j] <- 10 + beta.wt * (julianDay[j] - bDate[nimbleID[j]])
        mass[j] ~ dnorm(wt.hat[j], sd = sd.mass)
    }
    for (i in 1:nind) {
        bDate[i] ~ T(dnorm(mu.bd, sd = sd.bd), min.bd, first.bd[i]) # truncated birthdate distn (between minimal bdate and first entry)
    }
    
    beta.wt ~ dnorm(0, 0.001)
    mu.bd ~ dnorm(140, 0.001) 
    sd.mass ~ dunif(0, 10)
    sd.bd ~ dunif(0, 20)
    
    # first.bd : date vector, in julian day, for each individual (the minimal one)
    # last possible birthdate for these individuals never captured
    # also factor of julian day for my occasions - 10 values
    
    # Likelihood
    for (i in 1:nind) {
        # Define latent state at first capture
        z[i, f[i]] <- 1
        for (t in (f[i] + 1):n.occasions) {
            # State process
            z[i, t] ~ dbern(mu1[i, t])
            mu1[i, t] <- phi[i, t - 1] * z[i, t - 1]
            # Observation process
            y[i, t] ~ dbern(mu2[i, t])
            mu2[i, t] <- p[i, t - 1] * z[i, t]
        } #t
    } #i
    
    # derived survival from unequal occasions
    for (t in 1:(n.occasions-1)) {
        delta.occ[t] <- captureJJ[t + 1] - captureJJ[t]
            }
})
