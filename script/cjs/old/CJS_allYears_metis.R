# script for estimating preweaning survival in harbour seal using a cjs model
# code adapted from Kéry and Schaub 2012.

library(nimble)
library(tidyverse)
library(coda)
library(boot)

# mydat for all yrs -------------------------------------------------------
load("~/projects/def-pelleti2/renl2702/phoques/20211031_cmr_pup35.RData")
# load(
#     "/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Data/Mine/20211031_cmr_pup35.RData"
# )

# nimble model
cjs2 <- nimbleCode({
    # Priors and constraints
    for (i in 1:nind) {
        for (t in f[i]:(n.occasions - 1)) {
            phi[i, t] <- pho[i,t] ^ (delta.occ[t]/30)
            logit(pho[i,t]) <- mean.phi+sbw*weaned[i,t]+ranef.yr[t]
        }
        
        for (t in 1:n.occasions) {
            weaned[i, t] <- (captureJJ[t] - bDate[i]) > weanedAge # age # petti vecteur de 10 dates # weanedAge=constante
            logit(p[i, t]) <- mean.p + weaned[i, t] * betaWeaned+ranef.id[i]
        } #t
        ranef.id[i]~ dnorm(0, sd=sd.ip)
    } #i
    for(t in 1:(n.occasions-1)){
        ranef.yr[t]~ dnorm(0, sd=sd.yr)
    }
    betaWeaned ~ dnorm(0, 0.001)
    mean.phi ~dlogis(0, 1)# Prior for mean survival
    mean.p ~ dlogis(0, 1) # Prior for mean capture # changed to logis since changed to logit p...
    sbw ~ dnorm(0, 0.001)
    sd.yr~ dunif(0, 5)
    sd.ip~ dunif(0, 5)
    
    logit(wean.surv)<-mean.phi
    # tmp1<-wean.surv^(1/30)
    # mean.phi<-logit(tmp1)
    
    
    # add growth curve - linear
    for (j in 1:Nw) {
        # nb lines mass 1998
        wt.hat[j] <- 10 + beta.wt * (julianDay[j] - bDate[nimbleID[j]]) # by Dubé
        mass[j] ~ dnorm(wt.hat[j], sd = sd.mass)
    }
    for (i in 1:nind) {
        bDate[i] ~ T(dnorm(mu.bd, sd = sd.bd), min.bd, first.bd[i]) # distn tronquée entre bdate minimal et la premiere entrée
    }
    #int.wt ~ dnorm(0,0.001) # à mettre + informatif
    beta.wt ~ dnorm(0, 0.001)
    mu.bd ~ dnorm(140, 0.001) # à checker dans la litt
    sd.mass ~ dunif(0, 10)
    sd.bd ~ dunif(0, 20)
    
    # first bd : vector de dte en jDay pour chaque indivdu (la minimal)
    # pour les yaug = JJ de la dernière occasion = dernière sortie en mer. pour metisData351998
    # c la dernière date de naissance possible de ces individus jamais capturés
    # aussi vecteur de jour julien de mes occasions = 10 valeurs
    
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
        # daily.surv[t] <- phi[t]^(1/delta.occ[t]) # ici phi constant - pareil toutes les modèles
            }
})


# # Bundle data
dflist<-list()
for(i in 1:length(years)){ # treat each year separately
    now=as.character(years[i])
    #mydata <- list_ch_metis[[now]]
    get.first <- function(x)
        min(which(x != 0))
    f <- apply(list_ch_metis[[now]], 1, get.first)
    
    dflist[[i]]<-list(
        data=list(y=as.matrix(list_ch_metis[[now]]),
                  mass=metisData35_l[[now]]$mass),
        const=list()
    )
    dflist[[i]]$const <- list(f=f, 
                              nind = nrow(dflist[[i]]$data$y), #  nind = dim(mydata)[1],
                              n.occasions = ncol(dflist[[i]]$data$y), #  n.occasions = dim(mydata)[2],
                              captureJJ = unique(metisData35_l[[now]]$julianDay),
                              Nw =  nrow(metisData35_l[[now]]),
                              weanedAge = 30,# give a value? 
                              julianDay=metisData35_l[[now]]$julianDay,
                              # mass = metisData35_l[["2009"]]$mass, 
                              nimbleID = metisData35_l[[now]]$nimbleID,
                              first.bd=NA,
                              min.bd=100 # pas de naissance en hiver
    ) 
   
    tmptmp=  metisData35_l[[i]] %>% group_by(myID) %>% summarise(min.bd = min(julianDay)) # pull vector of minimal dates for existing ID - their entry date
    tmptmp=  tmptmp$min.bd[match(rownames(dflist[[i]]$data$y),tmptmp$myID)]
    dflist[[i]]$const$first.bd <- ifelse(is.na(tmptmp),max(unique(metisData35_l[[i]]$julianDay)),tmptmp)
    names(dflist)[[i]]=now
# In JAGS we have to give good initial values for the latent state z. At all occasions when an individual was observed, its state is z = 1 for sure. In addition, if an individual was not observed at an occasion, but was alive for sure, because it was observed before and thereafter (i.e. has a capture history of e.g. {101} or {10001}), then we know that the individual was alive at all of these occasions, and thus z = 1. Therefore, we should provide initial values of z = 1 at these positions as well. The following function provides such initial values from the observed capture histories:
known.state.cjs <- function(ch) {
    state <- ch
    for (i in 1:dim(ch)[1]) {
        n1 <- min(which(ch[i, ] == 1))
        n2 <- max(which(ch[i, ] == 1))
        state[i, 1:n2] <- 1 # added 1 but still NAs in Z
        state[i, n1] <- NA
    }
    state[state == 0] <- NA
    return(state)
}

# (Note that the function known.state.cjs is used in section 7.3.1 as well for another purpose)
inits <- function() {
    list(
        mean.phi = runif(1, 0, 1),
        mean.p = runif(1, 0, 1),
        z = known.state.cjs(dflist[[i]]$data$y),
        #beta = runif(constants$n.occasions,0,1),
        bDate=rep(130,nrow(dflist[[i]]$data$y)),
        sd.bd=runif(1,0,2),
        sd.mass=runif(1,0,1),
        mu.bd=rnorm(1,140,2),
        wt.hat=rnorm(dflist[[i]]$const$Nw,16,1),
        betaWeaned = runif(1, 0, 1)
    )
}
# parameters monitored
parameters <-
    c(
        "mean.p",
        "mean.phi",
        #"z",
        "betaWeaned",
        "beta.wt",
        "bDate",
        "mu.bd",
        "sd.bd",
        "sd.mass",
        "wean.surv",
        "delta.occ"
    ) # added w and z to WAIC - here z is shitty

newOut <- nimbleMCMC(
    code = cjs2,
    constants = dflist[[i]]$const,
    data = dflist[[i]]$data,
    #WAIC=TRUE,#
    inits = inits,
    monitors = parameters,
    nchains = 3,
    niter = 100000,
    thin = 80,
    nburnin = 20000,
    summary = TRUE,
    samplesAsCodaMCMC = TRUE
)
newOut$samples <- lapply(newOut$samples,function(x) x[,!grepl('z',colnames(x))])
saveRDS(newOut,file=paste0('~/projects/def-pelleti2/renl2702/phoques/outputs/metis_cjs_',years[i],'.rds'))
rm(newOut,data)

}