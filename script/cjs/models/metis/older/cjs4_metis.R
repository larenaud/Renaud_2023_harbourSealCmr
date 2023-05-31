# script for estimating preweaning survival in harbour seal using a cjs model
# code adapted from Kéry and Schaub 2012.

# this is NOT equivalent to superpop 4.
# model fitting a daily survival variation and constant p (mean.p)

library(dplyr)
library(magrittr)
library(nimble)
library(coda)
library(boot)

# mydat for all yrs -------------------------------------------------------
load("~/projects/def-pelleti2/renl2702/phoques/20211031_cmr_pup35.RData")

# nimble model
cjs2 <- nimbleCode({

    # add linear growth curve
    for (j in 1:Nw) {
        # nb lines mass 1998
        wt.hat[j] <- 10 + beta.wt * (julianDay[j] - bDate[nimbleID[j]]) # Dubé 2003 - 10 kg at birth
        mass[j] ~ dnorm(wt.hat[j], sd = sd.mass)
    }
    for (i in 1:nind) {
        bDate[i] ~ T(dnorm(mu.bd, sd = sd.bd), min.bd, first.bd[i]) # truncated distn between minimal bdate and first entry
    }

    # Priors and constraints
    for (i in 1:nind) { # individuals
        for (t in f[i]:(n.occasions - 1)) { # occasions
                logit(phi[i,t]) <- mean.phi+
                sbw*weaned[i,t]+
                ranef.yr[t] # this is the daily survival variation
        }

        for(t in 1:nrealOcc){
            logit(p[i, realOcc[t]]) <- mean.p +
                weaned[i, t] * betaWeaned #+
               # ranef.id[i]
        }
        for(t in 1:nfakeOcc){ # including dummy 0 between real capture occasions - even spaced out
            p[i, fakeOcc[t]] <- 0
        }

        for (t in 1:n.occasions) {
            weaned[i, t] <- (captureJJ[t] - bDate[i]) > weanedAge # age # vector of 10 dates # weanedAge=constant, specified below
        } #t
       # ranef.id[i]~ dnorm(0, sd=sd.ip) # individual heterogeneity
    } #i

    for(t in 1:(n.occasions-1)){
        ranef.yr[t]~ dnorm(0, sd=sd.yr)
    }
    betaWeaned ~ dnorm(0, 0.001)
    mean.phi ~dlogis(0, 1)# Prior for mean survival
    mean.p ~ dlogis(0, 1) # Prior for mean capture # changed to logis since changed to logit p...
    sbw ~ dnorm(0, 0.001)
    sd.yr~ dunif(0, 5)
   # sd.ip~ dunif(0, 5)


    #int.wt ~ dnorm(0,0.001) # à mettre + informatif
    beta.wt ~ dnorm(0.5, 0.01)
    mu.bd ~ dnorm(130, 0.01) # à checker dans la litt
    sd.mass ~ dunif(0, 10)
    sd.bd ~ dunif(0, 20)

    # Likelihood
    for (i in 1:nind) {
        # Define latent state at first capture
        z[i, f[i]] <- 1
        for (t in (f[i] + 1):n.occasions) {
        # for (t in (f[i] + 1):(bDate[i]+weanedAge-firstOcc+1)){   # potential alternative to really truncate after weaning, but beata.weaned and sbw will not longuer be identifiaable

            # State process
            z[i, t] ~ dbern(mu1[i, t])
            mu1[i, t] <- phi[i, t - 1] * z[i, t - 1]
            # Observation process
            y[i, t] ~ dbern(mu2[i, t])
            mu2[i, t] <- p[i, t] * z[i, t]
        } #t
    } #i

    # derived survival from unequal occasions
    logit(dailySurv) <- mean.phi
    weanSurv <- dailySurv^weanedAge
})


# Bundle data -loop over all years, independantly
dflist<-list()
outlist <- list()
for(i in 1:16){ # treat each year separately
    # i=6
    now=as.character(years[i])
    #mydata <- list_ch_metis[[now]]
    captureJJ = unique(metisData35_l[[now]]$julianDay)
    allJJ <- seq(min(captureJJ),max(captureJJ))
    ch_dum <- matrix(0,nrow = nrow(list_ch_metis[[now]]),ncol = length(allJJ))
    tmp <- match(captureJJ, allJJ)
    for(i2 in 1:length(tmp)){
        ch_dum[,tmp[i2]] <- list_ch_metis[[now]][,i2]
    }
    rownames(ch_dum) <- rownames(list_ch_metis[[now]])
    get.first <- function(x)    min(which(x != 0))

    dflist[[i]]<-list(
        data=list(y=as.matrix(ch_dum),
                  mass=metisData35_l[[now]]$mass),
        const=list()
    )
    dflist[[i]]$const <- list(f=apply(dflist[[i]]$data$y, 1,function(x) get.first(x)),
                              nind = nrow(dflist[[i]]$data$y), #  nind = dim(mydata)[1],
                              n.occasions = ncol(dflist[[i]]$data$y), #  n.occasions = dim(mydata)[2],
                              captureJJ = allJJ,firstOcc=min(allJJ),
                              Nw =  nrow(metisData35_l[[now]]),
                              weanedAge = 30,# give a value
                              julianDay=metisData35_l[[now]]$julianDay,
                              # mass = metisData35_l[["2009"]]$mass,
                              nimbleID = metisData35_l[[now]]$nimbleID,
                              first.bd=NA,
                              fakeOcc=which(colSums(dflist[[i]]$data$y)==0),
                              realOcc=which(colSums(dflist[[i]]$data$y)>0),
                              min.bd=100 # pas de naissance en hiver
    )

    tmptmp=  metisData35_l[[i]] %>% group_by(myID) %>% summarise(min.bd = min(julianDay)) # pull vector of minimal dates for existing ID - their entry date
    tmptmp=  tmptmp$min.bd[match(rownames(dflist[[i]]$data$y),tmptmp$myID)]
    dflist[[i]]$const$first.bd <- ifelse(is.na(tmptmp),max(unique(metisData35_l[[i]]$julianDay)),tmptmp)
    dflist[[i]]$const$nfakeOcc=length(dflist[[i]]$const$fakeOcc)
    dflist[[i]]$const$nrealOcc=length(dflist[[i]]$const$realOcc)

    names(dflist)[[i]]=now


# In JAGS we have to give good initial values for the latent state z.
# Kery and Schaub 2011
# At all occasions when an individual was observed, its state is z = 1 for sure.
# If an individual was not observed at an occasion, but was alive for sure, because it was observed before and thereafter (i.e. has a capture history of e.g. {101} or {10001}),
    # then we know that the individual was alive at all of these occasions, and thus z = 1.
    # Therefore, we should provide initial values of z = 1 at these positions as well.
    # The following function provides such initial values from the observed capture histories:
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

# (Note that the function known.state.cjs is used in section 7.3.1 of Kery and Schaub 2011 for another purpose)

inits <- function() {
    list(
        mean.phi = rnorm(1, 5, 1),
        mean.p = runif(1, -1, 0.5),
        z = known.state.cjs(dflist[[i]]$data$y),
        #beta = runif(constants$n.occasions,0,1),
        bDate=sample(138:142,size = nrow(dflist[[i]]$data$y),replace = T),
        sd.bd=runif(1,0,2),
        sd.mass=runif(1,0,1),
        mu.bd=round(rnorm(1,140,sd = 2)),
        # wt.hat=ifelse(is.na(dflist[[i]]$data$mass),rnorm(dflist[[i]]$const$Nw,16,1),1),
        beta.wt=rnorm(1,0.6,0.02),
        betaWeaned = runif(1, 0, 1)
    )
}
# parameters monitored
parameters <-
    c(
        "mean.p",
        "mean.phi",
        "z",
        "betaWeaned",
        "beta.wt",
        "bDate",
        "mu.bd",
        "sd.bd",
        "sd.mass",
        "weanSurv",
        'dailySurv'
        # "delta.occ",
        # "phi"
    ) # added w and z to WAIC - here z is shitty

newOut <- nimbleMCMC(
    code = cjs2,
    constants = dflist[[i]]$const,
    data = dflist[[i]]$data,
    inits = inits(),
    monitors = parameters,
    nchains = 3,
    niter = 500000, thin = 400,nburnin = 100000,
    # niter = 100000, thin = 80,nburnin = 20000,
    WAIC=TRUE,
    summary = TRUE,
    samplesAsCodaMCMC = TRUE
)

# newOut$samples <- lapply(newOut$samples,function(x) x[,!grepl('z',colnames(x))])

# rm(newOut,data)
outlist[[i]] <- newOut
print('done with:')
print(i)
}

#saveRDS(outlist,file=paste0('/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output/data/metis_cjs_Gab2.rds'),compress = 'xz')
saveRDS(outlist,file=paste0('~/projects/def-pelleti2/renl2702/phoques/outputs/cjs4_metis.rds'),compress = 'xz')
