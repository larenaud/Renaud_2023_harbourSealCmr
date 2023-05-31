# script for estimating preweaning survival in harbour seal using a cjs model # code adapted from Kéry and Schaub 2012.
# model with temporal variation in capture probability and survival, identical for all individuals # modified 2022 08 24 following semiRanef script by Gab 


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
        for (t in f[i]:(n.occasions - 1)) {
                logit(phi[i,t]) <- mean.phi +
                sbw*weaned[i,t] +
                ranef.yr[OccuN[t]] # added OccuN before [t] for daily surv variation
        }

        for(t in 1:nrealOcc){
            logit(p[i, realOcc[t]]) <- beta[t]+
                weaned[i, t] * betaWeaned # +
               #  ranef.id[i] 
            beta[t]~dlogis(0,1) # priors for time-spec. recapture # added here with addition of OccuN insteand of l. 55
        }

        for(t in 1:nfakeOcc){ # including dummy 0 between real capture occasions - even spaced out
            p[i, fakeOcc[t]] <- 0
        }

        for (t in 1:n.occasions) {
            weaned[i, t] <- (captureJJ[t] - bDate[i]) > weanedAge # age # vector of 10 dates # weanedAge=constant, specified below
        } #t
       # ranef.id[i]~ dnorm(0, sd=sd.ip) # individual heterogeneity
    } #i

    for(t in 1:(nrealOcc-1)){
        ranef.yr[t]~ dnorm(0, sd=sd.yr)
        #alpha[t]~dlogis(0,1)  # beta logis removed from here instead 
    }

    betaWeaned ~ dnorm(0, 0.001)
    mean.phi ~dlogis(0, 1)# Prior for mean survival
   # mean.p ~ dlogis(0, 1) # Prior for mean capture # changed to logis since changed to logit p...
    sbw ~ dnorm(0, 0.001)
    sd.yr~ dunif(0, 5)
  #  sd.ip~ dunif(0, 5)


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
        # for (t in (f[i] + 1):(bDate[i]+weanedAge-firstOcc+1)){   # potential alternative to really truncate after weaning, but beta.weaned and sbw will not longuer be identifiaable

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
    now=as.character(years[i])
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
                              nind = nrow(dflist[[i]]$data$y),
                              n.occasions = ncol(dflist[[i]]$data$y),
                              captureJJ = allJJ,firstOcc=min(allJJ),
                              Nw =  nrow(metisData35_l[[now]]),
                              weanedAge = 30,
                              julianDay=metisData35_l[[now]]$julianDay,
                              nimbleID = metisData35_l[[now]]$nimbleID,
                              first.bd=NA,
                              fakeOcc=which(colSums(dflist[[i]]$data$y)==0),
                              realOcc=which(colSums(dflist[[i]]$data$y)>0),
                              min.bd=100 
    )
    
    dflist[[i]]$const$OccuN=rep(1:(length(dflist[[i]]$const$realOcc)),c(diff(dflist[[i]]$const$realOcc),1))# added by gab to 'force' model to ignore order of daily surv multiplication between occasions
    tmptmp=  metisData35_l[[i]] %>% group_by(myID) %>% summarise(min.bd = min(julianDay)) # pull vector of minimal dates for existing ID - their entry date
    tmptmp=  tmptmp$min.bd[match(rownames(dflist[[i]]$data$y),tmptmp$myID)]
    dflist[[i]]$const$first.bd <- ifelse(is.na(tmptmp),max(unique(metisData35_l[[i]]$julianDay)),tmptmp)
    dflist[[i]]$const$nfakeOcc=length(dflist[[i]]$const$fakeOcc)
    dflist[[i]]$const$nrealOcc=length(dflist[[i]]$const$realOcc)

    names(dflist)[[i]]=now


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
       # "mean.p",
        "mean.phi",
        "z",
        "betaWeaned",
        "beta.wt",
        "bDate",
        "mu.bd",
        "sd.bd",
        "sd.mass",
        "weanSurv",
        "ranef.yr",
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

saveRDS(outlist,file=paste0('~/projects/def-pelleti2/renl2702/phoques/outputs/cjs3_semiRanefYr_metis.rds'),compress = 'xz')
