library(nimble)
library(tidyverse)
library(coda)
library(boot)

# model with constant survival, time dependent entry and constant capture


# load data ---------------------------------------------------------------
rm(list = ls())
load("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Data/Mine/20211025_cmr_pup35.RData")
#load("~/projects/def-pelleti2/renl2702/Phoques/20210608_cmr_pup35.RData")
# data_mass <- pup.data35 %>% filter(mySite == "bic") %>% split(., .$year) 
#bicData35_l[["1998"]]$nimbleID <- match(bicData35_l[["1998"]]$myID, rownames(bicData35.1998), nomatch = NA_integer_, incomparables = NULL)

# first.bd <- bicData35_l[["2008"]] %>% group_by(myID) %>% summarise(min.bd = min(julianDay))  %>% pull(min.bd) # minimal bdate for existing IDs - leur date d'entrée 
# # pour les fictifs, la dernière date de naissance possible est leur dernière occasion - jj176
# first.bd.f <- rep(max(unique(bicData35_l[["2008"]]$julianDay)), 500)
# first.bd <- c(first.bd, first.bd.f)
# unique(bicData35_l[["2000"]]$date)
# unique(bicData35_l[["2000"]]$julianDay)


# explo left and tidying --------------------------------------------------
# # explo growth curve
# ggplot(bicData35_l[["2009"]], aes(x=julianDay, y = mass, colour = factor(nimbleID))) + 
#   geom_path() +geom_point()
# 
# 
# first.bd <- list()
# first.bd.aug <- list()
# 
# for(i in 1:length(years)){ # treat each year separately
#   first.bd[[i]]<-bicData35_l[[i]] %>% group_by(myID) %>% summarise(min.bd = min(julianDay)) %>% pull(min.bd) # pull vector of minimal dates for existing ID - their entry date
#   first.bd.aug[[i]] <- rep(max(unique(bicData35_l[[i]]$julianDay)), 500) # a vector of the last possible date for augmented data 
#   first.bd[[i]] <- c(first.bd[[i]], first.bd.aug[[i]])
# }
# 
# 
# 
# # # now date of firt entry 
# # foo <- function(x){which(x ==1)[1]}
# # t=apply(bicData35.1998,1,foo)



# nimble model ------------------------------------------------------------

js.pop<-nimbleCode({
# Priors and constraints
for (i in 1:M){ 
  for (t in 1:(n.occasions-1)){
    phi[i,t] <- mean.phi^delta.occ[t] # le phi pas dans un glm, direct sur la probabilité
    } #t constnat over time without delta.occ. 
  # now depends on delta.occ- l'intervalle de temps entre 2 captures- devient journalière
  
  for (t in 1:n.occasions){ 
    weaned[i,t] <- (captureJJ[t] - bDate[i]) > weanedAge # age # petti vecteur de 10 dates # weanedAge=constante
    logit(p[i,t]) <- mean.p + weaned[i,t]*betaWeaned
  } #t
} #i
betaWeaned ~ dnorm(0,0.001)
mean.phi ~ dunif(0, 1) # Prior for mean survival
mean.p ~ dlogis(0, 1) # Prior for mean capture # changed to logis since changed to logit p... 
psi ~ dunif(0, 1) # Prior for inclusion probability

# add growth curve - linear dnas le dataframe de bicData35_l [1998]

for (j in 1:Nw){# nb lines mass 1998
  wt.hat[j] <- 10 + beta.wt*(julianDay[j] - bDate[nimbleID[j]]) # by Dubé
  mass[j]~dnorm(wt.hat[j], sd= sd.mass)
}
for (i in 1:M){
  bDate[i]~T(dnorm(mu.bd, sd=sd.bd), min.bd, first.bd[i]) # distn tronquée entre bdate minimal et la premiere entrée
  }
#int.wt ~ dnorm(0,0.001) # à mettre + informatif 
beta.wt ~ dnorm(0,0.001)
mu.bd ~ dnorm(140,0.001) # à checker dans la litt
sd.mass ~ dunif(0,10)
sd.bd ~ dunif(0,20)

# first bd : vector de dte en jDay pour chaque indivdu (la minimal)
# pour les yaug = JJ de la dernière occasion = dernière sortie en mer. pour bicData351998
# c la dernière date de naissance possible de ces individus jamais capturés
# aussi vecteur de jour julien de mes occasions = 10 valeurs
 

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
  
  # derived survival from unequal occasions
for (t in 1:(n.occasions-1)){
  delta.occ[t] <- captureJJ[t+1] - captureJJ[t]
  # daily.surv[t] <- phi[t]^(1/delta.occ[t]) # ici phi constant - pareil toutes les modèles
  wean.surv[t] <- mean.phi^30 # prop de survie à 34 jours selon la survie de chaque occ. ICI on a transformé mean.phi en valeur journaliere car on a considéré plus haut le delta.occ dans mean.phi directement. 
}
}) 
# ici c la survie au 30e jour, pas un produit des survie à chaque occasion jusqu'au sevrage, parce que la durée du sevrage était pas pris en compte
# suppose que ta survie distribuée uniformément sur la durée jusqu'au sevrage + entre 2 occasions
# suppose que la survie est répartie uniformément sur la durée (50% de survie serait 25% de 1-2, puis 25% de 2-3)
# si random effect on surv - diff. specifications here. 
# si random effet temporel, aléatoire autour de la moyenne journalière de phi. et non influencée par un nombre de jour (ou un delta.occ, interval variables entre des jours)

# Parameters monitored
parameters <- c("psi", "mean.p", "mean.phi", "b", "Nsuper", "N", "B", "nu", "w", "z", 
                "betaWeaned", "beta.wt", "bDate", "mu.bd", "sd.bd", "sd.mass", "wean.surv", "delta.occ") # added w and z to WAIC




# data select years or not
 years<-years[years=="2009"]



dflist<-list()
for(i in 1:length(years)){ # treat each year separately
  now=as.character(years[i])
  dflist[[i]]<-list(
    data=list(y=as.matrix(get(paste0("bicData35.",years[i]))),
              mass=bicData35_l[[now]]$mass),
    const=list()
  )
  
  dflist[[i]]$const <- list(M = nrow(dflist[[i]]$data$y), 
                            n.occasions = ncol(dflist[[i]]$data$y),
                            captureJJ = unique(bicData35_l[[now]]$julianDay),
                            Nw =  nrow(bicData35_l[[now]]),
                            weanedAge = 30,# give a value? 
                            julianDay=bicData35_l[[now]]$julianDay,
                            # mass = bicData35_l[["2009"]]$mass, 
                            nimbleID = bicData35_l[[now]]$nimbleID,
                            
                            first.bd=NA,
                            min.bd=100 # pas de naissance en hiver
      ) 
  tmptmp=  bicData35_l[[i]] %>% group_by(myID) %>% summarise(min.bd = min(julianDay)) # pull vector of minimal dates for existing ID - their entry date
  tmptmp=  tmptmp$min.bd[match(rownames(dflist[[i]]$data$y),tmptmp$myID)]
  dflist[[i]]$const$first.bd <- ifelse(is.na(tmptmp),max(unique(bicData35_l[[i]]$julianDay)),tmptmp)
  names(dflist)[[i]]=now
  # make init part of the list 
  z.init=dflist[[i]]$data$y # added this # this must be a matrix or O and 1, not a list of data 2
  # I think I have come up with a solution.  Jags is sensitive to initial values. One tactic people take to overcome this with Jags is to try to come up with starting values that approximate true values.
  #  I did this with the z matrix by copying the detection matrix and filling that copy with 1's from the first to the last detection for each individual.  Jags ran when I used that new matrix as initial values for z.
  z.init[z.init==0] <- 1 # added this 
  w.init=rep(1,nrow(dflist[[i]]$data$y)) # added this after example in Kery 
  inits <- function(){list(mean.phi = runif(1, 0.7, 0.9),  
                           beta = runif(dflist[[i]]$const$n.occasions,0,1),
                           mean.p = runif(1, 0.1, 0.2),
                           psi = runif(1, 0, 1), 
                           w=w.init,
                           z = z.init, 
                           betaWeaned = runif(1,0,1)
  )}

 newOut<-nimbleMCMC(
    code = js.pop,
    constants = dflist[[i]]$const,
    data = dflist[[i]]$data,
    WAIC=TRUE,#
    inits = inits,
    monitors = parameters,
    nchains=2,niter=1000,thin=1,nburnin=1,
    #niter = 5000,
    #nburnin = 1000,
    #thin = 4, 
    #nchains = 3, 
    summary=TRUE, 
    samplesAsCodaMCMC=TRUE)
    #newOut$samples <- lapply(newOut$samples,function(x) x[,!grepl('z',colnames(x))])
    #write_rds(newOut,file=paste0('/Outputs/bic_superpop1_',years[i],'.rds'))
    #rm(newOut,data)
  #dur=Sys.time()-start # removed this
  }
#str(dflist[['2013']])

# # start model 
# #start <- Sys.time()
# #for(i in 1:length(dflist)){
#   data <- list(y = as.matrix(bicData35.2009), 
#                mass = bicData35_l[["2009"]]$mass, 
#                julianDay = bicData35_l[["2009"]]$julianDay
#   )# make list of data #
#   const <- list(M = nrow(as.matrix(bicData35.2009)), 
#                 n.occasions = ncol(as.matrix(bicData35.2009)),
#                 captureJJ = unique(bicData35_l[["2009"]]$julianDay),
#                 Nw =  nrow(as.matrix(bicData35_l[["2009"]])),
#                 weanedAge = 30,# give a value? 
#                # mass = bicData35_l[["2009"]]$mass, 
#                 nimbleID = bicData35_l[["2009"]]$nimbleID,
#                 first.bd=as.numeric(first.bd[[8]]),
#                 min.bd=100 # pas de naissance en hiver
#                 ) # make list of constants <- list(y=bicData2009) # removed [[i]]
#   
#   # first bd : vector de dte en jDay pour chaque indivdu (la minimal)
  # pour les yaug = JJ de la dernière occasion = dernière sortie en mer. pour bicData352009
  # c la dernièere date de naissance 

# remove junk

codaObj <-  newOut$samples # %>% map(as.mcmc) %>% as.mcmc.list()
summary(codaObj[, c("betaWeaned", "mean.phi", "mean.p", "mu.bd", 'beta.wt')])
traceplot(codaObj[, c("betaWeaned", "mean.phi", "mean.p", "mu.bd", 'Nsuper')])

colnames(newOut$samples$all.chains)
newOut[[i]]
newOut[,grepl("B", colnames(newOut[[i]][[1]]))]
summary(codaObj[,grepl("B", colnames(newOut[[i]][[1]]))])
summary(codaObj[,grepl("B", colnames(newOut[[i]][[1]]))])[[1]][,1:2] # the empirical meanand Sd
summary(codaObj[,grepl("B", colnames(newOut[[i]][[1]]))])[[2]] # the CI
summary(codaObj[,grepl("B", colnames(newOut[[i]][[1]]))])[[2]][, c(1,5)] # the 95% ci only

newOut$summary$all.chains[grepl('B'), c("Median", "95%CI_low", "95%CI_upp")]

# extract results in nice table
tab <- 
  round(
    newOut$summary$all.chains[c("betaWeaned", "mean.phi", "mean.p", "mu.bd", 'beta.wt', 'Nsuper'), c("Median", "95%CI_low", "95%CI_upp")], 
    digits = 3
  )

#     newOut$samples <- lapply(newOut$samples,function(x) x[,!grepl('z',colnames(x))])
#     write_rds(newOut,file=paste0('/Outputs/bic_superpop1_',years[i],'.rds'))
#   rm(newOut,data)
# dur=Sys.time()-start

