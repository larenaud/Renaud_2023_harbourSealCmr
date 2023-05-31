# extract mcmc.list from model outputs
#install.packages("tidyverse")
#install.packages("coda")
#install.packages("kableExtra")
#install.packages("kableExtra")
library(kableExtra)
library(tidyverse)
library(coda)
library(igraph)
library(ggthemes)
library(pander)
library(ggpubr)
library(gridExtra)
library(grid)
library(lubridate)
rm(list = ls())


# reminder of what we should monitor here 
# n.occasions : number of capture occasions
# Nsuper : superpopulation size 
# phi : survival probabilities
# b : entry probabilities 
# p : capture probabilities 
# gamma : removal entry probability 
# mean.phi : mean surv
# mean.p : mean capture 
# N : actual population sizse 
# B : number of entries (recuits?? )


# exemple to extract .rds files from one folder at a time  ----------------------------------
# change directory as needed 
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Outputs.tmp") # here example 
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/bic/20210614/bic7") # here example 

# parameters <- c("p", "mean.phi", "b", "Nsuper", "N", "B", "z", "gamma")
file <-list.files(pattern = ".rds")
fd<-readRDS("bic_superpop7_1998.rds")
df <- data.frame(fd$samples %>% map(as.data.frame) %>% bind_rows())
df_l <- df %>% select(Nsuper, mean.phi, mean.p) %>% # select the 'right' raw parameters you monitored in the corresponding model
  mutate(chain=rep(1:length(fd$samples),each=1000),
         it=rep(1:1000,3)) %>% # ATTENTION bon seulement si tu gardes toujours le même nombre d'itérations - 1000 
  gather(key="parameter", value="value",-chain,-it)

ps <- df_l %>% ggplot(aes(x=it, y = value,color=chain)) + geom_line()
ps + facet_wrap(~parameter, scales = "free")
p <- ggplot(df_l,aes(value)) + geom_histogram(aes( y= ..density..),bins = 60)
p + facet_wrap(~parameter, scales = "free")


# test gelman rubin for convergence as loop -------------------------------------------------------

# change directory as appropriate
setwd("/Users/LimoilouARenaud/Documents/Scolarite/PostDocI/Phoque/Outputs.tmp/20210614/bic7")

# extract files 
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
}

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() 
}

#gelmanRubin <- gelman.diag(codaSamp[[3]][,'Nsuper'])
#gelmanRubin <- gelman.diag(codaSamp[[10]][,'Nsuper'])

gelmanRubin <- vector("list",length=16)

for(i in 1:length(gelmanRubin)){
  gelmanRubin[[i]] <- gelman.diag(codaSamp[[i]][,'mean.phi']) # not the good one I guess
}

# loop - convergence check for bic -------------------------------------------------
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Outputs.tmp/20210304/bic1")
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
}

#parameters <- c("psi", "mean.p", "mean.phi", "b", "Nsuper", "N", "B", "nu", "w", "z", "epsilon", "sd", "sd2") # added w and z to WAIC

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list()                    # transform to coda
  #plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y_", substr(file, 14, 18))[[i]])
plot(codaSamp[[i]][,'mean.p'], main = paste0("mean.p_y_", substr(file, 14, 18))[[i]])
#plot(codaSamp[[i]][,'mean.phi'], main = paste0("mean.phi_y_", substr(file, 14, 18))[[i]]) # plot chain # my last year did not converge well
 #plot(codaSamp[[i]][,grepl("B", colnames(codaSamp[[i]][[1]]))], main = paste0("B_y_", substr(file, 14, 18))[[i]]) # l nomb de la premieère chaine (pareille partout0)
  #plot(codaSamp[[i]][,grepl("b", colnames(codaSamp[[i]][[1]]))], main = paste0("b_y_", substr(file, 14, 18))[[i]])
  #plot(codaSamp[[i]][,grepl("N", colnames(codaSamp[[i]][[1]]))], main = paste0("N_y_", substr(file, 14, 18))[[i]])
}


setwd("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Outputs.tmp/20210304/bic2")
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
}
codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list()                    # transform to coda
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y_", substr(file, 13, 16))[[i]])
  #plot(codaSamp[[i]][,'mean.p'], main = paste0("mean.p_y_", substr(file, 12, 15))[[i]])
  plot(codaSamp[[i]][,'mean.phi'], main = paste0("mean.phi_y_", substr(file, 13, 16))[[i]]) # plot chain # my last year did not converge well
  plot(codaSamp[[i]][,grepl("p", colnames(codaSamp[[i]][[1]]))], main = paste0("p_y_", substr(file, 13, 16))[[i]])
  plot(codaSamp[[i]][,grepl("B", colnames(codaSamp[[i]][[1]]))], main = paste0("B_y_", substr(file,13, 16))[[i]]) # l nomb de la premieère chaine (pareille partout0)
  plot(codaSamp[[i]][,grepl("b", colnames(codaSamp[[i]][[1]]))], main = paste0("b_y_", substr(file,13, 16))[[i]])
  plot(codaSamp[[i]][,grepl("N", colnames(codaSamp[[i]][[1]]))], main = paste0("N_y_", substr(file, 13, 16))[[i]])
}



setwd("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Outputs.tmp/20210304/bic3")
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
}

colnames(codaSamp[[1]][[1]])

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list()                    # transform to coda
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("Nsuper_y_", substr(file, 13, 16))[[i]])
  plot(codaSamp[[i]][,grepl("phi", colnames(codaSamp[[i]][[1]]))], main = paste0("phi_y_", substr(file, 13, 16))[[i]])
  plot(codaSamp[[i]][,grepl("p", colnames(codaSamp[[i]][[1]]))], main = paste0("p_y_", substr(file,13, 16))[[i]])
  plot(codaSamp[[i]][,grepl("B", colnames(codaSamp[[i]][[1]]))], main = paste0("B_y_", substr(file,13, 16))[[i]]) # l nomb de la premieère chaine (pareille partout0)
  plot(codaSamp[[i]][,grepl("b", colnames(codaSamp[[i]][[1]]))], main = paste0("b_y_", substr(file,13, 16))[[i]])
  plot(codaSamp[[i]][,grepl("N", colnames(codaSamp[[i]][[1]]))], main = paste0("N_y_", substr(file, 13, 16))[[i]])
  #plot(codaSamp[[i]][,grepl("gamma", colnames(codaSamp[[i]][[1]]))], main = paste0("gamma_y_", substr(file, 13, 16))[[i]])
}



setwd("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Outputs.tmp/20210304/bic4")
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
}

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list()                    # transform to coda
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("Nsuper_y_", substr(file, 13, 16))[[i]])
  plot(codaSamp[[i]][,'mean.p'], main = paste0("p_y_", substr(file, 13, 16))[[i]])
  plot(codaSamp[[i]][,grepl("phi", colnames(codaSamp[[i]][[1]]))], main = paste0("phi_y_", substr(file, 13, 16))[[i]])
  plot(codaSamp[[i]][,grepl("p", colnames(codaSamp[[i]][[1]]))], main = paste0("p_y_", substr(file,13, 16))[[i]])
  plot(codaSamp[[i]][,grepl("B", colnames(codaSamp[[i]][[1]]))], main = paste0("B_y_", substr(file,13, 16))[[i]]) # l nomb de la premieère chaine (pareille partout0)
  plot(codaSamp[[i]][,grepl("b", colnames(codaSamp[[i]][[1]]))], main = paste0("b_y_", substr(file,13, 16))[[i]])
  plot(codaSamp[[i]][,grepl("N", colnames(codaSamp[[i]][[1]]))], main = paste0("N_y_", substr(file, 13, 16))[[i]])
  plot(codaSamp[[i]][,grepl("gamma", colnames(codaSamp[[i]][[1]]))], main = paste0("gamma_y_", substr(file, 13, 16))[[i]])
}

colnames(codaSamp[[1]][[1]])




# check what's in it
rm(list = ls())

setwd("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Outputs.tmp/20210304/bic5")
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
}

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list()                    # transform to coda
  # plot(codaSamp[[i]][,'sd.p'], main = paste0("sdp_y_", substr(file, 13, 16))[[i]]) 
  #plot(codaSamp[[i]][,'sd.phi'], main = paste0("sdphi_y_", substr(file, 13, 16))[[i]]) # BIZARRE
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("p_y_", substr(file, 13, 16))[[i]])
  #plot(codaSamp[[i]][,grepl("phi", colnames(codaSamp[[i]][[1]]))], main = paste0("phi_y_", substr(file, 13, 16))[[i]])
  # plot(codaSamp[[i]][,grepl("p", colnames(codaSamp[[i]][[1]]))], main = paste0("p_y_", substr(file,13, 16))[[i]])
  # plot(codaSamp[[i]][,grepl("B", colnames(codaSamp[[i]][[1]]))], main = paste0("B_y_", substr(file,13, 16))[[i]]) # l nomb de la premieère chaine (pareille partout0)
  # plot(codaSamp[[i]][,grepl("b", colnames(codaSamp[[i]][[1]]))], main = paste0("b_y_", substr(file,13, 16))[[i]])
  # plot(codaSamp[[i]][,grepl("N", colnames(codaSamp[[i]][[1]]))], main = paste0("N_y_", substr(file, 13, 16))[[i]])
  # plot(codaSamp[[i]][,grepl("gamma", colnames(codaSamp[[i]][[1]]))], main = paste0("gamma_y_", substr(file, 13, 16))[[i]])
}

colnames(codaSamp[[1]][[1]])

setwd("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Outputs.tmp/20210304/bic6")

# prepare df
aicb6<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)

# extract file 
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
}

# each .rds contains a list of results for each chain - there are 3 chains. We need to monitor convergence of each chains 
# the first step is to extract the codaSamp object with coda. Everything starts from there 

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # transform to coda # comprend les 3 chaînes
  #plot(codaSamp[[i]][,'Nsuper'], main = paste0("Nsuper_y_", substr(file, 15, 18))[[i]])
  #plot(codaSamp[[i]][,'mean.phi'], main = paste0("Mean.phi_y_", substr(file, 15, 18))[[i]])
  plot(codaSamp[[i]][,'mean.p'], main = paste0("Mean.p_y_", substr(file, 15, 18))[[i]])
  plot(codaSamp[[i]][,grepl("ranef.p", colnames(codaSamp[[i]][[1]]))], main = paste0("ranef.p_y_", substr(file, 15, 18))[[i]])
  # plot(codaSamp[[i]][,grepl("phi", colnames(codaSamp[[i]][[1]]))], main = paste0("phi_y_", substr(file, 13, 16))[[i]])
  # plot(codaSamp[[i]][,grepl("p", colnames(codaSamp[[i]][[1]]))], main = paste0("p_y_", substr(file,13, 16))[[i]])
  #plot(codaSamp[[i]][,grepl("B", colnames(codaSamp[[i]][[1]]))], main = paste0("B_y_", substr(file,13, 16))[[i]]) # l nomb de la premieère chaine (pareille partout0)
  #plot(codaSamp[[i]][,grepl("b", colnames(codaSamp[[i]][[1]]))], main = paste0("b_y_", substr(file,13, 16))[[i]])
  #plot(codaSamp[[i]][,grepl("N", colnames(codaSamp[[i]][[1]]))], main = paste0("N_y_", substr(file, 13, 16))[[i]])
  #plot(codaSamp[[i]][,grepl("gamma", colnames(codaSamp[[i]][[1]]))], main = paste0("gamma_y_", substr(file, 13, 16))[[i]])
}

setwd("/Users/LimoilouARenaud/Documents/Scolarite/PostDocI/Phoque/Outputs.tmp/20210614/bic7")
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
}

#parameters <- c("psi", "mean.p", "mean.phi", "b", "Nsuper", "N", "B", "nu", "w", "z", "epsilon", "sd", "sd2") # added w and z to WAIC

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list()                    # transform to coda
#  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y_", substr(file, 14, 18))[[i]])
##  plot(codaSamp[[i]][,'mean.p'], main = paste0("mean.p_y_", substr(file, 14, 18))[[i]])
  #plot(codaSamp[[i]][,'mean.phi'], main = paste0("mean.phi_y_", substr(file, 14, 18))[[i]]) # plot chain # my last year did not converge well
 # plot(codaSamp[[i]][,grepl("B", colnames(codaSamp[[i]][[1]]))], main = paste0("B_y_", substr(file, 14, 18))[[i]]) # l nomb de la premieère chaine (pareille partout0)
 # plot(codaSamp[[i]][,grepl("b", colnames(codaSamp[[i]][[1]]))], main = paste0("b_y_", substr(file, 14, 18))[[i]])
 # plot(codaSamp[[i]][,grepl("N", colnames(codaSamp[[i]][[1]]))], main = paste0("N_y_", substr(file, 14, 18))[[i]])
  plot(codaSamp[[i]][,grepl("epsilon", colnames(codaSamp[[i]][[1]]))], main = paste0("ranef.p_y_", substr(file, 15, 18))[[i]])
}
# loop - convergence check for metis -------------------------------------------------

# change directory as appropriate
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Outputs.tmp/20210304/metis1")

fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
}


codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list()                    # transform to coda
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("Nsup_y_", substr(file, 14,17))[[i]])
  plot(codaSamp[[i]][,'mean.p'], main = paste0("mean.p_y_", substr(file, 14,17))[[i]])
  plot(codaSamp[[i]][,'mean.phi'], main = paste0("mean.phi_y_", substr(file, 14,17))[[i]]) # plot chain # my last year did not converge well
  plot(codaSamp[[i]][,grepl("B", colnames(codaSamp[[i]][[1]]))], main = paste0("B_y_", substr(file, 14,17))[[i]]) # l nomb de la premieère chaine (pareille partout0)
  plot(codaSamp[[i]][,grepl("b", colnames(codaSamp[[i]][[1]]))], main = paste0("b_y_", substr(file,14,17))[[i]])
  plot(codaSamp[[i]][,grepl("N", colnames(codaSamp[[i]][[1]]))], main = paste0("N_y_", substr(file, 14,17))[[i]])
}

# extract gelman rubin
gelmanRubin <- vector("list",length=5) # change for nb of years

for(i in 1:length(gelmanRubin)){
  gelmanRubin[[i]] <- gelman.diag(codaSamp[[i]][,'mean.phi'])
}
for(i in 1:length(gelmanRubin)){
  gelmanRubin[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}

# model 2
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Outputs.tmp/20210304/metis2")
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
}
codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list()                    # transform to coda
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("Nsup_y_", substr(file, 15,18))[[i]])
  #plot(codaSamp[[i]][,'mean.p'], main = paste0("mean.p_y_", substr(file, 12, 15))[[i]])
  plot(codaSamp[[i]][,'mean.phi'], main = paste0("mean.phi_y_", substr(file, 15,18))[[i]]) # plot chain # my last year did not converge well
  plot(codaSamp[[i]][,grepl("p", colnames(codaSamp[[i]][[1]]))], main = paste0("p_y_", substr(file, 15,18))[[i]])
  plot(codaSamp[[i]][,grepl("B", colnames(codaSamp[[i]][[1]]))], main = paste0("B_y_", substr(file,15,18))[[i]]) # l nomb de la premieère chaine (pareille partout0)
  plot(codaSamp[[i]][,grepl("b", colnames(codaSamp[[i]][[1]]))], main = paste0("b_y_", substr(file,15,18))[[i]])
  plot(codaSamp[[i]][,grepl("N", colnames(codaSamp[[i]][[1]]))], main = paste0("N_y_", substr(file, 15,18))[[i]])
}
gelmanRubin <- vector("list",length=9) # change for nb of years

for(i in 1:length(gelmanRubin)){
  gelmanRubin[[i]] <- gelman.diag(codaSamp[[i]][,grepl("p", colnames(codaSamp[[i]][[1]]))])
}

for(i in 1:length(gelmanRubin)){
  gelmanRubin[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}

for(i in 1:length(gelmanRubin)){
  gelmanRubin[[i]] <- gelman.diag(codaSamp[[i]][,'mean.phi'])
}


# m3
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Outputs.tmp/20210224/metis3")
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
}

colnames(codaSamp[[1]][[1]])

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list()                    # transform to coda
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("Nsuper_y_", substr(file, 15,18))[[i]])
  plot(codaSamp[[i]][,grepl("N", colnames(codaSamp[[i]][[1]]))], main = paste0("N_y_", substr(file, 15,18))[[i]])
  plot(codaSamp[[i]][,grepl("phi", colnames(codaSamp[[i]][[1]]))], main = paste0("phi_y_", substr(file, 15,18))[[i]])
  plot(codaSamp[[i]][,grepl("p", colnames(codaSamp[[i]][[1]]))], main = paste0("p_y_", substr(file,15,18))[[i]])
  plot(codaSamp[[i]][,grepl("B", colnames(codaSamp[[i]][[1]]))], main = paste0("B_y_", substr(file,15,18))[[i]]) # l nomb de la premieère chaine (pareille partout0)
  plot(codaSamp[[i]][,grepl("b", colnames(codaSamp[[i]][[1]]))], main = paste0("b_y_", substr(file,15,18))[[i]])
  #plot(codaSamp[[i]][,grepl("gamma", colnames(codaSamp[[i]][[1]]))], main = paste0("gamma_y_", substr(file, 15,18))[[i]])
}


# m4
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Outputs.tmp/20210224/metis4")
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
}

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list()                    # transform to coda
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("Nsuper_y_", substr(file, 15,18))[[i]])
  plot(codaSamp[[i]][,'mean.p'], main = paste0("p_y_", substr(file, 15,18))[[i]])
  plot(codaSamp[[i]][,grepl("phi", colnames(codaSamp[[i]][[1]]))], main = paste0("phi_y_", substr(file, 15,18))[[i]])
  #plot(codaSamp[[i]][,grepl("p", colnames(codaSamp[[i]][[1]]))], main = paste0("p_y_", substr(file,15,18))[[i]])
  plot(codaSamp[[i]][,grepl("B", colnames(codaSamp[[i]][[1]]))], main = paste0("B_y_", substr(file,15,18))[[i]]) # l nomb de la premieère chaine (pareille partout0)
  plot(codaSamp[[i]][,grepl("b", colnames(codaSamp[[i]][[1]]))], main = paste0("b_y_", substr(file,15,18))[[i]])
  plot(codaSamp[[i]][,grepl("N", colnames(codaSamp[[i]][[1]]))], main = paste0("N_y_", substr(file, 15,18))[[i]])
  #  plot(codaSamp[[i]][,grepl("gamma", colnames(codaSamp[[i]][[1]]))], main = paste0("gamma_y_", substr(file, 15,18))[[i]])
}

colnames(codaSamp[[1]][[1]])




# m5

setwd("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Outputs.tmp/20210301/metis5")
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
}

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list()                    # transform to coda
  #plot(codaSamp[[i]][,'sd.p'], main = paste0("sdp_y_", substr(file, 15,18))[[i]])
  #plot(codaSamp[[i]][,'sd.phi'], main = paste0("sdphi_y_", substr(file, 15,18))[[i]])
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("Nsup_y_", substr(file, 15,18))[[i]])
  #plot(codaSamp[[i]][,'mean.p'], main = paste0("p_y_", substr(file, 15,18))[[i]])
  #plot(codaSamp[[i]][,grepl("phi", colnames(codaSamp[[i]][[1]]))], main = paste0("phi_y_", substr(file, 15,18))[[i]])
  # plot(codaSamp[[i]][,grepl("p", colnames(codaSamp[[i]][[1]]))], main = paste0("p_y_", substr(file,15,18))[[i]])
  # plot(codaSamp[[i]][,grepl("B", colnames(codaSamp[[i]][[1]]))], main = paste0("B_y_", substr(file,15,18))[[i]]) # l nomb de la premieère chaine (pareille partout0)
  # plot(codaSamp[[i]][,grepl("b", colnames(codaSamp[[i]][[1]]))], main = paste0("b_y_", substr(file,15,18))[[i]])
  # plot(codaSamp[[i]][,grepl("N", colnames(codaSamp[[i]][[1]]))], main = paste0("N_y_", substr(file, 15,18))[[i]])
  # plot(codaSamp[[i]][,grepl("gamma", colnames(codaSamp[[i]][[1]]))], main = paste0("gamma_y_", substr(file, 15,18))[[i]])
}

colnames(codaSamp[[1]][[1]])





setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js")

# prepare df
aicm6<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)

# extract file 
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
}

# each .rds contains a list of results for each chain - there are 3 chains. We need to monitor convergence of each chains 
# the first step is to extract the codaSamp object with coda. Everything starts from there 

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # transform to coda # comprend les 3 chaînes
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("Nsuper_y_", substr(file, 17, 20))[[i]])
  # plot(codaSamp[[i]][,'mean.phi'], main = paste0("Mean.phi_y_", substr(file, 15, 18))[[i]])
  #plot(codaSamp[[i]][,'mean.p'], main = paste0("Mean.p_y_", substr(file, 15, 18))[[i]])
  #plot(codaSamp[[i]][,grepl("ranef.p", colnames(codaSamp[[i]][[1]]))], main = paste0("ranef.p_y_", substr(file, 15, 18))[[i]])
  # plot(codaSamp[[i]][,grepl("phi", colnames(codaSamp[[i]][[1]]))], main = paste0("phi_y_", substr(file, 13, 16))[[i]])
  # plot(codaSamp[[i]][,grepl("p", colnames(codaSamp[[i]][[1]]))], main = paste0("p_y_", substr(file,13, 16))[[i]])
  #plot(codaSamp[[i]][,grepl("B", colnames(codaSamp[[i]][[1]]))], main = paste0("B_y_", substr(file,13, 16))[[i]]) # l nomb de la premieère chaine (pareille partout0)
  #plot(codaSamp[[i]][,grepl("b", colnames(codaSamp[[i]][[1]]))], main = paste0("b_y_", substr(file,13, 16))[[i]])
  #plot(codaSamp[[i]][,grepl("N", colnames(codaSamp[[i]][[1]]))], main = paste0("N_y_", substr(file, 13, 16))[[i]])
  #plot(codaSamp[[i]][,grepl("gamma", colnames(codaSamp[[i]][[1]]))], main = paste0("gamma_y_", substr(file, 13, 16))[[i]])
}

# 
sapply(outlist, function(x) x$WAIC$WAIC)
weanSurvOut <- sapply(outlist,function(x) x$summary$all.chains['weanSurv',]) %>%
    t() %>% as.data.frame() %>% rename(CIL=`95%CI_low`,CIH=`95%CI_upp`) %>% mutate(yr=years)
ggplot(weanSurvOut,aes(x=yr,y=Mean,ymin=CIL,ymax=CIH))+geom_pointrange()

map_dfr(1:length(outlist),function(x) {
    data.frame(yr=years[x],
          y=as.numeric(unlist(outlist[[x]]$samples[,'weanSurv']))
          )
    }) %>% as.data.frame() %>% ggplot(aes(x=yr,y=y))+geom_boxplot()

map_dfr(1:length(years),function(x) {
    data.frame(yr=years[x],
               y=as.numeric(unlist(outlist[[x]]$samples[,'dailySurv']))
    )
}) %>% as.data.frame() %>% ggplot(aes(x=yr,y=y))+geom_boxplot()

# explore what's in there
fd[[1]]$summary$all.chains # that gives you the summary for all chains, all iterations
fd[[1]]$summary$chain1

fd[[1]]$samples$chain1
sample_df <- fd[[i]]$samples %>% map(as.data.frame) %>% bind_rows()
#weanSurv <- nimble::ilogit(sample_df[,grepl('wean.surv',colnames(sample_df))])
weanSurv <- sample_df[,grepl('Nsuper',colnames(sample_df))]
mean(weanSurv) # 0.6802454
var(weanSurv) # 0.001657796
quantile(weanSurv, 0.025)
quantile(weanSurv, 0.975)


for(i in 1:length(fd)){
  sample_df=fd[[i]]$samples %>% map(as.data.frame) %>% bind_rows() 
  # mean.phi+ranef.phi[t] to add 
  #weanSurv=nimble::ilogit(sample_df[,grepl('wean.surv',colnames(sample_df))]) # un seul N, sur la bonne échelle
  #weanSurv=sample_df[,grepl('wean.surv',colnames(sample_df))] 
  weanSurv= nimble::ilogit(sample_df[,grepl('mean.phi',colnames(sample_df))])
  #weanSurv=sample_df[,grepl('wean.surv',colnames(sample_df))] # un seul N, sur la bonne échelle
  tmp[tmp$site=='bic',]$surv[i]=mean(weanSurv)
  tmp[tmp$site=='bic',]$min.surv[i]= quantile(weanSurv, 0.025)
  tmp[tmp$site=='bic',]$max.surv[i]=quantile(weanSurv, 0.975)
}


# 
# Nsuper <- sapply(fd,function(x) x$summary$all.chains['Nsuper',]) %>%
#   t() %>% as.data.frame() %>% rename(CIL=`95%CI_low`,CIH=`95%CI_upp`) %>% mutate(yr=years)
# ggplot(Nsuper,aes(x=yr,y=Mean,ymin=CIL,ymax=CIH))+geom_pointrange()
# 









# compare models with WAIC table ---------------------------------------------------------------

# repeat this operation for all models in folders
setwd("/Users/LimoilouARenaud/Documents/Scolarite/PostDocI/Phoque/Outputs.tmp/20210304/bic1") # to change

aicb1<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
  #filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
  #dfWAIC[i,2]<-filename_uid
  aicb1[i,3]<-fd[[i]]['WAIC']
}
setwd("/Users/LimoilouARenaud/Documents/Scolarite/PostDocI/Phoque/Outputs.tmp/20210304/bic2") # to change

aicb2<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
  #filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
  #dfWAIC[i,2]<-filename_uid
  aicb2[i,3]<-fd[[i]]['WAIC']
}
setwd("/Users/LimoilouARenaud/Documents/Scolarite/PostDocI/Phoque/Outputs.tmp/20210304/bic3") # best model now 

aicb3<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
  #filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
  #dfWAIC[i,2]<-filename_uid
  aicb3[i,3]<-fd[[i]]['WAIC']
}
mean(aicb3$WAIC, na.rm = T)# 510.2901 but with different data

setwd("/Users/LimoilouARenaud/Documents/Scolarite/PostDocI/Phoque/Outputs.tmp/20210304/bic4") # to change

aicb4<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
  #filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
  #dfWAIC[i,2]<-filename_uid
  aicb4[i,3]<-fd[[i]]['WAIC']
}

setwd("/Users/LimoilouARenaud/Documents/Scolarite/PostDocI/Phoque/Outputs.tmp/20210304/bic5") 

aicb5<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
  #filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
  #dfWAIC[i,2]<-filename_uid
  aicb5[i,3]<-fd[[i]]['WAIC']
}

setwd("/Users/LimoilouARenaud/Documents/Scolarite/PostDocI/Phoque/Outputs.tmp/20210614/bic6") # with all random

aicb6<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
  #filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
  #dfWAIC[i,2]<-filename_uid
  aicb6[i,3]<-fd[[i]]['WAIC']
}
mean(aicb6$WAIC, na.rm = T) # 662.1819

setwd("/Users/LimoilouARenaud/Documents/Scolarite/PostDocI/Phoque/Outputs.tmp/20210614/bic7") # to change

aicb7<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
  #filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
  #dfWAIC[i,2]<-filename_uid
  aicb7[i,3]<-fd[[i]]['WAIC']
}
mean(aicb7$WAIC, na.rm = T) # 591.1608 with pups 35 only, random p only

# now make equivalent to model 3 + random p on i










aicT<-data.frame(model = c(1:6), description=NA, WAIC = NA)
aicT[1,3]<-mean(aicb1$WAIC, na.rm = T) # need a specific name for model 1, bic 
aicT[2,3]<-mean(aicb2$WAIC, na.rm = T) # here change object name to refer to model 2 
aicT[3,3]<-mean(aicb3$WAIC, na.rm = T)
aicT[4,3]<-mean(aicb4$WAIC, na.rm = T)
aicT[5,3]<-mean(aicb5$WAIC, na.rm = T)
aicT[6,3]<-mean(aicb6$WAIC, na.rm = T) # from a diff dataframe 
aicT[1,2]<-"$p(.)\\phi(.)\\gamma(t)$"# could put math in markdown - tex though
aicT[2,2]<-"$p(t)\\phi(.)\\gamma(t)$"
aicT[3,2]<-"$p(t)\\phi(t)\\gamma(t)$"
aicT[4,2]<-"$p(.)\\phi(t)\\gamma(t)$"
aicT[5,2]<-"$p()\\phi()\\gamma(t)$"
aicT[6,2]<-"$p()\\phi()\\gamma(t)$"

knitr::kable(aicT, caption = "Average WAIC per model and per year", align = "llr") 


# old way to extract parameters from best model  --------------------------
setwd("/Users/LimoilouARenaud/Documents/Scolarite/PostDocI/Phoque/Outputs.tmp/20210304/bic3")

aicb3<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
  #filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
  #dfWAIC[i,2]<-filename_uid
  aicb3[i,3]<-fd[[i]]['WAIC']
}

tmp <- data.frame(yr = c(1998:2003, 2008:2016, 2019), N = NA, ymin = NA, ymax = NA, surv = NA, min.surv = NA, max.surv = NA)

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list()                    # transform to coda # pour le summary coda 
  tmp[i,2]= summary(codaSamp[[i]][,'Nsuper'])[[1]][1]     
  tmp[i,3]=summary(codaSamp[[i]][,'Nsuper'])[[2]][1]
  tmp[i,4]=summary(codaSamp[[i]][,'Nsuper'])[[2]][5]# get summary
  # tmp0<-codaSamp[[i]] %>% 
  #   map(as.data.frame) %>% 
  #   bind_rows() %>% select(contains("phi")) %>%  # no good - sort mean.phi, phi par occasion, ranef. phi
  #   apply(., 1, mean) 
  # 
  # tmp$surv[i]=mean(tmp0) # no good - sort la moyenne des survie entre chaque capture et non le multiple de chaque, en plus pas inv.logit
  # tmp$min.surv[i]= quantile(tmp0, 0.025)
  # tmp$max.surv[i]=quantile(tmp0, 0.975)
}

# add this in the loop to extract correlation 
cor[i] <- (codaSamp[[i]][,c("mean.phi", "Nsuper")] %>% map(as.data.frame) %>% bind_rows() %>% cor)[1,2] # 3 chaines en meme temps

# NEW extract parameters of best model with dplyr ------------------------------------------------------

# reextract files in case - change directory HERE to the GOOD one
# setwd("/Users/LimoilouARenaud/Documents/Scolarite/PostDocI/Phoque/Outputs.tmp/20210614/bic7")
# library(boot)
# fd<-list()
# file <-list.files(pattern = ".rds")
# for(i in seq_along(file)){
#   fd[[i]]<-readRDS(file[[i]])
# }
# 
# # extract Nsuper as the mean of all occasions 
# # extract mean.p + ranef p
# # extract mean.phi 
# 
# tmp <- data.frame(yr = c(1998:2003, 2008:2016, 2019), N = NA, ymin = NA, ymax = NA, surv = NA, min.surv = NA, max.surv = NA)
# #parameters <- c("psi", "mean.p", "mean.phi", "b", "Nsuper", "N", "B", "nu", "w", "z", "epsilon", "sd", "sd2") # added w and z to WAIC
# 
# for(i in 1:length(fd)){
#   ttt=fd[[i]]$samples %>% map(as.data.frame) %>% bind_rows() # instead of making a mcmc list # pas besoin de (i) parce quenregistre au fur et à mesure
#   # mean.phi+ranef.phi[t] to add 
#   tttn=ttt[,grepl('Nsuper',colnames(ttt))] # un seul N, sur la bonne échelle
#   ttts=ttt[,grepl('mean.phi',colnames(ttt))] #+ ttt[,grepl('ranef.phi',colnames(ttt))]b# surv d'une occasion à l'autre et non la survie présevrage
#  # ttts=apply(ttts,2,function(x) boot::inv.logit(x)) #TRES important
#   tttp=ttt[,'mean.p']+ttt[,grepl('epsilon\\[',colnames(ttt))] # depends on model # ok pour p par occasion qui ignore les effets aléatoires de l'individu
#  # ttt[,grepl('mean.p',colnames(ttt))] # double check with that - you do not want phi with thiss
#    tttp=apply(tttp,2,function(x) boot::inv.logit(x))
# #  tmp$surv[i] <-  mean(apply(ttts, 1,  prod))
#  # tmp$min.surv[i]= quantile(apply(ttts, 1, prod), 0.025) 
#  # tmp$max.surv[i]= quantile(apply(ttts, 1, prod), 0.975) 
#   tmp$surv[i] =mean(ttts) # this is a model with constant phi across occasions 
#   tmp$min.surv[i]= quantile(ttts, 0.025)
#   tmp$max.surv[i]=quantile(ttts, 0.975)
#   tmp$p[i]=mean(tttp)
#   tmp$min.p[i]= quantile(tttp, 0.025)
#   tmp$max.p[i]=quantile(tttp, 0.975)
#   tmp$N[i]=mean(tttn)
#   tmp$ymin[i]= quantile(tttn, 0.025)
#   tmp$ymax[i]=quantile(tttn, 0.975)
#   #tmp$var[i]=mean(tttn)
#   # tttc=sapply(1:nrow(ttts),function(x) cor(ttts[x,],tttp[x,]))
#     #  tmp$cor_sp[i]
# }
# ttts=ttt[,grepl('^p',colnames(ttt))] #+ ttt[,grepl('ranef.phi',colnames(ttt))]b# surv d'une occasion à l'autre et non la survie présevrage
# 
# # save this object 
# # save(tmp, file = "outputModel7.RData")
# 


# extract + plot model 3 for ms for now ------------------------------------------
setwd("/Users/LimoilouARenaud/Documents/Scolarite/PostDocI/Phoque/Outputs.tmp/20210304/bic3")

setwd("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Outputs.tmp/metis")

fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
}

tmp <- data.frame(yr = c(1998:2003, 2008:2016, 2019), N = NA, ymin = NA, ymax = NA, surv = NA, min.surv = NA, max.surv = NA)

for(i in 1:length(fd)){
  ttt=fd[[i]]$samples %>% map(as.data.frame) %>% bind_rows() # instead of making a mcmc list # pas besoin de (i) parce quenregistre au fur et à mesure
  # mean.phi+ranef.phi[t] to add 
  tttn=ttt[,grepl('Nsuper',colnames(ttt))] # un seul N, sur la bonne échelle
  ttts=ttt[,grepl('wean.surv',colnames(ttt))] 
  #ttts=apply(ttts,2,function(x) boot::inv.logit(x)) #TRES important
 # tmp$surv[i] <-  mean(apply(ttts, 1, prod))
#  tmp$min.surv[i]= quantile(apply(ttts, 1, prod), 0.025) 
 # tmp$max.surv[i]= quantile(apply(ttts, 1, prod), 0.975)
  tmp$N[i]=mean(tttn)
  tmp$ymin[i]= quantile(tttn, 0.025)
  tmp$ymax[i]=quantile(tttn, 0.975)
  tmp$surv[i]=colMeans(ttts[1]) # not sure this is right
  tmp$min.surv[i]= quantile(ttts[,1], 0.025)
  tmp$max.surv[i]=quantile(ttts[,1], 0.975)
  #tmp$var[i]=mean(tttn)
  # tttc=sapply(1:nrow(ttts),function(x) cor(ttts[x,],tttp[x,]))
  #  tmp$cor_sp[i]
}


# ggplot custom  ----------------------------------------------------------
theme_set(theme_pander(10))
          
scale_colour_discrete <- function(...,palette="Set1") {
  scale_colour_brewer(...,palette=palette)
}
scale_colour_orig <- ggplot2::scale_colour_discrete
scale_fill_discrete <- function(...,palette="Set1") {
  scale_fill_brewer(...,palette=palette)
}

#for grid arrange- margin = theme(plot.margin = unit(c(0,0,0,0.2), "cm"))
margin= theme(axis.title.y = element_text(margin = margin(t = 0, r = 2, b = 0, l = 0)), 
              plot.margin = unit(c(0,0.2,0.2,0), "cm"))


colnames(tmp)
tmp1 <- data.frame(yr = c(2004:2007, 2017:2018), N = NA, ymin = NA, ymax = NA, surv = NA, min.surv = NA, max.surv = NA)
resultsBic<-rbind(tmp, tmp1)
levels(factor(resultsBic$yr))
resultsBic$yr <- as.numeric(as.character(resultsBic$yr))


# figures for articles 
p1=ggplot(resultsBic, aes(x = yr, y = N)) + 
  geom_line(color="grey") +
  geom_pointrange(data = resultsBic, aes(x = yr, y = N, ymin=ymin, ymax=ymax),
                  shape=21, color="black", fill="#0072B2", size = 1.5) + 
  labs(x=expression('Year')) + 
  labs(y="Number of individuals (N)") +
  scale_y_continuous(limits = c(0,400),breaks = seq(from = 0, to = 400, by = 100)) + 
  theme(axis.line = element_line(),plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
  
p2=ggplot(resultsBic, aes(x = yr, y = surv)) + 
geom_line(color="grey") +
geom_pointrange(data = resultsBic, aes(x = yr, y = surv, ymin=min.surv, ymax=max.surv),
                  shape=21, color="black", fill="#69b3a2", size=1.5) + 
labs(x=expression('Year')) + 
labs(y="Survival probability") +
  scale_y_continuous(limits = c(0,0.5),breaks = seq(from = 0, to = 0.5, by = 0.1)) + 
  theme(axis.line = element_line (),plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))



# here from model 3 metis
setwd("/Users/LimoilouARenaud/Documents/Scolarite/PostDocI/Phoque/Outputs.tmp/20210304/metis3")

fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
}

tmp <- data.frame(yr = c(1998:2003, 2008:2016, 2019), N = NA, ymin = NA, ymax = NA, surv = NA, min.surv = NA, max.surv = NA)

for(i in 1:length(fd)){
  ttt=fd[[i]]$samples %>% map(as.data.frame) %>% bind_rows() # instead of making a mcmc list # pas besoin de (i) parce quenregistre au fur et à mesure
  # mean.phi+ranef.phi[t] to add 
  tttn=ttt[,grepl('Nsuper',colnames(ttt))] # un seul N, sur la bonne échelle
  ttts=ttt[,grepl('phi',colnames(ttt))] 
  #ttts=apply(ttts,2,function(x) boot::inv.logit(x)) #TRES important
  tmp$surv[i] <-  mean(apply(ttts, 1, prod))
  tmp$min.surv[i]= quantile(apply(ttts, 1, prod), 0.025) 
  tmp$max.surv[i]= quantile(apply(ttts, 1, prod), 0.975)
  tmp$N[i]=mean(tttn)
  tmp$ymin[i]= quantile(tttn, 0.025)
  tmp$ymax[i]=quantile(tttn, 0.975)
  #tmp$var[i]=mean(tttn)
  # tttc=sapply(1:nrow(ttts),function(x) cor(ttts[x,],tttp[x,]))
  #  tmp$cor_sp[i]
}

# plots Na nd surv Metis

colnames(tmp)
tmp1 <- data.frame(yr = c(2004:2007, 2017:2018), N = NA, ymin = NA, ymax = NA, surv = NA, min.surv = NA, max.surv = NA)# p = NA, min.p=NA, max.p=NA
resultsMetis<-rbind(tmp, tmp1)

# figures for articles 
p3=ggplot(resultsMetis, aes(x = yr, y = N)) + 
  geom_line(color="grey") +
  geom_pointrange(data = resultsMetis, aes(x = yr, y = N, ymin=ymin, ymax=ymax),
                  shape=21, color="black", fill="#0072B2", size = 1.5) + 
  labs(x=expression('Year')) + 
  labs(y="") +
  scale_y_continuous(limits = c(0,400),breaks = seq(from = 0, to = 400, by = 100)) + 
  theme(axis.line = element_line(),plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

p4=ggplot(resultsMetis, aes(x = yr, y = surv)) + 
  geom_line(color="grey") +
  geom_pointrange(data = resultsMetis, aes(x = yr, y = surv, ymin=min.surv, ymax=max.surv),
                  shape=21, color="black", fill="#69b3a2", size=1.5) + 
  labs(x=expression('Year')) + 
  labs(y="") +  
  scale_y_continuous(limits = c(0,0.5),breaks = seq(from = 0, to = 0.5, by = 0.1)) + 
  theme(axis.line = element_line(),plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))


# multiplots N and surv BIC ---------------------------------------------------------------

# p2 = ggplot(dataOne,aes(x = year, y = Unique_Elements)) +
#   geom_line( color="grey") +
#   geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
#   labs(x = "Year", y = "Number of marked individuals") + 
#   theme_pander() +
#   theme(axis.line = element_line(),plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
# getwd()


# # https://cdnsciencepub.com/authors-and-reviewers/preparing-figures
# Supply figures sized for publication: 1-column width is 8.84 cm, 
#2-column is 18.2 cm with a max. height of 23.7 cm and a resolution of 300 dpi
# plot les N

ggarrange(p1, p3, ncol = 2, labels = c("A","B"))


ggsave("/Users/LimoilouARenaud/Documents/Scolarite/PostDocI/Phoque/Graphs/FIG1abundancePanel.tiff", 
       height= 6, 
       width = 9, 
       unit= "in", 
       dpi = 300)#
ggsave("/Users/LimoilouARenaud/Documents/Scolarite/PostDocI/Phoque/Graphs/FIG1abundancePanel.png", 
       height= 14, # max height of 23.7 cm
       width = 18.2, 
       unit= "cm", 
       dpi = 300)#
# # https://cdnsciencepub.com/authors-and-reviewers/preparing-figures
# Supply figures sized for publication: 1-column width is 8.84 cm, 
#2-column is 18.2 cm with a max. height of 23.7 cm and a resolution of 300 dpi


# plot les surv
ggarrange(p2, p4, ncol = 2, labels = c("A","B"))


ggsave("/Users/LimoilouARenaud/Documents/Scolarite/PostDocI/Phoque/Graphs/FIG2_survPanel.tiff", 
       height= 6, 
       width = 9, 
       unit= "in", 
       dpi = 300)#
ggsave("/Users/LimoilouARenaud/Documents/Scolarite/PostDocI/Phoque/Graphs/FIG2_survPanel.png", 
       height= 14, # max height of 23.7 cm
       width = 18.2, 
       unit= "cm", 
       dpi = 300)#
# # https://cdnsciencepub.com/authors-and-reviewers/preparing-figures
# Supply figures sized for publication: 1-column width is 8.84 cm, 
#2-column is 18.2 cm with a max. height of 23.7 cm and a resolution of 300 dpi


# plot p, b and phi per sampling occasion BIC -------------------------------
# get date from original data 
load("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Data/Mine/20210608dataBySitep_Pup35.RData")
data<-bicData35.

for (t in 1:length(years)) { # takes a while
  days<-unique(data$date[data$year==years[t]]) # make sure dates are arranged
}

# extract coda sample from good model 
setwd("/Users/LimoilouARenaud/Documents/Scolarite/PostDocI/Phoque/Outputs.tmp/20210304/bic3")
setwd("/Users/LimoilouARenaud/Documents/Scolarite/PostDocI/Phoque/Outputs.tmp/20210614/bic7")
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Outputs.tmp")

fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
}

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # transform to coda # pour le summary coda 
}

# # if need to illustrate GOOD
# summary(codaSamp[[i]][,grepl("phi", colnames(codaSamp[[i]][[1]]))])[[1]][,1] 
# summary(codaSamp[[i]][,grepl("phi", colnames(codaSamp[[i]][[1]]))])[[2]][,1] # en mettre min 2 pour savoir c quoi 
# summary(codaSamp[[i]][,grepl("phi", colnames(codaSamp[[i]][[1]]))])[[2]][,5] # en mettre min 2 pour savoir c quoi 
# 
# # with p 
# days<-unique(data$date[data$year==years[1]])
# tmp <-summary(codaSamp[[1]][,grepl("p", colnames(codaSamp[[1]][[1]]))])[[1]][,1:2] 
# tmp1<-summary(codaSamp[[1]][,grepl("p", colnames(codaSamp[[1]][[1]]))])[[2]][,c(1,5)] # en mettre min 2 pour savoir c quoi 
# tmp2<-data.frame(days, tmp[3:12,1], tmp1[3:12,]) # knowing length day s= 10

# perhaps need original data frames  - OLD
# sait pas comment itérer sur codaSamp[i]
# for (t in 1:length(years)) { # takes a while
#   days<-unique(data$date[data$year==years[1]]) # make sure dates are arranged
#   tmp<-data.frame(summary(codaSamp[[1]])[[1]][,1],summary(codaSamp[[1]])[[2]][, c(1,5)]) # la moyenne
#   tmp<-tmp[grepl("p",rownames(tmp)),]
#   tmp2<-data.frame(days,tmp)
# }
unique(bicData2016$date)
unique(bicData2019$date)
unique(bicData2003$date)
tmp[[t]] <-summary(codaSamp[[t]][,grepl("B", colnames(codaSamp[[t]][[1]]))])[[1]][,1:2] #tu commences avec les charactères “p", rien devant - ceci inclut psi



# p 
# ATTENTION IL FAUT AJOUTER LES BONS P SELON LE MODÈLE. PAS DE PSI, PAS DE RANEF.P, ETC.
tmp <- list()
tmp1 <- list()
tmp2 <- list()
for (t in 1:length(years)) { # takes a while
  days<-unique(data$date[data$year==years[t]])
  tmp[[t]] <-summary(codaSamp[[t]][,grepl("^p", colnames(codaSamp[[t]][[1]]))])[[1]][,1:2] #tu commences avec les charactères “p", rien devant - ceci inclut psi
  tmp1[[t]]<-summary(codaSamp[[t]][,grepl("^p", colnames(codaSamp[[t]][[1]]))])[[2]][,c(1,5)] # en mettre min 2 pour savoir c quoi 
  tmp2[[t]]<-data.frame(days, tmp[[t]][1:length(days),1], tmp1[[t]][1:length(days),]) # knowing length day s= 10
  names(tmp2[[t]])<-c("date", "mean", "low", "high")
 tmp2[[t]]$year <- as.numeric(year(ymd(tmp2[[t]]$date))) # needs lubridate
}

p <- lapply(tmp2, function(d) ggplot(data = d, aes(x=date, y=mean)) + geom_pointrange(data = d, aes(ymin=low, ymax=high), size = 0.3) + #geom_smooth(se=F) + 
              ylab("capture probability") + xlab("Date") +  ggtitle(paste("Year ", d$year, sep=""))+ 
              scale_y_continuous(limits = c(0,1),breaks = seq(from = 0, to = 1, by = 0.2)) + 
  theme(axis.line = element_line()))


# geom_pointrange(data = tmp, aes(x = yr, y = N, ymin=ymin, ymax=ymax),
#                 shape=21, color="black", fill="grey", size = 1.5)

pdf("TESTmyplots_model3.pdf", width = "8", height="8")
#do.call("grid.arrange", p)
grid.arrange(grobs = lapply(p, "+", margin))

dev.off()

# b

# state process + observation process. the probability that a member of Ns enters the pop at occasion t is bt and is called the entry probability. 
# it is the probability than an indvidual is new in the population and has entered the population since the preceding occasion.
# entry coul result either from in situ recruitment (locally born individuals) or from immigration. 
# THI SIS NOT A RECUITMENT PROBABILITY. THE NUMBER OF IND ENTEREING THE POPULATION AT T IS BT=NSBT. THE FRACTION OF INDIVIDUALS ALREAD PRESENT AT TEH FIRST OCCASION IS B1 = THE ENTRY PROB HAS NO CLEAR ECOLOGICAL MEANING BECUASE IT IS A COMPLEX FUNCTION OF ALL ENTRIES BEFORE THE FIRST OCCASION. ALL ENTRY PROB MUST SUM TO 1 to ensure tha ll Ns individuals enter the population sometime during teh studyl 
# the superpop model uses entry probabilities b and an inclusion parameter psi. b are analogous to removal entry prob but not calculated the same way 
# the dirichlet prior is used. 
# allocate the entries of al indivdiuals uniformly over T occasions, alpha of 1 for all t. 
tmp <- list()
tmp1 <- list()
tmp2 <- list()

for (t in 1:length(years)) { # takes a while
  days<-unique(data$date[data$year==years[t]])
  tmp[[t]] <-summary(codaSamp[[t]][,grepl("^b", colnames(codaSamp[[t]][[1]]))])[[1]][,1:2] #tu commences avec les charactères “p", rien devant - ceci inclut psi
  tmp1[[t]]<-summary(codaSamp[[t]][,grepl("^b", colnames(codaSamp[[t]][[1]]))])[[2]][,c(1,5)] # en mettre min 2 pour savoir c quoi 
  tmp2[[t]]<-data.frame(days, tmp[[t]][1:length(days),1], tmp1[[t]][1:length(days),]) # knowing length day s= 10
  names(tmp2[[t]])<-c("date", "mean", "low", "high")
  tmp2[[t]]$year <- as.numeric(year(ymd(tmp2[[t]]$date))) # needs lubridate
  tmp2[[t]]$sum=sum(tmp2[[t]]$mean)# pent should sum to 1
}
  

p <- lapply(tmp2, function(d) ggplot(data = d, aes(x=date, y=mean)) + geom_pointrange(data = d, aes(ymin=low, ymax=high), size = 0.3) + #geom_smooth(se=F) + 
              ylab("Entry probability") + xlab("Date")+ ggtitle(paste("Year ", d$year, sep=""))+
              scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.2))+
              theme(axis.line = element_line()))

# geom_pointrange(data = tmp, aes(x = yr, y = N, ymin=ymin, ymax=ymax),
#                 shape=21, color="black", fill="grey", size = 1.5)

pdf("TESTEntryProbmyplots_model7.pdf", width = "8", height="8")
#do.call("grid.arrange", p)
grid.arrange(grobs = lapply(p, "+", margin))
dev.off()



# phi  # this is between occasions so it's t-1
tmp <- list()
tmp1 <- list()
tmp2 <- list()
for (t in 1:length(years)) { # takes a while
  #days<-unique(data$date[data$year==years[t]])
  tmp[[t]] <-summary(codaSamp[[t]][,grepl("^phi", colnames(codaSamp[[t]][[1]]))])[[1]][,1:2] #tu commences avec les charactères “p", rien devant - ceci inclut psi
  tmp1[[t]]<-summary(codaSamp[[t]][,grepl("^phi", colnames(codaSamp[[t]][[1]]))])[[2]][,c(1,5)] # en mettre min 2 pour savoir c quoi 
  tmp2[[t]]<-data.frame(1:nrow(tmp[[t]]), tmp[[t]][1:nrow(tmp[[t]]),1], tmp1[[t]][1:nrow(tmp[[t]]),]) # knowing length day s= 10
  names(tmp2[[t]])<-c("occasion", "mean", "low", "high")
  tmp2[[t]]$year <- years[t] # needs lubridate
}

p <- lapply(tmp2, function(d) ggplot(data = d, aes(x=as.factor(occasion), y=mean)) + 
              geom_pointrange(data = d, aes(ymin=low, ymax=high), size = 0.3) + #geom_smooth(se=F) + 
             scale_y_continuous(limits = c(0,1),breaks = seq(from = 0, to = 1, by = 0.2))+ # labels = scales::percent_format(accuracy = 1))
              ggtitle(paste("Year ", d$year, sep=""))+
              theme(axis.line = element_line()) +
              ylab("Survival probability") + xlab("Occasion")  
              )
pdf("TESTPhiplots_model3.pdf" , width = "8", height="8")
#do.call("grid.arrange", p)
grid.arrange(grobs = lapply(p, "+", margin))
dev.off()





# p, phi and b for Metis --------------------------------------------------

# get date from original data 
load("/Users/LimoilouARenaud/Documents/Scolarite/PostDocI/Phoque/Data/Mine/20210608dataBySitep_Pup35.RData")
data<-metisData35.

for (t in 1:length(years)) { # takes a while
  days<-unique(data$date[data$year==years[t]]) # make sure dates are arranged
}

# extract coda sample from good model 
setwd("/Users/LimoilouARenaud/Documents/Scolarite/PostDocI/Phoque/Outputs.tmp/20210304/metis3")

fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
}

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # transform to coda # pour le summary coda 
}

# p 
# ATTENTION IL FAUT AJOUTER LES BONS P SELON LE MODÈLE. PAS DE PSI, PAS DE RANEF.P, ETC.
tmp <- list()
tmp1 <- list()
tmp2 <- list()
for (t in 1:length(years)) { # takes a while
  days<-unique(data$date[data$year==years[t]])
  tmp[[t]] <-summary(codaSamp[[t]][,grepl("^p", colnames(codaSamp[[t]][[1]]))])[[1]][,1:2] #tu commences avec les charactères “p", rien devant - ceci inclut psi
  tmp1[[t]]<-summary(codaSamp[[t]][,grepl("^p", colnames(codaSamp[[t]][[1]]))])[[2]][,c(1,5)] # en mettre min 2 pour savoir c quoi 
  tmp2[[t]]<-data.frame(days, tmp[[t]][1:length(days),1], tmp1[[t]][1:length(days),]) # knowing length day s= 10
  names(tmp2[[t]])<-c("date", "mean", "low", "high")
  tmp2[[t]]$year <- as.numeric(year(ymd(tmp2[[t]]$date))) # needs lubridate
}

p <- lapply(tmp2, function(d) ggplot(data = d, aes(x=date, y=mean)) + geom_pointrange(data = d, aes(ymin=low, ymax=high), size = 0.3) + #geom_smooth(se=F) + 
              ylab("capture probability") + xlab("Date") +  ggtitle(paste("Year ", d$year, sep=""))+ 
              scale_y_continuous(limits = c(0,1),breaks = seq(from = 0, to = 1, by = 0.2)) + 
              theme(axis.line = element_line()))

pdf("TESTmyplots_model3.pdf", width = "8", height="8")
#do.call("grid.arrange", p)
grid.arrange(grobs = lapply(p, "+", margin))

dev.off()

# b

tmp <- list()
tmp1 <- list()
tmp2 <- list()
for (t in 1:length(years)) { # takes a while
  days<-unique(data$date[data$year==years[t]])
  tmp[[t]] <-summary(codaSamp[[t]][,grepl("^b", colnames(codaSamp[[t]][[1]]))])[[1]][,1:2] #tu commences avec les charactères “p", rien devant - ceci inclut psi
  tmp1[[t]]<-summary(codaSamp[[t]][,grepl("^b", colnames(codaSamp[[t]][[1]]))])[[2]][,c(1,5)] # en mettre min 2 pour savoir c quoi 
  tmp2[[t]]<-data.frame(days, tmp[[t]][1:length(days),1], tmp1[[t]][1:length(days),]) # knowing length day s= 10
  names(tmp2[[t]])<-c("date", "mean", "low", "high")
  tmp2[[t]]$year <- as.numeric(year(ymd(tmp2[[t]]$date))) # needs lubridate
  tmp2[[t]]$sum=sum(tmp2[[t]]$mean)# pent should sum to 1
}
library(scales)

p <- lapply(tmp2, function(d) ggplot(data = d, aes(x=date, y=mean)) + geom_pointrange(data = d, aes(ymin=low, ymax=high), size = 0.3) + #geom_smooth(se=F) + 
              ylab("Entry probability") + xlab("Date")+ ggtitle(paste("Year ", d$year, sep=""))+
              scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.2))+
              #scale_x_date(labels = date_format("%m/%d"))+
              theme(axis.line = element_line()))
pdf("TESTEntryProbmyplots_model3.pdf", width = "8", height="8")
#do.call("grid.arrange", p)
grid.arrange(grobs = lapply(p, "+", margin))
dev.off()



# phi  # this is between occasions so it's t-1
tmp <- list()
tmp1 <- list()
tmp2 <- list()

for (t in 1:length(years)) { # takes a while
  #days<-unique(data$date[data$year==years[t]])
  tmp[[t]] <-summary(codaSamp[[t]][,grepl("^phi", colnames(codaSamp[[t]][[1]]))])[[1]][,1:2] #tu commences avec les charactères “p", rien devant - ceci inclut psi
  tmp1[[t]]<-summary(codaSamp[[t]][,grepl("^phi", colnames(codaSamp[[t]][[1]]))])[[2]][,c(1,5)] # en mettre min 2 pour savoir c quoi 
  tmp2[[t]]<-data.frame(1:nrow(tmp[[t]]), tmp[[t]][1:nrow(tmp[[t]]),1], tmp1[[t]][1:nrow(tmp[[t]]),]) # knowing length day s= 10
  names(tmp2[[t]])<-c("occasion", "mean", "low", "high")
  tmp2[[t]]$year <- years[t] # needs lubridate
}

p <- lapply(tmp2, function(d) ggplot(data = d, aes(x=as.factor(occasion), y=mean)) + 
              geom_pointrange(data = d, aes(ymin=low, ymax=high), size = 0.3) + #geom_smooth(se=F) + 
              scale_y_continuous(limits = c(0,1),breaks = seq(from = 0, to = 1, by = 0.2))+ # labels = scales::percent_format(accuracy = 1))
              ggtitle(paste("Year ", d$year, sep=""))+
              theme(axis.line = element_line()) +
              ylab("Survival probability") + xlab("Occasion")  
)


pdf("TESTPhiplots_model3.pdf" , width = "8", height="8")
#do.call("grid.arrange", p)
grid.arrange(grobs = lapply(p, "+", margin))
dev.off()



# lambda ------------------------------------------------------------------

fig.bic

tibble(Nt1 = lead(fig.bic$N), fig.bic)




# text from Dubé ----------------------------------------------------------


# At Bic, the survival rate in 1999 was estimated to be constant for the entire study period while the capture probability
# varied between capture occasions (Fig. 3.1). The estimated survival rate of pups was 96.6 % (SE = 3.6 %) between each
# capture occasions. If weaning does not occur until the pups are 24-32 days old (Muelbert and Bowen 1993; Chapter 2)
# then the estimated pre-weaning survival ranged between 85.5 and 88.9 %, while estimated
# survival rate for the two months post-weaning was 74.5 %. In 2000, the survival rate was constant
#during the overall period of the study while the capture probability was influenced by weaning effects.
#The estimated survival was 97.0 % (SE = 2.9 %) between captureoccasions. The estimated capture probability
# was 46.6 % (SE = 5.5 %) during each capture occasion during the pre-weaning period and 16.2 % (SE = 4.3 %)
# during each post-weaning capture occasion. Estimated pre-weaning survival ranged from 87.0 to 90.1 % and was 77.0 % during the two months post-weaning.
# The difference observed between years was not
# significant (P > 0.5).



#Overall, at Métis the estimated survival rate varied between 50.2 and 59.6 % during the pre-weaning period and 100.0 % for
#the two months post-weaning.

# In both Métis data sets, the survival rate increased following weaning. In 1999, survival rate was influenced by weaning effect with a constant capture probability during the study period.
# The survival rate was estimated to be 86.0 % (SE = 5.0 %) between each capture occasion during the pre-weaning period and 100.0 % (SE = 0.003 %) between each capture occasion during the post-weaning period.
#The capture probability was estimated at 38.8 % (SE = 5.9 %) at each capture occasion during the entire study period. Overall, at Métis the estimated survival rate varied between 50.2 and 59.6 % during the pre-weaning period
# and 100.0 % for the two months post-weaning.

# In 2000, the best model for the Métis data set was a model with the weaning effect on survival rate and the sex and weaning effects with an interaction between the two on capture probability.
# The survival rate was estimated to be 87.5 % (SE = 4.3 %) between each capture occasion during the pre-weaning period and 99.6 % (SE = 2.4 %) between each capture occasion during the post-weaning period.
# The pre-weaning capture probability was estimated to be 54.5 % (SE = 10.6 %) for males and 76.5 % (SE = 8.2 %) for females during each capture occasion and 53.9 % (SE = 7.8 %) and 36.8 % (SE = 7.8 %)
# during each post-weaning capture occasion respectively. The estimated pre-weaning survival rate varied between 54.4 and 63.3 % and was 96.6 % for the two months post-weaning.

# The difference between pre and post-weaning survival estimates was significant for both years (In 1999, x2= 7.72, df = 1, P = 0.0054 ; In 2000, x2= 5.9275, df = I, P = 0.0149) and thus the suggestion of an increase in survival after the weaning might be considered reliable. However, the difference observed between years was not significant (P > 0.5).







# extract + plot Covariate model ---------------------------------------------------------
rm(list = ls())
# here from model 3 metis
setwd("/Users/LimoilouARenaud/Documents/Scolarite/PostDocI/Phoque/Outputs.tmp/20210614/Cov")

fd<-list()
file <-list.files(pattern = "rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
}

tmp <- data.frame(yr = c(1998:2003, 2008:2016, 2019), N = NA, ymin = NA, ymax = NA, surv = NA, min.surv = NA, max.surv = NA)
# choisir une année représentative

i=3

# sortir le dataframe
for(i in 1:length(fd)){
  ttt=fd[[i]]$samples %>% map(as.data.frame) %>% bind_rows() # instead of making a mcmc list # pas besoin de (i) parce quenregistre au fur et à mesure
  # mean.phi+ranef.phi[t] to add 
  tttn=ttt[,grepl('Nsuper',colnames(ttt))] # un seul N, sur la bonne échelle
  #ttts=ttt[,grepl('phi',colnames(ttt))] 
  ttts=ttt[,grepl('mean.phi',colnames(ttt))]# + ttt[,grepl('coef.mass',colnames(ttt))]# surv d'une occasion à l'autre et non la survie présevrage
 # ttts=apply(ttts,2,function(x) boot::inv.logit(x)) #TRES important
  # tmp$surv[i] <-  mean(apply(ttts, 1, prod)) # ceci donne une valeur par année - on veut les produits par ligne 
  # tmp$min.surv[i]= quantile(apply(ttts, 1, prod), 0.025) # les quantiles des produits par ligne 
  # tmp$max.surv[i]= quantile(apply(ttts, 1, prod), 0.975)
  # tmp$N[i]=mean(tttn)
  # tmp$ymin[i]= quantile(tttn, 0.025)
  # tmp$ymax[i]=quantile(tttn, 0.975)
  # #tmp$var[i]=mean(tttn)
  # tttc=sapply(1:nrow(ttts),function(x) cor(ttts[x,],tttp[x,]))
  #  tmp$cor_sp[i]
}


# effect of mass for female pup (reference = coded as 0) 
# make prediction # il faut itérer ceci pour toutes les années # ici la dernière année seulement 


# load adjusted mass 
load("/Users/LimoilouARenaud/Documents/Scolarite/PostDocI/Phoque/Data/covariateDataOneSite.RData")

library(boot)
dat <- data.frame(x=1:1000)
dat$pred.mass <- seq(0, 30, length.out =1000) # cov values for prediction) 
# choisir une année car les masses sont scalées par année pour le tidying et le modèle

# il faudrait que ceci soit tiré de la même année que celle qui est choisie dans la boucle 
# attn a mass est données répétées
dat$pred.mass.st <- (dat$pred.mass - mean(amass$adjMassJJ161, na.rm=T))/sd(amass$adjMassJJ161)  # scale them    
# need true vector of mass     
pred.p <-  sapply(1:nrow(dat), function(x){
  apply(apply(ttts+ttt[,grepl('coef.mass',colnames(ttt))]*dat$pred.mass.st[x], 1:2, inv.logit),1,prod)})
dat$pred.mean <- apply(pred.p, 2, mean)# ajouter quantile 
dat$pred.lci= apply(pred.p,2, quantile, probs = 0.025,  na.rm = TRUE)
dat$pred.uci= apply(pred.p,2, quantile, probs = 0.975,  na.rm = TRUE)

p1=ggplot(dat,aes(x=pred.mass)) +  
  geom_line(aes(y=pred.mean), color="black") + xlab("Pup mass (Kg)") + ylab("") + 
  scale_y_continuous(limits = c(0,1),breaks = seq(from = 0, to = 1, by = 0.2)) + 
  theme(axis.line = element_line(),
        plot.margin = unit(c(t = 0, r = 0.5, b = 0.5, l = 0), "cm"),
        axis.title.y=element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
# geom_ribbon(data=dat, aes(ymin = pred.lci, ymax = pred.uci), alpha = 0.2) 
# effet dans le ms # NS
tmp=ttt[,grepl('coef.mass',colnames(ttt))]
beta.mass <-  round(mean(tmp),3) # -0.2954839
beta.mass.l= round(quantile(tmp, 0.025),3) # -0.7652164
beta.mass.u= round(quantile(tmp, 0.975),3) #0.1228464 


# effect of sex for pup of average mass 
newdata <- data.frame(sex=as.factor(c('Female',"Male")))
newdata$x=1:2
newdata$sexe <- seq(0, 1, length.out =2) 
# cov values for prediction) # choisir une année car les masses sont scalées par année pour le tidying et le modèle
#dat$pred.mass.st <- (dat$pred.mass - mean(amass$adjMassJJ161, na.rm=T))/sd(amass$adjMassJJ161)  # scale them    # need true vector of mass      # attn a mass est données répétées
pred.p <-  sapply(1:nrow(newdata), function(x){
  apply(apply(ttts+ttt[,grepl('coef.sex',colnames(ttt))]*newdata$sexe[x], 1:2, inv.logit),1,prod)})
newdata$pred.mean <- apply(pred.p, 2, mean)# ajouter quantile 
newdata$pred.lci= apply(pred.p,2, quantile, probs = 0.025,  na.rm = TRUE)
newdata$pred.uci= apply(pred.p,2, quantile, probs = 0.975,  na.rm = TRUE)
# sex x sexe   pred.mean     pred.lci   pred.uci
# 1 Female 1    0 0.006796348 4.147127e-05 0.03710376
# 2   Male 2    1 0.996497748 9.534002e-01 1.00000000

p2=ggplot(newdata,aes(x=sex)) +  
  #geom_line(aes(y=pred.mean), color="black")+theme_pander() + xlab("Sex") + ylab("Survival probability")
  geom_pointrange(aes(y=pred.mean,ymin = pred.lci, ymax = pred.uci), size= 1.2) + xlab("Pup sex") + 
  ylab("Survival probability") + 
  scale_y_continuous(limits = c(0,1),breaks = seq(from = 0, to = 1, by = 0.2)) + 
  theme(axis.line = element_line(),
        plot.margin = unit(c(t = 0, r = 0.5, b = 0.5, l = 0), "cm"),
        axis.title.y=element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
# geom_ribbon(data=dat, aes(ymin = pred.lci, ymax = pred.uci), alpha = 0.2) 

hist(apply(apply(ttts,1:2,inv.logit),1,prod ))
hist(ttt[,grepl('coef.sex',colnames(ttt))] )


ggarrange(p2,p1, labels = c("A", "B"), ncol=2)

ggsave("/Users/LimoilouARenaud/Documents/Scolarite/PostDocI/Phoque/Graphs/FIG3_SexMassPanel.png",
       height= 6,
       width = 9,
       unit= "in",
       dpi = 300)#

ggsave("/Users/LimoilouARenaud/Documents/Scolarite/PostDocI/Phoque/Graphs/FIG3_SexMassPanel.tiff",
       height= 14, # max height of 23.7 cm
       width = 18.2,
       unit= "cm",
       dpi = 300)#


# newdata <- data.frame(x=1:1000)
# newdata$pred.mass <- seq(0, 30, length.out =1000) 
# newdata$sex <- seq(0, 1, length.out =1000) 
# 
# newdata$pred.mass.st <- (newdata$pred.mass - mean(amass$adjMassJJ161, na.rm=T))/sd(amass$adjMassJJ161)  # scale them    # need true vector of mass      # attn a mass est données répétées
# pred.p <-  sapply(1:nrow(newdata), function(x){
#   apply(apply(ttts+ttt[,grepl('coef.mass',colnames(ttt))]*newdata$pred.mass.st[x] + ttt[,grepl('coef.sex',colnames(ttt))]*newdata$sex[x], 1:2, inv.logit),1,prod)})
# newdata$pred.mean <- apply(pred.p, 2, mean)# ajouter quantile 
# newdata$pred.lci= apply(pred.p,2, quantile, probs = 0.025,  na.rm = TRUE)
# newdata$pred.uci= apply(pred.p,2, quantile, probs = 0.975,  na.rm = TRUE)
# 
# ggplot(newdata,aes(x=pred.mass)) +  
#   geom_line(aes(y=pred.mean), color="black") +theme_pander() + xlab("Pup mass (Kg)") + ylab("Survival probability")
#   



#   