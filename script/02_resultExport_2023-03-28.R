#Values substantially above 1 indicate lack of convergence. If the chains have not converged, Bayesian credible intervals based on the t-distribution are too wide, and have the potential to shrink by this factor if the MCMC run is continued.


library(tidyverse)
library(coda)
library(bookdown)
library(cowplot)
library(coda)
library(boot)
library(ggmcmc)

rm(list = ls())

# JS BIC- get gelmanRubin and WAIC for Jolly Seber at Bic--------------------------------------------------------

years = c(1998:2003, 2008:2016, 2019)

setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/bic/20210304/bic1")

aicb1<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)

fd<-list()
file <-list.files(pattern = ".rds")

for(i in seq_along(file)){
    fd[[i]]<-readRDS(file[[i]])
    filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
    aicb1[i,2]<-filename_uid
    aicb1[i,3]<-fd[[i]]['WAIC']
}


codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 14, 18))[[i]])
  }


gelmanN_bic <- vector("list",length=16)

for(i in 1:length(gelmanN_bic)){
  gelmanN_bic[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_bic)

# get nicer df - stil has to pivot 
gelman1 <- map_dfr(1:length(gelmanN_bic),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_bic[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) # none is above 1.1



setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/bic/20210304/bic2")

aicb2<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)

fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
    fd[[i]]<-readRDS(file[[i]])
    #filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
    #dfWAIC[i,2]<-filename_uid
    aicb2[i,3]<-fd[[i]]['WAIC']
}

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 14, 18))[[i]])
}

gelmanN_bic <- vector("list",length=16)

for(i in 1:length(gelmanN_bic)){
  gelmanN_bic[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_bic)


# get nicer df - stil has to pivot 
gelman2 <-map_dfr(1:length(gelmanN_bic),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_bic[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)
# yr     y
# <dbl> <dbl>
# 1  2001  1.15
# 2  2012  1.48


setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/bic/20210304/bic3")

aicb3<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)

fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
    fd[[i]]<-readRDS(file[[i]])
    #filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
    #dfWAIC[i,2]<-filename_uid
    aicb3[i,3]<-fd[[i]]['WAIC']
}

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 14, 18))[[i]])
}

gelmanN_bic <- vector("list",length=16)

for(i in 1:length(gelmanN_bic)){
  gelmanN_bic[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_bic)


# get nicer df - stil has to pivot 
gelman3 <- map_dfr(1:length(gelmanN_bic),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_bic[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)
# 1  1999  1.11


setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/bic/20210304/bic4")

aicb4<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)

fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
    fd[[i]]<-readRDS(file[[i]])
    #filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
    #dfWAIC[i,2]<-filename_uid
    aicb4[i,3]<-fd[[i]]['WAIC']
}

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 14, 18))[[i]])
}

gelmanN_bic <- vector("list",length=16)

for(i in 1:length(gelmanN_bic)){
  gelmanN_bic[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_bic)

gelman4 <- map_dfr(1:length(gelmanN_bic),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_bic[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)

# 1  2019  1.11 but 2003 doesn't look good


# add no5 here #ERROR IN SD RUNIF -REPLACE BY DUNIF
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/bic/20210304/bic5")
aicb5<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
    fd[[i]]<-readRDS(file[[i]])
    filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
    aicb5[i,2]<-filename_uid
    aicb5[i,3]<-fd[[i]]['WAIC']
}
codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 14, 18))[[i]])
}

gelmanN_bic <- vector("list",length=16)

for(i in 1:length(gelmanN_bic)){
  gelmanN_bic[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_bic)


# get nicer df - stil has to pivot 
gelman5 <- map_dfr(1:length(gelmanN_bic),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_bic[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)


# yr     y
# <dbl> <dbl>
# 1  1999  1.23
# 2  2003  2.70
# 3  2010  1.13
# 4  2012  1.13
# 5  2013  1.22


# 
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/bic/20210614/bic6")
aicb6<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
    fd[[i]]<-readRDS(file[[i]])
    filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
    aicb6[i,2]<-filename_uid
    aicb6[i,3]<-fd[[i]]['WAIC']
}

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 14, 18))[[i]])
}

gelmanN_bic <- vector("list",length=16)

for(i in 1:length(gelmanN_bic)){
  gelmanN_bic[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_bic)

gelman6 <- map_dfr(1:length(gelmanN_bic),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_bic[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)

# yr     y
# <dbl> <dbl>
#   1  1998  1.51
# 2  2002  1.26
# 3  2008  1.17
# 4  2014  1.21
# 5  2015  1.15
# 6  2016  1.10

# AND MORE 
# yr     y
# <dbl> <dbl>
# 1  1998  5.06
# 2  1999  1.32
# 3  2000  1.16
# 4  2001  1.44
# 5  2002  1.29
# 6  2010  1.42
# 7  2011  1.30
# 8  2012  1.24
# 9  2013  1.26
# 10  2015  1.14
# 11  2016  1.22


# m7
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/bic/20210614/bic7")
aicb7<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
    fd[[i]]<-readRDS(file[[i]])
    filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
    aicb7[i,2]<-filename_uid
    aicb7[i,3]<-fd[[i]]['WAIC']
}

# quick check
codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 14, 18))[[i]])
}

gelmanN_bic <- vector("list",length=16)

for(i in 1:length(gelmanN_bic)){
  gelmanN_bic[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_bic)
gelman7 <- map_dfr(1:length(gelmanN_bic),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_bic[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)

# yr     y
# <dbl> <dbl>
# 1  1998  1.51
# 2  2002  1.26
# 3  2008  1.17
# 4  2014  1.21
# 5  2015  1.15
# 6  2016  1.10



# get nice table to export
aicT<-data.frame(model = c(1:7), description=NA, WAIC = NA)



# JS METIS - get gelmanRubin and WAIC for Jolly Seber at MEtis--------------------------------------------------------
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/metis/20210304/metis1")

aicm1<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
  filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
  aicm1[i,2]<-filename_uid
  aicm1[i,3]<-fd[[i]]['WAIC']
}

# quick check
codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 16, 20))[[i]])
}

gelmanN_metis <- vector("list",length=16)

for(i in 1:length(gelmanN_metis)){
  gelmanN_metis[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_metis)
gelman1 <- map_dfr(1:length(gelmanN_metis),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_metis[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)



setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/metis/20210304/metis2")

codaSamp<-list()
for(i in 1:length(fd)){
    codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list()                    # transform to coda
    plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y_", substr(file, 16, 20))[[i]])
}

aicm2<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
  #filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
  #dfWAIC[i,2]<-filename_uid
  aicm2[i,3]<-fd[[i]]['WAIC']
}
codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 16, 20))[[i]])
}

gelmanN_metis <- vector("list",length=16)

for(i in 1:length(gelmanN_metis)){
  gelmanN_metis[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_metis)
gelman2 <- map_dfr(1:length(gelmanN_metis),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_metis[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)




setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/metis/20210304/metis3")

# quick check
codaSamp<-list()
for(i in 1:length(fd)){
    codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list()                    # transform to coda
    plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y_", substr(file, 16, 20))[[i]])
}


# nice table
aicm3<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
  #filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
  #dfWAIC[i,2]<-filename_uid
  aicm3[i,3]<-fd[[i]]['WAIC']
}
codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 16, 20))[[i]])
}

gelmanN_metis <- vector("list",length=16)

for(i in 1:length(gelmanN_metis)){
  gelmanN_metis[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_metis)
gelman3 <- map_dfr(1:length(gelmanN_metis),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_metis[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) # 2013-2016 doesn't look so good 



setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/metis/20210304/metis4")
aicm4<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
  #filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
  #dfWAIC[i,2]<-filename_uid
  aicm4[i,3]<-fd[[i]]['WAIC']
}
codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 16, 20))[[i]])
}

gelmanN_metis <- vector("list",length=16)

for(i in 1:length(gelmanN_metis)){
  gelmanN_metis[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_metis)
gelman4 <- map_dfr(1:length(gelmanN_metis),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_metis[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)



#  MODEL 5 HAS AN ERROR - REPLACE RUNIF BY DUNIF

setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/metis/20210304/metis5")

aicm5<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)

fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
  #filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
  #dfWAIC[i,2]<-filename_uid
  aicm5[i,3]<-fd[[i]]['WAIC'] # ATTENTION MIGHT NEED TO CHANGE 
}

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014 2013 either
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 16, 20))[[i]])
}

gelmanN_metis <- vector("list",length=16)

for(i in 1:length(gelmanN_metis)){
  gelmanN_metis[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_metis)
gelman5 <- map_dfr(1:length(gelmanN_metis),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_metis[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)



setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/metis/20220319/metis6")

# waic table 
aicm6<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)

fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
  #filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
  #aicm6[i,2]<-filename_uid
  aicm6[i,3]<-fd[[i]]$WAIC$WAIC# NEW to this way of doing since nimble new version?? 
}

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 16, 20))[[i]])
}

gelmanN_metis <- vector("list",length=16)

for(i in 1:length(gelmanN_metis)){
  gelmanN_metis[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_metis)
gelman6 <- map_dfr(1:length(gelmanN_metis),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_metis[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)



setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/metis/20220319/metis7")

aicm7<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)

fd<-list()
file <-list.files(pattern = ".rds")

for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
  #filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
  #dfWAIC[i,2]<-filename_uid
  aicm7[i,3]<-fd[[i]]$WAIC$WAIC # NEW to this way of doing
}

codaSamp<-list()

for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 16, 20))[[i]])
}

gelmanN_metis <- vector("list",length=16)

for(i in 1:length(gelmanN_metis)){
  gelmanN_metis[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_metis)
gelman7 <- map_dfr(1:length(gelmanN_metis),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_metis[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)




# not so sure about mod 6 and 7 since Nsuper was not extracted correctly - all look the same 

aicTM<-data.frame(model = c(1:7), description=NA, WAIC = NA)


gelman1 # none
gelman2
# yr     y
# <dbl> <dbl>
# 1  2000  1.21
# 2  2015  1.12
# 3  2019  1.31
gelman3 # none
gelman4 # none
gelman5
# yr     y
# <dbl> <dbl>
#  1  1999  1.11
# 2  2000  1.11
# 3  2002  1.17
# 4  2009  1.21
# 5  2012  1.17
# 6  2013  2.75
# 7  2014  1.19
# 8  2015  1.20
# 9  2016  1.25

gelman6 # none
gelman7 # none


# quick convergence check for model 7 - metis - DIDNT'T WORK
codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list()                    # transform to coda
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y_", substr(file, 16, 20))[[i]])
  #plot(codaSamp[[i]][,'mean.phi'], main = paste0("mean.phi_y_", substr(file, 14, 18))[[i]]) # plot chain # my last year did not converge well
  #plot(codaSamp[[i]][,grepl("B", colnames(codaSamp[[i]][[1]]))], main = paste0("B_y_", substr(file, 14, 18))[[i]]) # l nomb de la premieère chaine (pareille partout0)
  #plot(codaSamp[[i]][,grepl("b", colnames(codaSamp[[i]][[1]]))], main = paste0("b_y_", substr(file, 14, 18))[[i]])
  #plot(codaSamp[[i]][,grepl("N", colnames(codaSamp[[i]][[1]]))], main = paste0("N_y_", substr(file, 14, 18))[[i]])
}


# clean up  ---------------------------------------------------------------

rm(list=setdiff(ls(), c("years", "aicm1","aicm2","aicm3","aicm4","aicm5","aicm6","aicm7",
                        "aicb1","aicb2","aicb3","aicb4","aicb5","aicb6","aicb7", 'aicT', 
                        'aicTM')))







# CJS BIC - Gelman and WAIC at Bic --------------------------------------------

# decompresser le rds avec une connection speciale
# ATTENTION CA DIT PAS TOUJOURS SI Y A UNE ERREUR D'EMPLACEMENT

years <- c(1998:2003, 2008:2016, 2019)

# models were rerun with 500K 
con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/20221205/bic_cjs1_Growth2.rds")
outlist <- readRDS(con)

map(1:16,~ plot(outlist[[.x]]$samples[,'mean.phi'],main=years[[.x]])) # 2002 - 2008 - 2009 NOT 
map(1:16,~ plot(outlist[[.x]]$samples[,'weanSurv'],main=years[[.x]])) # 2008- 2009

WAIC.b.1<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) # no need to transpose here

gelmanCJS_b <- vector("list",length=16)
for(i in 1:length(outlist)){
  gelmanCJS_b[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}
# print(gelmanCJS_b)

map_dfr(1:length(gelmanCJS_b),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanCJS_b[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) # most do not converge 
# yr     y
# <dbl> <dbl>
# 1  2003  1.10
# 2  2008  1.63
# 3  2009  1.41 
# 4  2015  1.26



# m2 
con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/20221205/bic_cjs2_growth2.rds")

outlist <- readRDS(con)

# convergence visual
map(1:16,~ plot(outlist[[.x]]$samples[,'mean.phi'],main=years[[.x]])) # 2015 - 2009 -2008 - 2003
map(1:16,~ plot(outlist[[.x]]$samples[,'weanSurv'],main=years[[.x]])) # 2019 - 2015 - 2009 - 2008


# waic
WAIC.b.2<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) # no need to transpose here

gelmanCJS_b <- vector("list",length=16)
for(i in 1:length(outlist)){
  gelmanCJS_b[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}
map_dfr(1:length(gelmanCJS_b),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanCJS_b[[x]]))
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) # 2012 is very bad with less than 500 k it
# yr     y
# <dbl> <dbl>
# 1  1999  1.11
# 2  2008  1.11
# 3  2019  1.10


# m3
con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/20221205/cjs3_bic_semiRanefTGrowth2.rds")
outlist <- readRDS(con)

# convergence 
map(1:16,~ plot(outlist[[.x]]$samples[,'mean.phi'],main=years[[.x]])) # 2015 - 2008
map(1:16,~ plot(outlist[[.x]]$samples[,'weanSurv'],main=years[[.x]])) # 2019 -most weird and 2014

# selection
WAIC.b.3<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) # no need to transpose here

gelmanCJS_b <- vector("list",length=16)
for(i in 1:length(outlist)){
  gelmanCJS_b[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}

map_dfr(1:length(gelmanCJS_b),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanCJS_b[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) # 2012  6.01

# 2015  1.35




# m4 
con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/20221205/cjs4_bic_semiRanefTGrowth2.rds") 
outlist <- readRDS(con)

# convergence 
map(1:16,~ plot(outlist[[.x]]$samples[,'mean.phi'],main=years[[.x]])) # 2008 - 2009
map(1:16,~ plot(outlist[[.x]]$samples[,'weanSurv'],main=years[[.x]])) # ugly 2008 2019

# selection
WAIC.b.4<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) # no need to transpose here

gelmanCJS_b <- vector("list",length=16)
for(i in 1:length(outlist)){
  gelmanCJS_b[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}
map_dfr(1:length(gelmanCJS_b),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanCJS_b[[x]]))# $samples[,'weanSurv']
  )
 }) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)

# 1  2002  1.26
# 2  2008  1.60

gelmanCJS_b <- vector("list",length=16)
for(i in 1:length(outlist)){
    gelmanCJS_b[[i]] <- gelman.diag(outlist[[i]]$samples[,'mean.phi'])
}
map_dfr(1:length(gelmanCJS_b),function(x) {
    data.frame(yr=years[x],
               y=as.numeric(unlist(gelmanCJS_b[[x]]))# $samples[,'weanSurv']
    )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)

# same years too




# m5 
# ranef id only on capture prob
con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/20221205/cjs5_bic_growth2.rds")
outlist <- readRDS(con)

# convergence 
map(1:16,~ plot(outlist[[.x]]$samples[,'mean.phi'],main=years[[.x]])) # 2015 - 2009 - 2008
map(1:16,~ plot(outlist[[.x]]$samples[,'weanSurv'],main=years[[.x]])) # ugly 2009- 2008 -2003

# sele tion
WAIC.b.5<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) # no need to transpose here

gelmanCJS_b <- vector("list",length=16)
for(i in 1:length(outlist)){
  gelmanCJS_b[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}
map_dfr(1:length(gelmanCJS_b),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanCJS_b[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) # a lot ! 2008-2009 are 4 and 5
# yr     y
# <dbl> <dbl>
#     1  2002  1.44
# 2  2008  2.90
# 3  2009  1.33
# 4  2016  1.10

#1  2002  1.44


# m6 
con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/20221205/cjs6_bic_semiRanefTID_Growth2.rds")
outlist <- readRDS(con)


# convergence 
map(1:16,~ plot(outlist[[.x]]$samples[,'mean.phi'],main=years[[.x]])) # 2008
map(1:16,~ plot(outlist[[.x]]$samples[,'weanSurv'],main=years[[.x]])) # 2008 - 2009 - 2016

# selection
WAIC.b.6<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) # no need to transpose here

gelmanCJS_b <- vector("list",length=16)
for(i in 1:length(outlist)){
  gelmanCJS_b[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}
map_dfr(1:length(gelmanCJS_b),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanCJS_b[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) # 2009 and 2013 are over 2

# yr     y
# 1 2002  1.14
# 2  2009  1.48
# 3  2013  1.19









# CJS METIS - Gelman and WAIC at Metis  -------------------------------------------

years <- c(1998:2003, 2008:2016, 2019)

# m1 
metis1 <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/metis/20221205/metis_cjs1_Growth2.rds")
outlist <- readRDS(metis1)

# convergence 
map(1:16,~ plot(outlist[[.x]]$samples[,'mean.phi'],main=years[[.x]])) # 2008 - 2009 - 2013 - 1998
map(1:16,~ plot(outlist[[.x]]$samples[,'weanSurv'],main=years[[.x]])) # 2013 - 2011 - 2009 - 2008

# waic
class(outlist)
WAIC.m.1<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) # 

# gelman sur le wean surv
gelmanCJS_m <- vector("list",length=16)
for(i in 1:length(outlist)){
  gelmanCJS_m[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}
print(gelmanCJS_m)

map_dfr(1:length(gelmanCJS_m),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanCJS_m[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) # all of which above 1.1, 1998 worst

# yr     y
# <dbl> <dbl>
# 1  1998  1.23
# 2  2001  1.23
# 3  2002  2.99
# 4  2009  1.44
# 5  2013  1.18
# 6  2014  2.19
# 7  2016  1.12


# m2   model is called few it but 500K it were ran 
m2 <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/metis/20221205/metis_cjs2_growth2.rds")
outlist <- readRDS(m2)

# convergence 
map(1:16,~ plot(outlist[[.x]]$samples[,'mean.phi'],main=years[[.x]])) # 2011 2009 2003 2002
map(1:16,~ plot(outlist[[.x]]$samples[,'weanSurv'],main=years[[.x]])) # 2013 2003

# waic
WAIC.m.2<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) 


gelmanCJS_m <- vector("list",length=16)
for(i in 1:length(outlist)){
  gelmanCJS_m[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}
print(gelmanCJS_m)

map_dfr(1:length(gelmanCJS_m),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanCJS_m[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) # all of which above 1.1, 2000 worst

# yr     y
# <dbl> <dbl>
# 1  2009  1.18
# 2  2010  1.11
# 3  2011  1.27
# 4  2013  1.30


# m3
m3 <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/metis/20221205/cjs3_metis_semiRanefTGrowth2.rds")
outlist <- readRDS(m3)

# convergence 
map(1:16,~ plot(outlist[[.x]]$samples[,'mean.phi'],main=years[[.x]])) # with semi ranef WOW except 2011 - 2008
map(1:16,~ plot(outlist[[.x]]$samples[,'weanSurv'],main=years[[.x]])) # all ugly stil after semi ranef

# waic
WAIC.m.3<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) 
# 12  31.58797 2013  # louche 


# gelman
gelmanCJS_m <- vector("list",length=16)
for(i in 1:length(outlist)){
  gelmanCJS_m[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}
print(gelmanCJS_m)

map_dfr(1:length(gelmanCJS_m),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanCJS_m[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) # OK
# 1  1999  3.86
# 2  2009  1.10
# 3  2013  1.21
# 

# m4
m4 <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/metis/20221205/cjs4_metis_semiRanefTGrowth2.rds")
outlist <- readRDS(m4)


# convergence 
map(1:16,~ plot(outlist[[.x]]$samples[,'mean.phi'],main=years[[.x]])) #
map(1:16,~ plot(outlist[[.x]]$samples[,'weanSurv'],main=years[[.x]])) # 2013 2019

# waic
WAIC.m.4<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years)

# gelman 
gelmanCJS_m <- vector("list",length=16)
for(i in 1:length(outlist)){
  gelmanCJS_m[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}
print(gelmanCJS_m)

map_dfr(1:length(gelmanCJS_m),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanCJS_m[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) #
# 1  2000  1.19
# 2  2002  1.20
# 3  2003  1.11

# gelman 
gelmanCJS_m <- vector("list",length=16)
for(i in 1:length(outlist)){
    gelmanCJS_m[[i]] <- gelman.diag(outlist[[i]]$samples[,'mean.phi'])
}
print(gelmanCJS_m)

map_dfr(1:length(gelmanCJS_m),function(x) {
    data.frame(yr=years[x],
               y=as.numeric(unlist(gelmanCJS_m[[x]]))# $samples[,'weanSurv']
    )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) #

# 1  2000  1.18
# 2  2001  1.12
# 3  2002  1.28
# 4  2003  1.11
# 


# m5
m5 <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/metis/20221205/cjs5_metis_growth2.rds")
outlist <- readRDS(m5)

# convergence 
map(1:16,~ plot(outlist[[.x]]$samples[,'mean.phi'],main=years[[.x]])) # 2011 2010
map(1:16,~ plot(outlist[[.x]]$samples[,'weanSurv'],main=years[[.x]])) # 2013 2008 2003

# waic 
WAIC.m.5<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) # no need to transpose here

# gelman
gelmanCJS_m <- vector("list",length=16)
for(i in 1:length(outlist)){
  gelmanCJS_m[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}
#print(gelmanCJS_m)

map_dfr(1:length(gelmanCJS_m),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanCJS_m[[x]]))#
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)
# 1  2001  1.41
# 2  2002  2.92
# 3  2009  1.48
# 4  2014  1.28


# m6 
m6 <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/metis/20221205/cjs6_metis_semiRanefTID_Growth2.rds")
outlist <- readRDS(m6)

# convergence 
map(1:16,~ plot(outlist[[.x]]$samples[,'mean.phi'],main=years[[.x]])) # assez beau
map(1:16,~ plot(outlist[[.x]]$samples[,'weanSurv'],main=years[[.x]])) # laid

# waic 
WAIC.m.6<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) 

# gelman 
gelmanCJS_m <- vector("list",length=16)
for(i in 1:length(outlist)){
  gelmanCJS_m[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}
# print(gelmanCJS_m)

map_dfr(1:length(gelmanCJS_m),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanCJS_m[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) 
#1  2009  1.35




# clean up and save AIC tables ----------------------------------------------------------------

rm(list=setdiff(ls(), c("years", "aicT", "aicTM", "aicm1","aicm2","aicm3","aicm4","aicm5","aicm6","aicm7",
                        "aicb1","aicb2","aicb3","aicb4","aicb5","aicb6","aicb7", 
                        "WAIC.m.1", "WAIC.m.2", "WAIC.m.3", "WAIC.m.4", "WAIC.m.5","WAIC.m.6",  
                        "WAIC.b.1", "WAIC.b.2", "WAIC.b.3", "WAIC.b.4", "WAIC.b.5", "WAIC.b.6")))
#save(list = ls(), file = "/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/2022-12-07_modelSelection.RData")












# extract abundance from final JS models  ---------------------------------

rm(list =ls())

load("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/data/mine/2022-12-05_dataJS_pup35.RData")
years

# prepare df to graph
tmp <- data.frame(yr = rep(c(1998:2003, 2008:2016, 2019),2), site = c(rep('bic',16),rep('metis',16)), mean.N = NA, median.N=NA,ymin = NA, ymax = NA, 
                  mean.surv = NA, median.surv = NA,min.surv = NA, max.surv = NA)


# bst model for abundance 
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/bic/20210304/bic3") # lowest WAIC in most years 

fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
    fd[[i]]<-readRDS(file[[i]])
}

for(i in 1:length(fd)){
    sample_df=fd[[i]]$samples %>% map(as.data.frame) %>% bind_rows() # c long 
    N=sample_df[,grepl('Nsuper',colnames(sample_df))]
    tmp[tmp$site=='bic',]$mean.N[i]=mean(N)
    tmp[tmp$site=='bic',]$median.N[i]=median(N)
    tmp[tmp$site=='bic',]$ymin[i]= quantile(N, 0.025)
    tmp[tmp$site=='bic',]$ymax[i]=quantile(N, 0.975)
}


# now extract with a time lag
# tmp <- tibble(Nt1 = lead(tmp$N), tmp)
# tmp$lambda <- tmp$Nt1/tmp$N


# metis
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/metis/20210304/metis3")


fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
    fd[[i]]<-readRDS(file[[i]])
}

# check convergence 
codaSamp<-list()
for(i in 1:length(fd)){
    codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list()                 
    plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 16, 20))[[i]])
}


# get N
for(i in 1:length(fd)){
    sample_df=fd[[i]]$samples %>% map(as.data.frame) %>% bind_rows() 
    N=sample_df[,grepl('Nsuper',colnames(sample_df))] # un seul N, sur la bonne échelle
    tmp[tmp$site=='metis',]$mean.N[i]=mean(N)
    tmp[tmp$site=='metis',]$median.N[i]=median(N)
    tmp[tmp$site=='metis',]$ymin[i]= quantile(N, 0.025)
    tmp[tmp$site=='metis',]$ymax[i]=quantile(N, 0.975)
}

# N <- sapply(fd,function(x) x$summary$all.chains['Nsuper',]) %>%
#   t() %>% as.data.frame() %>% rename(CIL=`95%CI_low`,CIH=`95%CI_upp`) %>% mutate(yr=years)
# N$site <- "metis"
# ggplot(N,aes(x=yr,y=Mean,ymin=CIL,ymax=CIH))+geom_pointrange()


tmp1 <- data.frame(yr = rep(c(2004:2007, 2017:2018),2), site = c(rep('bic',6),rep('metis',6)), mean.N = NA, median.N=NA,ymin = NA, ymax = NA, mean.surv = NA, median.surv=NA,min.surv = NA, max.surv = NA)
results_jollySeber<-rbind(tmp, tmp1)

#export table with probabilities
results_jollySeber %>%
    mutate_if(is.numeric, round, digits = 1) 

ggplot(tmp,aes(x=yr,y=mean.N,ymin=ymin,ymax=ymax))+geom_pointrange()
ggplot(tmp,aes(x=yr,y=median.N,ymin=ymin,ymax=ymax))+geom_pointrange() # mean and median very similar



# get survival from the best CJS model at each site -----------------------------------------------------------
years = c(1998:2003, 2008:2016, 2019)

# these are the new versions with corrected birthdates - given that convergence was poor, sample size low and model complexity model 4 was selected for illustration. 

# second attempt,full it - estiamtes are back to low
con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/20221205/cjs4_bic_semiRanefTGrowth2.rds") 

outlist_b <- readRDS(con)

# compare to model 2
# con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/20221205/bic_cjs2_growth2.rds")


# extract df to plot
weanSurvOut <- sapply(outlist_b,function(x) x$summary$all.chains['weanSurv',]) %>%
  t() %>% as.data.frame() %>% rename(CIL=`95%CI_low`,CIH=`95%CI_upp`) %>% mutate(yr=years)
weanSurvOut$site <- "bic"
ggplot(weanSurvOut,aes(x=yr,y=Mean,ymin=CIL,ymax=CIH))+geom_pointrange()
ggplot(weanSurvOut,aes(x=yr,y=Median,ymin=CIL,ymax=CIH))+geom_pointrange()

#export table with probabilities if necessary 
weanSurvOut %>%
  mutate_if(is.numeric, round, digits = 2) 


# waic 
mean(sapply(outlist_b, function(x) x$WAIC$WAIC)) # double check with what's in WAIc.b2





# metis: no dominant models 

# con_m <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/metis/20221205/cjs4_metis_semiRanefTGrowth2.rds")

# con_m <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/metis/20221210/cjs4_metis_semiRanefTGrowth2.rds")

# ran with 2 M iterations
con_m <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/metis/20221223/cjs4_metis_semiRanefTGrowth2_23Dec12h.rds")

outlist_m <- readRDS(con_m)

mean(sapply(outlist_m, function(x) x$WAIC$WAIC))

# prepare df 
weanSurvOut_m <- sapply(outlist_m,function(x) x$summary$all.chains['weanSurv',]) %>%
  t() %>% as.data.frame() %>% rename(CIL=`95%CI_low`,CIH=`95%CI_upp`) %>% mutate(yr=years)

# quick plot
ggplot(weanSurvOut_m,aes(x=yr,y=Mean,ymin=CIL,ymax=CIH))+geom_pointrange()
ggplot(weanSurvOut_m,aes(x=yr,y=Median,ymin=CIL,ymax=CIH))+geom_pointrange()

  

# extract correlations between N (JS) and Surv (CJS) from lists for bic ------------------------------------------

# this is the df for wean surv
weanSurvOut_b <- map_dfr(1:length(outlist_b),function(x) {
  data.frame(yr=years[x],
             weanSurv=as.numeric(unlist(outlist_b[[x]]$samples[,'weanSurv'])),
               it=1:3000
    )
  }) %>% as.data.frame() #%>% ggplot(aes(x=yr,y=y))+geom_boxplot()

# this is the summary for plotting and exporting as supp 
weanSurvOut_b_summa <-weanSurvOut_b %>% group_by(yr) %>% 
  summarise(weanSurv_mean=mean(weanSurv),
            weanSurv_med=median(weanSurv),
             weanSurv_cil=as.numeric(quantile(weanSurv,0.025)),
              weanSurv_c25=as.numeric(quantile(weanSurv,0.25)),
              weanSurv_c75=as.numeric(quantile(weanSurv,0.75)),
              weanSurv_cih=as.numeric(quantile(weanSurv,0.975))
              
    )
ggplot(weanSurvOut_b_summa,aes(x=as.factor(yr)))+
    geom_linerange(aes(ymin=weanSurv_cil,ymax=weanSurv_cih))+
    geom_linerange(aes(ymin=weanSurv_c25,ymax=weanSurv_c75),size=2,color='grey40')+
    geom_point(aes(y=weanSurv_mean),shape=18,size=4)+
    labs(x='Year',y='survival until weaning')
  
  # getwd()
  
# best JS is model3 
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/bic/20210304/bic3")
mod3.bic<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
   mod3.bic[[i]]<-readRDS(file[[i]])
}
  
N_bic.3 <- map_dfr(1:length(mod3.bic),function(x) {
   l=which(colnames(mod3.bic[[x]]$samples[[1]])=='Nsuper')
   #map_df(mod3.bic[[x]]$samples,~ .x[,l])
   data.frame(yr=years[x],
              Nsuper=  as.numeric(unlist(map(mod3.bic[[x]]$samples,~ .x[,l]))),
             it=1:3000
  )
 }) %>% as.data.frame() #%>% ggplot(aes(x=yr,y=y))+geom_boxplot()

# sumamry 
N_bic.3_summa<-N_bic.3 %>% group_by(yr) %>% 
    summarise(Nsuper_mean=mean(Nsuper),
              Nsuper_median=median(Nsuper),
              Nsuper_cil=as.numeric(quantile(Nsuper,0.025)),
              Nsuper_c25=as.numeric(quantile(Nsuper,0.25)),
              Nsuper_c75=as.numeric(quantile(Nsuper,0.75)),
              Nsuper_cih=as.numeric(quantile(Nsuper,0.975))
    )
  

  ggplot(N_bic.3_summa,aes(x=as.factor(yr)))+
    geom_linerange(aes(ymin=Nsuper_cil,ymax=Nsuper_cih))+
    geom_linerange(aes(ymin=Nsuper_c25,ymax=Nsuper_c75),size=2,color='grey40')+
    geom_point(aes(y=Nsuper_mean),shape=18,size=4)+
    labs(x='Year',y='Population size')
  
  
  lbda_bic.3<- N_bic.3 %>% arrange(it,yr) %>% group_by(it) %>% 
    mutate(n_tp1=lead(Nsuper)) %>% filter(yr<2019) %>% # on pourrait enlever les ann/es crap en faisant un crap
    mutate(growth=n_tp1/Nsuper) %>% 
    left_join(weanSurvOut_b) %>% 
    as.data.frame()
 
  # lbda_bic.3 %>% #group_by(yr) %>% 
  #   summarise(lbda_mean=mean(growth),
  #             lbda_cil=as.numeric(quantile(growth,0.025)),
  #             lbda_c25=as.numeric(quantile(growth,0.25)),
  #             lbda_c75=as.numeric(quantile(growth,0.75)),
  #             lbda_cih=as.numeric(quantile(growth,0.975))
  #   )
  
  correlation_bic<- lbda_bic.3 %>% group_by(it) %>% 
    summarise(cor_sN=cor(weanSurv,Nsuper),
              cor_sgrowth=cor(weanSurv,growth))
  
  quantile(correlation_bic$cor_sN,probs = c(0.025,.5,0.975))
  # 
  # 2.5%         50%       97.5% 
  # -0.34819134  0.08120283  0.49321252 
  
  hist(correlation_bic$cor_sN);abline(v=0,col='red')
  
  mean(correlation_bic$cor_sN>0)  # 0.6466667
  
  quantile(correlation_bic$cor_sgrowth,probs = c(0.025,.5,0.975)) 
  # 2.5%         50%       97.5% 
  # -0.45749148 -0.01007952  0.52373828 
  
  
  hist(correlation_bic$cor_sgrowth);abline(v=0,col='red')
  mean(correlation_bic$cor_sgrowth>0)  # pseudo p-value=0.4916667
  
  

 # extract correlations from best models for metis -------------------------

# model 3 CJS is best but no clear best model 
# model 3 JS is best since models 6 and 7 did not converge 

years<- c(1998:2003, 2008:2016, 2019)

weanSurvOut_m <- map_dfr(1:length(outlist_m),function(x) {
    data.frame(yr=years[x],
          weanSurv=as.numeric(unlist(outlist_m[[x]]$samples[,'weanSurv'])),
          it=1:3000
          )
    }) %>% as.data.frame() #%>% ggplot(aes(x=yr,y=y))+geom_boxplot()

weanSurvOut_m_summa <-weanSurvOut_m %>% group_by(yr) %>% 
  summarise(weanSurv_mean=mean(weanSurv),
            weanSurv_med=median(weanSurv),
            weanSurv_cil=as.numeric(quantile(weanSurv,0.025)),
            weanSurv_c25=as.numeric(quantile(weanSurv,0.25)),
            weanSurv_c75=as.numeric(quantile(weanSurv,0.75)),
            weanSurv_cih=as.numeric(quantile(weanSurv,0.975))
            
  )
ggplot(weanSurvOut_m_summa,aes(x=as.factor(yr)))+
  geom_linerange(aes(ymin=weanSurv_cil,ymax=weanSurv_cih))+
  geom_linerange(aes(ymin=weanSurv_c25,ymax=weanSurv_c75),size=2,color='grey40')+
  geom_point(aes(y=weanSurv_med),shape=18,size=4)+
  labs(x='Year',y='survival until weaning')




# get abundance from JS metis
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/metis/20210304/metis3")
mod3.metis<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  mod3.metis[[i]]<-readRDS(file[[i]])
}

N_metis.3 <- map_dfr(1:length(mod3.metis),function(x) {
  l=which(colnames(mod3.metis[[x]]$samples[[1]])=='Nsuper')
  #map_df(mod3.metis[[x]]$samples,~ .x[,l])
  data.frame(yr=years[x],
             Nsuper=  as.numeric(unlist(map(mod3.metis[[x]]$samples,~ .x[,l]))),
             it=1:3000
  )
}) %>% as.data.frame() #%>% ggplot(aes(x=yr,y=y))+geom_boxplot()

N_metis.3_summa<-N_metis.3 %>% group_by(yr) %>% 
  summarise(Nsuper_mean=mean(Nsuper),
            Nsuper_median=median(Nsuper),
            Nsuper_cil=as.numeric(quantile(Nsuper,0.025)),
            Nsuper_c25=as.numeric(quantile(Nsuper,0.25)),
            Nsuper_c75=as.numeric(quantile(Nsuper,0.75)),
            Nsuper_cih=as.numeric(quantile(Nsuper,0.975))
  )

ggplot(N_metis.3_summa,aes(x=as.factor(yr)))+
  geom_linerange(aes(ymin=Nsuper_cil,ymax=Nsuper_cih))+
  geom_linerange(aes(ymin=Nsuper_c25,ymax=Nsuper_c75),size=2,color='grey40')+
  geom_point(aes(y=Nsuper_mean),shape=18,size=4)+
  labs(x='Year',y='Population size')


lbda_metis.3<- N_metis.3 %>% arrange(it,yr) %>% group_by(it) %>% 
  mutate(n_tp1=lead(Nsuper)) %>% filter(yr<2019) %>% # on pourrait enlever les ann/es crap en faisant un crap
  mutate(growth=n_tp1/Nsuper) %>% 
  left_join(weanSurvOut_m) %>% 
  as.data.frame()

correlation_Metis<- lbda_metis.3 %>% group_by(it) %>% 
  summarise(cor_sN=cor(weanSurv,Nsuper),
            cor_sgrowth=cor(weanSurv,growth))

quantile(correlation_Metis$cor_sN,probs = c(0.025,.5,0.975))
# 2.5%        50%      97.5% 
# -0.5131074 -0.1873888  0.2505913 

hist(correlation_Metis$cor_sN);abline(v=0,col='red')
mean(correlation_Metis$cor_sN>0)  # pseudo p-value 0.1916667

quantile(correlation_Metis$cor_sgrowth,probs = c(0.025,.5,0.975))
# 2.5%        50%      97.5% 
# -0.35811009 -0.09645841  0.38021881 

hist(correlation_Metis$cor_sgrowth);abline(v=0,col='red')
mean(correlation_Metis$cor_sgrowth>0)  # pseudo p-value 0.3103333


# save abundance and surv  ----------------------------------------

# remove all raw objects that are no longer useful 
rm(codaSamp, fd, data, tmp, tmp1, tmp2, x)

# save(list = ls(),
# file = "/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/2022-12-23_resultsCMR.RData")










# Supp Online Material  ---------------------------------------------------

# CJS model selection - both sites comined --------------------------------------------
years = c(1998:2003, 2008:2016, 2019)

# m1
con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/combined/20220822/cjs1_constP_Phi_combined.rds")
outlist <- readRDS(con)

# convergence 
map(1:16,~ plot(outlist[[.x]]$samples[,'mean.phi'],main=years[[.x]])) 
map(1:16,~ plot(outlist[[.x]]$samples[,'weanSurv'],main=years[[.x]]))

# waic
WAIC.1<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) 

# gelman stat
gelmanCJS <- vector("list",length=16)
for(i in 1:length(outlist)){
    gelmanCJS[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}


map_dfr(1:length(gelmanCJS),function(x) {
    data.frame(yr=years[x],
               y=as.numeric(unlist(gelmanCJS[[x]]))# $samples[,'weanSurv']
    )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) # most do not converge 

# yr     y
# 
# 1  1998  1.18
# 2  2001  1.28
# 3  2002  1.36
# 4  2008  4.93
# 5  2009  1.11
# 6  2011  1.13
# 7  2013  1.10


# m2
con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/combined/20220822/cjs2_fixPconstPhi_combined.rds")
outlist <- readRDS(con)

# convergence 
map(1:16,~ plot(outlist[[.x]]$samples[,'mean.phi'],main=years[[.x]])) 
map(1:16,~ plot(outlist[[.x]]$samples[,'weanSurv'],main=years[[.x]]))

# waic
WAIC.2<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) 
# gelman stat
gelmanCJS <- vector("list",length=16)
for(i in 1:length(outlist)){
    gelmanCJS[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}

map_dfr(1:length(gelmanCJS),function(x) {
    data.frame(yr=years[x],
               y=as.numeric(unlist(gelmanCJS[[x]]))# $samples[,'weanSurv']
    )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) # most do not converge 

# 1  2000  1.26
# 2  2001  1.15
# 3  2002  1.18
# 4  2003  1.20
# 5  2009  1.23
# 6  2013  1.27
# 7  2015  1.16



# m3
con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/combined/20220822/cjs3_fixPranefYrPhi_combined.rds")
outlist <- readRDS(con)

# convergence 
map(1:16,~ plot(outlist[[.x]]$samples[,'mean.phi'],main=years[[.x]])) 
map(1:16,~ plot(outlist[[.x]]$samples[,'weanSurv'],main=years[[.x]])) 

# waic
WAIC.3<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) # no need to transpose here

# gelman stat
gelmanCJS <- vector("list",length=16)
for(i in 1:length(outlist)){
    gelmanCJS[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}
# print(gelmanCJS)

map_dfr(1:length(gelmanCJS),function(x) {
    data.frame(yr=years[x],
               y=as.numeric(unlist(gelmanCJS[[x]]))# $samples[,'weanSurv']
    )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) # most do not converge 

# yr     y
# <dbl> <dbl>
# 1  2008  1.17
# 2  2011  1.21
# 3  2013  1.16


# m4
con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/combined/20220822/cjs4_constPranefYrPhi_combined.rds") 
outlist <- readRDS(con)

# convergence 
map(1:16,~ plot(outlist[[.x]]$samples[,'mean.phi'],main=years[[.x]])) 
map(1:16,~ plot(outlist[[.x]]$samples[,'weanSurv'],main=years[[.x]])) 

# waic
WAIC.4<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) 
# gelman stat
gelmanCJS <- vector("list",length=16)
for(i in 1:length(outlist)){
    gelmanCJS[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}


map_dfr(1:length(gelmanCJS),function(x) {
    data.frame(yr=years[x],
               y=as.numeric(unlist(gelmanCJS[[x]]))
    )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) 

# yr     y
# <dbl> <dbl>
# 1  2001  1.20
# 2  2002  1.37
# 3  2008  1.46
# 4  2009  1.39
# 5  2013  1.13


# m5
con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/combined/20220822/cjs5_ranefIdPconstPhi_combined.rds")
outlist <- readRDS(con)

# convergence 
map(1:16,~ plot(outlist[[.x]]$samples[,'mean.phi'],main=years[[.x]])) 
map(1:16,~ plot(outlist[[.x]]$samples[,'weanSurv'],main=years[[.x]])) 

# waic
WAIC.5<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) # no need to transpose here

# gelman stat
gelmanCJS <- vector("list",length=16)
for(i in 1:length(outlist)){
    gelmanCJS[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}


map_dfr(1:length(gelmanCJS),function(x) {
    data.frame(yr=years[x],
               y=as.numeric(unlist(gelmanCJS[[x]]))
    )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)
# yr     y
# 
# 1  2000  1.33
# 2  2001  1.22
# 3  2008  2.69
# 4  2009  1.67
# 5  2012  1.18
# 6  2013  1.23
# 7  2015  1.16
# 8  2019  1.19


# m6
con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/combined/20220822/cjs6_ranefidPranefYrPhi_combined.rds")
outlist <- readRDS(con)

# convergence 
map(1:16,~ plot(outlist[[.x]]$samples[,'mean.phi'],main=years[[.x]])) 
map(1:16,~ plot(outlist[[.x]]$samples[,'weanSurv'],main=years[[.x]])) 

# waic
WAIC.6<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) 

# gelman stat
gelmanCJS <- vector("list",length=16)
for(i in 1:length(outlist)){
    gelmanCJS[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}

#nicer
map_dfr(1:length(gelmanCJS),function(x) {
    data.frame(yr=years[x],
               y=as.numeric(unlist(gelmanCJS[[x]]))
    )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) 

# yr     y
# <dbl> <dbl>
# 1  1998  1.15
# 2  2000  1.27
# 3  2001  1.19
# 4  2002  1.13
# 5  2008  1.24
# 6  2009  1.17



# determine which is best when both sites are combined 
long_Surv_dt <- cbind(WAIC.1[,c(2,1)], WAIC.2[1], WAIC.3[1],WAIC.4[1], WAIC.5[1], WAIC.6[1])
names(long_Surv_dt)<-c("year", "model1", "model2", "model3", "model4", "model5", 'model6')
long_Surv_dt <- long_Surv_dt %>% mutate(across(where(is.numeric), round, 0))

bestmod <- apply(long_Surv_dt[,-1],1,which.min)
tmp <-cbind(long_Surv_dt$year,colnames(long_Surv_dt)[bestmod+1])

# [,1]   [,2]    
# [1,] "1998" "model3"
# [2,] "1999" "model2"
# [3,] "2000" "model6"
# [4,] "2001" "model2"
# [5,] "2002" "model2"
# [6,] "2003" "model3"
# [7,] "2008" "model4"
# [8,] "2009" "model3"
# [9,] "2010" "model2"
# [10,] "2011" "model2"
# [11,] "2012" "model2"
# [12,] "2013" "model5"
# [13,] "2014" "model2"
# [14,] "2015" "model2"
# [15,] "2016" "model3"
# [16,] "2019" "model3"


# survival at both sites using model 2 ------------------------------------
years <- c(1998:2003, 2008:2016, 2019)


# m2
con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/combined/20220822/cjs2_fixPconstPhi_combined.rds")
outlist_combined <- readRDS(con)

# convergence 
map(1:16,~ plot(outlist_combined[[.x]]$samples[,'mean.phi'],main=years[[.x]])) 
map(1:16,~ plot(outlist_combined[[.x]]$samples[,'weanSurv'],main=years[[.x]]))

# waic
WAIC.2<- sapply(outlist_combined,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) 
# gelman stat
gelmanCJS <- vector("list",length=16)
for(i in 1:length(outlist_combined)){
    gelmanCJS[[i]] <- gelman.diag(outlist_combined[[i]]$samples[,'weanSurv'])
}

map_dfr(1:length(gelmanCJS),function(x) {
    data.frame(yr=years[x],
               y=as.numeric(unlist(gelmanCJS[[x]]))# $samples[,'weanSurv']
    )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) 

# yr     y
# <dbl> <dbl>
# 1  2000  1.26 # best to exclude
# 2  2001  1.15
# 3  2002  1.18
# 4  2003  1.20
# 5  2009  1.23
# 6  2013  1.27 # best to exclude
# 7  2015  1.16
 

# this is the df for wean surv
weanSurvOut_2 <- map_dfr(1:length(outlist_combined),function(x) {
    data.frame(yr=years[x],
               weanSurv=as.numeric(unlist(outlist_combined[[x]]$samples[,'weanSurv'])),
               it=1:3000
    )
}) %>% as.data.frame() #%>% ggplot(aes(x=yr,y=y))+geom_boxplot()

# this is the summary for plotting and exporting as supp 
weanSurvOut_2_summa <-weanSurvOut_2 %>% group_by(yr) %>% 
    summarise(weanSurv_mean=mean(weanSurv),
              weanSurv_med=median(weanSurv),
              weanSurv_cil=as.numeric(quantile(weanSurv,0.025)),
              weanSurv_c25=as.numeric(quantile(weanSurv,0.25)),
              weanSurv_c75=as.numeric(quantile(weanSurv,0.75)),
              weanSurv_cih=as.numeric(quantile(weanSurv,0.975))
              
    )

# on the same figure, put separate sites as well, using model 2. 
con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/20220818/bic_cjs2.rds")
mod2_bic <- readRDS(con)
                    
# this is the df for wean surv
weanSurvOut_bic <- map_dfr(1:length(mod2_bic),function(x) {
    data.frame(yr=years[x],
               weanSurv=as.numeric(unlist(mod2_bic[[x]]$samples[,'weanSurv'])),
               it=1:3000
    )
}) %>% as.data.frame() #%>% ggplot(aes(x=yr,y=y))+geom_boxplot()

# this is the summary for plotting and exporting as supp 
weanSurvOut_bic_summa <-weanSurvOut_bic %>% group_by(yr) %>% 
    summarise(weanSurv_mean=mean(weanSurv),
              weanSurv_med=median(weanSurv),
              weanSurv_cil=as.numeric(quantile(weanSurv,0.025)),
              weanSurv_c25=as.numeric(quantile(weanSurv,0.25)),
              weanSurv_c75=as.numeric(quantile(weanSurv,0.75)),
              weanSurv_cih=as.numeric(quantile(weanSurv,0.975))
              
    )


# the model is called few it but 500K it were ran 
m2 <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/metis/20220517/metis_cjs2_fewIT.rds")
mod2_metis <- readRDS(m2)


# this is the df for wean surv
weanSurvOut_metis <- map_dfr(1:length(mod2_metis),function(x) {
    data.frame(yr=years[x],
               weanSurv=as.numeric(unlist(mod2_metis[[x]]$samples[,'weanSurv'])),
               it=1:3000
    )
}) %>% as.data.frame() #%>% ggplot(aes(x=yr,y=y))+geom_boxplot()

# this is the summary for plotting and exporting as supp 
weanSurvOut_metis_summa <-weanSurvOut_metis %>% group_by(yr) %>% 
    summarise(weanSurv_mean=mean(weanSurv),
              weanSurv_med=median(weanSurv),
              weanSurv_cil=as.numeric(quantile(weanSurv,0.025)),
              weanSurv_c25=as.numeric(quantile(weanSurv,0.25)),
              weanSurv_c75=as.numeric(quantile(weanSurv,0.75)),
              weanSurv_cih=as.numeric(quantile(weanSurv,0.975))
              
    )

# combine df
weanSurvOut_2_summa$site <- 'combined'
weanSurvOut_bic_summa$site <- 'bic'
weanSurvOut_metis_summa$site <- 'metis'
df_weanSurv <- rbind(weanSurvOut_2_summa, weanSurvOut_bic_summa, weanSurvOut_metis_summa)


# as of 2022 09 14 I remove 4 years with low convergence or cdrap results 
df_weanSurv <- df_weanSurv %>% filter(yr!=2003&yr!=2008&yr!=2019&yr!=2013)

# figure 2022 09 09 now with median 
p.m2=ggplot(df_weanSurv,aes(x=as.factor(yr), color = factor(site, levels=c('bic', 'metis', 'combined'))))+
    geom_linerange(aes(ymin=weanSurv_cil,ymax=weanSurv_cih), position=position_dodge(w=0.75)) +
    geom_linerange(aes(ymin=weanSurv_c25,ymax=weanSurv_c75),size=2, position=position_dodge(w=0.75))+
    geom_point(aes(y=weanSurv_med),shape=18,size=4, position=position_dodge(w=0.75))+
    labs(x='Year',y='Pre-weaning survival')+
    scale_y_continuous(limits = c(0,1),breaks = seq(from = 0, to = 1, by = 0.1)) +
    scale_colour_manual(labels = c("Bic Island", "Métis", 'Two sites'),values = c("bic" = "#FFDB6D", "metis" = "#00AFBB", 'combined'='grey40')) + guides(color=guide_legend(title=NULL)) +
    theme_cowplot(12) +
    theme(panel.grid.major = element_line(color="lightgrey", linetype = 'dotted'))

rm(df_weanSurv, gelmanCJS, gelmanCJS_b, gelmanCJS_m, mod2_bic, mod2_metis, outlist_b, outlist, outlist_m, outlist_combined, weanSurvOut_m_summa, weanSurvOut_b_summa, weanSurvOut_m, weanSurvOut_b, weanSurvOut_metis,weanSurvOut_metis_summa, weanSurvOut_2,weanSurvOut_2_summa, weanSurvOut_bic, weanSurvOut_bic_summa)

# combine 2 sites, m3 -----------------------------------------------------
con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/combined/20220822/cjs3_fixPranefYrPhi_combined.rds")
outlist_combined <- readRDS(con)

# this is the df for wean surv
weanSurvOut_2 <- map_dfr(1:length(outlist_combined),function(x) {
    data.frame(yr=years[x],
               weanSurv=as.numeric(unlist(outlist_combined[[x]]$samples[,'weanSurv'])),
               it=1:3000
    )
}) %>% as.data.frame() #%>% ggplot(aes(x=yr,y=y))+geom_boxplot()

# this is the summary for plotting and exporting as supp 
weanSurvOut_2_summa <-weanSurvOut_2 %>% group_by(yr) %>% 
    summarise(weanSurv_mean=mean(weanSurv),
              weanSurv_med=median(weanSurv),
              weanSurv_cil=as.numeric(quantile(weanSurv,0.025)),
              weanSurv_c25=as.numeric(quantile(weanSurv,0.25)),
              weanSurv_c75=as.numeric(quantile(weanSurv,0.75)),
              weanSurv_cih=as.numeric(quantile(weanSurv,0.975))
              
    )



# this is the chunk to extract from gab's code with fake occasions spaced out every day.
con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/20220908/cjs3_bic_semiRanefYr.rds") 
outlist_b <- readRDS(con)

# this is the df for wean surv
weanSurvOut_bic <- map_dfr(1:length(outlist_b),function(x) {
    data.frame(yr=years[x],
               weanSurv=as.numeric(unlist(outlist_b[[x]]$samples[,'weanSurv'])),
               it=1:3000
    )
}) %>% as.data.frame() #%>% ggplot(aes(x=yr,y=y))+geom_boxplot()

# this is the summary for plotting and exporting as supp 
weanSurvOut_bic_summa <-weanSurvOut_bic %>% group_by(yr) %>% 
    summarise(weanSurv_mean=mean(weanSurv),
              weanSurv_med=median(weanSurv),
              weanSurv_cil=as.numeric(quantile(weanSurv,0.025)),
              weanSurv_c25=as.numeric(quantile(weanSurv,0.25)),
              weanSurv_c75=as.numeric(quantile(weanSurv,0.75)),
              weanSurv_cih=as.numeric(quantile(weanSurv,0.975))
              
    )


# metis: model 3 too but no dominant models 
con_m <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/metis/20220908/cjs3_semiRanefYr_metis.rds")
outlist_m <- readRDS(con_m)

# this is the df for wean surv
weanSurvOut_metis <- map_dfr(1:length(outlist_m),function(x) {
    data.frame(yr=years[x],
               weanSurv=as.numeric(unlist(outlist_m[[x]]$samples[,'weanSurv'])),
               it=1:3000
    )
}) %>% as.data.frame() #%>% ggplot(aes(x=yr,y=y))+geom_boxplot()

# this is the summary for plotting and exporting as supp 
weanSurvOut_metis_summa <-weanSurvOut_metis %>% group_by(yr) %>% 
    summarise(weanSurv_mean=mean(weanSurv),
              weanSurv_med=median(weanSurv),
              weanSurv_cil=as.numeric(quantile(weanSurv,0.025)),
              weanSurv_c25=as.numeric(quantile(weanSurv,0.25)),
              weanSurv_c75=as.numeric(quantile(weanSurv,0.75)),
              weanSurv_cih=as.numeric(quantile(weanSurv,0.975))
              
    )

# combine df
weanSurvOut_2_summa$site <- 'combined'
weanSurvOut_bic_summa$site <- 'bic'
weanSurvOut_metis_summa$site <- 'metis'
df_weanSurv <- rbind(weanSurvOut_2_summa, weanSurvOut_bic_summa, weanSurvOut_metis_summa)
# df_weanSurv <- df_weanSurv %>% filter(yr!=2003|site!='bic') %>% filter(yr!=2013|site!='metis')


df_weanSurv <- df_weanSurv %>% filter(yr!=2003&yr!=2008&yr!=2019&yr!=2013)



# figure 2022 09 09 now with median 
p.m3=ggplot(df_weanSurv,aes(x=as.factor(yr), color = factor(site, levels=c('bic', 'metis', 'combined'))))+
    geom_linerange(aes(ymin=weanSurv_cil,ymax=weanSurv_cih), position=position_dodge(w=0.75)) +
    geom_linerange(aes(ymin=weanSurv_c25,ymax=weanSurv_c75),size=2, position=position_dodge(w=0.75))+
    geom_point(aes(y=weanSurv_med),shape=18,size=4, position=position_dodge(w=0.75))+
    labs(x='Year',y='Pre-weaning survival')+
    scale_y_continuous(limits = c(0,1),breaks = seq(from = 0, to = 1, by = 0.1)) +
    scale_colour_manual(labels = c("Bic Island", "Métis", 'Two sites'),values = c("bic" = "#FFDB6D", "metis" = "#00AFBB", 'combined'='grey40')) + guides(color=guide_legend(title=NULL)) +
    theme_cowplot(12) +
    theme(panel.grid.major = element_line(color="lightgrey", linetype = 'dotted'))

cowplot::plot_grid(p.m2, p.m3, ncol=1, labels=c('A', 'B'))


# ggsave("FIGS1_survival_allSites_panel.png", height=10,width=18,unit='in', dpi = 600)









# Supplementary figures  --------------------------------------------------


# extract and plot p per sampling occasion BIC from the Jolly Seber -------------------------------
# get date from original data 
load("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/data/mine/20211031_cmr_pup35.RData")
data<-bicData35.

for (t in 1:length(years)) { # takes a while
  days<-unique(data$date[data$year==years[t]]) # make sure dates are arranged
}

# extract coda sample from good model 
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/bic/20210304/bic3")

fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
}

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # transform to coda # pour le summary coda 
}


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


p_bic <- lapply(tmp2, function(d) ggplot(data = d, aes(x=date, y=mean)) + 
              geom_pointrange(data = d, aes(ymin=low, ymax=high), size = 0.3) + #geom_smooth(se=F) + 
              ylab("Capture probability") + xlab("Date") +  ggtitle(paste("Year ", d$year, sep=""))+ 
              scale_y_continuous(limits = c(0,1),breaks = seq(from = 0, to = 1, by = 0.2)) + 
              theme_cowplot(8)+
              theme(axis.line = element_line(),
                    panel.grid.minor = element_line(color="transparent"),
                    panel.grid.major = element_line(color="lightgrey", linetype = 'dotted')))

# # exporter tous les graphs
# pdf("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/graph/FIGS2_2022-09-11_pPerOccasionsBic.pdf", height= 11, width=9)
do.call("grid.arrange", p_bic)
dev.off()


 # show only year 2019 (n=56 p. recapt of 0.052) and 1999 n=37, p=0.811)\
p_panel <- lapply(tmp2, function(d) ggplot(data = d, aes(x=date, y=mean)) + 
                  geom_pointrange(data = d, aes(ymin=low, ymax=high), size = 0.3) + #geom_smooth(se=F) + 
                  ylab("Capture probability") + xlab("Date") +  # ggtitle(paste("Year ", d$year, sep=""))+ 
                  scale_y_continuous(limits = c(0,1),breaks = seq(from = 0, to = 1, by = 0.2)) +
                  theme_cowplot()+
                  theme(axis.line = element_line(),
                        panel.grid.minor = element_line(color="transparent"),
                        panel.grid.major = element_line(color="lightgrey", linetype = 'dotted')))

cowplot::plot_grid(p_panel[[16]],p_panel[[2]], labels=c('a)','b)'))

# ggsave("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/graph/FIG_2panel_2022-05-13_pPerOccasionsBic.pdf",
#        height= 10, # max height of 23.7 cm
#        width = 18.2,
#        unit= "cm",
#        dpi = 300)#





# b - recruitment from JS model per occasions -----------------------------


# state process + observation process. the probability that a member of Ns enters the pop at occasion t is bt and is called the entry probability. 
# it is the probability than an indvidual is new in the population and has entered the population since the preceding occasion.
# entry coul result either from in situ recruitment (locally born individuals) or from immigration. 
# THI SIS NOT A RECUITMENT PROBABILITY. THE NUMBER OF IND ENTEREING THE POPULATION AT T IS BT=NSBT. THE FRACTION OF INDIVIDUALS ALREAD PRESENT AT TEH FIRST OCCASION IS B1 = THE ENTRY PROB HAS NO CLEAR ECOLOGICAL MEANING BECUASE IT IS A COMPLEX FUNCTION OF ALL ENTRIES BEFORE THE FIRST OCCASION. ALL ENTRY PROB MUST SUM TO 1 to ensure tha ll Ns individuals enter the population sometime during teh studyl 
# the superpop model uses entry probabilities b and an inclusion parameter psi. b are analogous to removal entry prob but not calculated the same way 
# the dirichlet prior is used. 
# allocate the entries of al indivdiuals uniformly over T occasions, alpha of 1 for all t. 

rm(tmp, tmp1, tmp2)
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


b_bic <- lapply(tmp2, function(d) ggplot(data = d, aes(x=date, y=mean)) + 
              geom_pointrange(data = d, aes(ymin=low, ymax=high), size = 0.3) + #geom_smooth(se=F) + 
              ylab("Entry probability") + xlab("Date")+
                  ggtitle(paste("Year ", d$year, sep=""))+
              scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.2))+
                  theme_cowplot(8)+
                  theme(axis.line = element_line(),
                        panel.grid.minor = element_line(color="transparent"),
                        panel.grid.major = element_line(color="lightgrey", linetype = 'dotted')))

# export
# pdf("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/graph/FIGS3_2022-09-11_bPerOccasion_bic.pdf", height= 11, width=9)
do.call("grid.arrange", b_bic)
dev.off()


# pdf("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/graph/2022-05-13_bPerOccasion_bic.pdf", width = "8", height="8")
# do.call("grid.arrange", b_bic)
# dev.off()


# p - Metis -------------------------------
# get date from original data 


rm(tmp, tmp1, tmp2, codaSamp)


data<-metisData35.

for (t in 1:length(years)) { # takes a while
  days<-unique(data$date[data$year==years[t]]) # make sure dates are arranged
}

# extract coda sample from good model 
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/metis/20210304/metis3")

fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
}

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # transform to coda # pour le summary coda 
}


# start binding things 
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

p_metis <- lapply(tmp2, function(d) ggplot(data = d, aes(x=date, y=mean)) + 
                    geom_pointrange(data = d, aes(ymin=low, ymax=high), size = 0.3) + #geom_smooth(se=F) + 
                    ylab("Capture probability") + xlab("Date") +  ggtitle(paste("Year ", d$year, sep=""))+ 
                    scale_y_continuous(limits = c(0,1),breaks = seq(from = 0, to = 1, by = 0.2)) + 
                    theme_cowplot(8)+
                    theme(axis.line = element_line(),
                          panel.grid.minor = element_line(color="transparent"),
                          panel.grid.major = element_line(color="lightgrey", linetype = 'dotted')))

# export
#pdf("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/graph/FIGS4_2022-09-11_pPerOccasions_metis.pdf", height= 11, width=9)
do.call("grid.arrange", p_metis)
dev.off()



# b - metis ---------------------------------------------------------------
# get date from original data 

rm(tmp, tmp1, tmp2)
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


b_metis <- lapply(tmp2, function(d) ggplot(data = d, aes(x=date, y=mean)) + 
                  geom_pointrange(data = d, aes(ymin=low, ymax=high), size = 0.3) + #geom_smooth(se=F) + 
                  ylab("Entry probability") + xlab("Date")+ ggtitle(paste("Year ", d$year, sep=""))+
                  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.2))+
                      theme_cowplot(8)+
                      theme(axis.line = element_line(),
                            panel.grid.minor = element_line(color="transparent"),
                            panel.grid.major = element_line(color="lightgrey", linetype = 'dotted')))

#pdf("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/graph/FIGS5_2022-09-11_bPerOccasions_metis.pdf", height= 11, width=9)
do.call("grid.arrange", b_metis)
dev.off()








# daily surv from CJS per occasion at Bic ----------------------------------------------
load("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/data/mine/2022-12-05_dataCJS_pup35.RData")

#extract dates, starting at bic 
data<-bicData35.
years=c(1998:2003, 2008:2016, 2019)

# need to compute interval between date to match to surv between occasions

t=data %>% group_by(year) %>% summarise(date =unique(date),
                                        num.date=1:length(unique(date)), 
                                        occ=lead(num.date)-1) %>% 
    dplyr::select(year, date, occ) %>% na.omit()
    


con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/20221205/cjs4_bic_semiRanefTGrowth2.rds")
outlist <- readRDS(con)

survJourn=map_dfr(1:length(outlist), function(t){
    tmp=outlist[[t]]$samples
    tmp2=tmp[,grepl("ranef.t",colnames(tmp[[1]]))] %>% map_dfr(~as.data.frame(.x))
    dailysurvs<-as.data.frame(matrix(NA, nrow(tmp2),ncol(tmp2)))
    for(d in 1:ncol(tmp2)){
        dailysurvs[,d]=inv.logit(unlist(tmp[,'mean.phi'])+ tmp2[,d]) # sauve pas mean.phi
    }
    survJourn=tidyr::pivot_longer(dailysurvs,everything(),names_to = 'occ', values_to = 'dailySurv')
    survJourn$occ=as.numeric(substr(survJourn$occ, 2, 3))
    survJourn$year = years[t]
    return(survJourn)
}
)



# survJourn %>% group_by(year, occ) %>% summarise(mean=mean(dailySurv), cil=quantile(dailySurv, 0.025), cih=quantile(dailySurv, 0.975)) %>% ggplot(aes(x=occ, y=mean, ymin=cil,ymax=cih))+geom_pointrange()+facet_wrap(~year)


# my tests 
test=survJourn %>% group_by(year, occ)  %>% filter(year!=2002&year!=2008) %>% summarise(mean=mean(dailySurv), cil=quantile(dailySurv, 0.025), cih=quantile(dailySurv, 0.975)) 

m=merge(t, test) %>% arrange(year, date, occ) 
m$yday=yday(m$date)

m%>% ggplot(aes(x=yday(date), y=mean, ymin=cil,ymax=cih))+geom_pointrange() + facet_wrap(~year) + xlab('Date (Julian Day)') + ylab('Daily survival')+
#+scale_x_date(date_labels = "%b %d")
theme_cowplot(8)+
    theme(axis.line = element_line(),
          panel.grid.minor = element_line(color="transparent"),
          panel.grid.major = element_line(color="lightgrey", linetype = 'dotted'))
# ggsave('/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/graph/FigS7_dailySurv_bic_2022-12-10.png',height= 11, width=9)



# metis 

#extract dates, starting at bic 
data<-metisData35.
years=c(1998:2003, 2008:2016, 2019)

# need to compute interval between date to match to surv between occasions

t=data %>% group_by(year) %>% summarise(date =unique(date),
                                        num.date=1:length(unique(date)), 
                                        occ=lead(num.date)-1) %>% 
    dplyr::select(year, date, occ) %>% na.omit()



con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/metis/20221223/cjs4_metis_semiRanefTGrowth2_23Dec12h.rds")
outlist <- readRDS(con)

# this extracts daily surv at each iteration and plot them
survJourn=map_dfr(1:length(outlist), function(t){
    tmp=outlist[[t]]$samples
    tmp2=tmp[,grepl("ranef.t",colnames(tmp[[1]]))] %>% map_dfr(~as.data.frame(.x))
    dailysurvs<-as.data.frame(matrix(NA, nrow(tmp2),ncol(tmp2)))
    for(d in 1:ncol(tmp2)){
        dailysurvs[,d]=inv.logit(unlist(tmp[,'mean.phi'])+ tmp2[,d]) # sauve pas mean.phi
    }
    survJourn=tidyr::pivot_longer(dailysurvs,everything(),names_to = 'occ', values_to = 'dailySurv')
    survJourn$occ=as.numeric(substr(survJourn$occ, 2, 3))
    survJourn$year = years[t]
    return(survJourn)
}
)
# survJourn %>% group_by(year, occ) %>% summarise(mean=mean(dailySurv), cil=quantile(dailySurv, 0.025), cih=quantile(dailySurv, 0.975)) %>% ggplot(aes(x=occ, y=mean, ymin=cil,ymax=cih))+geom_pointrange()+facet_wrap(~year)

# my tests 
test=survJourn %>% group_by(year, occ)  %>% 
    filter(year!=2000&year!=2002&year!=2013) %>% 
    summarise(mean=mean(dailySurv), cil=quantile(dailySurv, 0.025), cih=quantile(dailySurv, 0.975)) 

m=merge(t, test) %>% arrange(year, date, occ) 
m$yday=yday(m$date)

m%>% ggplot(aes(x=yday(date), y=mean, ymin=cil,ymax=cih))+geom_pointrange() + facet_wrap(~year) + xlab('Date (Julian Day)') + ylab('Daily survival')+
    #+scale_x_date(date_labels = "%b %d")
    theme_cowplot(8)+
    theme(axis.line = element_line(),
          panel.grid.minor = element_line(color="transparent"),
          panel.grid.major = element_line(color="lightgrey", linetype = 'dotted'))

# ggsave('/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/graph/FigS7_dailySurv_metis-2022-12-10.png',height= 11, width=9)





















# 
# # this is the chunk to extract from gab's code with fake occasions spaced out every day.
# load(here("data/mine/20211031_cmr_pup35.RData"))
# years
# 
# con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/20220422/bic_cjs2_fewIT.rds")
# outlist <- readRDS(con)
# sapply(outlist, function(x) x$WAIC$WAIC) # double check with what's in WAIc.b2
# 
# # extract df to plot
# weanSurvOut <- sapply(outlist,function(x) x$summary$all.chains['weanSurv',]) %>%
#   t() %>% as.data.frame() %>% rename(CIL=`95%CI_low`,CIH=`95%CI_upp`) %>% mutate(yr=years)
# weanSurvOut$site <- "bic"
# ggplot(weanSurvOut,aes(x=yr,y=Mean,ymin=CIL,ymax=CIH))+geom_pointrange()
# 
# #export table with probabilities if necessary 
# weanSurvOut %>%
#   mutate_if(is.numeric, round, digits = 2) 
# 
# 
# 
# # metis: seems to be model 3
# con_m <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/metis/20220511/cjs3_ranef_metis.rds")
# outlist_m <- readRDS(con_m)
# sapply(outlist_m, function(x) x$WAIC$WAIC)
# 
# # prepare df 
# weanSurvOut_m <- sapply(outlist_m,function(x) x$summary$all.chains['weanSurv',]) %>%
#   t() %>% as.data.frame() %>% rename(CIL=`95%CI_low`,CIH=`95%CI_upp`) %>% mutate(yr=years)
# 
# weanSurvOut_m$site <- 'metis'
# colnames(weanSurvOut_m)
# colnames(results_weanSurv)
# 
# 





# # this is the chunk to extract from gab's original code with fake occasions spaced out every day.
# load(here("data/mine/20211031_cmr_pup35.RData"))
# years
# 
# con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/20220422/bic_cjs2_fewIT.rds")
# outlist <- readRDS(con)
# sapply(outlist, function(x) x$WAIC$WAIC) # double check with what's in WAIc.b2
# 
# # extract df to plot
# weanSurvOut <- sapply(outlist,function(x) x$summary$all.chains['weanSurv',]) %>%
#   t() %>% as.data.frame() %>% rename(CIL=`95%CI_low`,CIH=`95%CI_upp`) %>% mutate(yr=years)
# weanSurvOut$site <- "bic"
# ggplot(weanSurvOut,aes(x=yr,y=Mean,ymin=CIL,ymax=CIH))+geom_pointrange()
# 
# #export table with probabilities
# weanSurvOut %>%
#   mutate_if(is.numeric, round, digits = 2) 
# 
# # for supp mat - all iterations
# map_dfr(1:length(outlist),function(x) {
#   data.frame(yr=years[x],
#              y=as.numeric(unlist(outlist[[x]]$samples[,'weanSurv']))
#   )
# }) %>% as.data.frame() %>% ggplot(aes(x=yr,y=y))+geom_boxplot()
# 
# # map_dfr(1:length(years),function(x) {
# #   data.frame(yr=years[x],
# #              y=as.numeric(unlist(outlist[[x]]$samples[,'dailySurv']))
# #   )
# # }) %>% as.data.frame() %>% ggplot(aes(x=yr,y=y))+geom_boxplot()
# 
# # extract birthdate 
# map_dfr(1:length(years),function(x) {
#   data.frame(yr=years[x],
#              y=as.numeric(unlist(outlist[[x ]]$samples[,'bDate']))
#   )
# }) %>% as.data.frame()

