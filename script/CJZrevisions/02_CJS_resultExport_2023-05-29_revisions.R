
# SCRIPT TO UPTDATE FOR REVISIONS




library(tidyverse)
library(coda)
library(cowplot)
library(coda)
library(boot)
library(ggmcmc)
library(dplyr)
library(data.table)

rm(list = ls())

# change path according to day of results 
# setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal")

# load df
load("data/mine/2023-04-04_revisedDf.RData")

# extract table of sample size -------------------------------------------------------------

# calculate unique IDs and dates
n_unique_ids <- nlevels(droplevels(pvData_filtered$myID))
n_unique_dates <- length(unique(pvData_filtered$date))
pvData_filtered$myID <- droplevels(pvData_filtered$myID)

tableS1 <- pvData_filtered %>%
    group_by(mySite, year) %>%
    reframe(
        unique_ID = n_distinct(myID),
        nb_male = n_distinct(ifelse(sex == "male", myID, NA), na.rm = TRUE),
        nb_female = n_distinct(ifelse(sex == "female", myID, NA), na.rm = TRUE),
        nb_unknown_sex = n_distinct(ifelse(is.na(sex), myID, NA), na.rm = TRUE),
        min_date = min(ymd(date)),
        max_date = max(ymd(date)),
        recapture_prop = {
            t1 <- table(myID)
            tmp <- filter(pvData_filtered, myID %in% names(t1)[t1 > 1])
            tmp %>%
                filter(mySite == first(mySite)) %>%
                group_by(year) %>%
                reframe(recapt = n_distinct(myID)) %>%
                mutate(prop = recapt / unique_ID) %>%
                pull(prop) %>%
                first()
        },
        effort = n_distinct(ymd(date)) # calculate the unique number of dates
    )


tableS1 <- tableS1 %>%mutate_if(is.numeric, round, digits = 2) 
write.csv2(tableS1, file = "output.nosync/data/revisions/tableS1.csv", row.names=F)


sum(tableS1$unique_ID) # 1368
sum(tableS1$nb_female) # 638
sum(tableS1$nb_male) # 725
sum(tableS1$nb_unknown_sex) # 7


# the HTML file will be saved as my_table.html in the working directory


# model selection ---------------------------------------------------------

# change path according to day of results 
# setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/revisions")

daypath="output.nosync/data/revisions"
daypath="cache"



# build WAIC table and fill it

fd<-list()
file <-list.files(path = daypath,pattern = ".rds",full.names = T)
file <- file[str_detect(file,"_m")]

waic_table <- data.frame(cjs_m = rep(NA,length(file)),fd.id=NA, WAIC = NA, pWAIC=NA)
for(i in seq_along(file)){
    fd[[i]]<-readRDS(file[[i]])
    filename_uid <- file[i]
    waic_table[i,1]<-filename_uid
    waic_table[i,2]<-i
    waic_table[i,3]<-fd[[i]][["WAIC"]][["WAIC"]]
    waic_table[i,4]<-fd[[i]][["WAIC"]][["pWAIC"]]
}

waic_table <- arrange(waic_table,WAIC)
waic_table

# Model convergence -----------------------------------------

bestOut <- fd[[waic_table$fd.id[1]]]  # lowest WAIC


# plot important parameters
goodParam <- c("sd.bd","mu.bd", "weanSurv", "dailySurv","p.","p.mean","mean.p", "phi.")
goodParam <- unique(unlist(sapply(goodParam, function(p) grep(p, x = colnames(bestOut$samples[[1]])))))
goodParam <-c(colnames(bestOut$samples[[1]])[goodParam],
              paste0("bDate[",sample(1:1000,5),"]")
)
traceplot(bestOut$samples[,goodParam])
nogood=row.names(bestOut$summary$all.chains)[bestOut$summary$all.chains[,"St.Dev."]==0]
if(length(nogood)>0) goodParam <- goodParam[-which(goodParam%in%nogood)]
rm(nogood)

bestOut$summary$all.chains[goodParam,]

# get Gelman-Rubin statistics of convergence 
# Values substantially above 1 indicate lack of convergence. 
# If the chains have not converged, Bayesian credible intervals based on the t-distribution are too wide, and have the potential to shrink by this factor if the MCMC run is continued.

gelmanCJS.m1 <- gelman.diag(bestOut$samples[,goodParam])
gelmanCJS.m1

# correlation between chains
betaz <- map_dfr(bestOut$samples,~ as.data.frame(.x))
cor(betaz[,goodParam]) %>% ggcorrplot::ggcorrplot()


# extract-plot P -------------------------------------------------------

#    P ~ yr (m6)
P.table=betaz %>% transmute_at(vars(`betaYear[2]`:`betaYear[16]`),~ plogis(.x+mean.p )) %>% 
    mutate(`betaYear[1]`=plogis(betaz$mean.p)) %>% 
    select(`betaYear[1]`,everything()) %>% 
    map_dfr(function(x){
    tibble(mean=mean(x),
           cil=quantile(x,0.025),
           cih=quantile(x,0.975)
    )
}) %>% mutate(year=c(1998:2003, 2008:2016, 2019))

ggplot(P.table,aes(x=year,y=mean,ymin=cil,ymax=cih))+geom_pointrange()+
    labs(x='Year',y='Capture probability')+
    theme_bw()


#    P ~ site + (1|yr)  (m9,m15)    ====   option1 
P.table=betaz %>% transmute(site0=plogis(p.mean),
                    site1=plogis(p.mean+p.betaSite)) %>% 
    map_dfr(function(x){
        tibble(mean=mean(x),
               cil=quantile(x,0.025),
               cih=quantile(x,0.975)
        )
    },.id = "site")

P.table %>%  ggplot(aes(x=site,y=mean,ymin=cil,ymax=cih))+geom_pointrange()+
    labs(x='Year',y='Capture probability')+
    theme_bw()

#    P ~ site + (1|yr)  (m9,m15)    ====   option2
P.table=betaz %>% transmute_at(vars(`p.betaYear[1]`:`p.betaYear[16]`),
                       .funs = list(site0=~ plogis(.x ),site1=~ plogis(.x+p.betaSite ))) %>% 
    map_dfr(function(x){
        tibble(mean=mean(x),
               cil=quantile(x,0.025),
               cih=quantile(x,0.975)
        )
    },.id = "yrSite") %>% 
    mutate(year=rep(c(1998:2003, 2008:2016, 2019),2),
           site=rep(0:1,each=16))%>% 
    mutate(year=ifelse(site==0,year-0.1,year+0.1))

ggplot(P.table,aes(x=year,color=as.factor(site),y=mean,ymin=cil,ymax=cih))+
    geom_pointrange()+
    labs(x='Year',y='Capture probability')+
    labs(x='Year',y='Capture probability') +
    scale_shape_manual(labels = c("Bic Island", "Métis"),values = c(21,19)) +
    guides(shape=guide_legend(title=NULL)) +
    cowplot::theme_cowplot(10) +
    theme(panel.grid.major = element_line(color="lightgrey", linetype = 'dotted'))


#    P ~ site*yr fix (m2c,m15c)
P.table=betaz %>% transmute_at(vars(`betaYear[1, 1]`:`betaYear[2, 16]`),~ plogis(.x)) %>% 
    map_dfr(function(x){
        tibble(mean=mean(x),
               cil=quantile(x,0.025),
               cih=quantile(x,0.975)
        )
    },.id = "siteYr") %>% 
    mutate(site=rep(0:1,16),year=rep(c(1998:2003, 2008:2016, 2019),each=2)) %>% 
    mutate(year=ifelse(site==0,year-0.1,year+0.1))

ggplot(P.table,aes(x=year,shape=as.factor(site),y=mean,ymin=cil,ymax=cih))+geom_pointrange()+
    labs(x='Year',y='Capture probability') +
    scale_shape_manual(labels = c("Bic Island", "Métis"),values = c(21,19)) +
    guides(shape=guide_legend(title=NULL)) +
    cowplot::theme_cowplot(10) +
    theme(panel.grid.major = element_line(color="lightgrey", linetype = 'dotted'))


# p ~ year*Occ

betaYearjj[year_int[i],t]

p.occ=expand.grid(year=1:16,Occ=1:55)
p.occ=apply(p.occ,1,function(x){
    tmp=plogis(betaz[,paste0("betaYearjj[",x[1],", ",x[2],"]")]+betaz[,paste0("mean.p[1, ",x[1],"]")])
    c(x[1],x[2], mean=mean(tmp),
      cil=as.numeric(quantile(tmp,0.025)),
      cih=as.numeric(quantile(tmp,0.975))
    )
}) %>% t() %>% as.data.frame()

ggplot(p.occ,aes(x=Occ,color=as.factor(year),y=mean))+
    geom_path(position = position_dodge(w=0.2))+
    labs(x='year',y='Daily capture probability')+
    theme_bw()





# extract-plot  phi -------------------------------------------------------

#  phi~ sex+site m15

Phi.table=betaz %>% transmute(m0=plogis(`phi.mean[1]`),
                               m1=plogis(`phi.mean[2]`),
                               f0=plogis(`phi.mean[1]`+phi.sex),
                               f1=plogis(`phi.mean[1]`+phi.sex)) %>% 
    map_dfr(function(x){
        tibble(mean=mean(x),
               cil=quantile(x,0.025),
               cih=quantile(x,0.975)
        )
    },.id = "sexSite") %>% 
    mutate(site=rep(0:1,2),sex=as.factor(rep(c("male","female"))),each=2)
ggplot(Phi.table,aes(x=sex,shape=as.factor(site),y=mean,ymin=cil,ymax=cih))+
    geom_pointrange(position = position_dodge(w=0.2))+
    labs(x='sex',y='Daily survival probability')+
    scale_shape_manual(labels = c("Bic Island", "Métis"),values = c(21,19)) +
    guides(shape=guide_legend(title=NULL)) +
    cowplot::theme_cowplot(10) +
    theme(panel.grid.major = element_line(color="lightgrey", linetype = 'dotted'))

# phi ~  site*year
Phi.table=P.table=betaz %>% transmute_at(vars(`phi.betaYear[1, 1]`:`phi.betaYear[2, 16]`),~ plogis(.x)) %>% 
    map_dfr(function(x){
        tibble(mean=mean(x),
               cil=quantile(x,0.025),
               cih=quantile(x,0.975)
        )
    },.id = "siteYr") %>% 
    mutate(site=rep(0:1,16),year=rep(c(1998:2003, 2008:2016, 2019),each=2)) %>% 
    mutate(year=ifelse(site==0,year-0.1,year+0.1))

  
ggplot(Phi.table,aes(x=year,color=as.factor(site),y=mean,ymin=cil,ymax=cih))+
    geom_pointrange(position = position_dodge(w=0.2))+
    labs(x='year',y='Daily survival probability')+
    theme_bw()



WeanSurv.table=betaz %>% transmute_at(vars(`phi.betaYear[1, 1]`:`phi.betaYear[2, 16]`),~ plogis(.x)^30) %>% 
    map_dfr(function(x){
        tibble(mean=mean(x),
               cil=quantile(x,0.025),
               cih=quantile(x,0.975)
        )
    },.id = "siteYr") %>% 
    mutate(site=rep(0:1,16),year=rep(c(1998:2003, 2008:2016, 2019),each=2)) %>% 
    mutate(year=ifelse(site==0,year-0.1,year+0.1))

ggplot(WeanSurv.table,aes(x=year,color=as.factor(site),y=mean,ymin=cil,ymax=cih))+
    geom_pointrange(position = position_dodge(w=0.2))+
    labs(x='year',y='Pre-weaning survival')+
    theme_bw()


# phi ~  Occ

phi.occ=expand.grid(year=1:16,Occ=1:55)
phi.occ=apply(phi.occ,1,function(x){
    tmp=plogis(betaz[,paste0("phi.occ[",x[1],", ",x[2],"]")]+betaz[,paste0("phi.betaYear[1, ",x[1],"]")])
    c(x[1],x[2], mean=mean(tmp),
               cil=as.numeric(quantile(tmp,0.025)),
               cih=as.numeric(quantile(tmp,0.975))
        )
    }) %>% t() %>% as.data.frame()

ggplot(phi.occ,aes(x=Occ,color=as.factor(year),y=mean))+
    geom_path(position = position_dodge(w=0.2))+
    labs(x='year',y='Daily survival probability')+
    theme_bw()




# output from JS model ----------------------------------------------------------


file <- paste0("cache/20230516-11h_js1c_",1:16,".rds")
fd.js <- list()
for(i in seq_along(file)){
    fd.js[[i]]<-try(readRDS(file[[i]]))
    
}
goodYear=which(map_chr(fd.js,class)!="try-error")
years=c(1998:2003,2008:2016,2019)
Nsupers <- map_dfr(goodYear,function(i){
    Nsup1 <- fd.js[[i]]$samples[,"NsuperS1"]
    Nsup0 <- fd.js[[i]]$samples[,"Nsuper"]-Nsup1
    weanSurv0=unlist((bestOut$samples[,paste0("phi.betaYear[1, ",i,"]")]))
    weanSurv1=unlist(bestOut$samples[,paste0("phi.betaYear[2, ",i,"]")])
    
    tibble(yr=i,year=years[i],
           NSuper=c(as.numeric(fd.js[[i]]$samples[,"Nsuper"]-Nsup1),as.numeric(Nsup1)),
           weaSurv=c(sample(weanSurv0,length(Nsup1)),sample(weanSurv1,length(Nsup1))),
           site=rep(0:1,each=length(Nsup1)),
           it=rep(1:length(Nsup1),2))
})

Nsupers %>% group_by(year,site) %>% 
    summarise(y=median(NSuper),ymin=quantile(NSuper,0.025),ymax=quantile(NSuper,0.975)) %>% 
ggplot(aes(x=year,y=y,ymin=ymin,ymax=ymax,shape=as.factor(site)))+
    geom_pointrange(position = position_dodge(w=0.4))+
    labs(x='Year',y='Seal pup abundance') +
    scale_shape_manual(labels = c("Bic Island", "Métis"),values = c(21,19)) +
    guides(shape=guide_legend(title=NULL)) +
    cowplot::theme_cowplot(10) +
    theme(panel.grid.major = element_line(color="lightgrey", linetype = 'dotted'))

Nsupers %>% group_by(it) %>% summ


# 
# # extract weanSurv over years and site 
# weanSurvOut_b <- map_dfr(1:length(newOut$samples),function(x) {
#     data.frame(weanSurv=as.numeric(unlist(newOut[[x]]$samples[,'weanSurv']))
#     )
# }) %>% as.data.frame() #%>% ggplot(aes(x=yr,y=y))+geom_boxplot()
# 
# # get model
# #newOut <- readRDS('/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/revisions/2023-04-05/cjs_m2_fewIt.rds')
# newOut <- readRDS("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/revisions/202304261441_m7b.rds")
# 
# # extract posterior samples for betaSite
# betaSite_samples <- data.frame(iteration = 1:length(unlist(newOut$samples[,'betaSite'])),
#                                betaSite = unlist(newOut$samples[,'betaSite']))
# 
# # extract the samples for the current year'
# library(purrr)
# betaYear_samples <- map_dfr(newOut$samples[,paste0("betaYear[", 1:16, "]")], as.data.frame,.id="chain")
# dim(betaSite_samples)
# dim(betaYear_samples)    
# 
# predp <- matrix(NA,nrow=nrow(betaYear_samples),ncol=16*2)
# 
# predp[,17:32] <-  as.matrix(betaYear_samples[,-1])+ unlist(newOut$sample[,"mean.p"])+ unlist(betaSite_samples$betaSite)
# predp[,1:16] <-  as.matrix(betaYear_samples[,-1])+unlist(newOut$sample[,"mean.p"])
# colnames(predp) <- paste0(rep(c("S1","S2"),each=16),"Y",1:16)
# predp=plogis(predp)
# 
# # view the first few rows of the data frame
# head(betaYear_samples)
# 
# # calculate mean and credible intervals for predp
# predp_graph_df <- data.frame(y=apply(predp, 2, mean),
#                              ymin=apply(predp,2,quantile,0.025),
#                              ymax=apply(predp,2, quantile,0.975)) %>% 
#     mutate(site=rep(c('bic','metis'),each=16),year=rep(c(1998:2003, 2008:2016,2019),2))
# 
# ggplot(predp_graph_df,aes(x=as.factor(year), y=y, shape=as.factor(site)))+
#     geom_linerange(aes(ymin=ymin,ymax=ymax), position=position_dodge(w=0.75))+
#     geom_point(aes(y=y), size=4,position=position_dodge(w=0.75))+
#     #geom_linerange(aes(ymin=weanSurv_c25,ymax=weanSurv_c75),size=2,color='grey40')+
#     labs(x='Year',y='Capture probability') +
#     scale_shape_manual(labels = c("Bic Island", "Métis"),values = c(21,19)) +
#     guides(shape=guide_legend(title=NULL)) +
#     cowplot::theme_cowplot(10) +
#     theme(panel.grid.major = element_line(color="lightgrey", linetype = 'dotted'))
# 
# # interactive effect 
# newOut <- readRDS('/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/revisions/fewIt/cjs_m2_interactionYrSite_fewIt.rds')
# 
# # extract samples from each site
# betaSite_samples <- data.frame(iteration = 1:length(unlist(newOut$samples[,'betaSite'])),
#                                betaSite = unlist(newOut$samples[,'betaSite']))
# 
# # extract the samples for the current year' - additive
# betaYear_samples <- map_dfr(newOut$samples[,paste0("betaYearAdd[", 1:16, "]")], as.data.frame,.id="chain")
# 
# dim(betaSite_samples)
# dim(betaYear_samples)    
# 
# betaYearSite_samples <- map_dfr(newOut$samples[,paste0("betaYearSite[", 1:16, "]")], as.data.frame,.id="chain")
# dim(betaYearSite_samples)
# 
# predp <- matrix(NA,nrow=nrow(betaYear_samples),ncol=16*2)
# 
# # commence par site 2 - interactif
# class(newOut$sample[, "mean.p"])
# class(betaYearSite_samples)
# 
# predp[,17:32] <-  as.matrix(betaYear_samples[,-1]) + unlist(newOut$sample[,"mean.p"]) + unlist(betaSite_samples$betaSite) + unlist(newOut$sample[,"mean.p"]) * as.matrix(betaYearSite_samples[,-1])
# 
# # site 1 - interactif
# predp[,1:16] <-  as.matrix(betaYear_samples[,-1])+unlist(newOut$sample[,"mean.p"])+unlist(newOut$sample[,"mean.p"])*as.matrix(betaYearSite_samples[,-1])
# 
# colnames(predp) <- paste0(rep(c("S1","S2"),each=16),"Y",1:16)
# 
# # put back to original scale
# predp=plogis(predp)
# 
# # calculate mean and credible intervals for predp
# predp_graph_df <- data.frame(y=apply(predp, 2, mean),
#                              ymin=apply(predp,2,quantile,0.025),
#                              ymax=apply(predp,2, quantile,0.975)) %>% 
#     mutate(site=rep(c('bic','metis'),each=16),year=rep(c(1998:2003, 2008:2016,2019),2))
# 
# ggplot(predp_graph_df,aes(x=as.factor(year), y=y, shape=as.factor(site)))+
#     geom_linerange(aes(ymin=ymin,ymax=ymax), position=position_dodge(w=0.75))+
#     geom_point(aes(y=y), size=4,position=position_dodge(w=0.75))+
#     #geom_linerange(aes(ymin=weanSurv_c25,ymax=weanSurv_c75),size=2,color='grey40')+
#     labs(x='Year',y='Capture probability') +
#     scale_shape_manual(labels = c("Bic Island", "Métis"),values = c(21,19)) +
#     guides(shape=guide_legend(title=NULL)) +
#     theme_cowplot(10) +
#     theme(panel.grid.major = element_line(color="lightgrey", linetype = 'dotted'))















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



# model 7b
outm7 <- read_rds("cache/202304060943_m7.rds")
outm7b <- read_rds("output.nosync/data/revisions/202304261441_m7b.rds")
outm7b <- read_rds("output.nosync/data/revisions/202304281441_m7b.rds")

outm7c <- read_rds("cache/202304060943_m7c.rds")

library(coda)


traceplot(outm7$samples[,c('mean.p',"mean.phi","betaSite",
                           "betaYear[4]","betaYear[12]","sd.p",
                           "bDate[6]","mu.bd","sd.bd")
])
traceplot(outm7b$samples[,c('mean.p',"mean.phi","betaSite",
                            "betaYear[4]","betaYear[12]","sd.p",
                            "bDate[6]","mu.bd","sd.bd")]) # mubd sd bd flat

traceplot(outm7c$samples[,c('mean.p',"mean.phi","betaSite",
                            "betaYear[4]","betaYear[12]","sd.p",
                            "bDate[6]","mu.bd","sd.bd")])









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

