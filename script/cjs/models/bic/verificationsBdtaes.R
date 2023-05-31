post=newOut$samples$chain1
post=outlist_m[[1]]$samples$chain1
bloup=newOut
bloup=outlist_m[[1]]
bloup$summary$all.chains[1:38,]


colnames(post)

newdat=data.frame(id=dflist[[1]]$const$nimbleID,julianDay=dflist[[1]]$const$julianDay, rawwt=dflist[[1]]$data$mass)
newdat
postpred=matrix(nrow=nrow(newdat),ncol = nrow(post))
for(i in 1:nrow(newdat)){
postpred[i,] <- 10 + post[,"beta.wt"] * (newdat$julianDay[i] -
                post[,paste0("bDate[",newdat$id[i],"]")]) 
}

newdat$predWt=apply(postpred,1,mean)
newdat$predWt_cil=apply(postpred,1,quantile, 0.025)
newdat$predWt_cih=apply(postpred,1,quantile, 0.975)

ggplot(newdat,aes(x=julianDay,y=predWt,ymin=predWt_cil,ymax=predWt_cih))+geom_pointrange()+geom_point(aes(y=rawwt),color='red')

newdat2= expand.grid(id=1:38,julianDay=seq(130,180,10)) %>% arrange(id)

for(i in 1:nrow(newdat2)){
    newdat2$predGrowth[i] <- 10 + bloup$summary$all.chains["beta.wt","Mean"] * (newdat2$julianDay[i] -
                                                                                    bloup$summary$all.chains[paste0("bDate[",newdat2$id[i],"]"),"Mean"]) 
}
ggplot(newdat,aes(x=julianDay,y=predWt))+geom_pointrange(aes(ymin=predWt_cil,ymax=predWt_cih))+geom_point(aes(y=rawwt),color='red')+
    geom_line(data=newdat2,aes(y=predGrowth,color=as.factor(id)))

weanSurvOut <- sapply(outlist,function(x) x$summary$all.chains['weanSurv',]) %>%
    t() %>% as.data.frame() %>% rename(CIL=`95%CI_low`,CIH=`95%CI_upp`) %>% mutate(yr=years)
weanSurvOut$site <- "bic"
ggplot(weanSurvOut,aes(x=yr,y=Median,ymin=CIL,ymax=CIH))+geom_pointrange()

#ggplot(weanSur