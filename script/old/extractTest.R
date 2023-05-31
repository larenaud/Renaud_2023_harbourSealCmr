# weaning test 
years = c(1998:2003, 2008:2016, 2019)


# 3 semi ranef
con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/20220908/cjs3_bic_semiRanefYr.rds")
outlist_b <- readRDS(con)

# extract df to plot
weanSurvOut <- sapply(outlist_b,function(x) x$summary$all.chains['weanSurv',]) %>%
    t() %>% as.data.frame() %>% rename(CIL=`95%CI_low`,CIH=`95%CI_upp`) %>% mutate(yr=years)

# quick plot
ggplot(weanSurvOut,aes(x=yr,y=Mean,ymin=CIL,ymax=CIH))+geom_pointrange() # low




# 3 NOT semi ranef
con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/20220517/cjs3_ranef.rds")
outlist_b <- readRDS(con)

# extract df to plot
weanSurvOut <- sapply(outlist_b,function(x) x$summary$all.chains['weanSurv',]) %>%
    t() %>% as.data.frame() %>% rename(CIL=`95%CI_low`,CIH=`95%CI_upp`) %>% mutate(yr=years)

# quick plot
ggplot(weanSurvOut,aes(x=yr,y=Mean,ymin=CIL,ymax=CIH))+geom_pointrange() # low



# to extract daily surv
load("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/data/mine/20211031_cmr_pup35.RData")

#extract dates, starting at bic 
data<-bicData35.
years=c(1998:2003, 2008:2016, 2019)

# need to compute interval between date to match to surv between occasions

t=data %>% group_by(year) %>% summarise(date =unique(date),
                                        num.date=1:length(unique(date)), 
                                        occ=lead(num.date)-1) %>% 
    select(year, date, occ) %>% na.omit()



con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/20220908/cjs3_bic_semiRanefYr.rds")
outlist <- readRDS(con)

survJourn=map_dfr(1:length(outlist), function(t){
    tmp=outlist[[t]]$samples
    tmp2=tmp[,grepl("ranef.yr",colnames(tmp[[1]]))] %>% map_dfr(~as.data.frame(.x))
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


test=survJourn %>% group_by(year, occ)  %>% filter(year!=2003&year!=2008&year!=2019) %>% summarise(mean=mean(dailySurv), cil=quantile(dailySurv, 0.025), cih=quantile(dailySurv, 0.975)) 

m=merge(t, test) %>% arrange(year, date, occ) 
m$yday=lubridate::yday(m$date)

m%>% ggplot(aes(x=lubridate::yday(date), y=mean, ymin=cil,ymax=cih))+geom_pointrange() + facet_wrap(~year) + xlab('Date (Julian Day)') + ylab('Daily survival')+
    #+scale_x_date(date_labels = "%b %d")
    theme_cowplot(8)+
    theme(axis.line = element_line(),
          panel.grid.minor = element_line(color="transparent"),
          panel.grid.major = element_line(color="lightgrey", linetype = 'dotted'))




