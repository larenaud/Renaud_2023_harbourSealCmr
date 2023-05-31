rm(list = ls())
load("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/data/mine/20211031_cmr_pup35.RData")

# calculating delta occ to add dummy var ----------------------------------
t=bicData35_l[['1998']] %>%
    group_by(myID) %>%
    mutate(delta.occ = julianDay - lag(julianDay, default = julianDay[1])) %>% summarise(delta.occ)
mean(t$delta.occ)
t <- sort(unique(bicData35_l[["1998"]]$julianDay), decreasing = F)
diff(t)
mean(diff(t))


# data <- bicData35_l[['1998']]
bicData35_l[['1998']]$myID<-droplevels(bicData35_l[['1998']]$myID)
ch=with(bicData35_l[['1998']],table(myID,date))
dim(ch)

mat = matrix(NA, 38, 2)
#cbind(mat, ch)
ch <- cbind(ch[,1:8],mat, ch[,10])
colnames(ch) <- NULL

#must replace this year in the list. 
list_ch_bic[['1998']] <- ch


# data <- bicData35_l[['1998']]
bicData35_l[['1999']]$myID<-droplevels(bicData35_l[['1999']]$myID)
ch=with(bicData35_l[['1999']],table(myID,date))
dim(ch)
t <- sort(unique(bicData35_l[["1999"]]$julianDay), decreasing = F)
diff(t)

mat = matrix(NA, 37, 2)

#cbind(mat, ch)
ch <- cbind(ch[,1:11],mat, ch[,12])
colnames(ch) <- NULL

#must replace this year in the list. 
list_ch_bic[['1999']] <- ch

# try another year 
t <- sort(unique(bicData35_l[["2000"]]$julianDay), decreasing = F) # only 3 dates
diff(t)
mean(diff(t))


# data <- bicData35_l[['1998']]
bicData35_l[['2000']]$myID<-droplevels(bicData35_l[['2000']]$myID)
ch=with(bicData35_l[['2000']],table(myID,date))
dim(ch)

mat = matrix(NA, 41, 1)

#cbind(mat, ch)
ch <- cbind(ch[,1:14],mat, ch[,15])
colnames(ch) <- NULL

#must replace this year in the list. 
list_ch_bic[['2000']] <- ch


# try another year 
t <- sort(unique(bicData35_l[["2001"]]$julianDay), decreasing = F) # only 3 dates
diff(t)
mean(diff(t))


# data <- bicData35_l[['1998']]
bicData35_l[['2001']]$myID<-droplevels(bicData35_l[['2001']]$myID)
ch=with(bicData35_l[['2001']],table(myID,date))
dim(ch)

mat = matrix(NA, 46, 1)

#cbind(mat, ch)
ch <- cbind(ch[,1:3],mat, ch[,4:7], mat, ch[,8:11])
colnames(ch) <- NULL

#must replace this year in the list. 
list_ch_bic[['2001']] <- ch



# try another year 
t <- sort(unique(bicData35_l[["2002"]]$julianDay), decreasing = F) # only 3 dates
diff(t)
mean(diff(t))


# data <- bicData35_l[['1998']]
bicData35_l[['2002']]$myID<-droplevels(bicData35_l[['2002']]$myID)
ch=with(bicData35_l[['2002']],table(myID,date))
dim(ch)

mat = matrix(NA, 54, 1)

#cbind(mat, ch)
ch <- cbind(ch[,1:3],mat, ch[,4:9], mat, ch[,10:16])
colnames(ch) <- NULL

#must replace this year in the list. 
list_ch_bic[['2002']] <- ch



# try another year 
t <- sort(unique(bicData35_l[["2003"]]$julianDay), decreasing = F) # only 3 dates
diff(t)
mean(diff(t))


# data <- bicData35_l[['1998']]
bicData35_l[['2003']]$myID<-droplevels(bicData35_l[['2003']]$myID)
ch=with(bicData35_l[['2003']],table(myID,date))
dim(ch)

mat = matrix(NA, 29, 2)

#cbind(mat, ch)
ch <- cbind(ch[,1],mat, ch[,2], mat, ch[,3])
colnames(ch) <- NULL

#must replace this year in the list. 
list_ch_bic[['2003']] <- ch


# bicData35_l[[j]]$nimbleID <- match(bicData35_l[[j]]$myID, rownames(list_ch_bic[[j]]), nomatch = NA_integer_, incomparables = NULL)
# data$nimbleID <- match(data$myID, rownames(ch), nomatch = NA_integer_, incomparables = NULL)





# save as new dummy df
save(list = ls(), file = "/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/data/mine/2022-02-25_dummyDf.RData") # by site for pup less than 35 kg, males and females


