library(missForest)

print(load('data/studentAggData.RData'))
#aggDat <- as.data.frame(t(aggDat))
print(load('israel/noDuplicates.RData'))
#aggDat$id <- unique(xNoDup$id)

                                        #load in covariates
for(lev in c('M','H')) for(yr in c(1,2))
    assign(paste('covs',lev,yr,sep=''),subset(read.csv(paste('data/',lev,yr,'_algebra_rcal_20121119_fieldid.csv',sep='')),
           select=-X))

## start with H2

dat <- aggDat[aggDat$study.year==2,]


### isolate pre-treatment variables
## dropflag is post-treatment
outcomesCols <- c(10,22:25,28:29,grep('post',names(covsH2)),56:62,93)
names(covsH2)[outcomesCols] <- paste(names(covsH2)[outcomesCols],'OUT',sep='')
covsCols <- seq(1,ncol(covsH2)-1)[-outcomesCols]
names(covsH2)[covsCols] <- paste(names(covsH2)[covsCols],'COV',sep='')
#outcomes <- covsH2[,outcomesCols]
#covs <- covsH2[,-outcomesCols]
names(dat) <- paste(names(dat), 'USE',sep='')

dat <- merge(dat,covsH2,by.x='idUSE',by.y='field_id',all.y=TRUE)

covs <- dat[,grep('COV',names(dat))]
names(covs) <- sub('COV','',names(covs))
outcomes <- dat[,grep('OUT',names(dat))]
names(outcomes) <- sub('OUT','',names(outcomes))
usage <- dat[,grep('USE',names(dat))]
names(usage) <- sub('USE','',names(usage))

#save(list=c('dat','covs','outcomes','usage'),file='model1dat.RData')

## missing covariate data
miss <- sapply(covs,function(x) mean(is.na(x)))
covs <- covs[,miss<.5]
ids <- covs[,c('pair','classid2','schoolid2','teachid','teachid2')]
covs <- subset(covs,select=-c(pair,classid2,schoolid2,schoolidn,classid,teachid,teachid2))
covs <- as.data.frame(lapply(covs,function(varb){
    if(is.numeric(varb))
        if(length(unique(varb))<5) return(as.factor(varb))
    varb}))
between <- function(x,a,b) x>a & x<b
covs <- covs[,!between(sapply(covs,nlevels),0,2)]
covsImp <- missForest(covs)

save(list=c('covs','usage','dat','outcomes','covsImp','ids'),file='data/model1dat.RData')
