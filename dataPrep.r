library(randomForest)
library(reshape2)
print(load('RAND_study_student_problem_stats_anon.Robj'))
names(x)[2] <- 'id'
## hints

(tab <- table(x$nhints1))
#hist(x$nhints1[x$nhints1>5])

# duplicates across years?
yrStuds <- list()
for(yr in 1:3)
    yrStuds[[yr]] <- unique(x$id[x$study.year==yr])
numStuds <- vapply(yrStuds,function(yr) sum(length(yr)),1)
sum(numStuds)
length(unique(x$id))
## 131 stuidents re-took (unless someone took 3 yrs...)
length(intersect(intersect(yrStuds[[1]],yrStuds[[2]]),yrStuds[[3]]))
#nope no 3-years

## if someone's in there multiple times, use only first showing
oneAndTwo <- intersect(yrStuds[[1]],yrStuds[[2]])
drop <- 1:nrow(x)
drop <- drop[x$id%in%oneAndTwo & x$study.year==2]
x <- x[-drop,]
length(unique(x$id)) #should =9696

twoAndThree <- intersect(yrStuds[[2]],yrStuds[[3]])
length(twoAndThree)
length(intersect(yrStuds[[1]],yrStuds[[3]])) #for completeness
### k problem solved


units=function(dat1){
#    dat1 <- x[x$id==sid,,drop=FALSE]
    units1 <- unique(dat1$unit)
    units <- vapply(levels(dat1$unit),
                 function(un) is.element(un,units1),
                 logical(1))
    c(numUnit=sum(units),units)
}

sections <- function(dat1){
    sections1 <- unique(dat1$section)
    sum(sections1%in%levels(dat1$section))
}


## what do we want?
studDat <- function(sid){
    dat1 <- x[x$id==sid,]
    c(
        study.year=mean(dat1$study.year),
        minHints =min(dat1$nhints1,na.rm=TRUE),
        meanHints =mean(dat1$nhints1,na.rm=TRUE),
        medHints = median(dat1$nhints1,na.rm=TRUE),
        maxHints=max(dat1$nhints1,na.rm=TRUE),
        nProbs=sum(!is.na(dat1$nerrs1)),
        minTime=min(dat1$total_time1,na.rm=TRUE),
        meanTime =mean(dat1$total_time1,na.rm=TRUE),
        medTime= median(dat1$total_time1,na.rm=TRUE),
        maxTime=max(dat1$total_time1,na.rm=TRUE),
        minErr=min(dat1$nerrs1,na.rm=TRUE),
        meanErr=mean(dat1$nerrs1,na.rm=TRUE),
        medErr=median(dat1$nerrs1,na.rm=TRUE),
        maxErr=max(dat1$nerrs1,na.rm=TRUE),
        numSec=sections(dat1),
        units(dat1)
        )
}

aggDat <- vapply(unique(x$id),studDat,studDat(x$id[1])); save(list=c('aggDat'),file='studentAggData.RData')
load('studentAggData.RData')
#aggDat <- t(aggDat)
#aggDat <- as.data.frame(aggDat)

pdf('histograms.pdf')
for(i in c(1:11,269)) hist(aggDat[,i],main=colnames(aggDat)[i])
dev.off()

library(xtable)
sumTab <- NULL
for(i in c(1:11,269)) sumTab <- rbind(sumTab,summary(aggDat[,i]))
rownames(sumTab) <- colnames(aggDat)[c(1:11,269)]
xtable(sumTab)

stud.pca <- princomp(aggDat[,c(1:11,269)],cor=TRUE)
plot(stud.pca)
loadings(stud.pca)
biplot(stud.pca)

## merge aggDat with covariates
aggDat$sid <- unique(x$id)
m1 <- read.csv('M1_algebra_rcal_20121119_fieldid.csv')
m2 <- read.csv('M2_algebra_rcal_20121119_fieldid.csv')
h1 <- read.csv('H1_algebra_rcal_20121119_fieldid.csv')
h2 <- read.csv('H2_algebra_rcal_20121119_fieldid.csv')

covs <- rbind(m1[,2:78],m2[,2:78],h1[,2:78],h2[,2:78])
covs$sid <- c(m1$field_id,m2$field_id,h1$field_id,h2$field_id)

covs <- covs[,-grep('post',names(covs))]

covsMis <- vapply(covs,function(varb) mean(is.na(varb)),1)
covs <- covs[,covsMis<0.25]

## for now, mean/mode imputation
Mode <- function(x) levels(x)[which.max(table(x))]

for(i in 1:ncol(covs)){
    if(is.numeric(covs[,i]))
        covs[is.na(covs[,i]),i] <- mean(covs[,i],na.rm=TRUE)
    else{
        if(!is.factor(covs[,i])) covs[,i] <- as.factor(covs[,i])
        covs[is.na(covs[,i]),i] <- Mode(covs[,i])
    }
}

aggDatCov <- merge(aggDat,covs,by='sid',all.y=TRUE)

aggDatCov$miss <- is.na(aggDatCov$nProbs)

rfMiss <- randomForest(subset(covs,treatment==1,
                              select=-c(teachid2,classid2,schoolid2,pair)),
                       as.factor(aggDatCov$miss[covs$treatment==1]),)





pcr.cov <- princomp(aggDatCov,cor=T)


for(lev in c('m','h')) for(yr in c(1,2)){
    dat <- eval(parse(text=paste(lev,yr,sep='')))
    datCov <- dat[,c(2:32,grep('lag',names(dat)),
                     which(names(dat)=='field_id'))])

}




unitDat <- vapply(
    unique(x$unit),
           function(un){
               print(un);
               return(c(minHints=min(x$nhints1[x$unit==un],na.rm=TRUE),
               meanHints=mean(x$nhints1[x$unit==un],na.rm=TRUE),
               medHind=median(x$nhints1[x$unit==un],na.rm=TRUE),
               maxHints=max(x$nhints1[x$unit==un],na.rm=TRUE),
               minTime=min(x$total_time1[x$unit==un],na.rm=TRUE),
               meanTime=mean(x$total_time1[x$unit==un],na.rm=TRUE),
               medTime=median(x$total_time1[x$unit==un],na.rm=TRUE),
               maxTime=max(x$total_time1[x$unit==un],na.rm=TRUE),
               minErr=min(x$nerrs1[x$unit==un],na.rm=TRUE),
               meanErr=mean(x$nerrs1[x$unit==un],na.rm=TRUE),
               medErr=median(x$nerrs1[x$unit==un],na.rm=TRUE),
               maxErr=max(x$nerrs1[x$unit==un],na.rm=TRUE)))},
           numeric(12))

unitDat <- t(unitDat)
save(list='unitDat',file='unitDat.RData')
unitDat <- as.data.frame(unitDat)
unitDat$unitName <- unique(x$unit)




###########
#### I wanted to re-compute numSec, since several "sections" were actually tests
### I did this separately
### I loaded the
load('RAND_study_student_problem_stats_anon.Robj')
names(x)[2] <- 'id'
source('../code/dups.r')

test <- grepl('[Tt]est$',levels(x$section))
notTest <- levels(x$section)[test==FALSE]
xNoTest <- x[x$section%in%notTest,]
numSec2 <- vapply(unique(xNoTest$id),function(id) length(unique(xNoTest$section[xNoTest$id==id])),1)

load('studentAggData.RData')
noTest <- data.frame(id=unique(xNoTest$id),numSec2=numSec2)
aggDat <- as.data.frame(t(aggDat))
aggDat$id <- unique(x$id)
aggDat <- merge(aggDat,noTest,by='id',all.x=TRUE)
aggDat$numSec2[is.na(aggDat$numSec2)] <- 0
save(aggDat,file='studentAggData.RData')


