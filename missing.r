library(randomForest)

load('data/RAND_study_student_problem_stats_anon.Robj')
h2 <- read.csv('data/H2_algebra_rcal_20121119_fieldid.csv')
load('data/studentAggData.RData')

newDat <- merge(aggDat,h2,by.x='id',by.y='field_id',all.y=TRUE)
Zsch <- vapply(unique(newDat$schoolid2),function(id) mean(newDat$treatment[newDat$schoolid2==id]),1)

usageNA <- vapply(unique(newDat$schoolid2), function(id) mean(is.na(newDat$numSec[newDat$schoolid2==id])),1)

nas <- vapply(h2,function(col) mean(is.na(col)),2)

pred <- h2
pred <- pred[,nas==0]

keep <- vapply(pred,function(col) ifelse(is.factor(col),ifelse(nlevels(col)<10,TRUE,FALSE),TRUE),TRUE)

pred <- pred[,keep]

missingSchools <- unique(newDat$schoolid2)[usageNA==1]

pred <- pred[!newDat$schoolid2%in%missingSchools,]

rf <- randomForest(pred,as.factor(is.na(newDat$numSec[!newDat$schoolid2%in%missingSchools])))
