load('RAND_study_student_problem_stats_anon.Robj')

normalize <- function(section){
    xSec <- x[x$section==section,]
    mHints <- mean(xSec$nhints1,na.rm=TRUE)
    mErr <- mean(xSec$nerrs1,na.rm=TRUE)
    sdHints <- sd(xSec$nhints1,na.rm=TRUE)
    sdErr <- sd(xSec$nerrs1,na.rm=TRUE)

    mat <- cbind((xSec$nhints1-mHints)/sdHints,
                 (xSec$nerrs1-mErr)/sdErr,
                 xSec$RAND_field_id)

    mat
}

normalized <- NULL
for(s in unique(x$section)) normalized <- rbind(normalized,normalize(s))




errHintMean <- function(id){
    datID <- normalized[normalized[,3]==id,1:2]
    if(!is.matrix(datID)) return(c(id,datID))
    c(id,apply(datID,2,function(col) mean(col,na.rm=TRUE)))
}

means <- vapply(unique(x$RAND_field_id),errHintMean,numeric(3))

means[means==NaN] <- 0
