
usage$totalTime <- usage$meanTime*usage$nProbs

inRange <- is.na(usage$totalTime) |(usage$totalTime>0 & usage$totalTime<1e9)
inRange[covs$x_xirtMIS==1] <- FALSE

covs <- covsImp$ximp[inRange,,drop=TRUE]
outcomes <- outcomes[inRange,,drop=TRUE]
ids <- ids[inRange,,drop=TRUE]
ids$teachidFac <- dat$teachid2COV[inRange,drop=TRUE]
usage <- usage[inRange,,drop=TRUE]



boxcox <- function(x,lambda){
    if(lambda==0) return(log(x))
    else (x^lambda-1)/lambda
}

tt <- boxcox(usage$totalTime,.3)


Y <- outcomes$y_yirt

Z <- covs$treatment=='1'

Xt<- model.matrix(lag2_math_score~ state + as.factor(round(grade)) + race + sex
                 + spec_speced
                 + spec_gifted + spec_esl + frl + xirt + x_spec_giftedMIS
                 + x_gradeMIS + x_raceMIS + x_sexMIS + x_frlMIS,# + x_xirtMIS,
                 data=covs)
Xt <- Xt[,-1]

Xy <- model.matrix(rep(1,nrow(covs))~state+grade+race+sex+spec_speced+spec_gifted+spec_esl+frl+t1scale_score+xirt+lag2_math_score+lag1_math_score+lag2_read_score+lag1_read_score+lag2_grade+lag1_grade+x_spec_giftedMIS+x_eslMIS+x_gradeMIS+x_raceMIS+x_sexMIS+x_frlMIS#+x_xirtMIS
                   ,data=covs)
Xy <- Xy[,-1]


MED <- median(tt,na.rm=TRUE)

## try D in D
Y <- Y-Xy[,'xirt']
Xy <- Xy[,-which(colnames(Xy)=='xirt')]
py <- ncol(Xy)
