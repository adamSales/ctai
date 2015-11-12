print(load('../data/model1dat.RData'))
library(lme4)
library(R2jags)

usage$totalTime <- usage$meanTime*usage$nProbs

inRange <- is.na(usage$totalTime) |(usage$totalTime>0 & usage$totalTime<1e9)

covs <- covsImp$ximp[inRange,]
outcomes <- outcomes[inRange,]
ids <- ids[inRange,]
ids$teachidFac <- dat$teachid2COV[inRange]
usage <- usage[inRange,]



boxcox <- function(x,lambda){
    if(lambda==0) return(log(x))
    else (x^lambda-1)/lambda
}

tt <- boxcox(usage$totalTime,.3)


posttest <- outcomes$y_yirt

Z <- covs$treatment=='1'


X <- model.matrix(rep(1,nrow(covs))~state+grade+race+sex+spec_speced+spec_gifted+spec_esl+frl+t1scale_score+xirt+lag2_math_score+lag1_math_score+lag2_read_score+lag1_read_score+lag2_grade+lag1_grade+x_spec_giftedMIS+x_eslMIS+x_gradeMIS+x_raceMIS+x_sexMIS+x_frlMIS+x_xirtMIS,data=covs)
X <- X[,-1]


MED <- median(tt,na.rm=TRUE)

teacher <- as.numeric(ids$teachidFac)
nteachers <- max(teacher)
sschool <- as.numeric(as.factor(ids$schoolid2))
sschools <- unique(sschool)
nsschool <- max(sschool)
#X <- model.matrix(timeMod)[,-1]
ids$schoolid2 <- as.character(ids$schoolid2)
school <- vapply(1:length(unique(teacher)),function(teach)
                 ids$schoolid2[teacher==teach][1],'a')
nschools <- length(unique(school))
pair <- vapply(unique(school),function(schl) ids$pair[ids$schoolid2==schl][1],1)
school <- as.numeric(as.factor(school))
npairs <- length(unique(pair))

N <- nrow(X)
py <- ncol(X)
pt <- ncol(X)

jags.data <- list("teacher","Xy","school","Xt","N","nschools","nteachers","py","pt", "Y","Z",'tt','MED','pair','npairs')


jags.params <- c('tau1','tau2','lambda','betaHat','gammaHat','var.tt','var.s','var.t','tauDiff','yhat','ttNew','Mnew')
## try D in D
pretest <- X[,'xirt']
gainscore <- posttest-pretest



print(load('../data/errHintsNormalized.RData'))
eh <- as.data.frame(t(means))
names(eh) <- c('id','meanHintsNorm','meanErrorsNorm')
usage2 <- merge(usage,eh,all.x=TRUE,by='id')
hints <- usage2$meanHintsNorm
hints[is.nan(hints)] <- 0
hintsPlusErrors <- hints+usage2$meanErrorsNorm
tt <- boxcox((hintsPlusErrors-min(hintsPlusErrors,na.rm=TRUE)),0.3)
MED <- median(tt,na.rm=TRUE)

jags.params <- c('tau1','tau2','lambda','betaHat','gammaHat','var.tt','var.s','var.t','tauDiff','yhat','ttNew','Mnew','tau')
#modDinDhe <- jags(data=jags.data,parameters=jags.params,n.iter=3500,n.burnin=1000,n.thin=3,model.file='code/jags/totalModel.jags')
#modDinDhe2 <- autojags(modDinDhe)

#save(modDinDhe,file='assistanceModel2cat.RData')

 m1 <- quantile(tt,1/3,na.rm=TRUE)
 m2 <- quantile(tt,2/3,na.rm=TRUE)

jags.data <- list("teacher","Xy","school","Xt","N","nschools","nteachers","py","pt","Y","Z",'tt','pair','npairs','m1','m2')

 jags.params <- c('tau1','tau2','tau3','tau12Diff','tau23Diff','lambda','var.tt','var.s','var.t','betaHat','gammaHat')

#modDinD3 <- jags(data=jags.data,parameters=jags.params,n.iter=5000,n.burnin=2000,n.thin=3,model.file='code/jags/totalModels3Lev.jags')
##  modDinD3.2 <- autojags(modDinD3,n.iter=1000)


##  save(modDinD2,modDinD3.2,file='modAssistance23cat.RData')

## ## tthat <- modDinD3.2$BUGSoutput$sims.list$tthat
## ## yhat <- modDinD3.2$BUGSoutput$sims.list$yhat
## ## tthat <- apply(tthat,2,mean)
## ## yhat <- apply(yhat,2,mean)

## ## pdf('timePlots3.pdf')
## ## plot(tt,tthat)
## ## plot(tthat,tt-tthat)
## ## plot(Y,yhat)
## ## plot(yhat,Y-yhat)
## ## dev.off()


## ## tthat <- modDinD2$BUGSoutput$sims.list$tthat
## ## yhat <- modDinD2$BUGSoutput$sims.list$yhat
## ## tthat <- apply(tthat,2,mean)
## ## yhat <- apply(yhat,2,mean)

## ## pdf('timePlots2.pdf')
## ## plot(tt,tthat)
## ## plot(tthat,tt-tthat)
## ## plot(Y,yhat)
## ## plot(yhat,Y-yhat)
## ## dev.off()

## ttAvg <- apply(ttNew,2,mean)
## plot(tt,ttAvg)

## mAvg <- vapply(1:ncol(Mnew),function(col) mean(Mnew[,col]==(tt[col]>MED)),1)



### some models:

## full postest
Xy <- Xt <- X
pt <- py <- ncol(X)
Y <- posttest
modOutcomeFull <- jags.parallel(data=jags.data,parameters=jags.params,n.iter=500,n.burnin=200,n.thin=3,model.file='code/jags/totalModels3Lev.jags')

#full DinD model
Y <- gainscore
Xy <- X[,-which(colnames(X)=='xirt')]
py <- ncol(Xy)
Xt <- X
pt <- ncol(X)

modgainscore <- jags.parallel(data=jags.data,parameters=jags.params,n.iter=5000,n.burnin=2000,n.thin=3,model.file='code/jags/totalModels3Lev.jags')

# full placebo Model
Y <- pretest+rnorm(N,,sd(posttest-pretest))
Xy <- Xt <- X
pt <- py <- ncol(X)
modPlaceboFull <- jags.parallel(data=jags.data,parameters=jags.params,n.iter=5000,n.burnin=2000,n.thin=3,model.file='code/jags/totalModels3Lev.jags')

### outcome model with only one covariate
Y <- posttest
Xy <- Xt <- cbind(pretest)
pt <- py <- 1
modPostest1 <- jags.parallel(data=jags.data,parameters=jags.params,n.iter=5000,n.burnin=2000,n.thin=3,model.file='code/jags/totalModels3Lev.jags')

# placebo model with only one covariate
Y <- pretest+rnorm(N,,sd(posttest-pretest))
jags.parallel(data=jags.data,parameters=jags.params,n.iter=5000,n.burnin=2000,n.thin=3,model.file='code/jags/totalModels3Lev.jags')


##########################################
## try modeling usage continuously, not in strata
#############################################

jags.params <- c('tau','tau1','tau2','lambda1','lambda2','betaHat','gammaHat')

placebo <- pretest+rnorm(N,0,sd(posttest-pretest))

### some models:
ctsMods <- list()
## full postest
Xy <- Xt <- X
pt <- py <- ncol(X)
Y <- posttest
modOutcomeFullCts <- jags.parallel(data=jags.data,parameters=jags.params,n.iter=500,n.burnin=200,n.thin=3,model.file='code/jags/ctsModel.jags')

#full DinD model
Y <- gainscore
Xy <- X[,-which(colnames(X)=='xirt')]
py <- ncol(Xy)
Xt <- X
pt <- ncol(X)

modgainscoreCts <- jags.parallel(data=jags.data,parameters=jags.params,n.iter=5000,n.burnin=2000,n.thin=3,model.file='code/jags/ctsModel.jags')

# full placebo Model
Y <- placebo
Xy <- Xt <- X
pt <- py <- ncol(X)
modPlaceboFullCts <- jags.parallel(data=jags.data,parameters=jags.params,n.iter=5000,n.burnin=2000,n.thin=3,model.file='code/jags/ctsModel.jags')

## full placebo gainscore
Y <- placebo-pretest
Xy <- X[,-which(colnames(X)=='xirt')]
py <- ncol(Xy)
Xt <- X
pt <- ncol(X)

modgainscorePlaceboFull <- jags.parallel(data=jags.data,parameters=jags.params,n.iter=5000,n.burnin=2000,n.thin=3,model.file='code/jags/ctsModel.jags')

### outcome model with only one covariate
Y <- posttest
Xy <- Xt <- cbind(pretest)
pt <- py <- 1
modPostest1Cts <- jags.parallel(data=jags.data,parameters=jags.params,n.iter=5000,n.burnin=2000,n.thin=3,model.file='code/jags/ctsModel.jags')

## gainscore with only one covariate
Y <- gainscore
Xt <- cbind(pretest)
Xy <- cbind(X[,'lag2_math_score'])
pt <- py <- 1
Gainscore1 <- jags.parallel(data=jags.data,parameters=jags.params,n.iter=5000,n.burnin=2000,n.thin=3,model.file='code/jags/ctsModel.jags')

# placebo model with only one covariate
Y <- pretest+rnorm(N,,sd(posttest-pretest))
placebo1cts <- jags.parallel(data=jags.data,parameters=jags.params,n.iter=5000,n.burnin=2000,n.thin=3,model.file='code/jags/ctsModel.jags')

# placebo gain score model
Y <- Y-pretest
Xt <- cbind(pretest)
Xy <- cbind(X[,'lag2_math_score'])
placeboGainscore1 <- jags.parallel(data=jags.data,parameters=jags.params,n.iter=5000,n.burnin=2000,n.thin=3,model.file='code/jags/ctsModel.jags')

