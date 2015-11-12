print(load('data/model1dat.RData'))
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

jags.data <- list("teacher","Xy","school","Xt","N","nschools","nteachers","py","pt", "Y","Z",'tt','MED','pair','npairs')

Y <- outcomes$y_yirt

Z <- covs$treatment=='1'

Xt<- model.matrix(lag2_math_score~ state + as.factor(round(grade)) + race + sex
                 + spec_speced
                 + spec_gifted + spec_esl + frl + xirt + x_spec_giftedMIS
                 + x_gradeMIS + x_raceMIS + x_sexMIS + x_frlMIS + x_xirtMIS,
                 data=covs)
Xt <- Xt[,-1]

Xy <- model.matrix(rep(1,nrow(covs))~state+grade+race+sex+spec_speced+spec_gifted+spec_esl+frl+t1scale_score+xirt+lag2_math_score+lag1_math_score+lag2_read_score+lag1_read_score+lag2_grade+lag1_grade+x_spec_giftedMIS+x_eslMIS+x_gradeMIS+x_raceMIS+x_sexMIS+x_frlMIS+x_xirtMIS,data=covs)
Xy <- Xy[,-1]


MED <- median(tt,na.rm=TRUE)

teacher <- as.numeric(ids$teachidFac)
nteachers <- max(teacher)
#X <- model.matrix(timeMod)[,-1]
ids$schoolid2 <- as.character(ids$schoolid2)
school <- vapply(1:length(unique(teacher)),function(teach)
                 ids$schoolid2[teacher==teach][1],'a')
nschools <- length(unique(school))
pair <- vapply(unique(school),function(schl) ids$pair[ids$schoolid2==schl][1],1)
school <- as.numeric(as.factor(school))
npairs <- length(unique(pair))

N <- nrow(Xt)
py <- ncol(Xy)
pt <- ncol(Xt)



#jags.params <- c('kappa','tau','kappa2','lambda','lambda2','var.y','var.s','var.t','var.time') #list('tau','kappa'
jags.params <- c('tau1','tau2','lambda','var.tt','var.s','var.t','tauDiff')
#mod <- jags(data=jags.data,parameters=jags.params,n.iter=500,model.file='totalModel.jags')
#mod2 <- autojags(mod,n.iter=1000)

## try D in D
Y <- Y-Xy[,'xirt']
Xy <- Xy[,-which(colnames(Xy)=='xirt')]
py <- ncol(Xy)

modDinD <- jags(data=jags.data,parameters=jags.params,n.iter=10,model.file='totalModel.jags')
modDinD2 <- autojags(modDinD,n.iter=1000)


m1 <- quantile(tt,1/3,na.rm=TRUE)
m2 <- quantile(tt,2/3,na.rm=TRUE)

jags.data <- list("teacher","Xy","school","Xt","N","nschools","nteachers","py","pt","Y","Z",'tt','pair','npairs','m1','m2')

jags.params <- c('tau1','tau2','tau3','tau12Diff','tau23Diff','lambda','var.tt','var.s','var.t','tthat','yhat')

modDinD3 <- jags(data=jags.data,parameters=jags.params,n.iter=10,model.file='totalModels3Lev.jags')
modDinD3.2 <- autojags(modDinD3,n.iter=1000)


### now try with mean hints
print(load('errHintsNormalized.RData'))
eh <- as.data.frame(t(means))
names(eh) <- c('id','meanHintsNorm','meanErrorsNorm')
usage2 <- merge(usage,eh,all.x=TRUE,by='id')
hints <- usage2$meanHintsNorm
hints[is.nan(hints)] <- 0
hintsPlusErrors <- hints+usage2$meanErrorsNorm
tt <- boxcox((hintsPlusErrors-min(hintsPlusErrors,na.rm=TRUE)),0.3)
MED <- median(tt,na.rm=TRUE)

jags.params <- c('tau1','tau2','lambda','var.tt','var.s','var.t','tauDiff','tthat','yhat')
modDinDhe <- jags(data=jags.data,parameters=jags.params,n.iter=10,model.file='totalModel.jags')
modDinDhe2 <- autojags(modDinDhe,n.iter=1000)


## now try with sections encountered
tt <- log(usage$numSec)
MED <- median(tt,na.rm=TRUE)
modDinDns <- jags(data=jags.data,parameters=jags.params,n.iter=10,model.file='totalModel.jags')
modDinDns2 <- autojags(modDinDns,n.iter=1000)

## break tt into 3 levels
m1 <- quantile(tt,1/3,na.rm=TRUE)
m2 <- quantile(tt,2/3,na.rm=TRUE)

jags.data <- list("teacher","Xy","school","Xt","N","nschools","nteachers","py","pt",
                  "Y","Z",'tt','pair','npairs','m1','m2')



jags.params <- c('tau1','tau2','tau3','tau12Diff','tau23Diff','lambda','var.tt','var.s','var.t','Ynew','ttNew')

modDinD3 <- jags(data=jags.data,parameters=jags.params,n.iter=10,model.file='totalModels3Lev.jags')
modDinD3.2 <- autojags(modDinD3,n.iter=1000)
ttSim <- modDinD3$BUGSoutput$sims.list$ttNew
Ysim <- modDinD3$BUGSoutput$sims.list$Ynew
ttPred <- apply(ttSim,2,mean)
yPred <- apply(Ysim,2,mean)



##
