print(load('model1dat.RData'))
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

jags.data <- list("teacher","Xy","school","Xt","N","nschools","nteachers","py","pt", "Y","Z",'tt','MED','pair','npairs')

jags.params <- c('tau1','tau2','lambda','var.tt','var.s','var.t','tauDiff','yhat','tthat')

## try D in D
Y <- Y-Xy[,'xirt']
Xy <- Xy[,-which(colnames(Xy)=='xirt')]
py <- ncol(Xy)



m1 <- quantile(tt,1/3,na.rm=TRUE)
m2 <- quantile(tt,2/3,na.rm=TRUE)

jags.data <- list("teacher","Xy","school","Xt","N","nschools","nteachers","py","pt","Y","Z",'tt','pair','npairs','m1','m2')

jags.params <- c('tau1','tau2','tau3','tau12Diff','tau23Diff','lambda','var.tt','var.s','var.t','tthat','yhat')

modDinD3 <- jags(data=jags.data,parameters=jags.params,n.iter=10,model.file='totalModels3Lev.jags')
modDinD3.2 <- autojags(modDinD3,n.iter=1000)


save(modDinD3.2,file='modTime3cat.RData')

tthat <- modDinD3.2$BUGSoutput$sims.list$tthat
yhat <- modDinD3.2$BUGSoutput$sims.list$yhat
tthat <- apply(tthat,2,mean)
yhat <- apply(yhat,2,mean)

pdf('timePlots3.pdf')
plot(tt,tthat)
plot(tthat,tt-tthat)
plot(Y,yhat)
plot(yhat,Y-yhat)
dev.off()
