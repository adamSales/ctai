print(load('data/model1dat.RData'))
library(lme4)
library(R2jags)

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

tt <- log(usage$numSec)
MED <- median(tt,na.rm=TRUE)



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




teacher <- as.numeric(ids$teachidFac)
nteachers <- max(teacher)
#X <- model.matrix(timeMod)[,-1]
ids$schoolid2 <- as.character(ids$schoolid2)
school <- vapply(1:length(unique(teacher)),function(teach)
                 ids$schoolid2[teacher==teach][1],'a')
nschools <- length(unique(school))
pair <- vapply(unique(school),function(schl) ids$pair[ids$schoolid2==schl][1],1)
pair <- as.numeric(as.factor(pair)) ## makes sure the pair numbers are consecutive
school <- as.numeric(as.factor(school))
npairs <- length(unique(pair))

N <- nrow(Xt)
py <- ncol(Xy)
pt <- ncol(Xt)

jags.data <- list("teacher","Xy","school","Xt","N","nschools","nteachers","py","pt", "Y","Z",'tt','MED','pair','npairs')

jags.params <- c('tau1','tau2','lambda','betaHat','gammaHat','var.tt','var.s','var.t','tauDiff','tau','yhat','ttNew','Mnew')

### imputation Model
jags.params <- ('M1')
impMod <- jags(data=jags.data,parameters=jags.params,model.file='code/imputation.jags')
M1 <- tt>MED
M1imp <- NULL
for(run in 1:3){
    M1imp <- rbind(M1imp,
                   impMod$BUGSoutput$sims.array[seq(500,1000,length=7),run,])
}

M1imp <- M1imp[,-ncol(M1imp)] #draws for deviance, not M1

## try D in D
Y <- Y-Xy[,'xirt']
Xy <- Xy[,-which(colnames(Xy)=='xirt')]
py <- ncol(Xy)

teacher <- as.factor(ids$teachid)
school <- ids$schoolid2

## multiple imputation with lmer
miMod <- list()
for(imp in 1:nrow(M1imp)){
    M <- M1imp[imp,]
    miMod[[imp]] <- lmer(Y~Z*M+Xt+(1|ids$pair)+(1|ids$school)+(1|ids$teachid))
}

modDinDns <- jags(data=jags.data,parameters=jags.params,n.iter=7000,n.burn=2000,n.thin=4,model.file='code/totalModel.jags')
#modDinDns2 <- autojags(modDinDns)


save(modDinDns,file='results/modSec2cat.RData')

## ttNew <- modDinD2$BUGSoutput$sims.list$ttNew
## Ynew <- modDinD2$BUGSoutput$sims.list$Ynew
## ttNew <- apply(ttNew,2,median)
## Ynew <- apply(Ynew,2,mean)

## pdf('timePlots.pdf')
## plot(tt,ttNew)
## plot(ttNew,tt-ttNew)
## plot(Y,Ynew)
## plot(Ynew,Y-Ynew)
## dev.off()

## disp <- function(vec) {
##     res <- c(mean(vec),sd(vec),quantile(vec,c(0.025,0.975)))
##     names(res)[1] <- 'mean'
##     names(res)[2] <- 'sd'
##     res
## }

## attach(modDinDns2$BUGSoutput$sims.list)

## colnames(gammaHat) <- colnames(Xy)
## colnames(betaHat) <- colnames(Xt)

## print(xtable(t(apply(gammaHat,2,disp))))
## print(xtable(t(apply(betaHat,2,disp))))

## print('Predicting M:')
## correct <- sapply(1:ncol(Mnew), function(col) Mnew[,col]==mTRUE[col])
## print(mean(correct),na.rm=T)
