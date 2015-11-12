

print(Sys.time())
modDinDns <- jags.parallel(data=jags.data,parameters=jags.params,n.iter=100000,n.burn=50000,n.thin=50,model.file='code/jags/totalModel.jags')
#modDinDns2 <- autojags(modDinDns)
simpleModel <- jags.parallel(data=simple.data,parameters=simple.params,n.iter=100000,n.burn=50000,n.thin=50,model.file='code/jags/simpleModel.jags')

#modSimp <- jags(data=jags.data,parameters=jags.params.simp,n.iter=5000,n.burn=3000,model.file='code/simple.jags')


Xypostest <- Xy
pypostest <- py
Xygainscore <- Xy[,-which(colnames(Xy)=='xirt')]
pygainscore <- py-1

Y <- placebo
Xy <- Xypostest
py <- pypostest
PlaceboMod <- jags.parallel(data=jags.data,parameters=jags.params,n.iter=10000,n.burn=5000,n.thin=5,model.file='code/jags/totalModel.jags')

Y <- placebo-pretest
Xy <- Xygainscore
py <- pygainscore
placeboGainscoreMod <- jags.parallel(data=jags.data,parameters=jags.params,n.iter=100000,n.burn=50000,n.thin=50,model.file='code/jags/totalModel.jags')


#Xt <- Xy <- Xt[,'xirt']
#pt <- py <- 1

#modPretest <- jags(data=jags.data,parameters=jags.params,n.iter=50000,n.burn=1000,n.thin=5,model.file='code/jags/totalModel.jags')

#save(modDinDns,file='results/modSec2cat.RData')

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

school <- as.numeric(as.factor(ids$schoolid2))
nschools <- max(school)
#M1 <- tt>MED
#M0 <- 1-M1
simple.data <- list('school','N','nschools','Y','Z','pair','npairs','M1')#,'M0')

Y <- postest
simpleJags <- jags.parallel(data=simple.data,n.iter=15000,n.burn=6000,model.file='code/jags/simple1.jags',n.chains=3,parameters=c('tau1','tau2','lambda','tauDiff'))#,'tau'))

Y <- pretest
simpleJagsPlacebo <- jags.parallel(data=simple.data,n.iter=15000,n.burn=6000,model.file='code/jags/simple1.jags',n.chains=3,parameters=c('tau1','tau2','lambda','tauDiff'))#,'tau'))

Y <- gainscore
simpleJagsGainscore <- jags.parallel(data=simple.data,n.iter=15000,n.burn=6000,model.file='code/jags/simple1.jags',n.chains=3,parameters=c('tau1','tau2','lambda','tauDiff'))#,'tau'))


### mixture models
M1 <- as.numeric(M1)
params <- c('muOfClust','precOfClust','pi')
                                        #init.fun <- function()

Y <- postest
simple2Jags <- jags.parallel(data=simple.data,n.iter=15000,n.burn=6000,model.file='code/jags/simple2.jags',n.chains=3,parameters=params)

Y <- pretest
simple2JagsPlacebo <- jags.parallel(data=simple.data,n.iter=15000,n.burn=6000,model.file='code/jags/simple2.jags',n.chains=3,parameters=params)

Y <- gainscore
simple2JagsGainscore <- jags.parallel(data=simple.data,n.iter=15000,n.burn=6000,model.file='code/jags/simple2.jags',n.chains=3,parameters=params)
