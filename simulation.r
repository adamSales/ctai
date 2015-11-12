library(R2jags)
library(mice)
library(arm)

## parameters
n <- 200
b0 <- 0
b1 <- 2
a0 <- 0
a1 <- 2
a2 <- 0
a3 <- 0#1
a4 <- 1
x <- rnorm(n)
simple <- TRUE

invLogit <- function(lp) exp(lp)/(1+exp(lp))

## simulate strata
simM <- function(x){
    probs <- invLogit(b0+b1*x)
    M <- rbinom(n,1,probs)
    M
}

simY <- function(x,M,Z){
    Y <- a0 +a1*x+a2*Z+a3*M +a4*M*Z+rnorm(n)
    Y
}

jagsEst <- function(Z,Mobs,Y,x){
    M <- Mobs
    jags.data <- list('n','x','Y','Z','M')
    jags.params <- c('a4','M')
    mod <- jags(data=jags.data,parameters=jags.params,model.file='sim.jags',progress='none')
    if(simple) return(mod$BUGSoutput$summary['a4',1:2])
    return(list(summary=mod$BUGSoutput$summary,sims=mod$BUGSoutput$sims.array))
}

MIest <- function(Z,Mobs,Y,x,useY=FALSE){
    impdat <- data.frame(M=(Mobs==1),x=x,Z=Z)
    if(useY) impdat$Y <- Y
    imp <- mice(impdat,m=20)
    mod <- pool(with(imp,lm(Y~x+Z*M)))
    if(simple) return(summary(mod)[5,1:2])
     mod                                   #    summary(mod)$coef[4,1:2]
}


simOne <- function(useY=TRUE){
    Z <- rbinom(n,1,.5)
    M <- simM(x)
    Mobs <- M
    Mobs[Z==0] <- NA
    Y <- simY(x,M,Z)
    return(list(jags=jagsEst(Z,Mobs,Y,x),mi=MIest(Z,Mobs,Y,x),data=data.frame(Z=Z,M=M,Y=Y)))
}

simTot <- function(B,useY=TRUE){
    lapply(1:B,function(i) {cat(i,'/',B,' '); simOne()})
}

between <- function(m,a1,a2){
    m>=a1 & m<=a2
}


coef.sim <- function(sim){
    if(simple)
        return(rbind(vapply(sim,function(run) run[[1]],numeric(2)),vapply(sim, function(run) run[[2]], numeric(2))))
    jags <- vapply(1:length(sim),function(i) return(sim[[i]]$jags$summary['a4',1:2]),numeric(2))
    mi <- vapply(1:length(sim), function(i) return(summary(sim[[i]]$mi)['Z:M',1:2]),numeric(2))
    coefs <- rbind(jags,mi)
    rownames(coefs) <- c('jags.mean','jags.sd','mi.est','mi.sd')
    return(coefs)

}

sumSim <- function(sim){
    sim <- coef.sim(sim)
    cat('means\n')
    print(apply(sim,1,mean))
    cat('sds\n')
    print(apply(sim,1,sd))
    cat('confidence interval coverage\n')
    cat('jags ',mean(between(a4,sim[1,]-1.96*sim[2,],sim[1,]+1.96*sim[2,])),
        ' mi ',mean(between(a4,sim[3,]-1.96*sim[4,],sim[3,]+1.96*sim[4,])),'\n')
    cat('power\n')
    cat('jags ',mean(sim[1,]/sim[2,]>=1.96),' mi ',mean(sim[3,]/sim[4,]>=1.96),'\n')
}

mcmcInfo <- function(sim){

    Mavg <- sapply(1:length(sim), function(i) cbind(sim[[i]]$data$M,sim[[i]]$jags$summary[1:200,1]))#,matrix(nrow=200,ncol=2))
binnedplot(as.vector(Mavg[1:200,]),as.vector(Mavg[201:400,]))

}
