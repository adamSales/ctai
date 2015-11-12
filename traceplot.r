Traceplot <- function(mod,varb,...){
    sa <- mod$BUGSoutput$sims.array
    n.chain <- dim(sa)[2]
    plot(sa[,1,varb],type='l',col=1,main=paste('Traceplot for',varb))
    for(ch in 2:n.chain) lines(sa[,ch,varb],col=ch)
}


mediatorPlot <- function(...){
    ttAvg <- apply(ttNew,2,mean)
    plot(tt,ttAvg,...)
    abline(0,1)
    text(0,mean(ttAvg)+1,paste('R^2=',cor(tt,ttAvg,use='pairwise')^2))
}

misClass <- function(){
    M <- tt>MED
    right1 <- mean(Mnew[,M>MED],na.rm=TRUE)
    wrong0 <- mean(Mnew[,M<MED],na.rm=TRUE)
    confTab <- round(matrix(c(right1,wrong0,1-right1,1-wrong0),2,2,byrow=TRUE),2)
    colnames(confTab) <- c("M=1","M=0")
    rownames(confTab) <- c("Mhat=1","Mhat=0")
    confTab
}


Yplot <- function(...){
    Yavg <- apply(yhat,2,mean)
    plot(Y,Yavg,...)
    abline(0,1)
    text(0,mean(Yavg)+1,paste('R^2=',cor(Y,Yavg,use='pairwise')^2))
}

YresidPlot <- function(...){
    Yavg <- apply(yhat,2,mean)
    plot(Yavg,Y-Yavg,...)
    abline(0,0)
}

sumMain <- function(mod){
    round(mod$BUGSoutput$summary[mod$parameters[grep('tau|var|lambda',mod$parameters)],],2)
}

sumCoefY <- function(mod){
    summ <- mod$BUGSoutput$summary
    tab <- summ[grep('gammaHat',rownames(summ)),c(1:3,7:9)]
    rownames(tab) <- colnames(Xy)
    round(tab,2)
}

sumCoefT <- function(mod){
    summ <- mod$BUGSoutput$summary
    tab <- summ[grep('betaHat',rownames(summ)),c(1:3,7:9)]
    rownames(tab) <- colnames(Xt)
    round(tab,2)
}
