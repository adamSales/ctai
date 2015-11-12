print(load('model1dat.RData'))
library(lme4)

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


jags.data <- list("tt","teacher","X","school","N","nschools","nteachers","p",
                  "Y","MED")#,"Z")
jags.data2 <- list("teacher","X","school","N","nschools","nteachers","p","Y")
Y <- outcomes$y_yirt
tt <- boxcox(usage$totalTime,0.3)
MED <- median(tt)
Z <- covs$treatment=='1'

X<- model.matrix(lag2_math_score~ state + as.factor(round(grade)) + race + sex
                 + spec_speced
                 + spec_gifted + spec_esl + frl + xirt + x_spec_giftedMIS
                 + x_gradeMIS + x_raceMIS + x_sexMIS + x_frlMIS + x_xirtMIS,
                 data=covs)
X <- X[,-1]


teacher <- as.numeric(ids$teachidFac)
nteachers <- max(teacher)
#X <- model.matrix(timeMod)[,-1]
school <- vapply(1:length(unique(teacher)),function(teach)
                 ids$schoolid2[teacher==teach][1],1)
nschools <- length(unique(school))
school <- as.numeric(as.factor(school))
N <- nrow(X)
p <- ncol(X)

jags.init <- function() list(betaHat=rnorm(p),prec.y=rgamma(1, 1),prec.t=rgamma(1,1),prec.s=rgamma(1,1), alpha=rnorm(1),teacherEffect=rnorm(nteachers),schoolEffect=rnorm(nschools))
jags.params <- list('tau','kappa','lambda')
jags(data=jags.data2,parameters=jags.params,n.iter=10,n.chains=3,model.file='totalModel.jags')
