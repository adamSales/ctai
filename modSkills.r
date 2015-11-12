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


## load mastery data
mas <- usage$mastered
try <- usage$incomplete

mas[mas<=0] <- NA # 5 cases
mas <- log(mas)

try <- log(try+1)


Y <- outcomes$y_yirt

Z <- covs$treatment=='1'

Xt<- model.matrix(lag2_math_score~ state + as.factor(round(grade)) + race + sex
                 + spec_speced
                 + spec_gifted + spec_esl + frl + xirt + x_spec_giftedMIS
                 + x_gradeMIS + x_raceMIS + x_sexMIS + x_frlMIS,# + x_xirtMIS,
                 data=covs)
Xt <- Xt[,-1]
Xt <- scale(Xt)

Xy <- model.matrix(rep(1,nrow(covs))~state+grade+race+sex+spec_speced+spec_gifted+spec_esl+frl+t1scale_score+xirt+lag2_math_score+lag1_math_score+lag2_read_score+lag1_read_score+lag2_grade+lag1_grade+x_spec_giftedMIS+x_eslMIS+x_gradeMIS+x_raceMIS+x_sexMIS+x_frlMIS#+x_xirtMIS
                   ,data=covs)
Xy <- Xy[,-1]
Xy <- scale(Xy)

MEDtry <- median(try,na.rm=TRUE)
MEDmas <- median(mas,na.rm=TRUE)

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

jags.data <- list("teacher","Xy","school","Xt","N","nschools","nteachers","py","pt", "Y","Z",'mas','try','MEDmas','MEDtry','pair','npairs')

jags.params <- c('tau00','tau11','tau01','tau10','tauDiffLowHigh','tauDiffMasTry')

## try D in D
Y <- Y-Xy[,'xirt']
Xy <- Xy[,-which(colnames(Xy)=='xirt')]
py <- ncol(Xy)

modDinD <- jags(data=jags.data,parameters=jags.params,n.iter=5000,n.burnin=1500,n.thin=3,model.file='code/skills.jags')
#modDinD2 <- autojags(modDinD,n.iter=1000)

save(modDinD,file='modskillz.RData')
