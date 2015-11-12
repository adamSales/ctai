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


trtSchools <- unique(ids$schoolid2)[sapply(unique(ids$schoolid2),
                                           function(schl)
                                             mean(covs$treatment[ids$schoolid2==schl]=='1'))==1]

missingUsage <- sapply(trtSchools,function(scl) mean(is.na(usage$numSec[ids$schoolid2==scl])))

dropSchools <- trtSchools[missingUsage>0.9]  ### we can be flexible on this

badMatch <- vapply(dropSchools, function(sch) unique(as.character(ids$pair)[ids$schoolid2==sch]),'a')

exclude <- ids$pair%in%badMatch

covs <- covs[!exclude,,drop=TRUE]
outcomes <- outcomes[!exclude,,drop=TRUE]
usage <- usage[!exclude,,drop=TRUE]
ids <- ids[!exclude,,drop=TRUE]

CCA <- TRUE
if(CCA){
  del <- covs$treatment=='1' & is.na(usage$numSec2)
  covs <- covs[!del,,drop=TRUE]
  outcomes <- outcomes[!del,,drop=TRUE]
  usage <- usage[!del,,drop=TRUE]
  ids <- ids[!del,,drop=TRUE]
}

boxcox <- function(x,lambda){
    if(lambda==0) return(log(x))
    else (x^lambda-1)/lambda
}

tt <- log(usage$numSec2+0.05)
#MED <- median(tt,na.rm=TRUE)
MED <- log(6.05)
M1 <- tt>MED

postest <- outcomes$y_yirt

Z <- covs$treatment=='1'

Xt<- model.matrix(lag2_math_score~ state + as.factor(round(grade)) + race + sex
                 + spec_speced
                 + spec_gifted + spec_esl + frl + xirt + x_spec_giftedMIS
                 + x_gradeMIS + x_raceMIS + x_sexMIS + x_frlMIS,# + x_xirtMIS,
                 data=covs)
Xt <- Xt[,-1]
Xt <- Xt[,-which(colnames(Xt)=="as.factor(round(grade))14")]

Xy <- model.matrix(rep(1,nrow(covs))~state+grade+race+sex+spec_speced+spec_gifted+spec_esl+frl+t1scale_score+xirt+lag2_math_score+lag1_math_score+lag2_read_score+lag1_read_score+lag2_grade+lag1_grade+x_spec_giftedMIS+x_eslMIS+x_gradeMIS+x_raceMIS+x_sexMIS+x_frlMIS+x_xirtMIS
                   ,data=covs)
Xy <- Xy[,-1]




teacher <- as.numeric(ids$teachidFac)
nteachers <- max(teacher)
#X <- model.matrix(timeMod)[,-1]
ids$schoolid2 <- as.character(ids$schoolid2)
## school <- vapply(1:length(unique(teacher)),function(teach)
##                  ids$schoolid2[teacher==teach][1],'a')
## nschools <- length(unique(school))
## school <- as.numeric(as.factor(school))
school <- as.numeric(as.factor(ids$schoolid2))
nschools <- max(school)
pair <- vapply(unique(ids$schoolid2),function(schl) ids$pair[ids$schoolid2==schl][1],1)
pair <- as.numeric(as.factor(pair)) ## makes sure the pair numbers are consecutive
npairs <- length(unique(pair))

obs <- !is.na(tt)

N <- nrow(Xt)
py <- ncol(Xy)
pt <- ncol(Xt)
ttObs <- tt

jags.data <- list("teacher","Xy","school","Xt","N","nschools","nteachers","py","pt", "Y","Z",'tt','ttObs','MED','pair','npairs','obs')
simple.data <- list("teacher","Xy","school","Xt","N","nschools","nteachers","py","pt", "Y","Z",'tt','ttObs','MED','pair','npairs','obs')
jags.params <- c('tau1','tau2','lambda','var.tt','var.s','var.t','tauDiff','tau','betaHat','gammaHat','Ynew')
simple.params <- c('tau1','tau2','lambda','var.tt','var.s','var.t','tauDiff','tau')
jags.params.simp <- c('tau1','tau2','lambda','var.tt','var.s','var.t','tauDiff','tau','Ynew','tt','M1')


pretest <- Xt[,'xirt']
gainscore <- postest-pretest
placebo <- postest+rnorm(N,sd(gainscore))
#Xy <- Xy[,-which(colnames(Xy)=='xirt')]
#py <- ncol(Xy)


#Xt <- Xt[,-which(colnames(Xt)=='xirt')]
#pt <- ncol(Xt)



### delete students with missing data
# del <- Z==1 & is.na(M1)
# school <- school[!del]
# N <- length(school)
# postest <- postest[!del]
# pretest <- pretest[!del]
# gainscore <- gainscore[!del]
# placebo <- placebo[!del]
# Z <- Z[!del]
# M1 <- M1[!del]
# Xt <- Xt[!del,]
# Xy <- Xy[!del,]
