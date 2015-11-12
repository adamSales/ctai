print(load('model1dat.RData'))
library(lme4)
library(missForest)


## total time spent on problems
usage$totalTime <- usage$meanTime*usage$nProbs

hist(usage$totalTime)
deciles <- quantile(usage$totalTime,probs=seq(0,1,0.1))
hist(usage$totalTime[usage$totalTime<deciles[9]])
hist(usage$totalTime[usage$totalTime>0 & usage$totalTime<deciles[9]])
sum(usage$totalTime<0)
boxplot(log(usage$totalTime,10))
sum(usage$totalTime>1e9)
hist(usage$totalTime[usage$totalTime>0 & usage$totalTime<1e9])

### OK, cut out subjects with total Time outside of (0,1e9)
covs <- covsImp$ximp[usage$totalTime>0 & usage$totalTime<1e9,] # cut out 28 sub
outcomes <- outcomes[usage$totalTime>0 & usage$totalTime<1e9,]
ids <- ids[usage$totalTime>0 & usage$totalTime<1e9,]  # cut out 28 subjects
usage <- usage[usage$totalTime>0 & usage$totalTime<1e9,]  # cut out 28 subjects


hist(usage$totalTime)
hist(log(usage$totalTime))
hist(sqrt(usage$totalTime))

boxcox <- function(x,lambda){
    if(lambda==0) return(log(x))
    else (x^lambda-1)/lambda
}

pdf('boxcox.pdf')
for(lam in seq(0, 1, 1/40)) hist(boxcox(usage$totalTime,lam),main=lam)
dev.off()


## predictors <- dat[,c(277:279,283:344)]
## missingPreds <- sapply(predictors,function(x) mean(is.na(x)))
## predictors <- predictors[,missingPreds<.5]
## ids <- predictors[,c('pair','teachid2','classid2','schoolid2')]
## predictors <- subset(predictors,select=-c(pair,teachid2,classid2,schoolid2,schoolidn,classid,teachid))
## predictorsImp <- missForest(predictors)
## predictors <- cbind(ids,predictorsImp$ximp)


mod1 <- randomForest(covs,sqrt(usage$totalTime))
lmDat <- covs
lmDat$totalTime <- usage$totalTime
lmDat$cell <- NULL
lmDat$grdlvl <- NULL
lmDat$teachidFac <- as.factor(ids$teachid)
mod2 <- lm(totalTime~.,data=lmDat)
mod2a <- update(mod2,.~.-teachidFac)

mod3 <- lmer(sqrt(totalTime)~state+grade+race+sex+spec_speced+spec_gifted+spec_esl+frl+t1scale_score+xirt+lag2_math_score+lag1_math_score+lag2_read_score+lag1_read_score+lag2_grade+lag1_grade+x_spec_giftedMIS+x_eslMIS+x_gradeMIS+x_raceMIS+x_sexMIS+x_frlMIS+x_xirtMIS+(1|teachidFac),data=lmDat)

mod4 <- update(mod3,totalTime~.)
mod5 <- update(mod3,log(totalTime)~.)


pdf('residVarb.pdf')
for(i in 1:ncol(mod3@frame)){
 varb <- mod3@frame[[i]]
 if(is.numeric(varb)) plot(varb, resid(mod3),main=names(mod3@frame)[i])
}
dev.off()


summ <- function(mod){
    y <- mod@frame[[1]]
    par(mfrow=c(2,1))
    plot(y,fitted(mod))
    plot(fitted(mod),resid(mod))
    print(cor(fitted(mod),y)^2)
}

mod6 <- lm(sqrt(totalTime)~state+grade+race+sex+spec_speced+spec_gifted+spec_esl+frl+t1scale_score+xirt+lag2_math_score+lag1_math_score+lag2_read_score+lag1_read_score+lag2_grade+lag1_grade+x_spec_giftedMIS+x_eslMIS+x_gradeMIS+x_raceMIS+x_sexMIS+x_frlMIS+x_xirtMIS,data=lmDat)

mod7 <- update(mod3,boxcox(totalTime,.3)~.)
mod8 <- update(mod3,boxcox(totalTime,.4)~.)

mod3a <- update(mod3,.~.-grade+as.factor(grade))
mod3b <- update(mod3a,.~.-t1scale_score)
mod3c <- update(mod3b,.~.-xirt+poly(xirt,3))


## ok do variable selection
summary(mod7)

#take out a variable?
varSec <- function(varb){
    form <- as.formula(paste('.~.-',varb))
    anova(mod7,update(mod7,form))
}

## leave in grade, race, spec_*,*MIS
## take out t1scale_score, lag scores, grades
## AIC, BIC, ChiSq p-values agree on these decisions
ttTransformed <- boxcox(lmDat$totalTime, 0.3)/1000
mod7a <- lmer(tt~ state + grade + race + sex + spec_speced
              + spec_gifted + spec_esl + frl + xirt + x_spec_giftedMIS
              + x_gradeMIS + x_raceMIS + x_sexMIS + x_frlMIS + x_xirtMIS
              + (1 | teachidFac),data=lmDat)
anova(mod7,mod7a)

anova(mod7a,update(mod7a,.~.-grade+as.factor(round(grade))))
## model grade as factor (following AIC; BICs are basically the same)

mod7b <- update(mod7a,.~.-grade+as.factor(round(grade)))


## OK ready for jags version
library(R2jags)
jags.data <- list("tt","teacher","X","school","N","nschools","nteachers","p")
tt <- ttTransformed
teacher <- as.numeric(lmDat$teachidFac)
nteachers <- max(teacher)
X <- model.matrix(mod7b)[,-1]
school <- vapply(1:length(unique(teacher)),function(teach)
                 ids$schoolid2[teacher==teach][1],1)
nschools <- length(unique(school))
school <- as.numeric(as.factor(school))
N <- nrow(X)
p <- ncol(X)

jags.init <- function() list(betaHat=rnorm(p),prec.y=rgamma(1, 1),prec.t=rgamma(1,1),prec.s=rgamma(1,1), alpha=rnorm(1),teacherEffect=rnorm(nteachers),schoolEffect=rnorm(nschools))
jags.params <- list('betaHat','alpha','var.y','var.t','var.s')
jags(data=jags.data,inits=jags.init,jags.params,n.iter=10,n.chains=7,model.file='timeModel.jags')


## OK fit the total model now

## update: just use totalModel.r

                                        # but first heres the model for total time from lmer:
mod7a <- lmer(tt~ state + as.factor(round(grade)) + race + sex + spec_speced
              + spec_gifted + spec_esl + frl + xirt + x_spec_giftedMIS
              + x_gradeMIS + x_raceMIS + x_sexMIS + x_frlMIS + x_xirtMIS
              + (1 | teachidFac),data=covs)


jags.data <- list("tt","teacher","X","school","N","nschools","nteachers","p",
                  "Y","Z","MED")
Y <- outcomes$y_yirt
tt <- boxcox(usage$totalTime,0.3)
MED <- median(tt)
Z <- covs$treatment=='1'

timeMod <- lmer(tt~ state + as.factor(round(grade)) + race + sex + spec_speced
              + spec_gifted + spec_esl + frl + xirt + x_spec_giftedMIS
              + x_gradeMIS + x_raceMIS + x_sexMIS + x_frlMIS + x_xirtMIS
              + (1 | ids$teachidFac),data=covs)


teacher <- as.numeric(ids$teachidFac)
nteachers <- max(teacher)
X <- model.matrix(timeMod)[,-1]
school <- vapply(1:length(unique(teacher)),function(teach)
                 ids$schoolid2[teacher==teach][1],1)
nschools <- length(unique(school))
school <- as.numeric(as.factor(school))
N <- nrow(X)
p <- ncol(X)

jags.init <- function() list(betaHat=rnorm(p),prec.y=rgamma(1, 1),prec.t=rgamma(1,1),prec.s=rgamma(1,1), alpha=rnorm(1),teacherEffect=rnorm(nteachers),schoolEffect=rnorm(nschools))
jags.params <- list('tau','kappa','lambda')
jags(data=jags.data,parameters=jags.params,n.iter=10,n.chains=7,model.file='timeModel.jags')
