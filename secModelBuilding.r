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

tt <- log(usage$numSec2+0.1)
MED <- median(tt,na.rm=TRUE)

sec <- usage$numSec2


Y <- outcomes$y_yirt

Z <- covs$treatment=='1'

covs$grade[covs$grade==14] <- 12

Xt <- model.matrix(lag2_math_score~ state + as.factor(round(grade)) + race + sex
                 + spec_speced
                 + spec_gifted + spec_esl + frl + xirt + x_spec_giftedMIS
                 + x_gradeMIS + x_raceMIS + x_sexMIS + x_frlMIS + x_xirtMIS,data=covs)

Xtint<- model.matrix(lag2_math_score~ (state + as.factor(round(grade)) + race + sex
                 + spec_speced
                 + spec_gifted + spec_esl + frl + xirt + x_spec_giftedMIS
                 + x_gradeMIS + x_raceMIS + x_sexMIS + x_frlMIS + x_xirtMIS)*(state + as.factor(round(grade)) + race + sex
                 + spec_speced
                 + spec_gifted + spec_esl + frl + xirt + x_spec_giftedMIS
                 + x_gradeMIS + x_raceMIS + x_sexMIS + x_frlMIS + x_xirtMIS)+ns(xirt,df=3),
                 data=covs)
Xt <- Xt[,-1]


Xy <- model.matrix(rep(1,nrow(covs))~state+grade+race+sex+spec_speced+spec_gifted+spec_esl+frl+t1scale_score+xirt+lag2_math_score+lag1_math_score+lag2_read_score+lag1_read_score+lag2_grade+lag1_grade+x_spec_giftedMIS+x_eslMIS+x_gradeMIS+x_raceMIS+x_sexMIS+x_frlMIS#+x_xirtMIS
                   ,data=covs)
Xy <- Xy[,-1]


### tt totally linear in Xt

xt <- Xt[is.finite(tt),]
id <- ids[is.finite(tt),]
tt <- tt[is.finite(tt)]

linMod <- lmer(tt~xt+(1|teachid2)+(1|schoolid2),data=id)
plot(fitted(linMod),resid(linMod))
plot(tt[is.finite(tt)],fitted(linMod))
## non-linearity?
summary(polyMod <- lm(tt[is.finite(tt)]~poly(fitted(linMod),4)))
lines(fitted(polyMod))


## interactions?
xint<- model.matrix(lag2_math_score~ state + as.factor(round(grade)) + race + sex
                    + spec_speced
                    + spec_gifted + spec_esl + frl + xirt + x_spec_giftedMIS
                    + x_gradeMIS + x_raceMIS + x_sexMIS + x_frlMIS, + x_xirtMIS
                    + state + as.factor(round(grade)) + race + sex
                    + spec_speced
                    + spec_gifted + spec_esl + frl + xirt + x_spec_giftedMIS
                    + x_gradeMIS + x_raceMIS + x_sexMIS + x_frlMIS,

                 data=covs)
intMod <- lmer(tt~xt:xt+(1|teachid2)+(1|schoolid2),data=id)

### non linearity in tt model
## must come from xirt--only continuous covariate
mod1 <- lm(tt~Xt[,-which(colnames(Xt)=='xirt')],subset=is.finite(tt))
mod2 <- lm(Xt[,'xirt']~Xt[,-which(colnames(Xt)=='xirt')],subset=is.finite(tt))
plot(resid(mod2),resid(mod1))
summary(lm(resid(mod1)~resid(mod2)))
abline(lm(resid(mod1)~resid(mod2)))

## try smoothign splines for xirt
bics <- NULL
bics[1] <- BIC(linMod)
for(df in 2:20){
    mod <- update(linMod,.~.+ns(xt[,'xirt'],df=df))
    bics[df] <- BIC(mod)
}

