print(load('data/model1dat.RData'))
library(lme4)
library(mice)

source('code/miPrep.r') ## Gets all the data ready--copied from the JAGS R file

## impute with mice

## tt is the mediator (total time)
## Xt is the matrix of predictors of the mediator
## I took out column 9 (indicator for grade '14', whatever that is) because apparently it was collinear
teacher <- as.factor(ids$teachid)
school <- ids$schoolid2
impDat <- data.frame(tt=tt,Xt=Xt[,-9],teacher=teacher,school=school,Y=Y)


M <- tt>MED  ## I'm treating this as binary to make it look more like
# summer learning (MED=median(tt))

#this turns the predictors of tt/M into a numeric matrix
mm <- model.matrix(rnorm(nrow(impDat))~.,data=impDat[,-1])

nimp <- 20 # the number of imputations

# predict nimp random imputations for each missing value

impTime <- mice(impDat,m=nimp,method='rf')
resultsTime <- with(impTime,lmer(Y~Z*I(tt>MED)+Xy +(1|ids$pair)+(1|ids$school)+(1|ids$teachid)))
print(summary(pool(resultsTime)))

## now for numSec
impDat$tt <- log(usage$numSec)
MED <- median(impDat$tt,na.rm=TRUE)
impSec <- mice(impDat,m=20,method='rf')

resultsSections <- with(impSec,lmer(Y~Xy+Z*I(tt>MED)+ids$pair+(1|ids$school)+(1|ids$teachid)))
resultsSections <- pool(resultsSections)






#### this was my original way of doing it
## ## fit the model nimp times with imputed data
imp <- replicate(nimp,mice.impute.rf(impDat$tt,!is.na(M),x=mm))
 models <- list()
 for(i in 1:nimp){
     ttimp <- impDat$tt
     ttimp[is.na(ttimp)] <- imp[,i]
     Mimp <- ttimp>MED
     models[[i]] <- lmer(Y~1+Xy+Mimp+Z+Z:Mimp+(1|ids$pair)+(1|ids$school)+(1|ids$teachid))
 }

# ## extract the estimated coefficients and their variances
 coefs <- vapply(models,function(m) summary(m)$coef[,1],numeric(nrow(summary(models[[1]])$coef)))
 vars <- vapply(models,function(m) summary(m)$coef[,2],numeric(nrow(summary(models[[1]])$coef)))^2


 estimates <- apply(coefs,1,mean) # estimated coefs are mean of MI estimates
 SEs <- sqrt(apply(vars,1,mean)+(1+1/nimp)*apply(coefs,1,var)) # get SEs w Rubin's formula
 Ts <- estimates/SEs ## T statistics

 print(cbind(estimates,SEs,Ts))



##### more models---why does MI give sucha different answer than mcmc?
h2 <- read.csv('H2_algebra_rcal_20121119_fieldid.csv')
reducedForm <- lmer(y_yirt-xirt~treatment+(1|state)+grade+race+sex+spec_speced+spec_gifted+spec_esl+frl+t1scale_score+lag2_math_score+lag1_math_score+lag2_read_score+lag1_read_score+lag2_grade+lag1_grade#+x_spec_giftedMIS+x_eslMIS+x_gradeMIS+x_raceMIS+x_sexMIS+x_frlMIS+x_xirtMIS
                    +(1|classid2)+(1|schoolid2)+pair,data=h2)

load('studentAggData.RData')
aggDat <- t(aggDat)
aggDat <- as.data.frame(aggDat)
aggDat$sid <- unique(x$RAND_field_id)
h2m <- merge(h2,aggDat[,c('sid','numSec')],by.x='field_id',by.y='sid',all.x=TRUE)

oneImpSecMod <- lmer(numSec~(1|state)+grade+race+sex+spec_speced+spec_gifted+spec_esl+frl+t1scale_score+lag2_math_score+lag1_math_score+lag2_read_score+lag1_read_score+lag2_grade#+lag1_grade#+x_spec_giftedMIS+x_eslMIS+x_gradeMIS+x_raceMIS+x_sexMIS+x_frlMIS+x_xirtMIS
                     +(1|classid2)+(1|schoolid2),data=h2m)

oneImpSec <- predict(oneImpSecMod,h2m,allow.new.levels=TRUE)#,fixed.only=F)#subset(h2m,!state%in%c('MI','NJ')))
MED <- median(h2m$numSec,na.rm=TRUE)
Mimp <- h2m$numSec>MED
Mimp[is.na(Mimp)] <- oneImpSec[is.na(Mimp)]>MED

psModOneImp <- lmer(y_yirt-xirt~I(treatment*(Mimp==TRUE))+I(treatment*(Mimp==FALSE))+Mimp+grade+race+sex+spec_speced+spec_gifted+spec_esl+frl+t1scale_score+lag2_math_score+lag1_math_score+lag2_read_score+lag1_read_score+lag2_grade+lag1_grade#+x_spec_giftedMIS+x_eslMIS+x_gradeMIS+x_raceMIS+x_sexMIS+x_frlMIS+x_xirtMIS
                      +(1|classid2)+(1|schoolid2)+pair,data=h2)
