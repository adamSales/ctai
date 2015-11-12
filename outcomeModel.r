library(R2jags)
print(load('model1dat.RData'))
classMeans <- vapply(unique(ids$classid2),function(class)
                            mean(outcomes$y_yirt[ids$classid2==class],na.rm=TRUE),1)

class <- as.numeric(ids$classid2)

school <- as.numeric(ids$schoolid2)


pairs <- covs[,45:77]
pairs <- vapply(pairs,as.numeric,numeric(nrow(pairs)))-1
N <- nrow(covs)
Z <- as.numeric(covs$treatment=='1')



jags.data=list('Z','N','class','classMeans','pairs','nschools')
params <- c('tau','var.y','var.s')
outcomeMod <- jags(data=jags.data,parameters=params,model.file='outcomeModel.jags')
