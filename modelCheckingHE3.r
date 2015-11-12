mod <- modDinDhe
attach.jags(mod)
## converged?
pdf('HEplots.pdf')
Traceplot(mod,'tau1')
Traceplot(mod,'tau2')
Traceplot(mod,'tau')
Traceplot(mod,'tauDiff')

#looks pretty good

## main results
sumMain(modDinDhe)
# orig result inside 95% interval from this model, but not by much.
# is this bad?

## usage model
mediatorPlot()
mediatorPlot(ylim=c(-1,2))
#there's one control subject with a crazy sd for his tt estimate
# mean=13, sd=27 subject #1538
#otherwise looks OK

#confusion table
misClass()
# could be better :/ c'est la vie

# coefficient table
sumCoefT(mod)
# interesting:
# 10th graders asked for more HE
# men asked for less
# IEPs asked for more
# gifted less (but not that much)
# ESL,FRL a bit more
# pretest predicts well


## Outcome model
Yplot()
YresidPlot()
## looks OK
sumCoefY(mod)

# not much here EXCEPT
# ESL do much better. why??

dev.off()
