library(pROC)
library(OptimalCutpoints)

a=  roc(unglROC$diagnoses,unglROC$NGAL) # place outcome as first argument, mistake as second

plot(a,col="red")
title(main ="ROC curve for NGAL in differentiating between HRS and intrinsic AKI",line = 3.0)


unglROC = unglROC %>% mutate(diagnoses=replace(diagnoses,diagnoses=="HRS",0))

unglROC = unglROC %>% mutate(diagnoses=replace(diagnoses,diagnoses=="iAKI",1))

summary(unglROC$diagnoses)

b= optimal.cutpoints(NGAL~diagnoses,tag.healthy=0,methods = "Youden",data=unglROC)
b$measures$cutoffs

summary(b)

optimal.cutpoints()
a$auc

summary(a)

opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(unglROC$NGAL, as.factor(unglROC$diagnoses)))