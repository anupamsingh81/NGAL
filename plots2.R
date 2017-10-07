# save multiple plots at once

library(ggplot2)
plots <- mtcars %>% 
  split(.$cyl) %>% 
  map(~ggplot(., aes(mpg, wt)) + geom_point())

pwalk(list(paths, plots), ggsave, path = getwd())

plots= master%>% select_if(is.numeric) %>% names() %>% map(function(y)ggplot(subset(master,!is.na(diagnoses)),aes(diagnoses))+ geom_boxplot(aes_string(y=y)))
paths <- stringr::str_c(1:length(plots), ".png")

paths
tempdir()

getwd()

length(plots)



# dodge plots


plots2= master %>% select_if(is.factor) %>%map(~ggplot(master, aes(diagnoses, ..count..)) + geom_bar(aes(fill = .), position = "dodge")+ labs(title=.))
                                               
no = length(plots)+ c(1:length(plots2))

noo = master %>% select_if(is.factor) %>% names()


nmas
str_c(no,".png")

paths <- stringr::str_c(noo, ".png")

pwalk(list(paths, plots2), ggsave, path = getwd())

n
no


library(plotROC)

master_rock = master %>% select(diagnoses,NGAL,feNa,urine_na,urine_creat,creat)


longtest1 <- melt_roc(master_rock1, "diagnoses", c("NGAL","feNa","urine_na","urine_creat","creat"))

longtest1= longtest1 %>% rename(D=D.diagnoses)   %>% select(name,M,D)

roc4 =ggplot(longtest1, aes(d = D, m = M, color = name)) + geom_roc() + style_roc()
roc4




library(pROC)
library(OptimalCutpoints)
library(tidyverse)

master_rock1 = master_rock %>% filter(diagnoses=="CKD"|diagnoses=="HRS") %>% mutate(diagnoses=ifelse(diagnoses=="HRS",1,0))


master_rock2 = master_rock %>% filter(diagnoses=="iAKI"|diagnoses=="HRS") %>% mutate(diagnoses=ifelse(diagnoses=="iAKI",1,0))

master_rock3 = master_rock %>% filter(diagnoses=="Prerenal"|diagnoses=="HRS") %>% mutate(diagnoses=ifelse(diagnoses=="HRS",1,0))

master_rock4 = master_rock %>% filter(diagnoses=="Prerenal"|diagnoses=="CKD") %>% mutate(diagnoses=ifelse(diagnoses=="Prerenal",1,0))





a = roc(master_rock3$diagnoses,master_rock3$creat)

plot(a,col="red")
title(main ="ROC curve for Creatinine in differentiating between HRS and prerenal",line = 3.0)

a$sensitivities
a$specificities
a$auc
a$thresholds

master_rock3 = as.data.frame(master_rock3)
b= optimal.cutpoints(creat~diagnoses,tag.healthy=0,methods = "Youden",data=master_rock3) # dont use tibble in data, first convert to data frame or unsupported index error

summary(b)





b= optimal.cutpoints(master_rock2$creat~master_rock2$diagnoses,tag.healthy=0,methods = "Youden",data=master_rock2)


summary(master_rock2$diagnoses)


# dca

source("dca.r")

dca(data=master_rock2,outcome="diagnoses",predictors = "ngalpred")



a1= glm(diagnoses~NGAL,family=binomial(),data=master_rock2)
a1$fitted.values

master_rock2$ngalpred = predict(a1,type="response")

b1= glm(diagnoses~feNa,family=binomial(),data=master_rock2)

master_rock2$feNapred = predict(b1,type="response")


c1= glm(diagnoses~urine_na,family=binomial(),data=master_rock2)

master_rock2$urinenapred = predict(c1,type="response")
dca(data=master_rock2,outcome="diagnoses",predictors = c("feNapred","ngalpred","urinenapred"))


# master_roc3
dca(data=master_rock2,outcome="diagnoses",predictors = "ngalpred")



a1= glm(diagnoses~NGAL,family=binomial(),data=master_rock3)


master_rock3$ngalpred = predict(a1,type="response")

b1= glm(diagnoses~feNa,family=binomial(),data=master_rock3)

master_rock3$feNapred = predict(b1,type="response")


c1= glm(diagnoses~urine_na,family=binomial(),data=master_rock3)

master_rock3$urinenapred = predict(c1,type="response")
dca(data=master_rock3,outcome="diagnoses",predictors = c("feNapred","ngalpred","urinenapred"))


                                


