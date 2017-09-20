ung$nga = c(1:90)
library(tidyr)
library(dplyr)

getwd()
ungl = read.csv("ungl.csv")

ungl1 = ungl%>% gather(`urine NGAL`,`AKI`,`HRS`,`iAKI`,`CKD`,`Prerenal`,key="Diagnosis",value="code")


ungl$diagnoses = ifelse(ungl$CKD=="y","CKD",
                        ifelse(ungl$HRS=="y","HRS",
                        ifelse(ungl$iAKI=="y","iAKI", 
                               ifelse(ungl$Prerenal=="y","Prerenal", "Control"))))       

summary(as.factor(ungl$diagnoses))
rm(ungl1)

ungl2= ungl%>% select(`urine.NGAL`,`feNa`,`diagnoses`,`CTP`,`Creat`,`MELD`,`Urine.na`,`hepatic.encephalopathy`,`SBP`)


ungl$`urine NGAL`


ungl=ungl%>%rename(NGAL=`urine.NGAL`)
ungl=ungl%>%rename(GRL=`X32`)
ungl=ungl%>%rename(Albumin=`X23`)

fit <- aov(NGAL ~ diagnoses, data=ungl) 

summary(fit)
TukeyHSD(fit)

unglsum <- ungl %>%
  group_by(diagnoses) %>%
  summarize(n=n(),mn=mean(Creat),sd=sd(Creat)) %>%
  mutate(se=sd/sqrt(n),LCI=mn+qnorm(0.025)*se,UCI=mn+qnorm(0.975)*se)
unglsum


unglngal = ungl %>%
  group_by(diagnoses) %>%
  summarize(n=n(),mn=mean(NGAL),sd=sd(NGAL)) %>%
  mutate(se=sd/sqrt(n),LCI=mn+qnorm(0.025)*se,UCI=mn+qnorm(0.975)*se)


unglfena = ungl %>%
  group_by(diagnoses) %>%
  summarize(n=n(),mn=mean(feNa),sd=sd(feNa)) %>%
  mutate(se=sd/sqrt(n),LCI=mn+qnorm(0.025)*se,UCI=mn+qnorm(0.975)*se)
unglfena







# 


unglngal


unglsummry = ungl %>%
  group_by(diagnoses) %>%
  summarize(n=n(),mn=mean(NGAL),sd=sd(NGAL)) %>%
  mutate(se=sd/sqrt(n),LCI=mn+qnorm(0.025)*se,UCI=mn+qnorm(0.975)*se)



fit <- aov(GFRL ~ diagnoses, data=ungl) 

summary(fit)
TukeyHSD(fit)


fit1 <- aov(feNa ~ diagnoses, data=ungl) 

summary(fit1)
TukeyHSD(fit1)




fit2 <- aov(Creat ~ diagnoses, data=ungl) 

summary(fit2)
TukeyHSD(fit2)


unglurina = ungl %>%
  group_by(diagnoses) %>%
  summarize(n=n(),mn=mean(Urine.na),sd=sd(Urine.na)) %>%
  mutate(se=sd/sqrt(n),LCI=mn+qnorm(0.025)*se,UCI=mn+qnorm(0.975)*se)

unglurina

fit3 <- aov(Urine.na ~ diagnoses, data=ungl) 

summary(fit3)
TukeyHSD(fit3)




unglMELD= ungl %>%
  group_by(diagnoses) %>%
  summarize(n=n(),mn=mean(MELD),sd=sd(MELD)) %>%
  mutate(se=sd/sqrt(n),LCI=mn+qnorm(0.025)*se,UCI=mn+qnorm(0.975)*se)

unglMELD

fit4<- aov(MELD ~ diagnoses, data=ungl) 

summary(fit4)
TukeyHSD(fit4)



ungl %>% cor.test(ungl$feNa,ungl$NGAL)

cor.test(ungl$MELD,ungl$NGAL)

cor.test(ungl$CTP,ungl$NGAL)

t.test(ungl$NGAL~ungl$SBP)


t.test(ungl$NGAL~ungl$hepatic.encephalopathy)

ungl$outcome30 = ifelse(ungl$outcome30=="d","Dead",
                        ifelse(ungl$outcome30=="a","Alive","NA"
                               ))

ungl30 = ungl %>%filter(outcome30 !="NA") 

 t.test(ungl30$NGAL~ungl30$outcome30)



summary(as.factor(ungl$outcome30))



summary(as.factor(ungl$outcome90))

t.test(ungl$NGAL~ungl$outcome90)
















unglsum1 = ungl %>% group_by(diagnoses,ascites)%>% summarize(count=n())
unglsum1

