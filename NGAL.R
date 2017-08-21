ung$nga = c(1:90)
library(tidyr)
library(dplyr)

ungl1 = ungl%>% gather(`urine NGAL`,`AKI`,`HRS`,`iAKI`,`CKD`,`Prerenal` ,key="Diagnosis",value="code")

ungl$diagnoses = ifelse(ungl$HRS=="y","HRS",
                        ifelse(ungl$iAKI=="y","iAKI", 
                               ifelse(ungl$Prerenal=="y","Prerenal", "CKD")))       
rm(ungl1)

ungl$`urine NGAL`


ungl=ungl%>%rename(NGAL=`urine NGAL`)
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

fit <- aov(GFRL ~ diagnoses, data=ungl) 

summary(fit)
TukeyHSD(fit)


fit <- aov(feNa ~ diagnoses, data=ungl) 

summary(fit)
TukeyHSD(fit)


unglsum1 = ungl %>% group_by(diagnoses,ascites)%>% summarize(count=n())
unglsum1

