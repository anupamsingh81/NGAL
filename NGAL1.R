ung$nga = c(1:90)
library(tidyr)
library(dplyr)

ungl1 = ungl%>% gather(`urine NGAL`,`AKI`,`HRS`,`iAKI`,`CKD`,`Prerenal` ,key="Diagnosis",value="code")

ungl = read.csv("ungl.csv")

ungl$diagnoses = ifelse(ungl$CKD =="y","CKD",
  ifelse(ungl$HRS=="y","HRS",
                        ifelse(ungl$iAKI=="y","iAKI", 
   ifelse(ungl$Prerenal=="y","Prerenal", "Control"))))      




ungl$`urine NGAL`



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


# ROC

library(pROC)
library(OptimalCutpoints)

unglROC = ungl %>% filter(diagnoses=="iAKI"| diagnoses=="HRS")
unglROC1 = ungl %>% filter(diagnoses=="Prerenal"| diagnoses=="HRS")

unglROC$diagnoses= as.factor(unglROC$diagnoses)

a=  roc(unglROC1$diagnoses,unglROC1$NGAL)

b=  roc(unglROC1$diagnoses,unglROC1$feNa)

c=  roc(unglROC1$diagnoses,unglROC1$Urine.na)

unglnames= ungl %>% select(NGAL,feNa,Urine.na,diagnoses)




plot(a,col="red")
plot(b,add=TRUE,col="blue")
plot(c,add=TRUE,col="green")

legend('bottomright', names(unglnames)[c(1:3)] , 
       lty=1, col=c('red', 'blue', 'green'),  cex=.75)


title(main ="   Biomarkers for  differentiating between HRS and Prerenal ",line = 3.0)

auc(a)
auc(b)
auc(c)

a1=optimal.cutpoints(NGAL~diagnoses,tag.healthy = "HRS",method="Youden",data=unglROC)

summary(a1)


b1=optimal.cutpoints(feNa~diagnoses,tag.healthy = "HRS",method="Youden",data=unglROC)

summary(b1)

c1=optimal.cutpoints(Urine.na~diagnoses,tag.healthy = "HRS",method="Youden",data=unglROC)

summary(c1)



library(tableone)
npar = c("NGAL","feNa","Urine.na")
ungl$sex=as.factor(ungl$sex)
myVars=c("age","sex","NGAL","feNa","Urine.na","HB","WBC","SGPT","INR","MELD","CTP")


tab1 <- CreateTableOne(vars = myVars, strata = "diagnoses" , data = ungl)

print(tab1,nonnormal=npar,quote = TRUE,noSpaces = TRUE)





# steps ..copy table after print command with quote and no spaces set to True, 2. open librecalc, 3. select paste special 4. select unformatted text 5. select quote "" in spaces 6. Adjust width. 6. if aligned above below copy adjacently


