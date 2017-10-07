str(ungl30)

rm(master)

library(tidyverse)
library(tableone)

# Extract Factor
fac =master %>% select_if(is.factor) %>% names()

str(fac)

fac[2]

# Extract numeric
num =master %>% select_if(is.numeric) %>% names()


master$diagnoses = ifelse(master$CKD ==1,"CKD",
                        ifelse(master$HRS==1,"HRS",
                               ifelse(master$`Intrinsic renal`==1,"iAKI", 
                                      ifelse(master$Prerenal==1,"Prerenal", "Control"))))      

master$diagnoses = factor(master$diagnoses, levels = c("Control","CKD","Prerenal","HRS","iAKI"))

str(master$diagnoses)

summary(master$diagnoses)

vars = c(num,fac)
vars

library(tableone)
tab2 <- CreateTableOne(vars = vars, strata = "diagnoses" , data = master)

#Error due to bad name

master$
  summary(master)


master %>% rename(blood_transfusion=blood.tra0sfusio0)

master= master %>% rename(blood_transfusion=`b,decreased_output =`decreased uri0e output` ,altered_sensorium = `altered se0sorium`)
master= master %>% rename(altered_sensorium = `altered se0sorium`)

master= master %>% rename(gi_bleed = `GI bleed`)

master= master %>% rename(`urine routine` = `urine routine`,NGAL =`urine NGAL`,urine_creat=`urine creat`,urine_na=`Urine na`)
master =master %>% rename(routine=`urine routine`)

master =master %>% rename(akin_admission =`AKIN stagee at admission`,akin_48=`Akin stage at 48 hrs`)

master =master %>% rename(hepatic_enceph = `hepatic encephalopathy`,hbs_ag=`HBsAg CLD`,hcv=`HCV CLD`,outcome_30=`outcome at day 30`,outcome_90=`outcome at day 90`,intrinsic_renal=`Intrinsic renal`,baseline_creat=`base line creatine`,creatinine_admission=`Creatinine on admission`,infection_sbp=`infection other than sbp`)

num
tab2

print(tab2,quote = TRUE,noSpaces = TRUE)



# multiple anovas

outcome= c("diagnoses")
anovas = 
  
  

m1 = c("master$feNa","master$NGAL")
m2= c("master$TLC")
m3=expand.grid(m1,m2,stringsAsFactors = FALSE)

m3$Var1[2]

m

library(tidyverse)

# Multiple ANOVAS with single map argument 

# write a custom function

av= function(x){
  
  f=aov(x~master$diagnoses)
  
  
  f1= summary(f)
  
  f2=TukeyHSD(f)
  
  f3=list(f2,f1)
  
  f3
}

# apply map argument


master%>% select_if(is.numeric) %>% map(~av(.))



# save multiple plots at once

library(ggplot2)
plots <- mtcars %>% 
  split(.$cyl) %>% 
  map(~ggplot(., aes(mpg, wt)) + geom_point())
paths <- stringr::str_c(names(plots), ".pdf")

pwalk(list(paths, plots), ggsave, path = tempdir())

# map2 2 arguments for t test ( Multiple t test)

# write function

sum_wrapper <- function (tbl=NULL, var1=NULL, var2=NULL) {
  tbl_pair <- tbl %>%
    select_(var1, var2)
  x <- tbl_pair %>% pull(var1)
  y <- tbl_pair %>% pull(var2)
  sum = x+y
  sum
}

#check
sum_wrapper(var1="hb",var2="TLC",tbl=master)



# execute
allcomb_var <- expand.grid(var1 = c("hb","TLC"), var2 = c("TLC"),
                           stringsAsFactors = FALSE)

allcomb_var %>% 
  purrr::pmap(.f = sum_wrapper, tbl=master)


test_wrapper <- function (tbl=NULL, var1=NULL, var2=NULL) {
  tbl_pair <- tbl %>%
    select_(var1, var2)
  x <- tbl_pair %>% pull(var1)
  y <- tbl_pair %>% pull(var2)
  f=t.test(x~y) # there was fault if no formula mode
  g= colnames(tbl_pair) # get col_names
  h=list(g,f)
  h
}

#check
colnames(master$Name)

test_wrapper(var1="feNa", var2 = "outcome_30",tbl=master) 


num
allcomb <- expand.grid(var1 = num, var2 = c("outcome_30","outcome_90"),
                           stringsAsFactors = FALSE)



library(tidyverse)

master =master %>% 
# got stuck at spaces so changed all col name white space to _ 
 
  # Bulk change 
  nn = names(master)

library(stringr)
patt = " "

mm = str_replace_all(nn,patt,"_")
mm

names(master)= mm

names(master)


allcomb %>%
  purrr::pmap(.f = test_wrapper, tbl=master)


master= master %>% mutate(outcome_31 = case_when(
  is.na(outcome_30)~"a",
  outcome_30=="d"~"d",
  TRUE~"a"))


summary(master$outcome_31)



master$outcome_31= factor(master$outcome_31,levels=c("a","d"))
summary(master$outcome_31)

master = master %>% mutate(outcome_30=outcome_31)
master$outcome_31<- NULL








summary(master)












