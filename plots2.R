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

master_rock1 = master_rock %>% filter(diagnoses=="CKD"|diagnoses=="HRS") %>% mutate(diagnoses=ifelse(diagnoses=="HRS",1,0))

longtest1 <- melt_roc(master_rock1, "diagnoses", c("NGAL","feNa","urine_na","urine_creat","creat"))

longtest1= longtest1 %>% rename(D=D.diagnoses)   %>% select(name,M,D)

roc4 =ggplot(longtest1, aes(d = D, m = M, color = name)) + geom_roc() + style_roc()
roc4




longtest1 = gather(master_rock1,key=)



