library(Boruta)
library(dplyr)
traindata = ungl
traindata[traindata == ""] <- NA # convert all missing valus into NA

summary(traindata)

df <- traindata %>% mutate_if(is.character,as.factor)

traindata = complete.cases(traindata) #complete cases only

# convert all categorical variable into 


df = df %>% select( -c(mob.no,X.3,base.line.creatine)) # -select to remove variables

df_complete = df[complete.cases(df),]

boruta.train <- Boruta(outcome30~., data = df_complete, doTrace = 2)
 print(boruta.train)
 
 summary(boruta.train)
 
 plot(boruta.train, xlab = "", xaxt = "n")
 
 
 lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
   boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
  names(lz) <- colnames(boruta.train$ImpHistory)
  Labels <- sort(sapply(lz,median))
 axis(side = 1,las=2,labels = names(Labels),
        at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
 
 final.boruta <- TentativeRoughFix(boruta.train)
  print(final.boruta)
 
 final.boruta <- TentativeRoughFix(boruta.train)
 print(final.boruta)
 
 boruta.train1 = final.boruta
 plot(boruta.train1, xlab = "", xaxt = "n")
 
 
 lz<-lapply(1:ncol(boruta.train1$ImpHistory),function(i)
   boruta.train1$ImpHistory[is.finite(boruta.train1$ImpHistory[,i]),i])
 names(lz) <- colnames(boruta.train1$ImpHistory)
 Labels <- sort(sapply(lz,median))
 axis(side = 1,las=2,labels = names(Labels),
      at = 1:ncol(boruta.train1$ImpHistory), cex.axis = 0.7)
 
 getSelectedAttributes(final.boruta, withTentative = F)
 
 boruta.df <- attStats(final.boruta)
  class(boruta.df)
  
  print(boruta.df)
  
  # randomeforest
  
  library(randomForest)
  library(caret)
  modelFit <- train( outcome30~.,data=df_complete, method="rf", importance = TRUE)  
  varImp(modelFit)
  
  
 # http://rstatistics.net/variable-importance-of-predictors-that-contribute-most-significantly-to-a-response-variable-in-r/
  
  glm(outcome30~.)
  
  cor(df_complete)
  
  library(psych)
  
  polychoric(df_complete)
 
  tetrachoric(df_complete)
  
  
  library(tilting)
  
  # multiple t test R
  # https://stackoverflow.com/questions/9661469/r-t-test-over-all-columns
  
  
  library(MASS)
  
  df_new = df_complete %>% select(-c(outcome90))
 
  fit <- glm(outcome30~.,data=df_new,family = binomial())
  step <- stepAIC(fit, direction="both")
  step$anova # display results 
  
  
  library(leaps)
 
  leaps<-regsubsets(outcome30~.,data=df_new,nbest=10 ,really.big = T)
  # view results
  summary(leaps)
  # plot a table of models showing variables in each model.
  # models are ordered by the selection statistic.
  plot(leaps,scale="r2")
  
  library(relaimpo)
  
  
  lmMod <- glm(outcome30~ . , data = df_new,family=binomial())  # fit lm() model
  
  relImportance <- calc.relimp(lmMod, type = "lmg", rela = TRUE)  # calculate relative importance scaled to 100
  
  sort(relImportance$lmg, decreasing=TRUE) 
