library(caret)
library("gbm")
library(dplyr)
install.packages("C50")
library(C50)

attributes(CompleteResponses)
summary(CompleteResponses)
str(CompleteResponses)

CompleteResponses$brand<-as.factor(CompleteResponses$brand)
CompleteResponses$car<-as.factor(CompleteResponses$car)
CompleteResponses$elevel<-as.factor(CompleteResponses$elevel)
sum(is.na(CompleteResponses))

set.seed(123)
CompleteResponses<-CompleteResponses[sample(1:nrow(CompleteResponses), 9898, replace=FALSE),]
inTraining<-createDataPartition(CompleteResponses$brand, p=.75, list=FALSE)
training<-CompleteResponses[inTraining,]
testing<-CompleteResponses[-inTraining,]

fitControl<-trainControl(method="repeatedcv", 
                         number=10, 
                         repeats=1)

mtryGrid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                        n.trees = seq(100, 1000, by = 100),
                        n.minobsinnode = 10,
                        shrinkage = c(0.01, 0.1))

#________GBM Model____________

gbm_model<-train(brand~., 
                 data=training,
                 method="gbm", 
                 trControl=fitControl, 
                 tuneLength=1, 
                 tuneGrid = mtryGrid,
                 preProc=c("center","scale"))
gbm_model
summary(gbm_model)

gbm_preds <- predict(gbm_model, testing)
gbm_preds
postResample(gbm_preds, testing$brand)

gbmImp <- varImp(gbm_model, scale=TRUE)
gbmImp
plot(gbmImp)

confusionMatrix(preds, testing$brand)
#               Accuracy : 0.9184          
#                  Kappa : 0.8274          

#_______________RF Model_____________

mtryGrid <- expand.grid(mtry=100)

rf_model<-train(brand~., 
                 data=training,
                 method="rf", 
                 trControl=fitControl, 
                 tuneLength=1, 
                 tuneGrid = mtryGrid,
                 preProc=c("center","scale"))
rf_model
summary(rf_model)

rf_preds <- predict(rf_model, testing)
rf_preds
postResample(rf_preds, testing$brand)

rfImp <- varImp(rf_model, scale=TRUE)
rfImp
plot(rfImp)

confusionMatrix(rf_preds, testing$brand)
#               Accuracy : 0.9127          
#                  Kappa : 0.8153     

#________________C5.0__________________

mtryGrid <- expand.grid( .winnow = c(TRUE,FALSE), .trials=c(1,5), .model="tree" )

c50_model<-train(brand~., 
                data=training,
                method="C5.0", 
                trControl=fitControl, 
                tuneLength=1, 
                tuneGrid = mtryGrid,
                preProc=c("center","scale"))
c50_model
summary(c50_model)

c50_preds <- predict(c50_model, testing)
c50_preds
postResample(c50_preds, testing$brand)

c50Imp <- varImp(c50_model, scale=TRUE)
c50Imp
plot(c50Imp)

confusionMatrix(c50_preds, testing$brand)
#               Accuracy : 0.92            
#                  Kappa : 0.8315          

#_________________C5.0 Model performed with highest accuracy score

SurveyIncomplete$brand<-as.factor(SurveyIncomplete$brand)
SurveyIncomplete$car<-as.factor(SurveyIncomplete$car)
SurveyIncomplete$elevel<-as.factor(SurveyIncomplete$elevel)

brand_preds <- predict(c50_model, SurveyIncomplete)
brand_preds
postResample(brand_preds, SurveyIncomplete$brand)

summary(brand_preds)
#   0 (Acer)   1 (Sony)
#  1966       3034 
