library(data.table)
setwd('D:/材料/研究生阶段/Trimester 1/Analytic Strategies/Final project')
#import data
data=fread('application_train.csv')
#select variables
data$na_count=rowSums(is.na(data))
data=data[,c(1,2,4,5,6,7,8,12,13,14,15,16,17,18,19,22,23,29,30,32,41,44,123)]
#deal with missing values
data[is.na(EXT_SOURCE_3)==T,EXT_SOURCE:=median(data$EXT_SOURCE_3,na.rm=T)]
data[is.na(EXT_SOURCE_3)==F,EXT_SOURCE:=EXT_SOURCE_3]
data$EXT_SOURCE_3=NULL
data[FLAG_OWN_CAR=='N',OWN_CAR_AGE:=0]
data[is.na(OWN_CAR_AGE)==T,OWN_CAR_AGE:=median(data$OWN_CAR_AGE,na.rm=T)]
data[is.na(CNT_FAM_MEMBERS)==T,CNT_FAM_MEMBERS:=median(data$CNT_FAM_MEMBERS,na.rm=T)]
sum(is.na(data))
#divide original sample into train sets and test sets
set.seed(1030)
target0=subset(data,TARGET==0)
target1=subset(data,TARGET==1)
s=sample(seq_len(nrow(target0)),nrow(target1),replace =F)  
target0=target0[s, ]
data1=rbind(target0,target1)  
size=sample(c(1:nrow(data1)),0.7*nrow(data1),replace=F)
trainset=data1[size,]
testset=data1[-size,]
#logistic regression model
library(caret)
logit_model=glm(TARGET~.-SK_ID_CURR,data=trainset,family=binomial)
logit_prob=predict(logit_model,newdata=testset,type='response')
logit_predict=ifelse(logit_prob>0.5,1,0)
logit_confusionMatrix=confusionMatrix(as.factor(logit_predict),as.factor(testset$TARGET))
logit_confusionMatrix
logit_accuracy=round(logit_confusionMatrix$overall['Accuracy'],4)
logit_sensitivity=round(logit_confusionMatrix$byClass['Sensitivity'],4)
logit_specificity=round(logit_confusionMatrix$byClass['Specificity'],4)
#Random Forest machine learning model
library(randomForest)
rf_model=randomForest(as.factor(TARGET)~.-SK_ID_CURR,data=trainset,importance=T)
rf_predict=predict(rf_model,newdata=testset,type='class')
rf_prob=predict(rf_model,newdata=testset,type='prob')
rf_confusionMatrix=confusionMatrix(as.factor(rf_predict),as.factor(testset$TARGET))
rf_confusionMatrix
rf_accuracy=round(rf_confusionMatrix$overall['Accuracy'],4)
rf_sensitivity=round(rf_confusionMatrix$byClass['Sensitivity'],4)
rf_specificity=round(rf_confusionMatrix$byClass['Specificity'],4)
rf_specificity
#evaluate the prediction performance
library(ggplot2)
library(pROC)
logit_roc=roc(testset$TARGET,logit_prob)  
plot.roc(logit_roc, col = "#e7bcbc", print.auc = T, main = "ROC")
rf_roc=roc(testset$TARGET, rf_prob[,2])  
plot.roc(rf_roc, add = TRUE, col = "#8d1f17",print.auc=T,print.auc.y =0.4, print.auc.x =0.5)
legend("bottomright", legend = c("Logit regression model", "Random forest machine learning model"), col = c("#e7bcbc","#8d1f17"), lwd = 2)
performance=data.frame(measure=c('Accuracy','Sensitivity','Specificity'),
                       Logit=c(logit_accuracy,logit_sensitivity,logit_specificity),
                       RandomForest=c(rf_accuracy,rf_sensitivity,rf_specificity))
library(reshape2)
performance=melt(performance,id.vars ='measure',variable.name = "metric", value.name = "value")
performance_plot=ggplot(performance,aes(x=measure,y=value,fill=metric))+geom_bar(stat='identity',position='dodge')+scale_fill_manual(values=c(Logit="#e7bcbc",RandomForest="#8d1f17"))+labs(title='Model comparison: prediction performance of different models',x='Model',y='Prediction performance')+geom_text(aes(label = value), vjust = -0.5, position = position_dodge(width = 0.9))
performance_plot
#Interpret effects of variables on prediction
importance_scores=importance(rf_model)
importance_scores
importance_accuracy=importance(rf_model,type=1)
importance_accuracy
importance_gini=importance(rf_model,type=2)
gini_plot=ggplot(importance_gini,aes(x=reorder(rownames(importance_gini),MeanDecreaseGini),y=MeanDecreaseGini,fill=MeanDecreaseGini))+geom_bar(stat='identity')+scale_fill_gradient(low='#e7bcbc',high='#8d1f17')+labs(title='Importance of background characteristics in prediction (MDG)',x='Background characteristics',y='Importance')+coord_flip()
gini_plot
accuracy_plot=ggplot(importance_accuracy,aes(x=reorder(rownames(importance_accuracy),MeanDecreaseAccuracy),y=MeanDecreaseAccuracy,fill=MeanDecreaseAccuracy))+geom_bar(stat='identity')+scale_fill_gradient(low='#e7bcbc',high='#8d1f17')+labs(title='Importance of background characteristics in accuracy (MDA)',x='Background characteristics',y='Importance')+coord_flip()
accuracy_plot
