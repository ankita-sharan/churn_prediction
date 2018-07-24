#LOADING LIBRARIES
X = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", 
      "C50", "dummy",
      "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees',"mlr",
      "gridExtra","outliers","partitions","class","e1071")
library(caret)
library(scales)
library(psych)
library(gplots)
lapply(X, require, character.only = TRUE)
rm(X)
#-------------------------------------------------------------------------------------------------
#LOADING WORKING DIRECTORY
setwd("C:/Users/DELL/Downloads")
getwd()
#LOADING DATA
train_data = read.csv("Train_data.csv")[-4]
test_data = read.csv("Test_data.csv")[-4]
dftrain = train_data
dftest = test_data
#---------------------------------------------------------------------------------------
#CHECKING STRUCTURE OF DATA
str(train_data)
str(test_data)
# CHANGING AREA CODE FROM NUMERIC TO FACTOR
train_data$area.code=as.factor(train_data$area.code)
test_data$area.code=as.factor(test_data$area.code)
#---------------------------------------------------------------------------------------
#CHECKING MISSING VALUES
sum(is.na(train_data)) #train
sum(is.na(test_data))  #test
#---------------------------------------------------------------------------------------
#giving levels to categorical data(train data)
for(i in 1:ncol(train_data)){
  
  if(class(train_data[,i]) == 'factor'){
    
    train_data[,i] = factor(train_data[,i], labels=(1:length(levels(factor(train_data[,i])))))
    
  }
}
#giving levels to categorical data(test data)
for(i in 1:ncol(test_data)){
  
  if(class(test_data[,i]) == 'factor'){
    
    test_data[,i] = factor(test_data[,i], labels=(1:length(levels(factor(test_data[,i])))))
    
  }
}
#-----------------------------------------------------------------------------------------------------
# SEPARATING NUMERIC DATA and plotting box plot
numeric_indextrain = sapply(train_data,is.numeric)  #train data
numeric_traindata = train_data[,numeric_indextrain]
cnames_train=colnames(numeric_traindata)
numeric_indextest = sapply(test_data,is.numeric)    #test data
numeric_testdata = test_data[,numeric_indextest]
cnames_test=colnames(numeric_testdata)
#-------------------------------------------------------------------------------------------------------
#BOX PLOT

# (train data)
for (i in 1:length(cnames_train))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames_train[i]), x = "Churn"), data = subset(train_data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames_train[i],x="Churn")+
           ggtitle(paste("Box plot of Churn for",cnames_train[i])))
}

#plotting plots together
gridExtra::grid.arrange(gn1,gn5,gn2,ncol=3)
gridExtra::grid.arrange(gn6,gn7,ncol=2)
gridExtra::grid.arrange(gn8,gn9,ncol=2)
gridExtra::grid.arrange(gn3,gn4,gn10,ncol=3)
gridExtra::grid.arrange(gn11,gn12,gn13,ncol=3)
gridExtra::grid.arrange(gn14,gn15,ncol=2)

# test data
for (i in 1:length(cnames_test))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames_test[i]), x = "Churn"), data = subset(test_data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames_test[i],x="Churn")+
           ggtitle(paste("Box plot of Churn for",cnames_test[i])))
}

#plotting plots together
gridExtra::grid.arrange(gn1,gn5,gn2,ncol=3)
gridExtra::grid.arrange(gn6,gn7,ncol=2)
gridExtra::grid.arrange(gn8,gn9,ncol=2)
gridExtra::grid.arrange(gn3,gn4,gn10,ncol=3)
gridExtra::grid.arrange(gn11,gn12,gn13,ncol=3)
gridExtra::grid.arrange(gn14,gn15,ncol=2)

#------------------------------------------------------------------------------------------------------

# SEPARATING CATEGORICAL DATA 
factor_indextrain = sapply(train_data, is.factor)   #train data
factor_traindata = train_data[,factor_indextrain]
factor_indextest = sapply(test_data, is.factor)    #test data
factor_testdata = test_data[,factor_indextest]
#-----------------------------------------------------------------------------------------------------
# BAR PLOT categorical data
# train data
barplot(table(train_data$state))
barplot(table(train_data$area.code))
barplot(table(train_data$international.plan))
barplot(table(train_data$voice.mail.plan))
barplot(table(train_data$Churn))

# test data
barplot(table(test_data$state))
barplot(table(test_data$area.code))
barplot(table(test_data$international.plan))
barplot(table(test_data$voice.mail.plan))
barplot(table(test_data$Churn))



#---------------------------------------------------------------------------------------------------------------
#outlier analysis
#train data
for(i in cnames_train){
  print(i)
  val = train_data[,i][train_data[,i] %in% boxplot.stats(train_data[,i])$out]
  print(length(val))
  train_data = train_data[which(!train_data[,i] %in% val),]
}

# test data
for(i in cnames_test){
  print(i)
  val = test_data[,i][test_data[,i] %in% boxplot.stats(test_data[,i])$out]
  print(length(val))
  test_data = test_data[which(!test_data[,i] %in% val),]
}
#---------------------------------------------------------------------------------------------------------------
# Feature selection  
##train data
# Correlation for continuous variable
corrgram(train_data[,numeric_indextrain],order=F,
         upper.panel=panel.pie,text.panel=panel.txt,main="Correlation plot")

# chi square test(train data)

for (i in 1:4)
{
  print(names(factor_traindata)[i])
  print(chisq.test(table(factor_traindata$Churn,factor_traindata[,i])))
}

# Dimension reduction
final_train = subset(train_data,select = -(area.code))

# TEST DATA

# chi square test(test data)

for (i in 1:4)
{
  print(names(factor_testdata)[i])
  print(chisq.test(table(factor_testdata$Churn,factor_testdata[,i])))
}

# Dimension reduction
final_test = subset(test_data,select = -(area.code))
#----------------------------------------------------------------------------------------------------------
# Histogram plot for continuous data
# train data

qqnorm(final_train$account.length)
hist(final_train$account.length)
qqnorm(final_train$number.vmail.messages)
hist(final_train$number.vmail.messages)
qqnorm(final_train$total.day.minutes)
hist(final_train$total.day.minutes)
qqnorm(final_train$total.day.calls)
hist(final_train$total.day.calls)
qqnorm(final_train$total.day.charge)
hist(final_train$total.day.charge)
qqnorm(final_train$total.eve.minutes)
hist(final_train$total.eve.minutes)
qqnorm(final_train$total.eve.calls)
hist(final_train$total.eve.calls)
qqnorm(final_train$total.eve.charge)
hist(final_train$total.eve.charge)
qqnorm(final_train$total.night.minutes)
hist(final_train$total.night.minutes)
qqnorm(final_train$total.night.calls)
hist(final_train$total.night.calls)
qqnorm(final_train$total.night.charge)
hist(final_train$total.night.charge)
qqnorm(final_train$total.intl.minutes)
hist(final_train$total.intl.minutes)
qqnorm(final_train$total.intl.charge)
hist(final_train$total.intl.charge)
qqnorm(final_train$total.intl.calls)
hist(final_train$total.intl.calls)
qqnorm(final_train$number.customer.service.calls)
hist(final_train$number.customer.service.calls)
#-------------------------------------------------------------------------------------------------------

# test data

qqnorm(final_test$account.length)
hist(final_test$account.length)
qqnorm(final_test$number.vmail.messages)
hist(final_test$number.vmail.messages)
qqnorm(final_test$total.day.minutes)
hist(final_test$total.day.minutes)
qqnorm(final_test$total.day.calls)
hist(final_test$total.day.calls)
qqnorm(final_test$total.day.charge)
hist(final_test$total.day.charge)
qqnorm(final_test$total.eve.minutes)
hist(final_test$total.eve.minutes)
qqnorm(final_test$total.eve.calls)
hist(final_test$total.eve.calls)
qqnorm(final_test$total.eve.charge)
hist(final_test$total.eve.charge)
qqnorm(final_test$total.night.minutes)
hist(final_test$total.night.minutes)
qqnorm(final_test$total.night.calls)
hist(final_test$total.night.calls)
qqnorm(final_test$total.night.charge)
hist(final_test$total.night.charge)
qqnorm(final_test$total.intl.minutes)
hist(final_test$total.intl.minutes)
qqnorm(final_test$total.intl.charge)
hist(final_test$total.intl.charge)
qqnorm(final_test$total.intl.calls)
hist(final_test$total.intl.calls)
qqnorm(final_test$number.customer.service.calls)
hist(final_test$number.customer.service.calls)


#-----------------------------------------MODEL------------------------------------------------------

# LOGISTIC REGRESSION
logistic_model = glm(Churn~.,data = final_train,family = "binomial")
summary(logistic_model)
logistic_pred = predict(logistic_model,newdata = final_test[,-19],type = "response")
logistic_pred=ifelse(logistic_pred>0.5,2,1)
clogistic = table(final_test$Churn,logistic_pred)
confusionMatrix(clogistic)
#accuracy 
89.95
#fnr 
35.00
#--------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#RANDOM FOREST
random_model = randomForest(Churn~.,final_train,importance=TRUE,ntree = 100)
treeList = RF2List(random_model)
exec=extractRules(treeList,final_train[,-19])
exec[1:2,]
readableRules= presentRules(exec,colnames(final_train))
readableRules[1:2,]
rf_predict=predict(random_model,final_test[,-19])
conf_rf = table(final_test$Churn,rf_predict)
confusionMatrix(conf_rf)

#ACCURACY
95.36
#FNR
77.28
#--------------------------------------------------------------------------------------------------------

#KNN IMPUTATION
knn_predict= knn(final_train[,1:18],final_test[,1:18],final_train$Churn,k=1)
conf_matrixKNN = table(final_test$Churn,knn_predict)
confusionMatrix(conf_matrixKNN)

#accuracy
87.3
fnr
54.46
# K=3
knn_predict3 = knn(final_train[,1:18],final_test[,1:18],final_train$Churn,k=3)
conf_matrixKNN3 = table(final_test$Churn,knn_predict3)
confusionMatrix(conf_matrixKNN3)
# accuracy 90.81
# fnr 27.86

# k=5
knn_predict5 = knn(final_train[,1:18],final_test[,1:18],final_train$Churn,k=5)
conf_matrixKNN5 = table(final_test$Churn,knn_predict5)
confusionMatrix(conf_matrixKNN5)
#accuracy 91.56

# k=7
knn_predict7 = knn(final_train[,1:18],final_test[,1:18],final_train$Churn,k=7)
conf_matrixKNN7 = table(final_test$Churn,knn_predict7)
confusionMatrix(conf_matrixKNN7)
#accuracy 91.37


#-----------------------------------------------------------------------------------------------------------
#NAIVE BAYES
naive_model= naiveBayes(Churn~.,data =final_test)
naive_predict = predict(naive_model,final_test[1:18],type = 'class')
confusionMatrix1= table(final_test$Churn,naive_predict)
confusionMatrix(confusionMatrix1)
#mean(naive_predict==final_test$Churn)
#accuracy
93.84
#fnr
37.90
#--------------------------------------------------------------------------------------------------------



