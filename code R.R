#install packages
install.packages('e1071', dependencies=TRUE)
install.packages('corrr', dependencies=TRUE)
install.packages('dplyr', dependencies=TRUE)
install.packages('caret')
install.packages("corrplot",dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages("DataExplorer")
install.packages("ROCR")
install.packages( "pROC")
install.packages( "data.table")
install.packages("ggplot2",dependencies = TRUE)
install.packages("Rcpp")
install.packages("Rtools")






#load libraries
library(e1071)
library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(dplyr) 
library(caret)



#load the dataset
traind=read.csv("train.csv")

#Exploratory data analysis
head(traind)
summary(traind)
str(traind)

#corrlation matrix
df_corr <- cor(traind)
df_corr

ggcorrplot(df_corr, type = "lower", outline.col = "black",
           method="circle",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))


#missing value 
sum(is.na(traind))
#split the data into a trainingset and testset
ind = sample(2, nrow(traind), replace = TRUE, prob = c(0.8, 0.2))
train = traind[ind==1,]
test = traind[ind==2,]

#featue scalling 
#train
xt=train[-21]
yt=train$price_range
xt = scale(xt)
#test
xtt=test[-21]
ytt=test$price_range
xtt = scale(xtt)
####
data_train=data.frame(xt,y=as.factor(yt))
data_test=data.frame(xtt,ytt)

#train the model
model= svm(y~., data=data_train, method="C-classification", kernal="radial", 
           gamma=0.1, cost=10)

summary(model)

#accuracy 
predt = predict(model, data_train)
predtt <- predict(model,data_test)
print(paste("train accuracy:", round(mean(predt == data_train$y), 4)))
print(paste("test accuracy:", round(mean(predtt == data_test$ytt), 4)))

# confusion matrix
cm=confusionMatrix(table(data_test$ytt,predictions = predtt))
cm


train[,22]=train['px_height']+train['px_width']
 train[,23]=train['three_g']+train['four_g']
train[,24]=train['pc']+train['fc']
x_train = train[,c('ram','battery_power',"int_memory","px_height.1","three_g.1","pc.1")]
x_test = test[,c('ram','battery_power',"int_memory","px_height",'px_width','four_g','fc',"three_g","pc")]

data_train1=data.frame(x_train,y=as.factor(yt))
data_test2=data.frame(x_test,ytt)

model1= svm(y~., data=data_train1, method="C-classification", kernal="radial", 
           gamma=0.1, cost=10)

summary(model)

#accuracy 
predt = predict(model1, data_train1)
predtt <- predict(model1,data_test2)
print(paste("train accuracy:", round(mean(predt == data_train1$y), 4)))
print(paste("test accuracy:", round(mean(predtt == data_test2$ytt), 4)))

cm=confusionMatrix(table(data_test2$ytt,predictions = predtt))
cm

#cross vallidation 
control <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
svm_model <- train(y ~ ., data = data_train1, method = "svmRadial", trControl = control)
print(svm_model)
svm_prediction <- predict(svm_model, newdata = data_test2)
predsvm = predict(svm_model, data_train1)
print(paste("train accuracy:", round(mean(predsvm == data_train1$y), 4)))
print(paste("test accuracy:", round(mean(svm_prediction == data_test2$ytt), 4)))

cmcv=confusionMatrix(table(data_test2$ytt,predictions = svm_prediction))
cmcv
















