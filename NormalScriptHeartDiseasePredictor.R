library(dplyr)
library(ggplot2)
library(caTools)
library(randomForest)

data<-read.csv('heart.csv')
head(data)

any(is.na(data))
str(data)
data$sex<-factor(data$sex)
data$fbs<-factor(data$fbs)
data$restecg<-factor(data$restecg)
data$exang<-factor(data$exang)
data$slope<-factor(data$slope)
data$ca<-factor(data$ca)
data$target<-factor(data$target)
data$thal<-factor(data$thal)

ggplot(data,aes(ca,trestbps))+geom_boxplot(aes(color=sex))+labs(x='number of major vessels (0-3) colored by flourosopy',y='resting blood pressure (in mm Hg on admission to the hospital)')
ggplot(data,aes(exang,trestbps))+geom_boxplot(aes(color=sex))+labs(x='exercise induced angina',y='resting blood pressure (in mm Hg on admission to the hospital)')
ggplot(data,aes(fbs,trestbps))+geom_boxplot(aes(color=sex))+labs(x='fasting blood sugar &gt; 120 mg/dl  (1 = true; 0 = false)',y='resting blood pressure (in mm Hg on admission to the hospital)')
ggplot(data,aes(thalach,trestbps))+geom_point(aes(color=sex))+labs(x='maximum heart rate achieved',y='resting blood pressure (in mm Hg on admission to the hospital)')
ggplot(data,aes(chol,trestbps))+geom_point(aes(color=sex))+labs(x='serum cholestoral in mg/dl',y='resting blood pressure (in mm Hg on admission to the hospital)')
ggplot(data,aes(chol,thalach))+geom_point(aes(color=sex))+labs(x='serum cholestoral in mg/dl',y='maximum heart rate achieved')
ggplot(data,aes(ï..age,trestbps))+geom_point(aes(color=sex))+labs(x='age',y='resting blood pressure (in mm Hg on admission to the hospital)')
ggplot(data,aes(ï..age,chol))+geom_point(aes(color=sex))+labs(x='(fasting blood sugar &gt; 120 mg/dl) (1 = true; 0 = false)',y='resting blood pressure (in mm Hg on admission to the hospital)')
ggplot(data,aes(ï..age,thalach))+geom_point(aes(color=sex))+labs(x='(fasting blood sugar &gt; 120 mg/dl) (1 = true; 0 = false)',y='resting blood pressure (in mm Hg on admission to the hospital)')

sample<-sample.split(data,SplitRatio=0.7)
train<-subset(data,sample=T)
test<-subset(data,sample=F)

model<-randomForest(target~., train, importance = T, ntree=500)
predictions<-predict(model,test)

cm<-table(predictions,test$target)
cm