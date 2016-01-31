rm(list = ls())
setwd("D:/Ludwik/R/Practical maschine learning/Final Project")
## getting data
train.input<-
  read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"),
           header=TRUE,
           na.string=c("NA",""))
test.input<- 
  read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"),
           header=TRUE,
           na.string=c("NA",""))

## clining data
usable.columns<-c()
for(i in 1:ncol(train.input))
{
  if(is.na(train.input[1,i])==FALSE)
  {
    usable.columns<-c(usable.columns,names(train.input[i]))

  }
}
## first 7 coumns are identification columns so are useless for algoritm.
usable.columns<-usable.columns[c(-1:-7)]
train.data<-train.input[,usable.columns]
test.data<-test.input[,usable.columns[usable.columns!="classe"]]

### creating test and train parts
library(caret)
inTrain = createDataPartition(y=train.data$classe, p=0.8, list=FALSE)
train = train.data[inTrain,]
test = train.data[-inTrain,]
dim(train); dim(test)

##exploratory analysis
p1<-ggplot(train,aes(roll_belt,pitch_belt,colour=classe))+stat_density2d() +geom_point(aes(colour = factor(classe)))
p2<-ggplot(train,aes(roll_belt,yaw_belt,colour=classe))+stat_density2d() +geom_point(aes(colour = factor(classe)))
p3<-ggplot(train,aes(pitch_belt,yaw_belt,colour=classe))+stat_density2d() +geom_point(aes(colour = factor(classe)))


pairs(train[1:4],bg = c("red","green3","blue","orange","gray")[unclass(train$classe)],pch=21)

## creating model
library(randomForest)
model=train(classe~.,method="rf",data=train)
mean(predict(model, testing) == testing$classe) * 100



