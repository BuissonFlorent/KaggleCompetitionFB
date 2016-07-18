### Script for final computations

dir = "C:/Users/Florent/Desktop/Data analysis applications/Facebook competition"
setwd(dir)

library(data.table)
library(bit64)
library(class)
library(dplyr)
library(Rcpp)
library(devtools)
library(inline)
library(magrittr)

# Getting the C++ functions
sourceCpp("multiknn3.cpp")

#Creating the train and test datasets for the final computations
train <- fread("train_NE.csv", stringsAsFactors = T)
train[,V1:=NULL]
test <- fread("test_NE.csv", stringsAsFactors = T)
test[,V1:=NULL]

train = train %>%
  mutate(xbin=as.integer(floor(x*3)))
train = train %>%
  mutate(ybin=as.integer(floor(y*3)))
setkey(train,xbin,ybin)


test = test %>%
  mutate(xbin=as.integer(floor(x*3)))
test = test %>%
  mutate(ybin=as.integer(floor(y*3)))
setkey(test,xbin,ybin)


#Weights for the time variables
l = 125
w = 500

train$hour = ((train$time/60) %% 24)/l 
train$weekday = ((train$time/(60*24)) %% 7)/w
train$month = ((train$time/(60*24*30)) %% 12)/w
train$year = (train$time/(60*24*365))/w

test$hour = ((test$time/60) %% 24)/l 
test$weekday = ((test$time/(60*24)) %% 7)/w
test$month = ((test$time/(60*24*30)) %% 12)/w
test$year = (test$time/(60*24*365))/w

#Applying the revised KNN algo
subtest_NE=as.matrix(test[,.(row_id,x,y,xbin,ybin,hour,weekday,month,year)])
class(subtest_NE)="numeric"
subtrain_NE=as.matrix(train[,.(row_id,x,y,xbin,ybin,hour,weekday,month,year,place_id)])
subtrain_NE=cbind(subtrain_NE,dist=rep(0,nrow(subtrain_NE)))
class(subtrain_NE)="numeric"
out_NE=multiknn3(subtest_NE,subtrain_NE)
Predict_NE=as.data.table(out_NE)
colnames(Predict_NE)=c("row_id","P1","nP1","P2","nP2",
                    "P3","nP3","P4","nP4","P5","nP5")
Predict_NE$P1=as.character.integer64(Predict_NE$P1)
Predict_NE$P2=as.character.integer64(Predict_NE$P2)
Predict_NE$P3=as.character.integer64(Predict_NE$P3)
Predict_NE$P4=as.character.integer64(Predict_NE$P4)
Predict_NE$P5=as.character.integer64(Predict_NE$P5)

write.csv(Predict_NE, "Predict_NE.csv", row.names = FALSE, na="",quote=FALSE)