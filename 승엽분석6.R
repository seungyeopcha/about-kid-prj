setwd("C:/Users/user/OneDrive/바탕 화면/학대아동/")

cp_17<-read.csv("cp_17_수정.csv")
cp_18<-read.csv("cp_18_수정.csv")

##아동학대 발생률
cp_17$nn_pop<-cp_17$nn/cp_17$child_pop*1000
cp_18$nn_pop<-cp_18$nn/cp_18$child_pop*1000

##name 제거
cp_17<-cp_17[,-c(1,3)]
cp_18<-cp_18[,-c(1,3)]

#e
options(scipen=999)

##변수/pop*1000
cp_17_pop<-cp_17
for(i in c(5:24,40,41)){
  cp_17_pop[i]=cp_17_pop[i]/cp_17_pop$pop*1000
}

cp_18_pop<-cp_18
for(i in c(5:24,40,41)){
  cp_18_pop[i]=cp_18_pop[i]/cp_18_pop$pop*1000
}
cp_17_pop
#rbind
cp<-rbind(cp_17_pop,cp_18_pop)

#위험도
summary(cp$nn_pop)
boxplot(cp$nn_pop)
quantile(cp$nn_pop,probs = c(0.33,0.66))

cp$level<-ifelse(cp$nn_pop<2.7,'a',ifelse(cp$nn_pop<4.5,'b','c'))

cp$level<-factor(cp$level)

#pop,nn_pop, nn X
cp<-cp[,-c(1,40,42)]

#train-test
##레벨의 비율
table(cp$level) 

set.seed(0101)

#a
df_a<-cp[cp$level=='a',]
idx_a<-sample(1:nrow(df_a),0.7*nrow(df_a))
train_a<-df_a[idx_a,]
test_a<-df_a[-idx_a,]

#b
df_b<-cp[cp$level=='b',]
idx_b<-sample(1:nrow(df_b),0.7*nrow(df_b))
train_b<-df_b[idx_b,]
test_b<-df_b[-idx_b,]

#c
df_c<-cp[cp$level=='c',]
idx_c<-sample(1:nrow(df_c),0.7*nrow(df_c))
train_c<-df_c[idx_c,]
test_c<-df_c[-idx_c,]


#합치기
train<-rbind(train_a,train_b,train_c)
test<-rbind(test_a,test_b,test_c)

### 랜덤포레스트 ###
#install.packages("tidyverse")
library(tidyverse)
library(caret)
library(randomForest)
library(e1071)
#install.packages("forecast")
library(forecast)

#install.packages("Metrics")
library(Metrics)

rt.fit<-randomForest(level~.,data=train,importance=T)

#train 예측
pred<-predict(rt.fit,train)
confusionMatrix(pred,train$level)

#test 예측
pred<-predict(rt.fit,test)
confusionMatrix(pred,test$level)

#중요도
varImpPlot(rt.fit,type = 1)
