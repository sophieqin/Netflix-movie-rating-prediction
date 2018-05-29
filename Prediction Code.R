##  Part1: Data Management
##  a) Load the rating data
data<-read.csv("Homework2Data.csv",sep=",",header = T)
is.data.frame(data)

##  b) Build a new dataset where each row is one consumer
##  and 5 columns corresponding to each film
df<-matrix(nrow=length(unique(data$consumerID)),ncol=6)
df<-as.data.frame(df)
colnames(df)<-c("consumerID","rocky1","rocky2","rocky3","rocky4","rocky5")

##  c) Fill the data frame with the rating data
##  install.packages("reshape2")
library(reshape2)
df1<-dcast(data,formula=consumerID~rockyID,value.var="rating")
df[1:80245,]<-df1[1:80245,]
rm(df1)

##  Part2: Data Exploration and Sampling Bias
##  a) Compare the correlations between ratings of each movie 
cor(df[,2:6],use="pairwise.complete.obs")

##  b) Calculate the mean rating of each movie
colMeans(df[,2:6],na.rm=T)

##  c) Calculate the mean rating of each movie with dataset only contains
##  consumers who rated rocky4
df2<-subset(df,!is.na(rocky4))
colMeans(df2[,2:6],na.rm=T)
##  d) Load new data
completeDB<-read.csv("completeDB.csv",sep=",",header = T)
colMeans(completeDB[,1:5])

##  Part3: Explantory Models
##  install.packages("glmnet")
library('glmnet')

##  a) Generate different orders of interactions
firstInteractions = model.matrix(~(rocky1+rocky2+rocky3+rocky4),completeDB)
secondInteractions = model.matrix(~(rocky1+rocky2+rocky3+rocky4)^2,completeDB)
thirdInteractions = model.matrix(~(rocky1+rocky2+rocky3+rocky4)^3,completeDB)
fourthInteractions = model.matrix(~(rocky1+rocky2+rocky3+rocky4)^4,completeDB)

##  b) Run and store a linear regression for each model
lm1=lm(rocky5~firstInteractions,data=completeDB)
lm2=lm(rocky5~secondInteractions,data=completeDB)
lm3=lm(rocky5~thirdInteractions,data=completeDB)
lm4=lm(rocky5~fourthInteractions,data=completeDB)

##  c) AIC, BIC for each linear regressions
model<-data.frame(model=c("first","second","third","fourth"),AIC=c(AIC(lm1),AIC(lm2),AIC(lm3),AIC(lm4)),BIC=c(BIC(lm1),BIC(lm2),BIC(lm3),BIC(lm4)))

##  d) Estimate the lasso model on the fourthInteractions
lassoFit = glmnet(fourthInteractions,completeDB$rocky5,alpha=1)
predict(lassoFit,s = 0.5, type = 'coefficients')
predict(lassoFit,s = 0.05, type = 'coefficients')

##  e) Estimate the optimal penalty parameter
lassoFit1<-cv.glmnet(fourthInteractions,completeDB$rocky5,alpha=1)
lasso<-predict(lassoFit1,s=lassoFit1$lambda.min,type='coefficients')

##  f) Implement a ridge estimator
ridgeFit=cv.glmnet(fourthInteractions,completeDB$rocky5,alpha=0)
ridge<-predict(ridgeFit,s=ridgeFit$lambda.min,type='coefficients')

##  g) Extract the coefficients from the lasso and the ridge regression
table<-as.data.frame(cbind(lm4$coefficients[1:17],lasso[1:17],ridge[1:17]))
rownames(table)[2:17]<-colnames(fourthInteractions)
colnames(table)<-c('linear','lasso','ridge')

##  Part4: Predictive Modelling
##  Use the leaps package to find the best regression of each sizes
##  install.packages('leaps') 
install.packages('leaps') 
library(leaps)
leapsModels = regsubsets(x=fourthInteractions, y=completeDB$rocky5, nbest=1,nvmax=ncol(fourthInteractions))
subsetSummary = summary(leapsModels)
place<-subsetSummary$which
as.data.frame(place)
place<-place[,-(1:2)]

## linear regression
set.seed(123)
nFold = 10 
valNum = floor(runif(nrow(completeDB))*nFold)+1 
a<-colnames(place) 
modelPerformance = matrix(NA,nFold,15)
for(fold in 1:nFold){ 
  for(j in 1:15){
    trainingData = subset(completeDB,valNum!=fold)
    validationData = subset(completeDB,valNum==fold)  
    assign(paste("l",j,sep =""),lm(as.formula(paste0("rocky5~",paste0(a[place[j,]==T],collapse = "+"))),trainingData))
    assign(paste("valid",j,sep =""), mean((validationData$rocky5 - predict(get(paste("l",j,sep="")),validationData))^2)^.5)
    modelPerformance[fold,j] = c(get(paste("valid",j,sep=""))) 
  } 
}
colMeans(modelPerformance)
which.min(colMeans(modelPerformance))

## Transform variables to log format. Use the leaps package to find the best regression of each sizes
fourthlog = model.matrix(~(log(rocky1)+log(rocky2)+log(rocky3)+log(rocky4))^4,completeDB)
leapslog = regsubsets(x=fourthlog, y=completeDB$rocky5, nbest=1,nvmax=ncol(fourthlog))
subsetSummarylog = summary(leapslog)
place1<-subsetSummarylog$which
as.data.frame(place1)
place1<-place1[,-(1:2)]
b<-colnames(place1) 
modelPerformance1 = matrix(NA,nFold,15)
for(fold in 1:nFold){ 
  for(j in 1:15){
    trainingData = subset(completeDB,valNum!=fold)
    validationData = subset(completeDB,valNum==fold)  
    assign(paste("llog",j,sep =""),lm(as.formula(paste0("rocky5~",paste0(b[place1[j,]==T],collapse = "+"))),trainingData))
    assign(paste("valid",j,sep =""), mean((validationData$rocky5 - predict(get(paste("llog",j,sep="")),validationData))^2)^.5)
    modelPerformance1[fold,j] = c(get(paste("valid",j,sep=""))) 
  } 
}
colMeans(modelPerformance1)
which.min(colMeans(modelPerformance1))
bestl<-llog9
## MARS
install.packages("earth")
library(earth)
set.seed(123)
isTraining = runif(nrow(completeDB)) < .8
trainingData = subset(completeDB,isTraining)
validationData = subset(completeDB,!isTraining) 
trace<-c(0,0.3,0.5) 
degree<-c(1,2,3) 
thresh<-c(0.001,0.1)
list<-c()
for (i in 1:3){ 
    for (j in 1:3){ 
      for (k in 1:2){ 
        for (p in 1:15){  
      m<-earth(as.formula(paste0("rocky5~",paste0(a[place[p,]==T],collapse = "+"))),data=trainingData,trace=trace[i],degree=degree[j],thres=thresh[k]) 
      list<-c(list,mean((validationData$rocky5 - predict(m,validationData))^2)^.5)
       } 
    } 
      }  
}   
list
list[which.min(list)]
which.min(list)
a[place[14,]==T]
bestm<-earth(as.formula(paste0("rocky5~",paste0(a[place[14,]==T],collapse = "+"))),data=trainingData,trace=trace[1],degree=degree[2],thres=thresh[1]) 
mean((validationData$rocky5 - predict(bestm,validationData))^2)^.5
list1<-c()
for (i in 1:3){ 
  for (j in 1:3){ 
    for (k in 1:2){ 
      for (p in 1:15){  
        ml<-earth(as.formula(paste0("rocky5~",paste0(b[place1[p,]==T],collapse = "+"))),data=trainingData,trace=trace[i],degree=degree[j],thres=thresh[k]) 
        list1<-c(list1,mean((validationData$rocky5 - predict(ml,validationData))^2)^.5)
      } 
    } 
  }  
}   
list1 
list1[which.min(list1)]

## knn
install.packages('kknn')
library('kknn')
listk<-c()
listk1<-c()
for (i in 1:20){  
  for (j in 1:15){
    r<-as.formula(paste0("rocky5~",paste0(a[place[j,]==T],collapse = "+")))
    d<-kknn(r,trainingData,validationData,k=i)
  listk<-c(listk,mean((validationData$rocky5 - d$fitted.values)^2)^.5)
  } 
}
listk[which.min(listk)]                                
bestk<-kknn(rocky5~rocky3:rocky4,trainingData,validationData,k=20)
mean((validationData$rocky5 - bestk$fitted.values)^2)^.5
for (i in 1:20){  
  for (j in 1:15){
    r<-as.formula(paste0("rocky5~",paste0(b[place1[j,]==T],collapse = "+")))
    d<-kknn(r,trainingData,validationData,k=i)
    listk1<-c(listk1,mean((validationData$rocky5 - d$fitted.values)^2)^.5)
  } 
}
listk1[which.min(listk1)]

## nnet
install.packages("nnet")
library("nnet")
listn<-c()
for (i in 1:30){ 
  for(j in 1:15){
  r<-as.formula(paste0("rocky5~",paste0(a[place[j,]==T],collapse = "+")))
  n<-nnet(r,data=trainingData,linout=1,size=i,maxit=1000) 
  listn<-c(listn,mean((validationData$rocky5 - predict(n,validationData))^2)^.5)
  } 
}
listn
listn[which.min(listn)]
listn1<-c()
for (i in 1:30){ 
  for(j in 1:15){
    r<-as.formula(paste0("rocky5~",paste0(b[place1[j,]==T],collapse = "+")))
    n<-nnet(r,data=trainingData,linout=1,size=i,maxit=1000) 
    listn1<-c(listn1,mean((validationData$rocky5 - predict(n,validationData))^2)^.5)
  } 
} 
listn1 
listn1[which.min(listn1)] 
bestn<-nnet(as.formula(paste0("rocky5~",paste0(b[place1[11,]==T],collapse = "+"))),data=trainingData,linout=1,size=4,maxit=1000)
mean((validationData$rocky5 - predict(bestn,validationData))^2)^.5

## Using entire data to re-estimate 
mean((completeDB$rocky5 - predict(bestl,completeDB))^2)^.5
mean((completeDB$rocky5 - predict(bestm,completeDB))^2)^.5
mean((completeDB$rocky5 - predict(bestn,completeDB))^2)^.5
bestk<-kknn(rocky5~rocky3:rocky4,trainingData,completeDB,k=20)
mean((completeDB$rocky5 - bestk$fitted.values)^2)^.5

## d)Using testdata to predict 
test<-read.csv("Homework 2 - Test Set.csv",header=T,sep=",")
linear<-predict(bestl,test)
test$linear<-linear
test$MARS<-predict(bestm,test)
bestk<-kknn(rocky5~rocky3:rocky4,trainingData,test,k=20)
test$knn<-bestk$fitted.values
test$nnet<-predict(bestn,test)
write.csv(test,file="bestPrediction8.csv") 
save(df,model,table,bestk,bestl,bestm,bestn,test,file="results.Rda")
