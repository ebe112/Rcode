rm(list=ls())
# 
library(ggplot2)
library(corrplot)
library(dplyr)
library(datasets)
# opening the data to Rstudio

housingdata<-read.csv('housing_data.csv',header=TRUE,sep=',')

# having a look for the datasets structure 

str(housingdata)

# checking if there is na/null values in the dataset
any(is.na(housingdata))
any(is.null(housingdata))

#boxplotting the variables to check how many of the variables have the outliers

boxplot(housingdata$PCCR)
boxplot(housingdata$PRLZ)
boxplot(housingdata$INDUS)
boxplot(housingdata$NOX)
boxplot(housingdata$AVR)
boxplot(housingdata$AGE)
boxplot(housingdata$DIS)
boxplot(housingdata$RAD)
boxplot(housingdata$TAX)
boxplot(housingdata$MEDV)

### start constructing the model 

# constructing simple model TO FIT DATA


#### trying different theta1values

trialval<-seq(-10,20,0.5)

costpred<-c()

for(theta in trialval){
  ypred<-theta*housingdata$AVR
  costpred=c(costpred,mycost(ypred,housingdata$MEDV))
}

resultsdf<-data.frame(theta=trialval,cost=costpred)

#### plotting min cost

ggplot(resultsdf,aes(x=theta,y=cost))+
  geom_line(col='blue')+
  geom_point(aes(x=theta[which.min(cost)],y=min(cost)))




cor(housingdata)

#making model

# start model with all variables and intercept, first is the target variable MEDV. 
# in the first model there is no zero value after ~ becouse the intercept is included, last is the datasest name


housingdata1<-housingdata
model1<-lm(MEDV~+PCCR+PRLZ+INDUS+NOX+AVR+AGE+DIS+RAD+TAX,housingdata)
summary(model1)

####Lets try predicting house values MEDV with the first model using housingdata and the model 1 Medvpred1 is all the predictions

medvpred1<-predict(model1,housingdata)

# lets do a vector from the medvpred1

medvpred1=c(medvpred1)

# lets join the predictions data to the data 

housingdata1<-cbind(housingdata,medvpred1)

# lets see how good, the predictive values fit to the real values of MEDV in the first model line is predicted values and 
#dots are the real values

ggplot(housingdata1,aes(x=1:506,y=MEDV))+
  geom_point()+
  geom_line(y=cmedvpred1,colour='red')

ggplot(housingdata1,aes(x=1:506,y=MEDV))+
  geom_point()+
  geom_point(y=cmedvpred1,colour='red')


### residual analysis model1

# first we plot te density residuals of the model 1 see how the data is distributed 
residuals(model1)
plot(density(residuals(model1)))

# now we plot the sd*3 lines and the residuals and see how the data is distributed, lots of outliers

plot(residuals(model1),pch=16,ylim=c(-50,50))
abline(a=(-3)*sd(residuals(model1)),b=0,col='red')
abline(a=3*sd(residuals(model1)),b=0,col='red')










# making of model 2 ############### 

# first is the normal model 
testmodel2<-lm(MEDV~+PCCR+PRLZ+INDUS+NOX+AVR+AGE+DIS+RAD+TAX,housingdata)
summary(testmodel2)

# then we take the intercept out as it is the p value of 0.605

testmodel3<-lm(MEDV~+0+PCCR+PRLZ+INDUS+NOX+AVR+AGE+DIS+RAD+TAX,housingdata)
summary(testmodel3)

# then we take the indus out as it is 0.281736 p value 

testmodel4<-lm(MEDV~+0+PCCR+PRLZ+NOX+AVR+AGE+DIS+RAD+TAX,housingdata)
summary(testmodel4)
# testmodel4 is the final model(model2), all the variables have good p-values

model2<-lm(MEDV~+0+PCCR+PRLZ+NOX+AVR+AGE+DIS+RAD+TAX,housingdata)
summary(model2)


### lets take the residual analysis model2

# first plotting the density of the model 2, first we see the distribution has a tail right side
# its pretty standard distributed

residuals(model2)
plot(density(residuals(model2)))


plot(residuals(model2),pch=16,ylim=c(-50,50))
abline(a=(-3)*sd(residuals(model2)),b=0,col='red')
abline(a=3*sd(residuals(model2)),b=0,col='red')
abline(a=0*(residuals(model2)),b=0,col='red')
# plotting the residuals with line sd*3 confirms this, there is outliers right
# the middle line shows, that there is distribution both sides of the middlevalues



####Fitted vs orginal model 





# medvpred2 is the model2 value which we now test against the original values visually

medvpred2<-predict(model2,housingdata)
medvpred2=c(medvpred2)
housingdata2<-cbind(housingdata,medvpred2)


# plotting the visual ggplot medv predictions versus original values

ggplot(housingdata2,aes(x=1:506,y=MEDV))+
  geom_point()+
  geom_line(y=medvpred2,colour='blue')

# lets do same visualisation with dots

ggplot(housingdata2,aes(x=1:506,y=MEDV))+
  geom_point()+
  geom_point(y=medvpred2,colour='blue')









##### t7 

featurevariables<-housingdata$PCCR+housingdata$PRLZ+housingdata$INDUS+housingdata$NOX+housingdata$AVR+housingdata$AGE+housingdata$DIS+housingdata$RAD+housingdata$TAX


housingdatanew<-housingdata %>% mutate(PCCR)
model12<-lm(MEDV~0+PCCR+PRLZ+NOX+AVR+AGE+DIS+RAD+TAX,housingdata)
summary(model12)

predict(model12,housingdata$RAD)




plot(residuals(model12),xlab="Observations",ylab="Residuals",main="Residual Analysis",col="blue",pch=16,ylim=c(-8,8))
abline(a=0,b=0,col="black",lty=2)
abline(a=-3*sd(residuals(model12)),b=0,col="red",lty=2)
abline(a=3*sd(residuals(model12)),b=0,col="red",lty=2)




###########correlation analysis between target variable and the different variables

cor(housingdata$MEDV,housingdata$PCCR)
cor(housingdata$MEDV,housingdata$PRLZ)
cor(housingdata$MEDV,housingdata$NOX)
cor(housingdata$MEDV,housingdata$AVR)
cor(housingdata$MEDV,housingdata$AGE)
cor(housingdata$MEDV,housingdata$DIS)
cor(housingdata$MEDV,housingdata$RAD)
cor(housingdata$MEDV,housingdata$TAX)
cor(housingdata$MEDV,housingdata$INDUS)


###############doing loop to calc the min,max values of the dataset

#first the vectors
c(housingdata[1],housingdata[2],housingdata[3],housingdata[4],housingdata[5],housingdata[6],housingdata[7],housingdata[8],housingdata[9],housingdata[10])
a<-vector()
b<-vector()
c<-vector()
d<-vector()
e<-vector()
f<-vector()
g<-vector()
h<-vector()
j<-vector()
k<-vector()

for (i in 1:10){
  
  
  a[i]=cor(housingdata[1],housingdata[i])
  b[i]=cor(housingdata[2],housingdata[i])
  c[i]=cor(housingdata[3],housingdata[i])
  d[i]=cor(housingdata[4],housingdata[i])
  e[i]=cor(housingdata[5],housingdata[i])
  f[i]=cor(housingdata[6],housingdata[i])
  g[i]=cor(housingdata[7],housingdata[i])
  h[i]=cor(housingdata[8],housingdata[i])
  j[i]=cor(housingdata[9],housingdata[i])
  k[i]=cor(housingdata[10],housingdata[i])
}

mymatrix=matrix(1:100,byrow=TRUE, nrow=10)


mymatrix[1,]=a
mymatrix[2,]=b
mymatrix[3,]=c
mymatrix[4,]=d
mymatrix[5,]=e
mymatrix[6,]=f
mymatrix[7,]=g
mymatrix[8,]=h
mymatrix[9,]=j
mymatrix[10,]=k

#looking for high values with the max()

max(mymatrix[mymatrix<0.999999&mymatrix>(-0.999999)])
#looking for min values with the min()

min(mymatrix[mymatrix<0.999999&mymatrix>(-0.999999)])

maxvalue=mymatrix==0.9102282
minvalue=mymatrix==-0.7692301


mymatrix[mymatrix>0.9102281&mymatrix<0.9102283]
mymatrix[mymatrix>-0.7692302&mymatrix<(-0.7692300)]
which.min(mymatrix)
which.max(mymatrix[mymatrix<0.999999&mymatrix>(-0.999999)])
cor(housingdata$NOX,housingdata$DIS)
cor(housingdata$TAX,housingdata$RAD)



##### PCCR	Exploratory data analysis 

#calculating quantiles for each of the variables
quantile(housingdata$PCCR,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7)
mean(housingdata$PCCR)

sum(housingdata$PCCR<0.082045)
sum(housingdata$PCCR<0.256510)
sum(housingdata$PCCR<3.677083)


ggplot(housingdata,aes(x=PCCR))+
  geom_histogram()

ggplot(housingdata,aes(y=PCCR))+
  geom_boxplot()


##### PRLZ	Exploratory data analysis

quantile(housingdata$PRLZ,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7)
mean(housingdata$PRLZ)

sum(housingdata$PRLZ>12.5)
sum(housingdata$PRLZ<0.256510)
sum(housingdata$PRLZ<3.677083)


ggplot(housingdata,aes(x=PRLZ))+
  geom_histogram()

ggplot(housingdata,aes(y=PRLZ))+
  geom_boxplot()


##### INDUS	Exploratory data analysis

quantile(housingdata$INDUS,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7)
mean(housingdata$INDUS)

sum(housingdata$INDUS>9.69)
sum(housingdata$INDUS<0.256510)
sum(housingdata$INDUS<3.677083)


ggplot(housingdata,aes(x=INDUS))+
  geom_histogram()

ggplot(housingdata,aes(y=INDUS))+
  geom_boxplot()

##### NOX	Exploratory data analysis

quantile(housingdata$NOX,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7)
round(mean(housingdata$NOX),digits=3)

sum(housingdata$NOX>9.69)
sum(housingdata$NOX<0.256510)
sum(housingdata$NOX<3.677083)


ggplot(housingdata,aes(x=NOX))+
  geom_histogram()

ggplot(housingdata,aes(y=NOX))+
  geom_boxplot()

##### 4	AVR	Exploratory data analysis

round(quantile(housingdata$AVR,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7),digits=3)
round(mean(housingdata$AVR),digits=3)

ggplot(housingdata,aes(x=AVR))+
  geom_histogram()

ggplot(housingdata,aes(y=AVR))+
  geom_boxplot()

##### 4	AGE	Exploratory data analysis

round(quantile(housingdata$AGE,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7),digits=3)
round(mean(housingdata$AGE),digits=3)

ggplot(housingdata,aes(x=AGE))+
  geom_histogram()

ggplot(housingdata,aes(y=AGE))+
  geom_boxplot()

##### 4	DIS	Exploratory data analysis

round(quantile(housingdata$DIS,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7),digits=3)
round(mean(housingdata$DIS),digits=3)

ggplot(housingdata,aes(x=DIS))+
  geom_histogram()

ggplot(housingdata,aes(y=DIS))+
  geom_boxplot()


##### 4	RAD	Exploratory data analysis

round(quantile(housingdata$RAD,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7),digits=3)
round(mean(housingdata$RAD),digits=3)

ggplot(housingdata,aes(x=RAD))+
  geom_histogram()

ggplot(housingdata,aes(y=RAD))+
  geom_boxplot()


##### 4	TAX	Exploratory data analysis

round(quantile(housingdata$TAX,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7),digits=3)
round(mean(housingdata$TAX),digits=3)

ggplot(housingdata,aes(x=TAX))+
  geom_histogram()

ggplot(housingdata,aes(y=TAX))+
  geom_boxplot()

##### MEDV Exploratory data analysis

round(quantile(housingdata$MEDV,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7),digits=3)
round(mean(housingdata$MEDV),digits=3)

ggplot(housingdata,aes(x=MEDV))+
  geom_histogram()

ggplot(housingdata,aes(y=MEDV))+
  geom_boxplot()

# doing the correlation matrix assigning to mymatrix1
mymatrix1<-cor(housingdata)

#corrplotting the visual 
corrplot(mymatrix1, method = "number")

# looking for the big / low values

mymatrix1[(mymatrix1>0.9|mymatrix1<(-0.9))&mymatrix1!=1]
mymatrix1
mymatrix1[(mymatrix1>0.75|mymatrix1<(-0.75))&mymatrix1!=1]



####################################################################
####################################################################
####################################################################
####################################################################




# PART 2



rm(list=ls())
library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)
library(caTools)
library(scales)
library(class)
#loading dataset

thyroidata<-read.csv('Thyroid_data.csv',header=TRUE,sep=',')

#looking the dataset characterics

str(thyroidata)

# no na in the data

any(is.na(thyroidata))

# how many obs, of clas 1,2,3, 1 150, 2 35, 3 30, 

table(thyroidata$CLASS)




#conducting exploratory data analysis for all variables and plotting histograms+boxplots with ggplot()

#Class data analysis


round(quantile(thyroidata$CLASS,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7),digits=3)


ggplot(thyroidata,aes(x=CLASS))+
  geom_bar()

filter(thyroidata,CLASS==1)
filter(thyroidata,CLASS==2)
filter(thyroidata,CLASS==3)


#T3 data analysis

ggplot(thyroidata,aes(x=T3))+
  geom_histogram()

ggplot(thyroidata,aes(y=T3))+
  geom_boxplot()

quantile(thyroidata$T3,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7)
mean(thyroidata$T3)


#TST data analysis

ggplot(thyroidata,aes(x=TST))+
  geom_histogram()

ggplot(thyroidata,aes(y=TST))+
  geom_boxplot()

quantile(thyroidata$TST,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7)
mean(thyroidata$TST)

#TSTR data analysis

ggplot(thyroidata,aes(x=TSTR))+
  geom_histogram()

ggplot(thyroidata,aes(y=TSTR))+
  geom_boxplot()

quantile(thyroidata$TSTR,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7)
mean(thyroidata$TSTR)


#TSH data analysis

ggplot(thyroidata,aes(x=TSH))+
  geom_histogram()

ggplot(thyroidata,aes(y=TSH))+
  geom_boxplot()

quantile(thyroidata$TSH,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7)
mean(thyroidata$TSH)

#	MAD-TSH data analysis

ggplot(thyroidata,aes(x=MAD.TSH))+
  geom_histogram()

ggplot(thyroidata,aes(y=MAD.TSH))+
  geom_boxplot()

quantile(thyroidata$MAD.TSH,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7)
mean(thyroidata$MAD.TSH)



# normalizing data using min-max method but not using this to categorical variable so we do it for the numerical variabs
#all the variables between 0-1 signing the minmax values to new thyroidatanew variable if the original data is needed

thyroidatanew<-apply(thyroidata[2:6],2,rescale,to=c(0,1))
thyroidatanew<-data.frame(thyroidatanew)

# putting the categorical variable back to the data by making a vector and assigning in to the data with mutate

c<-thyroidata$CLASS
thyroidatanew<-mutate(thyroidatanew,CLASS=c)


#dividing data to 70% 30% crossvalidation /datasplit to do a test and train sets


sample<-sample.split(thyroidatanew$CLASS,SplitRatio=0.7)
train<-subset(thyroidatanew,sample==TRUE)
test<-subset(thyroidatanew,sample==FALSE)

# checking if the sets are correctly constructed
str(train)
str(test)


# making and plotting the decision tree, CLASS is target variable and ., puts all the other variables to the model
#treemdl1 is learned by the training data, so the data=train, method=class, minbucket is standard as 10 when not signed

treemdl1<-rpart(CLASS~.,data=train,method='class')

# plotting the first model tree 

rpart.plot(treemdl1)
rpart.rules(treemdl1)
# visualizing the  decision tree mdl by plotting the data and the rules as ablines
#seems like the TST variable is important here, rules are tst >=0.21 and TST <0.5

# now we plot the training data and put ablines, with the rules learned with the rpart.rules() function

plot(train$TST,col=train$CLASS,xlab = 'observations', ylab='TST')
abline(a=0.5, b=0, col='red', lty=1)
abline(a=0.21, b=0, col='red', lty=1)
# we see, that the rules do very good on the data, seems not to over or underfit much

# our training predictions


#plotting with same abline rules to the test data and see if the model still runs good

plot(test$TST,
     col=test$CLASS,xlab = 'observations', ylab='TST testdata')
abline(a=0.5, b=0, col='red', lty=1)
abline(a=0.21, b=0, col='red', lty=1)
# looks like the model still rules


# assigning a model that predicts categories with the treemdl1
treetrain<-predict(treemdl1,train,type='class')


# assigning a model that predicts categories with the treemdl1 to testdata
treetest<-predict(treemdl1,test,type='class')

#making a confusion matrix for the treetrain model predictions here we can see what values are tp,tn,fp,fn

tab<-table(train$CLASS,treetrain)


# accuracy for the train mdl is the tn+tp+tp/(all the table values)
accuracytrain<-((tab[1,1]+tab[2,2]+tab[3,3])/(sum(tab[1:9])))

# for the first time good 95% accuracy



#confusion matrix for test data and accuracynumber

tab<-table(test$CLASS,treetest)
accuracytest<-((tab[1,1]+tab[2,2]+tab[3,3])/(sum(tab[1:9])))
# ~0.87 worse than training data accuracy


#dif leave sizes (r, minbucket) Traindata means

#first we have to do empty vectors for the data

atx<-vector()
# 40 riviä tekstiä 5 columnia, jotta saadaan 50 eri lottoarvoa
# we do this by doing 40 different bucket values and doing them 50 times 
# each col will have a new value with new sample split
atd<-matrix(0,nrow=40,ncol=50)

# k for 50 values for each bucket value 
for (k in 1:50){
  # i for 40 different bucketvalues
  for (i in 1:40){
    
    #with every loops, we do the sample split and point the train and test values all over again
    sample<-sample.split(thyroidatanew$CLASS,SplitRatio=0.7)
    train<-subset(thyroidatanew,sample==TRUE)
    test<-subset(thyroidatanew,sample==FALSE)
    
    
    
    # then we do the treemdl all over again
    treemdl1=rpart(CLASS~.,data=train,method='class',minbucket=i)
    # after doing the treemdl we do the predictions every time again
    treetrain<-predict(treemdl1,train,type='class')
    #we calc the accuracies every time as well doing the table and then the atd
    tab<-table(train$CLASS,treetrain)
    #atd has 40*50 values of accuracy
    atd[i,k]<-((tab[1,1]+tab[2,2]+tab[3,3])/(sum(tab[1:9])))
    # atx has the mean values of each row accuracies, so we get mean of 50 different values, to get the mean accuracy of each
    # minbucket values
    atx[i]<-(mean(atd[i,1:50]))
    
    
  }}

# then we print the values to take a look

print(atd)
print(atx)


#########################################################


#dif leave sizes (r, minbucket) TEST DATA means

# WE do the same than in the previous task, but for the training data, so we can compare the accuracyvalues of both data

#testidatavectorit
atdtest<-vector()
atxtest<-matrix(0,nrow=40,ncol=50)

# k on rivien määrä eri 50 kertaa lotto
# i on rivien määrä
for (l in 1:50){
  for (i in 1:40){
    
    #samplen lotto
    
    sample<-sample.split(thyroidatanew$CLASS,SplitRatio=0.7)
    train<-subset(thyroidatanew,sample==TRUE)
    test<-subset(thyroidatanew,sample==FALSE)
    
    #testidatalle
    
    treemdl1=rpart(CLASS~.,data=train,method='class',minbucket=i)
    treetest<-predict(treemdl1,test,type='class')
    tab<-table(test$CLASS,treetest)
    atxtest[i,l]<-((tab[1,1]+tab[2,2]+tab[3,3])/(sum(tab[1:9])))
    atdtest[i]<-(mean(atxtest[i,1:50]))
    
  }}

# printataan accuracies of testdata

print(atdtest)
print(atxtest)



# plotting accuracy values train data versus testdata and seeing, where is the best minbucket values

resultsdf=data.frame(minbuck=1:40,train_perf=atx, test_perf=atdtest)
ggplot(resultsdf,aes(x=minbuck, y=train_perf))+
  geom_line(col='blue')+
  geom_line(aes(x=minbuck, y=test_perf),col='red')





atd<-vector()
atrv<-vector()


# doing the knn with train and test sets 
trainpred<-knn(train[,1:5],train[,1:5],as.factor(train$CLASS),10)
testpred<-knn(train[,1:5],test[,1:5],as.factor(train$CLASS),10)


#calculating the accuracy of traindata

tab<-table(train$CLASS,trainpred)
accrtrain<-((tab[1,1]+tab[2,2]+tab[3,3])/(sum(tab[1:9])))

#accuracy of testdata

tab<-table(test$CLASS,testpred)
accrtest<-((tab[1,1]+tab[2,2]+tab[3,3])/(sum(tab[1:9])))

atd[i]<-accrtrain[i]
atrv[i]<-accrtest[i]




#Doing a knnloop, to test the data with multiple k values

for(i in 1:20){
  
  trainpredk = knn(train[,1:5], train[,1:5], factor(train$CLASS), i)
  testpredk = knn(train[,1:5], test[,1:5], factor(train$CLASS), i)
  
  
  
  k<-trainpredk
  
  trainew<-mutate(train[1:5],CLASS=k)
  
  plot(trainew$TST,col=trainew$CLASS,xlab = 'observations', ylab=i)
}


# 
KNNTrain=matrix(0,nrow=50,ncol=200)
KNNTest=matrix(0,nrow=50,ncol=200)


ssKNNTrain=matrix(0,nrow=50,ncol=200)
ssKNNTest=matrix(0,nrow=50,ncol=200)
# testing the mean error of real classes and k predicted classes train and testdata


# now we do the same loop, as in the previous case for the accuracy, for the best knn neigbour value size and so that
# we get the sample split hoax effect from not disturbing our picking of right neighbors value size now we have
# 50 different neigbor values and 200 different values of data per neighbor size


for(i in 1:50){
  for(j in 1:200){
    sample=sample.split(thyroidatanew$CLASS,SplitRatio=0.7)
    train=subset(thyroidatanew,sample==TRUE)
    test=subset(thyroidatanew,sample==FALSE)
    
    
    trainpredk = knn(train[,1:5], train[,1:5], factor(train$CLASS),i)
    testpredk = knn(train[,1:5], test[,1:5], factor(train$CLASS),i)
    KNNTrain[i,j]=1-mean(train$CLASS==trainpredk)
    KNNTest[i,j]=1-mean(test$CLASS==testpredk)
    
    
    
    ssKNNTrain[i,j]=mean(train$CLASS==trainpredk)
    ssKNNTest[i,j]=mean(test$CLASS==testpredk)
  }
}

ssKNNTrain=rowMeans(ssKNNTrain)
ssKNNTest=rowMeans(ssKNNTest)
KNNTrain=rowMeans(KNNTrain)
KNNTest=rowMeans(KNNTest)



#

resultsdf=data.frame(neighbors=1:50,train_perf=KNNTrain, test_perf=KNNTest)
ggplot(resultsdf,aes(x=neighbors, y=train_perf))+
  geom_line(col='blue')+
  geom_line(aes(x=neighbors, y=test_perf),col='red')

resultsdf=data.frame(neighbors=1:50,train_perf=ssKNNTrain, test_perf=ssKNNTest)
ggplot(resultsdf,aes(x=neighbors, y=train_perf))+
  geom_line(col='blue')+
  geom_line(aes(x=neighbors, y=test_perf),col='red')






# now we do the same loop, as in the previous case for the best knn neigbour value size but now we do not use normalized data.

KNNTrain=matrix(0,nrow=50,ncol=200)
KNNTest=matrix(0,nrow=50,ncol=200)

for(i in 1:50){
  for(j in 1:200){
    sample=sample.split(thyroidata$CLASS,SplitRatio=0.7)
    train=subset(thyroidata,sample==TRUE)
    test=subset(thyroidata,sample==FALSE)
    
    trainpredk = knn(train[,1:5], train[,1:5], factor(train$CLASS),i)
    testpredk = knn(train[,1:5], test[,1:5], factor(train$CLASS),i)
    KNNTrain[i,j]=1-mean(train$CLASS==trainpredk)
    KNNTest[i,j]=1-mean(test$CLASS==testpredk)
    
    
    
    ssKNNTrain[i,j]=mean(train$CLASS==trainpredk)
    ssKNNTest[i,j]=mean(test$CLASS==testpredk)
  }
}

KNNTrain=rowMeans(KNNTrain)
KNNTest=rowMeans(KNNTest)


resultsdf=data.frame(neighbors=1:50,train_perf=KNNTrain, test_perf=KNNTest)
ggplot(resultsdf,aes(x=neighbors, y=train_perf))+
  geom_line(col='blue')+
  geom_line(aes(x=neighbors, y=test_perf),col='red')


####################################################################
####################################################################
####################################################################
####################################################################



#Part 3 


rm(list=ls())
install.packages('dummies')
install.packages('gridExtra')

library(scales)
library(NbClust)
library(purrr)
library(ggplot2)
library(dplyr)
library(corrplot)
library(dummies)
library(gridExtra)

# reading the file

winedata<-read.csv('Wine_data.csv',header=TRUE,sep=',')


str(winedata)
# checking for na values --> there is na values in the dataset
any(is.na(winedata))

# checking how many na values there is, true == 1

sum(is.na(winedata))

###### filling the na values with mean values of the variable

# doing a loop, that fills every spot with the variables mean value without taking an account the na values

for (i in 1:14){
  winedata[,i][is.na(winedata[,i])]=mean(winedata[,i][!is.na(winedata[,i])])
}

#checking the na values again, there is none

any(is.na(winedata))



# doing a nother winedata for manipulation

winedata1<-winedata

#normalize the data after i str the data, i saw there was very high and low values in the dataset, without normalizing 
# its hard to get a good clustering result, as the big values make the scale so big, also lots of outliers in the dataset
# so we need to be able to kmean them

winedata2<-apply(winedata[2:14],2,rescale,to=c(0,1))

# must reconstruct the dataframe as it goes off to another format
winedata2<-data.frame(winedata2)


# putting the TYPE variable again in the data untouched

winedata2<-mutate(winedata2,winedata$TYPE)
names(winedata2)[14] <- "TYPE"

# checking the correlation pairs 
cormatrix<-cor(winedata)
# visualize the correlation pairs
corrplot(cormatrix, method = "number")
# checking the most correlated pair
cormatrix[cormatrix<1&cormatrix>0.8]

# trying to find the cormatrix biggest value

cormatrix>0.8605635&cormatrix<1

#FLAVANOIDS&PHENOLS


#EXPLATORY DATA ANALYSIS


#TYPE

plot0<-ggplot(winedata,aes(y=TYPE))+
  geom_bar

plot1<-ggplot(winedata,aes(y=TYPE))+
  geom_boxplot()

plot2<-ggplot(winedata,aes(x=TYPE))+
  geom_histogram()

# putting the plots together
grid.arrange(plot1, plot2, ncol=2)

quantile(winedata$TYPE,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7)
mean(winedata$TYPE)


#Alcohol 2.2

plot0<-ggplot(winedata,aes(y=ALCOHOL))+
  geom_bar

plot1<-ggplot(winedata,aes(y=ALCOHOL))+
  geom_boxplot()

plot2<-ggplot(winedata,aes(x=ALCOHOL))+
  geom_histogram()

grid.arrange(plot1, plot2, ncol=2)

quantile(winedata$ALCOHOL,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7)
mean(winedata$ALCOHOL)



#MALIC 2.3


plot0<-ggplot(winedata,aes(y=MALIC))+
  geom_bar

plot1<-ggplot(winedata,aes(y=MALIC))+
  geom_boxplot()

plot2<-ggplot(winedata,aes(x=MALIC))+
  geom_histogram()

grid.arrange(plot1, plot2, ncol=2)

quantile(winedata$MALIC,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7)
mean(winedata$MALIC)


#ASH 2.4

plot0<-ggplot(winedata,aes(y=ASH))+
  geom_bar

plot1<-ggplot(winedata,aes(y=ASH))+
  geom_boxplot()

plot2<-ggplot(winedata,aes(x=ASH))+
  geom_histogram()

grid.arrange(plot1, plot2, ncol=2)

quantile(winedata$ASH,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7)
mean(winedata$ASH)

#ALCALINITY 2.5

plot0<-ggplot(winedata,aes(y=ALCALINITY))+
  geom_bar

plot1<-ggplot(winedata,aes(y=ALCALINITY))+
  geom_boxplot()

plot2<-ggplot(winedata,aes(x=ALCALINITY))+
  geom_histogram()

grid.arrange(plot1, plot2, ncol=2)

quantile(winedata$ALCALINITY,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7)
mean(winedata$ALCALINITY)

#MAGNESIUM 2.6

plot0<-ggplot(winedata,aes(y=MAGNESIUM))+
  geom_bar

plot1<-ggplot(winedata,aes(y=MAGNESIUM))+
  geom_boxplot()

plot2<-ggplot(winedata,aes(x=MAGNESIUM))+
  geom_histogram()

grid.arrange(plot1, plot2, ncol=2)

quantile(winedata$MAGNESIUM,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7)
mean(winedata$MAGNESIUM)

#PHENOLS 2.7 

plot0<-ggplot(winedata,aes(y=PHENOLS))+
  geom_bar

plot1<-ggplot(winedata,aes(y=PHENOLS))+
  geom_boxplot()

plot2<-ggplot(winedata,aes(x=PHENOLS))+
  geom_histogram()

grid.arrange(plot1, plot2, ncol=2)

quantile(winedata$PHENOLS,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7)
mean(winedata$PHENOLS)


#FLAVANOIDS 2.8


plot0<-ggplot(winedata,aes(y=FLAVANOIDS))+
  geom_bar

plot1<-ggplot(winedata,aes(y=FLAVANOIDS))+
  geom_boxplot()

plot2<-ggplot(winedata,aes(x=FLAVANOIDS))+
  geom_histogram()

grid.arrange(plot1, plot2, ncol=2)

quantile(winedata$FLAVANOIDS,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7)
mean(winedata$FLAVANOIDS)

#NONFLAVANOIDS 2.9

plot0<-ggplot(winedata,aes(y=NONFLAVANOIDS))+
  geom_bar

plot1<-ggplot(winedata,aes(y=NONFLAVANOIDS))+
  geom_boxplot()

plot2<-ggplot(winedata,aes(x=NONFLAVANOIDS))+
  geom_histogram()

grid.arrange(plot1, plot2, ncol=2)

quantile(winedata$NONFLAVANOIDS,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7)
mean(winedata$NONFLAVANOIDS)

#PROANTHOCYANINS 2.9.1
plot0<-ggplot(winedata,aes(y=PROANTHOCYANINS))+
  geom_bar

plot1<-ggplot(winedata,aes(y=PROANTHOCYANINS))+
  geom_boxplot()

plot2<-ggplot(winedata,aes(x=PROANTHOCYANINS))+
  geom_histogram()

grid.arrange(plot1, plot2, ncol=2)

quantile(winedata$PROANTHOCYANINS,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7)
mean(winedata$PROANTHOCYANINS)

#COLOR 2.9.2
plot0<-ggplot(winedata,aes(y=COLOR))+
  geom_bar

plot1<-ggplot(winedata,aes(y=COLOR))+
  geom_boxplot()

plot2<-ggplot(winedata,aes(x=COLOR))+
  geom_histogram()

grid.arrange(plot1, plot2, ncol=2)

quantile(winedata$COLOR,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7)
mean(winedata$COLOR)

#HUE 2.9.3
plot0<-ggplot(winedata,aes(y=HUE))+
  geom_bar

plot1<-ggplot(winedata,aes(y=HUE))+
  geom_boxplot()

plot2<-ggplot(winedata,aes(x=HUE))+
  geom_histogram()

grid.arrange(plot1, plot2, ncol=2)

quantile(winedata$HUE,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7)
mean(winedata$HUE)

#DILUTION 2.9.4
plot0<-ggplot(winedata,aes(y=DILUTION))+
  geom_bar

plot1<-ggplot(winedata,aes(y=DILUTION))+
  geom_boxplot()

plot2<-ggplot(winedata,aes(x=DILUTION))+
  geom_histogram()

grid.arrange(plot1, plot2, ncol=2)

quantile(winedata$DILUTION,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7)
mean(winedata$DILUTION)

#PROLINE 2.9.5
plot0<-ggplot(winedata,aes(y=PROLINE))+
  geom_bar

plot1<-ggplot(winedata,aes(y=PROLINE))+
  geom_boxplot()

plot2<-ggplot(winedata,aes(x=PROLINE))+
  geom_histogram()

grid.arrange(plot1, plot2, ncol=2)

quantile(winedata$PROLINE,probs=seq(0,1,0.25),na.rm=FALSE,names=TRUE,type=7)
mean(winedata$PROLINE)





####

### plotting the normalized data first time with the variables

ggplot(winedata2,aes(x=1:178,y=ALCOHOL))+
  geom_point()+
  geom_point(aes(x=1:178,y=MALIC))+
  geom_point(aes(x=1:178,y=ASH))+
  geom_point(aes(x=1:178,y=ALCALINITY))+
  geom_point(aes(x=1:178,y=MAGNESIUM))+
  geom_point(aes(x=1:178,y=PHENOLS))+
  geom_point(aes(x=1:178,y=FLAVANOIDS))+
  geom_point(aes(x=1:178,y=NONFLAVANOIDS))+
  geom_point(aes(x=1:178,y=PROANTHOCYANINS))+
  geom_point(aes(x=1:178,y=COLOR))+
  geom_point(aes(x=1:178,y=HUE))+
  geom_point(aes(x=1:178,y=DILUTION))+
  geom_point(aes(x=1:178,y=PROLINE))


########

# doing the euclidean distance with all the data

distmat=dist(winedata2,method='euclidean')

# start hierarcical clustering doing the distancematrix with euclidean distances
# distance of each of the observation to all others


# we use clustering model to cluster the distancematrix with complete method
hmdl<-hclust(distmat,method='complete')


# constructing hmdl model to a dendogram
dendmodel<-as.dendrogram(hmdl)
# plotting the dendmodel
plot(dendmodel)

###### dendogram cuts original model and cutting the three from k spots
clustermember<-cutree(hmdl,k=3)

# doing the data with clustermember column (we can color the data by the cluster member)
datanew<-winedata2%>%mutate(member=clustermember)

## plotting with the cluster members the all of the data

ggplot(datanew,aes(x=1:178,y=MALIC,col=as.factor(member)))+
  geom_point()+
  geom_point(aes(x=1:178,y=ALCOHOL))+
  geom_point(aes(x=1:178,y=ASH))+
  geom_point(aes(x=1:178,y=ALCALINITY))+
  geom_point(aes(x=1:178,y=MAGNESIUM))+
  geom_point(aes(x=1:178,y=PHENOLS))+
  geom_point(aes(x=1:178,y=FLAVANOIDS))+
  geom_point(aes(x=1:178,y=NONFLAVANOIDS))+
  geom_point(aes(x=1:178,y=PROANTHOCYANINS))+
  geom_point(aes(x=1:178,y=COLOR))+
  geom_point(aes(x=1:178,y=DILUTION))+
  geom_point(aes(x=1:178,y=PROLINE))+ 
  ylab("Clustering")+
  xlab("Observations")


#k-means 

# doing the k means clustering
# using to winedata2 with 3 cluster centers and nstart is the the algorithm 
# starting model with 3 random points and doing that for twenty times and gives the best result


kmdl<-kmeans(winedata2,centers=3,nstart=20)


ggplot(winedata2,aes(x=1:178,y=MALIC,col=as.factor(kmdl$cluster)))+
  geom_point()+
  geom_point(aes(x=1:178,y=ALCOHOL))+
  geom_point(aes(x=1:178,y=ASH))+
  geom_point(aes(x=1:178,y=ALCALINITY))+
  geom_point(aes(x=1:178,y=MAGNESIUM))+
  geom_point(aes(x=1:178,y=PHENOLS))+
  geom_point(aes(x=1:178,y=FLAVANOIDS))+
  geom_point(aes(x=1:178,y=NONFLAVANOIDS))+
  geom_point(aes(x=1:178,y=PROANTHOCYANINS))+
  geom_point(aes(x=1:178,y=COLOR))+
  geom_point(aes(x=1:178,y=DILUTION))+
  geom_point(aes(x=1:178,y=PROLINE))+ 
  ylab("Clustering")+
  xlab("Observations")

kmdl$withinss
kmdl$centers
kmdl$tot.withinss

###### CHARACTERICS Plotting all the variables one by one to get the characterics of the member of the clusters

kmdl<-kmeans(winedata,centers=3,nstart=20)

plot1<-ggplot(datanew,aes(x=1:178,y=ALCOHOL,col=as.factor(member)))+
  geom_point()
plot2<-ggplot(datanew,aes(x=1:178,y=MALIC,col=as.factor(member)))+
  geom_point()
plot3<-ggplot(datanew,aes(x=1:178,y=ASH,col=as.factor(member)))+
  geom_point()
plot4<-ggplot(datanew,aes(x=1:178,y=ALCALINITY,col=as.factor(member)))+
  geom_point()
plot5<-ggplot(datanew,aes(x=1:178,y=MAGNESIUM,col=as.factor(member)))+
  geom_point()
plot6<-ggplot(datanew,aes(x=1:178,y=PHENOLS,col=as.factor(member)))+
  geom_point()
plot7<-ggplot(datanew,aes(x=1:178,y=FLAVANOIDS,col=as.factor(member)))+
  geom_point()
plot8<-ggplot(datanew,aes(x=1:178,y=NONFLAVANOIDS,col=as.factor(member)))+
  geom_point()
plot9<-ggplot(datanew,aes(x=1:178,y=PROANTHOCYANINS,col=as.factor(member)))+
  geom_point()

plot10<-ggplot(datanew,aes(x=1:178,y=COLOR,col=as.factor(member)))+
  geom_point()
plot11<-ggplot(datanew,aes(x=1:178,y=HUE,col=as.factor(member)))+
  geom_point()
plot12<-ggplot(datanew,aes(x=1:178,y=DILUTION,col=as.factor(member)))+
  geom_point()
plot13<-ggplot(datanew,aes(x=1:178,y=PROLINE,col=as.factor(member)))+
  geom_point()

# arranging them to one big grid

grid.arrange(plot1, plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,plot10,plot11,plot12,plot13, ncol=3,nrow=5,top = "Characterics")






#elbow 
# function to calc kmeans with 10 different k values and with 20 dif starting points

tot_withinss<-map_dbl(1:10,function(k){
  model<-kmeans(winedata2,centers=k,nstart=20)
  model$tot.withinss
})

# plotting the k value to see, where is the best k value

plot(1:10,tot_withinss,type='o',ylab='Elbow method')

#other methods

silclust<-NbClust(winedata2,distance='euclidean',min.nc=2,max.nc=10,method='kmeans',index='silhouette')
gapclust<-NbClust(winedata2,distance='euclidean',min.nc=2,max.nc=10,method='kmeans',index='gap')
chclust<-NbClust(winedata2,distance='euclidean',min.nc=2,max.nc=10,method='kmeans',index='ch')


par(mfrow=c(1,3))
plot(2:10,silclust$All.index,type='o',ylab="Silhouette")
plot(2:10,gapclust$All.index,type='o',ylab="Gap Statistic")
plot(2:10,chclust$All.index,type='o',ylab="Calinski-Harabasz Index")

















