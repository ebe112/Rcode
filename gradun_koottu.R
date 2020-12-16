###################################################### 

#SEMI-LOG REGRESSION

######################################################



rm(list=ls())

setwd("C:/Users/Elias/Desktop/Pro gradu/R - tiedostot")
library(devtools) # https://www.rdocumentation.org/packages/devtools
#install_github("dgrtwo/broom") 
library(broom) # https://cran.r-project.org/web/packages/broom/index.html
library(car) # https://cran.r-project.org/web/packages/car/index.html
library(tidyverse) # https://cran.r-project.org/web/packages/tidyverse/index.html
library(caret) # http://topepo.github.io/caret/index.html
library(olsrr) # https://cran.r-project.org/web/packages/olsrr/index.html
library("neuralnet") # https://cran.r-project.org/web/packages/neuralnet/neuralnet.pdf
library(MASS)# https://cran.r-project.org/web/packages/MASS/MASS.pdf
library(Metrics) #https://cran.r-project.org/web/packages/Metrics/Metrics.pdf
library(dplyr) #https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html
library(ggpubr) # https://cran.r-project.org/web/packages/ggpubr/index.html
library(grid) # https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/grid.html
library(multivar) # https://cran.rstudio.com/web/checks/check_results_multivar.html
library(NeuralNetTools) # https://cran.r-project.org/web/packages/NeuralNetTools/index.html


#importing csv file

data<-read.csv('data7.csv',header=TRUE,sep=';')



#muodostetaan tyhjiä vektoreita

rmsevek<-c()
corvek<-c()
maevek<-c()
for (i in 1:100){
  
  
  # data max
  max_data<-apply(data,2,max)
  # data min
  min_data<-apply(data,2,min)
  
  # minmax data to 20% 80% split
  # scale the data with the formula (X - min (X)) / (max (X) - min (X)) using function scale
  index=sample(1:nrow(data),round(0.8*nrow(data)))
  data_scaled<-scale(data,center=min_data,scale=max_data - min_data)
  train_data<-as.data.frame(data[index,])
  train_data1<-as.data.frame(data_scaled[index,])
  train_data1[,1]<-train_data[,1]
  
  #the apartment cost is log
  train_data1[,1]<-log(train_data1[,1])
  test_data<-as.data.frame(data[-index,])
  test_data1<-as.data.frame(data_scaled[-index,])
  test_data1[,1]<-test_data[,1]
  test_data1[,1]<-log(test_data[,1])
  

  
  #regr mdl
  
  Regression_Model <-lm(ï..Sales.price ~ m.2 + Income.level + Year.of.construction + Ground.floor + ElevatorÂ. + PlotÂ.Â. + 
                           GoodÂ. + BadÂ. + X2h + X3h  
                         + Sauna + Kitchenette + 
                           French.balcony + kaakkoinen + etelÃ.inen + keskinen + 
                           lÃ.ntinen + herttoniemi + 
                           alppiharju  + lauttasaari +  haaga  + vallila + vartiokylÃ. + 
                           mellunkylÃ. + pitÃ.jÃ.nmÃ.ki  + kaarela + 
                           kulosaari + malmi + vuosaari + latokartano + 
                           suutarila +  puistola + pukinmÃ.ki  + oulunkylÃ.,data=train_data1)
  summary(Regression_Model)
  
  
  test2<-(data[-index,])
  test<-(data[-index,])
  test[,1]<-log(test[,1])
  
  
  
  predict_lm<-predict(Regression_Model, test_data1)
  predict_lm<-exp(predict_lm)
  rmsevek[i]<-rmse(test_data$ï..Sales.price,predict_lm)
  maevek[i]<-mae(test_data$ï..Sales.price,predict_lm)
  corvek[i]<-cor(predict_lm,test_data[,1])
  
}

RMSE<-mean(rmsevek)
MAE<-mean(maevek)
correl<-mean(corvek)
RMSE
MAE
correl
#tidy_lmfit <- tidy(Regression_Model)
#tidy_lmfit
#write.csv2((tidy_lmfit), "semilog.csv")


# testing accuracy %

x123<-test_data$ï..Sales.price
x124<-predict_lm
# +-10,20,30

intervalli<-0.25
ylin<-x123*(1+intervalli)
#accuracyvector
alin<-x123*(1-intervalli)

# is the value inside the threshold or not 10, 20, 30%, if is 1 if not 0

lengthvektr<-(length(x124))
predictionaccuracyvector<-c()
for (i in 1:lengthvektr){
  
  if(
    x124[i]>alin[i]&&
    x124[i]<ylin[i]){predictionaccuracyvector[i]<-1
  }
  else {
    predictionaccuracyvector[i]<-0}
}



predictionaccuracyvector

tulos<-sum(predictionaccuracyvector)/(length(predictionaccuracyvector))

kokonaisluku<-(round(tulos,digits=2))*100
kokonaisluku



###################################################### 

#NEURAL NETWORK

######################################################

rm(list=ls())

setwd("C:/Users/Elias/Desktop/Pro gradu/R - tiedostot")
library(devtools)
#install_github("dgrtwo/broom")
library(broom)
library(car)
library(tidyverse)
library(caret)
library(olsrr)
library("neuralnet")
library(MASS)
library(Metrics)
library(dplyr)
library(ggpubr)
library(grid)
library(multivar)
library(NeuralNetTools)

set.seed(1)
#download dataset
data<-read.csv('data7.csv',header=TRUE,sep=';')

# normalize data with minmax normalization So all values are between 1 and 0.
max_data<-apply(data,2,max)
min_data<-apply(data,2,min)
data_scaled<-scale(data,center=min_data,scale=max_data - min_data)

# empty vectors to store the RMSE and MAE values
rmsevek<-c()
maevek<-c()
corvek<-c()
for (i in 1:1){
  
  #make index vector for splitting the data 20% and 80% samples
  index=sample(1:nrow(data),round(0.8*nrow(data)))
  #2190 observations set to training sample
  train_data<-as.data.frame(data_scaled[index,])
  #test observations set to test sample (548 observations)
  test_data<-as.data.frame(data_scaled[-index,])
  
  # the main model
#hidden layers 4 in the main model, stepmax 1e6, error factor is sum of squared errors, startweights start randomly, threshold when process stops, 0.01 same variables chosen in this model and the other models.
  
  net_data=neuralnet((ï..Sales.price ~ m.2 + Income.level + Year.of.construction + Ground.floor + ElevatorÂ. + PlotÂ.Â. + 
                         GoodÂ. + BadÂ. + X2h + X3h  
                       + Sauna + Kitchenette + 
                         French.balcony + kaakkoinen + etelÃ.inen + keskinen + 
                         lÃ.ntinen + herttoniemi + 
                         alppiharju  + lauttasaari +  haaga  + vallila + vartiokylÃ. + 
                         mellunkylÃ. + pitÃ.jÃ.nmÃ.ki  + kaarela + 
                         kulosaari + malmi + vuosaari + latokartano + 
                         suutarila +  puistola + pukinmÃ.ki  + oulunkylÃ.),data=train_data,hidden=4,stepmax=1e6,err.fct="sse", startweights=NULL, threshold=0.01,
                     linear.output=TRUE)
  
  
  
  # predicting the test data with the model 
  
  predict_net_test<-compute(net_data,test_data[,1:61])
  
  predict_net_test_start<-predict_net_test$net.result*(max(data$ï..Sales.price)-min(data$ï..Sales.price))+min(data$ï..Sales.price)
  
  test_start<-as.data.frame((test_data$ï..Sales.price)*(max(data$ï..Sales.price)-min(data$ï..Sales.price))+min(data$ï..Sales.price))
  
  #storing the error measure values
  
  rmsevek[i]<-sqrt(sum((test_start - predict_net_test_start)^2)/nrow(test_start))
  maevek[i]<-mae(test_start[,1], predict_net_test_start)
  corvek[i]<-cor(predict_net_test_start,test_start)
}


RMSE<-mean(rmsevek)
MAE<-mean(maevek)
R2<-mean(corvek)
RMSE
MAE
R2





# pred accuracy

x123<-test_start
x124<-predict_net_test_start
#tehdään vektori jossa +10%

intervalli<-0.25
ylin<-x123*(1+intervalli)
#tehdään vektori jossa -10%
alin<-x123*(1-intervalli)

#jos x124 kohde vektori näiden sisällä 1 jos ei 0

lengthvektr<-(length(x124))
predictionaccuracyvector<-c()
for (i in 1:lengthvektr){
  
  if(
    x124[i]>alin[i,1]&&
    x124[i]<ylin[i,1]){predictionaccuracyvector[i]<-1
  }
  else {
    predictionaccuracyvector[i]<-0}
}



predictionaccuracyvector

tulos<-sum(predictionaccuracyvector)/(length(predictionaccuracyvector))

kokonaisluku<-(round(tulos,digits=2))*100
kokonaisluku


###################################################### 

#DOUBLE-LOG REGRESSION

######################################################




rm(list=ls())

#download needed packages

setwd("C:/Users/Elias/Desktop/Pro gradu/R - tiedostot")
library(devtools)
#install_github("dgrtwo/broom")
library(broom)
library(car)
library(tidyverse)
library(caret)
library(olsrr)
library("neuralnet")
library(MASS)
library(Metrics)
library(dplyr)
library(ggpubr)
library(grid)
library(multivar)
library(NeuralNetTools)

# set data as data7 csv file


data<-read.csv('data7.csv',header=TRUE,sep=';')

# making vectors
rmsevek<-c()
maevek<-c()
#looping the model
for (i in 1:100){
  
  
  
  # normalize data with min-max normalization 

  max_data<-apply(data,2,max)
  min_data<-apply(data,2,min)
  
  #set data1 to be same as data
  data1<-data
  
  # make the sample 20 / 80 %
  
  index1=sample(1:nrow(data1),round(0.8*nrow(data1)))
  
  
  #train_data<-as.data.frame(data_scaled[index1,])
  
  # set the log values for the first 4 variables including ï..Sales.price, m.2, Income.level, Year.of.construction
  
  train_data<-as.data.frame(data1[index1,])
  train_data[,1]<-log(train_data[,1])
  train_data[,2]<-log(train_data[,2])
  train_data[,3]<-log(train_data[,3])
  train_data[,4]<-log(train_data[,4])
  
  # test data as non log variables
  
  
  test_data<-as.data.frame(data1[-index1,])
  
  
  # the main model does not need to be as log log as the prices of the first 4 variables are in log values
  
  
  Regression_Model <-lm(ï..Sales.price ~ m.2 + Income.level + Year.of.construction + Ground.floor + ElevatorÂ. + PlotÂ.Â. + 
                           GoodÂ. + BadÂ. + X2h + X3h  
                         + Sauna + Kitchenette + 
                           French.balcony + kaakkoinen + etelÃ.inen + keskinen + 
                           lÃ.ntinen + herttoniemi + 
                           alppiharju  + lauttasaari +  haaga  + vallila + vartiokylÃ. + 
                           mellunkylÃ. + pitÃ.jÃ.nmÃ.ki  + kaarela + 
                           kulosaari + malmi + vuosaari + latokartano + 
                           suutarila +  puistola + pukinmÃ.ki  + oulunkylÃ.
                         ,train_data)
  
  # saving non log values to test_data1 data frame
  test_data1<-test_data
  
  
  # set log values for the test_data to be able to predict the data, as it has to be in same format than the predicting dataset
  
  test_data[,1]<-log(test_data[,1])
  test_data[,2]<-log(test_data[,2])
  test_data[,3]<-log(test_data[,3])
  test_data[,4]<-log(test_data[,4])
  
  
  
  # predict the values from the test_data
  
  predict_lm<-predict(Regression_Model, test_data)
  # changing the log values to norm with exp 
  predict_lm<-exp(predict_lm)
  
  
  #calculating RMSE and MAE for the model
  
  rmsevek[i]<-rmse(test_data1$ï..Sales.price,predict_lm)
  maevek[i]<-mae(test_data1$ï..Sales.price,predict_lm)
  
}

RMSE<-mean(rmsevek)
MAE<-mean(maevek)
RMSE
MAE

tidy_lmfit <- tidy(Regression_Model)
tidy_lmfit
write.csv2((tidy_lmfit), "loglog.csv")





# testing prediction accuracy %

x123<-test_data1$ï..Sales.price
x124<-predict_lm
#tehdään vektori jossa +10%

intervalli<-0.25
ylin<-x123*(1+intervalli)
#tehdään vektori jossa -10%
alin<-x123*(1-intervalli)

#if x124 target vector is within these boundaries 1 if not 0


lengthvektr<-(length(x124))
predictionaccuracyvector<-c()
for (i in 1:lengthvektr){
  
  if(
    x124[i]>alin[i]&&
    x124[i]<ylin[i]){predictionaccuracyvector[i]<-1
  }
  else {
    predictionaccuracyvector[i]<-0}
}


summary(Regression_Model)
predictionaccuracyvector

tulos<-sum(predictionaccuracyvector)/(length(predictionaccuracyvector))

kokonaisluku<-(round(tulos,digits=2))*100
kokonaisluku



###################################################### 

# STEPWISE and OLS

######################################################



rm(list=ls())
setwd("C:/Users/Elias/Desktop/Pro gradu/R - tiedostot")
library(devtools)
#install_github("dgrtwo/broom")
library(broom)
library(car)
library(tidyverse)
library(caret)
library(olsrr)
library("neuralnet")
library(MASS)
library(Metrics)
library(dplyr)
library(ggpubr)
library(grid)
library(multivar)
library(NeuralNetTools)


set.seed(1)

data<-read.csv('data7.csv',header=TRUE,sep=';')

# min-max normalize
max_data<-apply(data,2,max)
min_data<-apply(data,2,min)
data_scaled<-scale(data,center=min_data,scale=max_data - min_data)


data_scaled1<-(data)
index1=sample(1:nrow(data),round(0.8*nrow(data)))
train_data1<-as.data.frame(data_scaled1[index1,])
test_data1<-as.data.frame(data_scaled1[-index1,])

#split 80% 20%

index=sample(1:nrow(data),round(0.8*nrow(data)))
alldata<-as.data.frame(data_scaled)
#treenidatan observaatiot data-frame train_data
train_data<-as.data.frame(data_scaled[index,])
#testidatan observaatiot data-frame test_data
test_data<-as.data.frame(data_scaled[-index,])


train_data<-as.data.frame

MSE.net_data<-c()
RMSE.net_data<-c()
MAE.net_data<-c()


#### stepwise reg

Regression_Model <-lm(ï..Sales.price~., data=data )
summary(Regression_Model)
test<-data[-index,]
predict_lm<-predict(Regression_Model, test)
MSE.lm<-sum((predict_lm - test$ï..Sales.price)^2)/nrow(test)

RMSE.lm<-sqrt(sum((predict_lm - test$ï..Sales.price)^2)/nrow(test))

REG_MAE<-mae(test$ï..Sales.price,predict_lm)

################# MSE

MSE.net_data
MSE.lm
################### RMSE
RMSE.net_data
RMSE.lm
#################
MAE.net_data
REG_MAE




#stepwise

model3<-lm (ï..Sales.price ~ m.2 + Income.level + Year.of.construction + 
               Top.floor + Ground.floor + ElevatorÂ. + PlotÂ.Â. + 
               GoodÂ. + BadÂ. + X2h + X3h + X4h + 
               + Balcony + Open.plan.kitchenÂ. + Sauna + Kitchenette + 
               Alcove + French.balcony + kaakkoinen + etelÃ.inen + keskinen + 
               lÃ.ntinen + herttoniemi + 
               kampinmalmi + ullanlinna + alppiharju + vironniemi + lauttasaari +  haaga + kallio + vallila + vartiokylÃ. + 
               mellunkylÃ. + munkkiniemi + pitÃ.jÃ.nmÃ.ki + pasila + kaarela + 
               kulosaari + malmi + vuosaari + latokartano + 
               suutarila +  puistola + pukinmÃ.ki + maunula + oulunkylÃ.  + lÃ.nsi.pakila 
             ,data)

summary(model3)


vif(model4) # vif tab

tidy_lmfit <- tidy(net_data$result.matrix)
tidy_lmfit
write.csv2((tidy_lmfit), "model5.csv")
write.csv2((tidy_lmfit), "annmodel.csv")
write.csv2(tidy(vif(model4)), "VIFValues.csv")




#Stepwiselle filu
tidy_lmfit <- tidy(Regression_Model)
tidy_lmfit
write.csv2((tidy_lmfit), "stepwisemalli.csv")



#######

# Residual diagnostiikat

# data on independently ja indetically distribured

ols_plot_resid_qq(model3)

ols_plot_resid_fit(model3)
ols_plot_resid_hist(model3)
ols_test_bartlet



# OLS 

model4<-lm (ï..Sales.price ~ m.2 + Income.level + Year.of.construction + Ground.floor + ElevatorÂ. + PlotÂ.Â. + 
               GoodÂ. + BadÂ. + X2h + X3h  
             + Sauna + Kitchenette + 
               French.balcony + kaakkoinen + etelÃ.inen + keskinen + 
               lÃ.ntinen + herttoniemi + 
               alppiharju  + lauttasaari +  haaga  + vallila + vartiokylÃ. + 
               mellunkylÃ. + pitÃ.jÃ.nmÃ.ki  + kaarela + 
               kulosaari + malmi + vuosaari + latokartano + 
               suutarila +  puistola + pukinmÃ.ki  + oulunkylÃ. 
             ,data)

summary(model4)
ols_plot_resid_qq(model4)
ols_plot_resid_fit(model4)
ols_plot_resid_hist(model4)


# OLS MSE,RMSE,MAE
test<-data[-index,]
predict_lm<-predict(model4, test)

cor(predict_lm,test$ï..Sales.price)

MSE.lm<-sum((predict_lm - test$ï..Sales.price)^2)/nrow(test)
RMSE.lm<-sqrt(sum((predict_lm - test$ï..Sales.price)^2)/nrow(test))
REG_MAE<-mae(test$ï..Sales.price,predict_lm)



x123<-test$ï..Sales.price
x124<-predict_lm


intervalli<-0.10
ylin<-x123*(1+intervalli)

alin<-x123*(1-intervalli)



lengthvektr<-(length(x124))
predictionaccuracyvector<-c()
for (i in 1:lengthvektr){
  
  if(
    x124[i]>alin[i]&&
    x124[i]<ylin[i]){predictionaccuracyvector[i]<-1
  }
  else {
    predictionaccuracyvector[i]<-0}
}



predictionaccuracyvector

tulos<-sum(predictionaccuracyvector)/(length(predictionaccuracyvector))

kokonaisluku<-(round(tulos,digits=2))*100
kokonaisluku
summary(model4)