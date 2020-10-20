# author: Fabrizio Miorelli
# licence: GNU AGPLv3 

rm(list=ls())

library(XML)
library(parallel)
library(rugarch)
library(fExtremes)


################################################################################
# 0) Preliminary tasks
################################################################################

# Loading the functions that are used here
path = 'insert your work directory'
setwd(path)
source('f_evt_quant.r')


# Set working directory
path = 'insert your work directory'
setwd(path)


# testing the xml parser and the xml to dataframe convertor

filename = '20200101MGPPrezzi.xml'

#Parsing xml file and extracting "Prezzi" nodes from NewDataSet Root
data<-xmlParse(filename)
df<-xmlToDataFrame(nodes=getNodeSet(data, "//NewDataSet/Prezzi"))


################################################################################
# 1) Getting the prices from the xmls
################################################################################

# Retrieve the dataset: iterating through the folder and parsing each xml file, 
# binding each dataframe to the df_data dataframe

names<-list.files(path="F:/Progetti/MGP/files", pattern='^2020|^2019|^2018|^2017|^2016|^2015|^2014|^2013|^2012|^2011|^2010', full.names=TRUE, recursive=FALSE)
xmls<-lapply(names, xmlParse)

for(i in 1:length(xmls))
{
  if(i == 1)
  {
    df_data<-xmlToDataFrame(nodes=getNodeSet(xmls[[i]], "//NewDataSet/Prezzi"))[,1:4]
  }
  else
  {
    data<-xmlToDataFrame(nodes=getNodeSet(xmls[[i]], "//NewDataSet/Prezzi"))
    print(ncol(data))
    df_data<-rbind(data[,1:4], df_data)
  }
}


# Arranging the dataframe: leaving unused columns and Converting strings to dates/deciamls
df_data$Data<-as.POSIXct(as.character(df_data$Data), format="%Y%m%d")
df_data$Ora<-as.numeric(as.character(df_data$Ora))
df_data$PUN<-gsub(',','.',df_data$PUN)
df_data$PUN<-as.numeric(df_data$PUN)


################################################################################
# 2) Calculating returns
################################################################################
# Prices


# Sort dataframe
df_data<-df_data[with(df_data, order(Data, Ora)), ]

# Calculating Returns
for(i in 2:nrow(df_data))
{
  df_data[i,'Return']<-df_data$PUN[i]/df_data$PUN[i-1]-1
}



################################################################################
# 2) Creating samples based on weekday names and hour
################################################################################
### Filtering for weekday: only workdays
df_data_wd<-subset(df_data, weekdays(df_data$Data)!='Sunday' & weekdays(df_data$Data)!='Saturday')


# Filtering for hours: 01
df_data_wd_01<-subset(df_data_wd, Ora == 1)
df_data_wd_01$Return<-replace(df_data_wd_01$Return, is.na(df_data_wd_01$Return), mean(na.omit(df_data_wd_01$Return)))

# Histogram and statistics
hist(df_data_wd_01$Return, breaks=30, freq=TRUE, xlab='%', ylab='Freq', xlim=c(-1,1), main='PUN - var % - ora 01:00')
summary(df_data_wd_01$Return)
boxplot(df_data_wd_01$Return)
acf(na.omit(df_data_wd_01$Return))
acf(na.omit(df_data_wd_01$Return^2))



# Filtering for hours: 02
df_data_wd_02<-subset(df_data_wd, Ora == 2)

# Histogram and statistics
hist(df_data_wd_02$Return, breaks=30, freq=TRUE, xlab='%', ylab='Freq', xlim=c(-1,1), main='PUN - var % - ora 02:00')
summary(df_data_wd_02$Return)
boxplot(df_data_wd_02$Return)
acf(na.omit(df_data_wd_02$Return))
acf(na.omit(df_data_wd_02$Return^2))
df_data_wd_02$Return<-replace(df_data_wd_02$Return, is.na(df_data_wd_02$Return), mean(na.omit(df_data_wd_02$Return)))


# Filtering for hours: 03
df_data_wd_03<-subset(df_data_wd, Ora == 3)

# Histogram and statistics
hist(df_data_wd_03$Return, breaks=30, freq=TRUE, xlab='%', ylab='Freq', xlim=c(-1,1), main='PUN - var % - ora 03:00')
summary(df_data_wd_03$Return)
boxplot(df_data_wd_03$Return)
acf(na.omit(df_data_wd_03$Return))
acf(na.omit(df_data_wd_03$Return^2))
df_data_wd_03$Return<-replace(df_data_wd_03$Return, is.na(df_data_wd_03$Return), mean(na.omit(df_data_wd_03$Return)))


# Filtering for hours: 04
df_data_wd_04<-subset(df_data_wd, Ora == 4)

# Histogram and statistics
hist(df_data_wd_04$Return, breaks=30, freq=TRUE, xlab='%', ylab='Freq', xlim=c(-1,1), main='PUN - var % - ora 04:00')
summary(df_data_wd_04$Return)
boxplot(df_data_wd_04$Return)
acf(na.omit(df_data_wd_04$Return))
acf(na.omit(df_data_wd_04$Return^2))
df_data_wd_04$Return<-replace(df_data_wd_04$Return, is.na(df_data_wd_04$Return), mean(na.omit(df_data_wd_04$Return)))


# Filtering for hours: 05
df_data_wd_05<-subset(df_data_wd, Ora == 5)

# Histogram and statistics
hist(df_data_wd_05$Return, breaks=30, freq=TRUE, xlab='%', ylab='Freq', xlim=c(-1,1), main='PUN - var % - ora 05:00')
summary(df_data_wd_05$Return)
boxplot(df_data_wd_05$Return)
acf(na.omit(df_data_wd_05$Return))
acf(na.omit(df_data_wd_05$Return^2))
df_data_wd_05$Return<-replace(df_data_wd_05$Return, is.na(df_data_wd_05$Return), mean(na.omit(df_data_wd_05$Return)))


# Filtering for hours: 06
df_data_wd_06<-subset(df_data_wd, Ora == 6)

# Histogram and statistics
hist(df_data_wd_06$Return, breaks=30, freq=TRUE, xlab='%', ylab='Freq', xlim=c(-1,1), main='PUN - var % - ora 06:00')
summary(df_data_wd_06$Return)
boxplot(df_data_wd_06$Return)
acf(na.omit(df_data_wd_06$Return))
acf(na.omit(df_data_wd_06$Return^2))
df_data_wd_06$Return<-replace(df_data_wd_06$Return, is.na(df_data_wd_06$Return), mean(na.omit(df_data_wd_06$Return)))


# Filtering for hours: 07
df_data_wd_07<-subset(df_data_wd, Ora == 7)

# Histogram and statistics
hist(df_data_wd_07$Return, breaks=30, freq=TRUE, xlab='%', ylab='Freq', xlim=c(-1,1), main='PUN - var % - ora 07:00')
summary(df_data_wd_07$Return)
boxplot(df_data_wd_07$Return)
acf(na.omit(df_data_wd_07$Return))
acf(na.omit(df_data_wd_07$Return^2))
df_data_wd_07$Return<-replace(df_data_wd_07$Return, is.na(df_data_wd_07$Return), mean(na.omit(df_data_wd_07$Return)))


# Filtering for hours: 08
df_data_wd_08<-subset(df_data_wd, Ora == 8)

# Histogram and statistics
hist(df_data_wd_08$Return, breaks=30, freq=TRUE, xlab='%', ylab='Freq', xlim=c(-1,1), main='PUN - var % - ora 08:00')
summary(df_data_wd_08$Return)
boxplot(df_data_wd_08$Return)
acf(na.omit(df_data_wd_08$Return))
acf(na.omit(df_data_wd_08$Return^2))
df_data_wd_08$Return<-replace(df_data_wd_08$Return, is.na(df_data_wd_08$Return), mean(na.omit(df_data_wd_08$Return)))


# Filtering for hours: 09
df_data_wd_09<-subset(df_data_wd, Ora == 9)

# Histogram and statistics
hist(df_data_wd_09$Return, breaks=30, freq=TRUE, xlab='%', ylab='Freq', xlim=c(-1,1), main='PUN - var % - ora 09:00')
summary(df_data_wd_09$Return)
boxplot(df_data_wd_09$Return)
acf(na.omit(df_data_wd_09$Return))
acf(na.omit(df_data_wd_09$Return^2))
df_data_wd_09$Return<-replace(df_data_wd_09$Return, is.na(df_data_wd_09$Return), mean(na.omit(df_data_wd_09$Return)))


# Filtering for hours: 10
df_data_wd_10<-subset(df_data_wd, Ora == 10)

# Histogram and statistics
hist(df_data_wd_10$Return, breaks=30, freq=TRUE, xlab='%', ylab='Freq', xlim=c(-1,1), main='PUN - var % - ora 10:00')
summary(df_data_wd_10$Return)
boxplot(df_data_wd_10$Return)
acf(na.omit(df_data_wd_10$Return))
acf(na.omit(df_data_wd_10$Return^2))
df_data_wd_10$Return<-replace(df_data_wd_10$Return, is.na(df_data_wd_10$Return), mean(na.omit(df_data_wd_10$Return)))


# Filtering for hours: 11
df_data_wd_11<-subset(df_data_wd, Ora == 11)

# Histogram and statistics
hist(df_data_wd_11$Return, breaks=30, freq=TRUE, xlab='%', ylab='Freq', xlim=c(-1,1), main='PUN - var % - ora 11:00')
summary(df_data_wd_11$Return)
boxplot(df_data_wd_11$Return)
acf(na.omit(df_data_wd_11$Return))
acf(na.omit(df_data_wd_11$Return^2))
df_data_wd_11$Return<-replace(df_data_wd_11$Return, is.na(df_data_wd_11$Return), mean(na.omit(df_data_wd_11$Return)))


# Filtering for hours: 12
df_data_wd_12<-subset(df_data_wd, Ora == 12)

# Histogram and statistics
hist(df_data_wd_12$Return, breaks=30, freq=TRUE, xlab='%', ylab='Freq', xlim=c(-1,1), main='PUN - var % - ora 12:00')
summary(df_data_wd_12$Return)
boxplot(df_data_wd_12$Return)
acf(na.omit(df_data_wd_12$Return))
acf(na.omit(df_data_wd_12$Return^2))
df_data_wd_12$Return<-replace(df_data_wd_12$Return, is.na(df_data_wd_12$Return), mean(na.omit(df_data_wd_12$Return)))


# Filtering for hours: 13
df_data_wd_13<-subset(df_data_wd, Ora == 13)

# Histogram and statistics
hist(df_data_wd_13$Return, breaks=30, freq=TRUE, xlab='%', ylab='Freq', xlim=c(-1,1), main='PUN - var % - ora 13:00')
summary(df_data_wd_13$Return)
boxplot(df_data_wd_13$Return)
acf(na.omit(df_data_wd_13$Return))
acf(na.omit(df_data_wd_13$Return^2))
df_data_wd_13$Return<-replace(df_data_wd_13$Return, is.na(df_data_wd_13$Return), mean(na.omit(df_data_wd_13$Return)))


# Filtering for hours: 14
df_data_wd_14<-subset(df_data_wd, Ora == 14)

# Histogram and statistics
hist(df_data_wd_14$Return, breaks=30, freq=TRUE, xlab='%', ylab='Freq', xlim=c(-1,1), main='PUN - var % - ora 14:00')
summary(df_data_wd_14$Return)
boxplot(df_data_wd_14$Return)
acf(na.omit(df_data_wd_14$Return))
acf(na.omit(df_data_wd_14$Return^2))
df_data_wd_14$Return<-replace(df_data_wd_14$Return, is.na(df_data_wd_14$Return), mean(na.omit(df_data_wd_14$Return)))


# Filtering for hours: 15
df_data_wd_15<-subset(df_data_wd, Ora == 15)

# Histogram and statistics
hist(df_data_wd_15$Return, breaks=30, freq=TRUE, xlab='%', ylab='Freq', xlim=c(-1,1), main='PUN - var % - ora 15:00')
summary(df_data_wd_15$Return)
boxplot(df_data_wd_15$Return)
acf(na.omit(df_data_wd_15$Return))
acf(na.omit(df_data_wd_15$Return^2))
df_data_wd_15$Return<-replace(df_data_wd_15$Return, is.na(df_data_wd_15$Return), mean(na.omit(df_data_wd_15$Return)))


# Filtering for hours: 16
df_data_wd_16<-subset(df_data_wd, Ora == 16)

# Histogram and statistics
hist(df_data_wd_16$Return, breaks=30, freq=TRUE, xlab='%', ylab='Freq', xlim=c(-1,1), main='PUN - var % - ora 16:00')
summary(df_data_wd_16$Return)
boxplot(df_data_wd_16$Return)
acf(na.omit(df_data_wd_16$Return))
acf(na.omit(df_data_wd_16$Return^2))
df_data_wd_16$Return<-replace(df_data_wd_16$Return, is.na(df_data_wd_16$Return), mean(na.omit(df_data_wd_16$Return)))


# Filtering for hours: 17
df_data_wd_17<-subset(df_data_wd, Ora == 17)

# Histogram and statistics
hist(df_data_wd_17$Return, breaks=30, freq=TRUE, xlab='%', ylab='Freq', xlim=c(-1,1), main='PUN - var % - ora 17:00')
summary(df_data_wd_17$Return)
boxplot(df_data_wd_17$Return)
acf(na.omit(df_data_wd_17$Return))
acf(na.omit(df_data_wd_17$Return^2))
df_data_wd_17$Return<-replace(df_data_wd_17$Return, is.na(df_data_wd_17$Return), mean(na.omit(df_data_wd_17$Return)))


# Filtering for hours: 18
df_data_wd_18<-subset(df_data_wd, Ora == 18)

# Histogram and statistics
hist(df_data_wd_18$Return, breaks=30, freq=TRUE, xlab='%', ylab='Freq', xlim=c(-1,1), main='PUN - var % - ora 18:00')
summary(df_data_wd_18$Return)
boxplot(df_data_wd_18$Return)
acf(na.omit(df_data_wd_18$Return))
acf(na.omit(df_data_wd_18$Return^2))
df_data_wd_18$Return<-replace(df_data_wd_18$Return, is.na(df_data_wd_18$Return), mean(na.omit(df_data_wd_18$Return)))


# Filtering for hours: 19
df_data_wd_19<-subset(df_data_wd, Ora == 19)

# Histogram and statistics
hist(df_data_wd_19$Return, breaks=30, freq=TRUE, xlab='%', ylab='Freq', xlim=c(-1,1), main='PUN - var % - ora 19:00')
summary(df_data_wd_19$Return)
boxplot(df_data_wd_19$Return)
acf(na.omit(df_data_wd_19$Return))
acf(na.omit(df_data_wd_19$Return^2))
df_data_wd_19$Return<-replace(df_data_wd_19$Return, is.na(df_data_wd_19$Return), mean(na.omit(df_data_wd_19$Return)))


# Filtering for hours: 20
df_data_wd_20<-subset(df_data_wd, Ora == 20)

# Histogram and statistics
hist(df_data_wd_20$Return, breaks=30, freq=TRUE, xlab='%', ylab='Freq', xlim=c(-1,1), main='PUN - var % - ora 20:00')
summary(df_data_wd_20$Return)
boxplot(df_data_wd_20$Return)
acf(na.omit(df_data_wd_20$Return))
acf(na.omit(df_data_wd_20$Return^2))
df_data_wd_20$Return<-replace(df_data_wd_20$Return, is.na(df_data_wd_20$Return), mean(na.omit(df_data_wd_20$Return)))


# Filtering for hours: 21
df_data_wd_21<-subset(df_data_wd, Ora == 21)

# Histogram and statistics
hist(df_data_wd_21$Return, breaks=30, freq=TRUE, xlab='%', ylab='Freq', xlim=c(-1,1), main='PUN - var % - ora 21:00')
summary(df_data_wd_21$Return)
boxplot(df_data_wd_21$Return)
acf(na.omit(df_data_wd_21$Return))
acf(na.omit(df_data_wd_21$Return^2))
df_data_wd_21$Return<-replace(df_data_wd_21$Return, is.na(df_data_wd_21$Return), mean(na.omit(df_data_wd_21$Return)))


# Filtering for hours: 22
df_data_wd_22<-subset(df_data_wd, Ora == 22)

# Histogram and statistics
hist(df_data_wd_22$Return, breaks=30, freq=TRUE, xlab='%', ylab='Freq', xlim=c(-1,1), main='PUN - var % - ora 22:00')
summary(df_data_wd_22$Return)
boxplot(df_data_wd_22$Return)
acf(na.omit(df_data_wd_22$Return))
acf(na.omit(df_data_wd_22$Return^2))
df_data_wd_22$Return<-replace(df_data_wd_22$Return, is.na(df_data_wd_22$Return), mean(na.omit(df_data_wd_22$Return)))


# Filtering for hours: 23
df_data_wd_23<-subset(df_data_wd, Ora == 23)

# Histogram and statistics
hist(df_data_wd_23$Return, breaks=30, freq=TRUE, xlab='%', ylab='Freq', xlim=c(-1,1), main='PUN - var % - ora 23:00')
summary(df_data_wd_23$Return)
boxplot(df_data_wd_23$Return)
acf(na.omit(df_data_wd_23$Return))
acf(na.omit(df_data_wd_23$Return^2))
df_data_wd_23$Return<-replace(df_data_wd_23$Return, is.na(df_data_wd_23$Return), mean(na.omit(df_data_wd_23$Return)))


# Filtering for hours: 24
df_data_wd_24<-subset(df_data_wd, Ora == 24)

# Histogram and statistics
hist(df_data_wd_24$Return, breaks=30, freq=TRUE, xlab='%', ylab='Freq', xlim=c(-1,1), main='PUN - var % - ora 24:00')
summary(df_data_wd_24$Return)
boxplot(df_data_wd_24$Return)
acf(na.omit(df_data_wd_24$Return))
acf(na.omit(df_data_wd_24$Return^2))
df_data_wd_24$Return<-replace(df_data_wd_24$Return, is.na(df_data_wd_24$Return), mean(na.omit(df_data_wd_24$Return)))



################################################################################
# 3) Fitting GARCH models and GPD
################################################################################



## 1 ##
xi_roll<-c()

mu_for<-c()
sigma_for<-c()
quant<-c()
quant2<-c()
VaR_evt<-c()
VaR_evt2<-c()
VaR_norm<-c()
ES<-c()
datepred<-c()
n_exc<-c()

solver<-'hybrid'
solvercontrol<-list("solver" = 1)

n<-649
dataset<-df_data_wd_01
last_i<-nrow(dataset)-n

df_backtest<-data.frame(Date = as.POSIXct('1970-01-01'), Loss = numeric(last_i), VaR_evt = numeric(last_i), VaR_evt2 = numeric(last_i), VaR_norm = numeric(last_i), Sigma_f = numeric(last_i))

for (i in 1:last_i)
{
  # defining which model to be fitted
  model=ugarchspec(variance.model=list(model='gjrGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model='norm')
  # fitting the model to the dataset
  modelfit=ugarchfit(spec=model, data=dataset$Return[i:(i+n)]*(-1), solver.control = solvercontrol, solver=solver)
  # forecasting the sd
  foref<-ugarchforecast(modelfit, n.ahead=1)
  # getting the std residuals
  res<-residuals(modelfit, standardize=TRUE)
  # fitting the gpd to the excesses (std residuals-threshold)
  gpd<-gpdFit(as.numeric(res), u=quantile(res, 0.85))
  gpd2<-gpdFit(as.numeric(res)*(-1), u=quantile(res, 0.85))
  
  n_exc[i]<-length(res[res>quantile(res, 0.85)])
  xi_roll[i]<-gpd@fit$par.ests[1]
  mu_for[i]<-foref@forecast$seriesFor[1]
  sigma_for[i]<-foref@forecast$sigmaFor[1]
  
  if(is.na(sigma_for[i])==TRUE)
  {
    sigma_for[i]<-mean(sigma_for[(i-2):(i-1)])
  }
  
  risk<-evt_quant(gpd, 0.01)[[1]]
  quant[i]<-gpdRiskMeasures(gpd, prob=0.99)$quantile
  quant2[i]<-gpdRiskMeasures(gpd2, prob=0.99)$quantile*(-1)
  #quant[i]<-risk[1]
  VaR_evt[i]<-mu_for[i]+quant[i]*sigma_for[i]
  VaR_evt2[i]<-mu_for[i]+quant2[i]*sigma_for[i]
  VaR_norm[i]<-mu_for[i]+qnorm(0.99)*sigma_for[i]
  
  # Filling the dataframe with results
  df_backtest['Date'][i,]<-as.POSIXct(dataset$Data[i+n+1])
  df_backtest['Loss'][i,]<-dataset$Return[i+n+1]*-1
  df_backtest['VaR_evt'][i,]<-VaR_evt[i]
  df_backtest['VaR_norm'][i,]<-VaR_norm[i]
  df_backtest['Sigma_f'][i,]<-sigma_for[i]
  
  print((i/last_i)*100)
}

# Defining the losses out of sample (test set)
loss<-df_backtest$Loss[1:(length(df_backtest$Loss)-1)]

# calculating vaR violations
VaR_evt_viol<-loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]
nv_evt<-sum(VaR_evt_viol)
VaR_norm_viol<-loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]
nv_norm<-sum(VaR_norm_viol)

VaR_evt_viol[VaR_evt_viol==TRUE]<-loss[loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]]
VaR_evt_viol[VaR_evt_viol==0]<-NA

VaR_norm_viol[VaR_norm_viol==TRUE]<-loss[loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]]-0.02
VaR_norm_viol[VaR_norm_viol==0]<-NA



plot(df_backtest$Date[1:(length(df_backtest$Loss)-1)],
     loss,
     xaxt="n",
     type='l', 
     ylim=c(min(loss)-0.1, max(df_backtest$Loss[1:(length(df_backtest$Loss)-1)])+0.1),
     xlab='Dates',
     ylab='Losses/VaR')
axis.Date(1,at=seq(df_backtest$Date[1],df_backtest$Date[(length(df_backtest$Loss)-1)],by="1 month"),format="%m/%Y")
lines(df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)], col='red', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
lines(df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)], col='green', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_evt_viol, lwd=4, col='red')
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_norm_viol, lwd=4, col='green')
title(main='Prezzo Unico Nazionale: 1:00 AM')
legend.txt<-c(paste("Violations gjrGARCH + POT(EVT): [",nv_evt,"]"),paste("Violations gjrGARCH + NORM: [",nv_norm,"]"))
legend('topright',legend.txt,lty=c(NA,NA),col=c("red","green"),lwd=c(3,3),bg="white",cex=0.75,bty="n",x.intersp=0,y.intersp=1.5,pch=c(21,21),merge=T)


VaRevt_001<-VaR_evt
VaRnorm_001<-VaR_norm








## 2 ##
xi_roll<-c()

mu_for<-c()
sigma_for<-c()
quant<-c()
VaR_evt<-c()
VaR_norm<-c()
ES<-c()
datepred<-c()
n_exc<-c()

solver<-'hybrid'
solvercontrol<-list("solver" = 1)

n<-649
dataset<-df_data_wd_02
last_i<-nrow(dataset)-n

df_backtest<-data.frame(Date = as.POSIXct('1970-01-01'), Loss = numeric(last_i), VaR_evt = numeric(last_i), VaR_norm = numeric(last_i), Sigma_f = numeric(last_i))

for (i in 1:last_i)
{
  # defining which model to be fitted
  model=ugarchspec(variance.model=list(model='gjrGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model='norm')
  # fitting the model to the dataset
  modelfit=ugarchfit(spec=model, data=dataset$Return[i:(i+n)]*(-1), solver.control = solvercontrol, solver=solver)
  # forecasting the sd
  foref<-ugarchforecast(modelfit, n.ahead=1)
  # getting the std residuals
  res<-residuals(modelfit, standardize=TRUE)
  # fitting the gpd to the excesses (std residuals-threshold)
  gpd<-gpdFit(as.numeric(res), u=quantile(res, 0.85))
  
  n_exc[i]<-length(res[res>quantile(res, 0.85)])
  xi_roll[i]<-gpd@fit$par.ests[1]
  mu_for[i]<-foref@forecast$seriesFor[1]
  sigma_for[i]<-foref@forecast$sigmaFor[1]
  
  if(is.na(sigma_for[i])==TRUE)
  {
    sigma_for[i]<-mean(sigma_for[(i-2):(i-1)])
  }
  
  risk<-evt_quant(gpd, 0.01)[[1]]
  quant[i]<-gpdRiskMeasures(gpd, prob=0.99)$quantile
  #quant[i]<-risk[1]
  VaR_evt[i]<-mu_for[i]+quant[i]*sigma_for[i]
  VaR_norm[i]<-mu_for[i]+qnorm(0.99)*sigma_for[i]
  
  # Filling the dataframe with results
  df_backtest['Date'][i,]<-as.POSIXct(dataset$Data[i+n+1])
  df_backtest['Loss'][i,]<-dataset$Return[i+n+1]*-1
  df_backtest['VaR_evt'][i,]<-VaR_evt[i]
  df_backtest['VaR_norm'][i,]<-VaR_norm[i]
  df_backtest['Sigma_f'][i,]<-sigma_for[i]
  
  print((i/last_i)*100)
}

# Defining the losses out of sample (test set)
loss<-df_backtest$Loss[1:(length(df_backtest$Loss)-1)]

# calculating vaR violations
VaR_evt_viol<-loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]
nv_evt<-sum(VaR_evt_viol)
VaR_norm_viol<-loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]
nv_norm<-sum(VaR_norm_viol)

VaR_evt_viol[VaR_evt_viol==TRUE]<-loss[loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]]
VaR_evt_viol[VaR_evt_viol==0]<-NA

VaR_norm_viol[VaR_norm_viol==TRUE]<-loss[loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]]-0.02
VaR_norm_viol[VaR_norm_viol==0]<-NA



plot(df_backtest$Date[1:(length(df_backtest$Loss)-1)],
     loss,
     xaxt="n",
     type='l', 
     ylim=c(min(loss)-0.1, max(df_backtest$Loss[1:(length(df_backtest$Loss)-1)])+0.1),
     xlab='Dates',
     ylab='Losses/VaR')
axis.Date(1,at=seq(df_backtest$Date[1],df_backtest$Date[(length(df_backtest$Loss)-1)],by="1 month"),format="%m/%Y")
lines(df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)], col='red', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
lines(df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)], col='green', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_evt_viol, lwd=4, col='red')
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_norm_viol, lwd=4, col='green')
title(main='Prezzo Unico Nazionale: 2:00 AM')
legend.txt<-c(paste("Violations gjrGARCH + POT(EVT): [",nv_evt,"]"),paste("Violations gjrGARCH + NORM: [",nv_norm,"]"))
legend('topright',legend.txt,lty=c(NA,NA),col=c("red","green"),lwd=c(3,3),bg="white",cex=0.75,bty="n",x.intersp=0,y.intersp=1.5,pch=c(21,21),merge=T)


VaRevt_002<-VaR_evt
VaRnorm_002<-VaR_norm










## 3 ##
xi_roll<-c()

mu_for<-c()
sigma_for<-c()
quant<-c()
VaR_evt<-c()
VaR_norm<-c()
ES<-c()
datepred<-c()
n_exc<-c()

solver<-'hybrid'
solvercontrol<-list("solver" = 1)

n<-649
dataset<-df_data_wd_03
last_i<-nrow(dataset)-n

df_backtest<-data.frame(Date = as.POSIXct('1970-01-01'), Loss = numeric(last_i), VaR_evt = numeric(last_i), VaR_norm = numeric(last_i), Sigma_f = numeric(last_i))

for (i in 1:last_i)
{
  # defining which model to be fitted
  model=ugarchspec(variance.model=list(model='gjrGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model='norm')
  # fitting the model to the dataset
  modelfit=ugarchfit(spec=model, data=dataset$Return[i:(i+n)]*(-1), solver.control = solvercontrol, solver=solver)
  # forecasting the sd
  foref<-ugarchforecast(modelfit, n.ahead=1)
  # getting the std residuals
  res<-residuals(modelfit, standardize=TRUE)
  # fitting the gpd to the excesses (std residuals-threshold)
  gpd<-gpdFit(as.numeric(res), u=quantile(res, 0.85))
  
  n_exc[i]<-length(res[res>quantile(res, 0.85)])
  xi_roll[i]<-gpd@fit$par.ests[1]
  mu_for[i]<-foref@forecast$seriesFor[1]
  sigma_for[i]<-foref@forecast$sigmaFor[1]
  
  if(is.na(sigma_for[i])==TRUE)
  {
    sigma_for[i]<-mean(sigma_for[(i-2):(i-1)])
  }
  
  risk<-evt_quant(gpd, 0.01)[[1]]
  quant[i]<-gpdRiskMeasures(gpd, prob=0.99)$quantile
  #quant[i]<-risk[1]
  VaR_evt[i]<-mu_for[i]+quant[i]*sigma_for[i]
  VaR_norm[i]<-mu_for[i]+qnorm(0.99)*sigma_for[i]
  
  # Filling the dataframe with results
  df_backtest['Date'][i,]<-as.POSIXct(dataset$Data[i+n+1])
  df_backtest['Loss'][i,]<-dataset$Return[i+n+1]*-1
  df_backtest['VaR_evt'][i,]<-VaR_evt[i]
  df_backtest['VaR_norm'][i,]<-VaR_norm[i]
  df_backtest['Sigma_f'][i,]<-sigma_for[i]
  
  print((i/last_i)*100)
}

# Defining the losses out of sample (test set)
loss<-df_backtest$Loss[1:(length(df_backtest$Loss)-1)]

# calculating vaR violations
VaR_evt_viol<-loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]
nv_evt<-sum(VaR_evt_viol)
VaR_norm_viol<-loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]
nv_norm<-sum(VaR_norm_viol)

VaR_evt_viol[VaR_evt_viol==TRUE]<-loss[loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]]
VaR_evt_viol[VaR_evt_viol==0]<-NA

VaR_norm_viol[VaR_norm_viol==TRUE]<-loss[loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]]-0.02
VaR_norm_viol[VaR_norm_viol==0]<-NA



plot(df_backtest$Date[1:(length(df_backtest$Loss)-1)],
     loss,
     xaxt="n",
     type='l', 
     ylim=c(min(loss)-0.1, max(df_backtest$Loss[1:(length(df_backtest$Loss)-1)])+0.1),
     xlab='Dates',
     ylab='Losses/VaR')
axis.Date(1,at=seq(df_backtest$Date[1],df_backtest$Date[(length(df_backtest$Loss)-1)],by="1 month"),format="%m/%Y")
lines(df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)], col='red', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
lines(df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)], col='green', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_evt_viol, lwd=4, col='red')
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_norm_viol, lwd=4, col='green')
title(main='Prezzo Unico Nazionale: 3:00 AM')
legend.txt<-c(paste("Violations gjrGARCH + POT(EVT): [",nv_evt,"]"),paste("Violations gjrGARCH + NORM: [",nv_norm,"]"))
legend('topright',legend.txt,lty=c(NA,NA),col=c("red","green"),lwd=c(3,3),bg="white",cex=0.75,bty="n",x.intersp=0,y.intersp=1.5,pch=c(21,21),merge=T)


VaRevt_003<-VaR_evt
VaRnorm_003<-VaR_norm










## 4 ##
xi_roll<-c()

mu_for<-c()
sigma_for<-c()
quant<-c()
VaR_evt<-c()
VaR_norm<-c()
ES<-c()
datepred<-c()
n_exc<-c()

solver<-'hybrid'
solvercontrol<-list("solver" = 1)

n<-649
dataset<-df_data_wd_04
last_i<-nrow(dataset)-n

df_backtest<-data.frame(Date = as.POSIXct('1970-01-01'), Loss = numeric(last_i), VaR_evt = numeric(last_i), VaR_norm = numeric(last_i), Sigma_f = numeric(last_i))

for (i in 1:last_i)
{
  # defining which model to be fitted
  model=ugarchspec(variance.model=list(model='gjrGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model='norm')
  # fitting the model to the dataset
  modelfit=ugarchfit(spec=model, data=dataset$Return[i:(i+n)]*(-1), solver.control = solvercontrol, solver=solver)
  # forecasting the sd
  foref<-ugarchforecast(modelfit, n.ahead=1)
  # getting the std residuals
  res<-residuals(modelfit, standardize=TRUE)
  # fitting the gpd to the excesses (std residuals-threshold)
  gpd<-gpdFit(as.numeric(res), u=quantile(res, 0.85))
  
  n_exc[i]<-length(res[res>quantile(res, 0.85)])
  xi_roll[i]<-gpd@fit$par.ests[1]
  mu_for[i]<-foref@forecast$seriesFor[1]
  sigma_for[i]<-foref@forecast$sigmaFor[1]
  
  if(is.na(sigma_for[i])==TRUE)
  {
    sigma_for[i]<-mean(sigma_for[(i-2):(i-1)])
  }
  
  risk<-evt_quant(gpd, 0.01)[[1]]
  quant[i]<-gpdRiskMeasures(gpd, prob=0.99)$quantile
  #quant[i]<-risk[1]
  VaR_evt[i]<-mu_for[i]+quant[i]*sigma_for[i]
  VaR_norm[i]<-mu_for[i]+qnorm(0.99)*sigma_for[i]
  
  # Filling the dataframe with results
  df_backtest['Date'][i,]<-as.POSIXct(dataset$Data[i+n+1])
  df_backtest['Loss'][i,]<-dataset$Return[i+n+1]*-1
  df_backtest['VaR_evt'][i,]<-VaR_evt[i]
  df_backtest['VaR_norm'][i,]<-VaR_norm[i]
  df_backtest['Sigma_f'][i,]<-sigma_for[i]
  
  print((i/last_i)*100)
}

# Defining the losses out of sample (test set)
loss<-df_backtest$Loss[1:(length(df_backtest$Loss)-1)]

# calculating vaR violations
VaR_evt_viol<-loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]
nv_evt<-sum(VaR_evt_viol)
VaR_norm_viol<-loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]
nv_norm<-sum(VaR_norm_viol)

VaR_evt_viol[VaR_evt_viol==TRUE]<-loss[loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]]
VaR_evt_viol[VaR_evt_viol==0]<-NA

VaR_norm_viol[VaR_norm_viol==TRUE]<-loss[loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]]-0.02
VaR_norm_viol[VaR_norm_viol==0]<-NA



plot(df_backtest$Date[1:(length(df_backtest$Loss)-1)],
     loss,
     xaxt="n",
     type='l', 
     ylim=c(min(loss)-0.1, max(df_backtest$Loss[1:(length(df_backtest$Loss)-1)])+0.1),
     xlab='Dates',
     ylab='Losses/VaR')
axis.Date(1,at=seq(df_backtest$Date[1],df_backtest$Date[(length(df_backtest$Loss)-1)],by="1 month"),format="%m/%Y")
lines(df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)], col='red', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
lines(df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)], col='green', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_evt_viol, lwd=4, col='red')
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_norm_viol, lwd=4, col='green')
title(main='Prezzo Unico Nazionale: 3:00 AM')
legend.txt<-c(paste("Violations gjrGARCH + POT(EVT): [",nv_evt,"]"),paste("Violations gjrGARCH + NORM: [",nv_norm,"]"))
legend('topright',legend.txt,lty=c(NA,NA),col=c("red","green"),lwd=c(3,3),bg="white",cex=0.75,bty="n",x.intersp=0,y.intersp=1.5,pch=c(21,21),merge=T)


VaRevt_004<-VaR_evt
VaRnorm_004<-VaR_norm










## 5 ##
xi_roll<-c()

mu_for<-c()
sigma_for<-c()
quant<-c()
VaR_evt<-c()
VaR_norm<-c()
ES<-c()
datepred<-c()
n_exc<-c()

solver<-'hybrid'
solvercontrol<-list("solver" = 1)

n<-649
dataset<-df_data_wd_05
last_i<-nrow(dataset)-n

df_backtest<-data.frame(Date = as.POSIXct('1970-01-01'), Loss = numeric(last_i), VaR_evt = numeric(last_i), VaR_norm = numeric(last_i), Sigma_f = numeric(last_i))

for (i in 1:last_i)
{
  # defining which model to be fitted
  model=ugarchspec(variance.model=list(model='gjrGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model='norm')
  # fitting the model to the dataset
  modelfit=ugarchfit(spec=model, data=dataset$Return[i:(i+n)]*(-1), solver.control = solvercontrol, solver=solver)
  # forecasting the sd
  foref<-ugarchforecast(modelfit, n.ahead=1)
  # getting the std residuals
  res<-residuals(modelfit, standardize=TRUE)
  # fitting the gpd to the excesses (std residuals-threshold)
  gpd<-gpdFit(as.numeric(res), u=quantile(res, 0.85))
  
  n_exc[i]<-length(res[res>quantile(res, 0.85)])
  xi_roll[i]<-gpd@fit$par.ests[1]
  mu_for[i]<-foref@forecast$seriesFor[1]
  sigma_for[i]<-foref@forecast$sigmaFor[1]
  
  if(is.na(sigma_for[i])==TRUE)
  {
    sigma_for[i]<-mean(sigma_for[(i-2):(i-1)])
  }
  
  risk<-evt_quant(gpd, 0.01)[[1]]
  quant[i]<-gpdRiskMeasures(gpd, prob=0.99)$quantile
  #quant[i]<-risk[1]
  VaR_evt[i]<-mu_for[i]+quant[i]*sigma_for[i]
  VaR_norm[i]<-mu_for[i]+qnorm(0.99)*sigma_for[i]
  
  # Filling the dataframe with results
  df_backtest['Date'][i,]<-as.POSIXct(dataset$Data[i+n+1])
  df_backtest['Loss'][i,]<-dataset$Return[i+n+1]*-1
  df_backtest['VaR_evt'][i,]<-VaR_evt[i]
  df_backtest['VaR_norm'][i,]<-VaR_norm[i]
  df_backtest['Sigma_f'][i,]<-sigma_for[i]
  
  print((i/last_i)*100)
}

# Defining the losses out of sample (test set)
loss<-df_backtest$Loss[1:(length(df_backtest$Loss)-1)]

# calculating vaR violations
VaR_evt_viol<-loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]
nv_evt<-sum(VaR_evt_viol)
VaR_norm_viol<-loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]
nv_norm<-sum(VaR_norm_viol)

VaR_evt_viol[VaR_evt_viol==TRUE]<-loss[loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]]
VaR_evt_viol[VaR_evt_viol==0]<-NA

VaR_norm_viol[VaR_norm_viol==TRUE]<-loss[loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]]-0.02
VaR_norm_viol[VaR_norm_viol==0]<-NA



plot(df_backtest$Date[1:(length(df_backtest$Loss)-1)],
     loss,
     xaxt="n",
     type='l', 
     ylim=c(min(loss)-0.1, max(df_backtest$Loss[1:(length(df_backtest$Loss)-1)])+0.1),
     xlab='Dates',
     ylab='Losses/VaR')
axis.Date(1,at=seq(df_backtest$Date[1],df_backtest$Date[(length(df_backtest$Loss)-1)],by="1 month"),format="%m/%Y")
lines(df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)], col='red', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
lines(df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)], col='green', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_evt_viol, lwd=4, col='red')
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_norm_viol, lwd=4, col='green')
title(main='Prezzo Unico Nazionale: 5:00 AM')
legend.txt<-c(paste("Violations gjrGARCH + POT(EVT): [",nv_evt,"]"),paste("Violations gjrGARCH + NORM: [",nv_norm,"]"))
legend('topright',legend.txt,lty=c(NA,NA),col=c("red","green"),lwd=c(3,3),bg="white",cex=0.75,bty="n",x.intersp=0,y.intersp=1.5,pch=c(21,21),merge=T)


VaRevt_005<-VaR_evt
VaRnorm_005<-VaR_norm










## 6 ##
xi_roll<-c()

mu_for<-c()
sigma_for<-c()
quant<-c()
VaR_evt<-c()
VaR_norm<-c()
ES<-c()
datepred<-c()
n_exc<-c()

solver<-'hybrid'
solvercontrol<-list("solver" = 1)

n<-649
dataset<-df_data_wd_06
last_i<-nrow(dataset)-n

df_backtest<-data.frame(Date = as.POSIXct('1970-01-01'), Loss = numeric(last_i), VaR_evt = numeric(last_i), VaR_norm = numeric(last_i), Sigma_f = numeric(last_i))

for (i in 1:last_i)
{
  # defining which model to be fitted
  model=ugarchspec(variance.model=list(model='gjrGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model='norm')
  # fitting the model to the dataset
  modelfit=ugarchfit(spec=model, data=dataset$Return[i:(i+n)]*(-1), solver.control = solvercontrol, solver=solver)
  # forecasting the sd
  foref<-ugarchforecast(modelfit, n.ahead=1)
  # getting the std residuals
  res<-residuals(modelfit, standardize=TRUE)
  # fitting the gpd to the excesses (std residuals-threshold)
  gpd<-gpdFit(as.numeric(res), u=quantile(res, 0.85))
  
  n_exc[i]<-length(res[res>quantile(res, 0.85)])
  xi_roll[i]<-gpd@fit$par.ests[1]
  mu_for[i]<-foref@forecast$seriesFor[1]
  sigma_for[i]<-foref@forecast$sigmaFor[1]
  
  if(is.na(sigma_for[i])==TRUE)
  {
    sigma_for[i]<-mean(sigma_for[(i-2):(i-1)])
  }
  
  risk<-evt_quant(gpd, 0.01)[[1]]
  quant[i]<-gpdRiskMeasures(gpd, prob=0.99)$quantile
  #quant[i]<-risk[1]
  VaR_evt[i]<-mu_for[i]+quant[i]*sigma_for[i]
  VaR_norm[i]<-mu_for[i]+qnorm(0.99)*sigma_for[i]
  
  # Filling the dataframe with results
  df_backtest['Date'][i,]<-as.POSIXct(dataset$Data[i+n+1])
  df_backtest['Loss'][i,]<-dataset$Return[i+n+1]*-1
  df_backtest['VaR_evt'][i,]<-VaR_evt[i]
  df_backtest['VaR_norm'][i,]<-VaR_norm[i]
  df_backtest['Sigma_f'][i,]<-sigma_for[i]
  
  print((i/last_i)*100)
}

# Defining the losses out of sample (test set)
loss<-df_backtest$Loss[1:(length(df_backtest$Loss)-1)]

# calculating vaR violations
VaR_evt_viol<-loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]
nv_evt<-sum(VaR_evt_viol)
VaR_norm_viol<-loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]
nv_norm<-sum(VaR_norm_viol)

VaR_evt_viol[VaR_evt_viol==TRUE]<-loss[loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]]
VaR_evt_viol[VaR_evt_viol==0]<-NA

VaR_norm_viol[VaR_norm_viol==TRUE]<-loss[loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]]-0.02
VaR_norm_viol[VaR_norm_viol==0]<-NA



plot(df_backtest$Date[1:(length(df_backtest$Loss)-1)],
     loss,
     xaxt="n",
     type='l', 
     ylim=c(min(loss)-0.1, max(df_backtest$Loss[1:(length(df_backtest$Loss)-1)])+0.1),
     xlab='Dates',
     ylab='Losses/VaR')
axis.Date(1,at=seq(df_backtest$Date[1],df_backtest$Date[(length(df_backtest$Loss)-1)],by="1 month"),format="%m/%Y")
lines(df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)], col='red', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
lines(df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)], col='green', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_evt_viol, lwd=4, col='red')
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_norm_viol, lwd=4, col='green')
title(main='Prezzo Unico Nazionale: 6:00 AM')
legend.txt<-c(paste("Violations gjrGARCH + POT(EVT): [",nv_evt,"]"),paste("Violations gjrGARCH + NORM: [",nv_norm,"]"))
legend('topright',legend.txt,lty=c(NA,NA),col=c("red","green"),lwd=c(3,3),bg="white",cex=0.75,bty="n",x.intersp=0,y.intersp=1.5,pch=c(21,21),merge=T)


VaRevt_006<-VaR_evt
VaRnorm_006<-VaR_norm










## 7 ##
xi_roll<-c()

mu_for<-c()
sigma_for<-c()
quant<-c()
VaR_evt<-c()
VaR_norm<-c()
ES<-c()
datepred<-c()
n_exc<-c()

solver<-'hybrid'
solvercontrol<-list("solver" = 1)

n<-649
dataset<-df_data_wd_07
last_i<-nrow(dataset)-n

df_backtest<-data.frame(Date = as.POSIXct('1970-01-01'), Loss = numeric(last_i), VaR_evt = numeric(last_i), VaR_norm = numeric(last_i), Sigma_f = numeric(last_i))

for (i in 1:last_i)
{
  # defining which model to be fitted
  model=ugarchspec(variance.model=list(model='gjrGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model='norm')
  # fitting the model to the dataset
  modelfit=ugarchfit(spec=model, data=dataset$Return[i:(i+n)]*(-1), solver.control = solvercontrol, solver=solver)
  # forecasting the sd
  foref<-ugarchforecast(modelfit, n.ahead=1)
  # getting the std residuals
  res<-residuals(modelfit, standardize=TRUE)
  # fitting the gpd to the excesses (std residuals-threshold)
  gpd<-gpdFit(as.numeric(res), u=quantile(res, 0.85))
  
  n_exc[i]<-length(res[res>quantile(res, 0.85)])
  xi_roll[i]<-gpd@fit$par.ests[1]
  mu_for[i]<-foref@forecast$seriesFor[1]
  sigma_for[i]<-foref@forecast$sigmaFor[1]
  
  if(is.na(sigma_for[i])==TRUE)
  {
    sigma_for[i]<-mean(sigma_for[(i-2):(i-1)])
  }
  
  risk<-evt_quant(gpd, 0.01)[[1]]
  quant[i]<-gpdRiskMeasures(gpd, prob=0.99)$quantile
  #quant[i]<-risk[1]
  VaR_evt[i]<-mu_for[i]+quant[i]*sigma_for[i]
  VaR_norm[i]<-mu_for[i]+qnorm(0.99)*sigma_for[i]
  
  # Filling the dataframe with results
  df_backtest['Date'][i,]<-as.POSIXct(dataset$Data[i+n+1])
  df_backtest['Loss'][i,]<-dataset$Return[i+n+1]*-1
  df_backtest['VaR_evt'][i,]<-VaR_evt[i]
  df_backtest['VaR_norm'][i,]<-VaR_norm[i]
  df_backtest['Sigma_f'][i,]<-sigma_for[i]
  
  print((i/last_i)*100)
}

# Defining the losses out of sample (test set)
loss<-df_backtest$Loss[1:(length(df_backtest$Loss)-1)]

# calculating vaR violations
VaR_evt_viol<-loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]
nv_evt<-sum(VaR_evt_viol)
VaR_norm_viol<-loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]
nv_norm<-sum(VaR_norm_viol)

VaR_evt_viol[VaR_evt_viol==TRUE]<-loss[loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]]
VaR_evt_viol[VaR_evt_viol==0]<-NA

VaR_norm_viol[VaR_norm_viol==TRUE]<-loss[loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]]-0.02
VaR_norm_viol[VaR_norm_viol==0]<-NA



plot(df_backtest$Date[1:(length(df_backtest$Loss)-1)],
     loss,
     xaxt="n",
     type='l', 
     ylim=c(min(loss)-0.1, max(df_backtest$Loss[1:(length(df_backtest$Loss)-1)])+0.1),
     xlab='Dates',
     ylab='Losses/VaR')
axis.Date(1,at=seq(df_backtest$Date[1],df_backtest$Date[(length(df_backtest$Loss)-1)],by="1 month"),format="%m/%Y")
lines(df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)], col='red', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
lines(df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)], col='green', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_evt_viol, lwd=4, col='red')
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_norm_viol, lwd=4, col='green')
title(main='Prezzo Unico Nazionale: 7:00 AM')
legend.txt<-c(paste("Violations gjrGARCH + POT(EVT): [",nv_evt,"]"),paste("Violations gjrGARCH + NORM: [",nv_norm,"]"))
legend('topright',legend.txt,lty=c(NA,NA),col=c("red","green"),lwd=c(3,3),bg="white",cex=0.75,bty="n",x.intersp=0,y.intersp=1.5,pch=c(21,21),merge=T)


VaRevt_007<-VaR_evt
VaRnorm_007<-VaR_norm










## 8 ##
xi_roll<-c()

mu_for<-c()
sigma_for<-c()
quant<-c()
VaR_evt<-c()
VaR_norm<-c()
ES<-c()
datepred<-c()
n_exc<-c()

solver<-'hybrid'
solvercontrol<-list("solver" = 1)

n<-649
dataset<-df_data_wd_08
last_i<-nrow(dataset)-n

df_backtest<-data.frame(Date = as.POSIXct('1970-01-01'), Loss = numeric(last_i), VaR_evt = numeric(last_i), VaR_norm = numeric(last_i), Sigma_f = numeric(last_i))

for (i in 1:last_i)
{
  # defining which model to be fitted
  model=ugarchspec(variance.model=list(model='gjrGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model='norm')
  # fitting the model to the dataset
  modelfit=ugarchfit(spec=model, data=dataset$Return[i:(i+n)]*(-1), solver.control = solvercontrol, solver=solver)
  # forecasting the sd
  foref<-ugarchforecast(modelfit, n.ahead=1)
  # getting the std residuals
  res<-residuals(modelfit, standardize=TRUE)
  # fitting the gpd to the excesses (std residuals-threshold)
  gpd<-gpdFit(as.numeric(res), u=quantile(res, 0.85))
  
  n_exc[i]<-length(res[res>quantile(res, 0.85)])
  xi_roll[i]<-gpd@fit$par.ests[1]
  mu_for[i]<-foref@forecast$seriesFor[1]
  sigma_for[i]<-foref@forecast$sigmaFor[1]
  
  if(is.na(sigma_for[i])==TRUE)
  {
    sigma_for[i]<-mean(sigma_for[(i-2):(i-1)])
  }
  
  risk<-evt_quant(gpd, 0.01)[[1]]
  quant[i]<-gpdRiskMeasures(gpd, prob=0.99)$quantile
  #quant[i]<-risk[1]
  VaR_evt[i]<-mu_for[i]+quant[i]*sigma_for[i]
  VaR_norm[i]<-mu_for[i]+qnorm(0.99)*sigma_for[i]
  
  # Filling the dataframe with results
  df_backtest['Date'][i,]<-as.POSIXct(dataset$Data[i+n+1])
  df_backtest['Loss'][i,]<-dataset$Return[i+n+1]*-1
  df_backtest['VaR_evt'][i,]<-VaR_evt[i]
  df_backtest['VaR_norm'][i,]<-VaR_norm[i]
  df_backtest['Sigma_f'][i,]<-sigma_for[i]
  
  print((i/last_i)*100)
}

# Defining the losses out of sample (test set)
loss<-df_backtest$Loss[1:(length(df_backtest$Loss)-1)]

# calculating vaR violations
VaR_evt_viol<-loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]
nv_evt<-sum(VaR_evt_viol)
VaR_norm_viol<-loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]
nv_norm<-sum(VaR_norm_viol)

VaR_evt_viol[VaR_evt_viol==TRUE]<-loss[loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]]
VaR_evt_viol[VaR_evt_viol==0]<-NA

VaR_norm_viol[VaR_norm_viol==TRUE]<-loss[loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]]-0.02
VaR_norm_viol[VaR_norm_viol==0]<-NA



plot(df_backtest$Date[1:(length(df_backtest$Loss)-1)],
     loss,
     xaxt="n",
     type='l', 
     ylim=c(min(loss)-0.1, max(df_backtest$Loss[1:(length(df_backtest$Loss)-1)])+0.1),
     xlab='Dates',
     ylab='Losses/VaR')
axis.Date(1,at=seq(df_backtest$Date[1],df_backtest$Date[(length(df_backtest$Loss)-1)],by="1 month"),format="%m/%Y")
lines(df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)], col='red', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
lines(df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)], col='green', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_evt_viol, lwd=4, col='red')
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_norm_viol, lwd=4, col='green')
title(main='Prezzo Unico Nazionale: 8:00 AM')
legend.txt<-c(paste("Violations gjrGARCH + POT(EVT): [",nv_evt,"]"),paste("Violations gjrGARCH + NORM: [",nv_norm,"]"))
legend('topright',legend.txt,lty=c(NA,NA),col=c("red","green"),lwd=c(3,3),bg="white",cex=0.75,bty="n",x.intersp=0,y.intersp=1.5,pch=c(21,21),merge=T)


VaRevt_008<-VaR_evt
VaRnorm_008<-VaR_norm










## 9 ##
xi_roll<-c()

mu_for<-c()
sigma_for<-c()
quant<-c()
VaR_evt<-c()
VaR_norm<-c()
ES<-c()
datepred<-c()
n_exc<-c()

solver<-'hybrid'
solvercontrol<-list("solver" = 1)

n<-649
dataset<-df_data_wd_09
last_i<-nrow(dataset)-n

df_backtest<-data.frame(Date = as.POSIXct('1970-01-01'), Loss = numeric(last_i), VaR_evt = numeric(last_i), VaR_norm = numeric(last_i), Sigma_f = numeric(last_i))

for (i in 1:last_i)
{
  # defining which model to be fitted
  model=ugarchspec(variance.model=list(model='gjrGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model='norm')
  # fitting the model to the dataset
  modelfit=ugarchfit(spec=model, data=dataset$Return[i:(i+n)]*(-1), solver.control = solvercontrol, solver=solver)
  # forecasting the sd
  foref<-ugarchforecast(modelfit, n.ahead=1)
  # getting the std residuals
  res<-residuals(modelfit, standardize=TRUE)
  # fitting the gpd to the excesses (std residuals-threshold)
  gpd<-gpdFit(as.numeric(res), u=quantile(res, 0.85))
  
  n_exc[i]<-length(res[res>quantile(res, 0.85)])
  xi_roll[i]<-gpd@fit$par.ests[1]
  mu_for[i]<-foref@forecast$seriesFor[1]
  sigma_for[i]<-foref@forecast$sigmaFor[1]
  
  if(is.na(sigma_for[i])==TRUE)
  {
    sigma_for[i]<-mean(sigma_for[(i-2):(i-1)])
  }
  
  risk<-evt_quant(gpd, 0.01)[[1]]
  quant[i]<-gpdRiskMeasures(gpd, prob=0.99)$quantile
  #quant[i]<-risk[1]
  VaR_evt[i]<-mu_for[i]+quant[i]*sigma_for[i]
  VaR_norm[i]<-mu_for[i]+qnorm(0.99)*sigma_for[i]
  
  # Filling the dataframe with results
  df_backtest['Date'][i,]<-as.POSIXct(dataset$Data[i+n+1])
  df_backtest['Loss'][i,]<-dataset$Return[i+n+1]*-1
  df_backtest['VaR_evt'][i,]<-VaR_evt[i]
  df_backtest['VaR_norm'][i,]<-VaR_norm[i]
  df_backtest['Sigma_f'][i,]<-sigma_for[i]
  
  print((i/last_i)*100)
}

# Defining the losses out of sample (test set)
loss<-df_backtest$Loss[1:(length(df_backtest$Loss)-1)]

# calculating vaR violations
VaR_evt_viol<-loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]
nv_evt<-sum(VaR_evt_viol)
VaR_norm_viol<-loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]
nv_norm<-sum(VaR_norm_viol)

VaR_evt_viol[VaR_evt_viol==TRUE]<-loss[loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]]
VaR_evt_viol[VaR_evt_viol==0]<-NA

VaR_norm_viol[VaR_norm_viol==TRUE]<-loss[loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]]-0.02
VaR_norm_viol[VaR_norm_viol==0]<-NA



plot(df_backtest$Date[1:(length(df_backtest$Loss)-1)],
     loss,
     xaxt="n",
     type='l', 
     ylim=c(min(loss)-0.1, max(df_backtest$Loss[1:(length(df_backtest$Loss)-1)])+0.1),
     xlab='Dates',
     ylab='Losses/VaR')
axis.Date(1,at=seq(df_backtest$Date[1],df_backtest$Date[(length(df_backtest$Loss)-1)],by="1 month"),format="%m/%Y")
lines(df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)], col='red', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
lines(df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)], col='green', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_evt_viol, lwd=4, col='red')
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_norm_viol, lwd=4, col='green')
title(main='Prezzo Unico Nazionale: 9:00 AM')
legend.txt<-c(paste("Violations gjrGARCH + POT(EVT): [",nv_evt,"]"),paste("Violations gjrGARCH + NORM: [",nv_norm,"]"))
legend('topright',legend.txt,lty=c(NA,NA),col=c("red","green"),lwd=c(3,3),bg="white",cex=0.75,bty="n",x.intersp=0,y.intersp=1.5,pch=c(21,21),merge=T)


VaRevt_009<-VaR_evt
VaRnorm_009<-VaR_norm










## 10 ##
xi_roll<-c()

mu_for<-c()
sigma_for<-c()
quant<-c()
VaR_evt<-c()
VaR_norm<-c()
ES<-c()
datepred<-c()
n_exc<-c()

solver<-'hybrid'
solvercontrol<-list("solver" = 1)

n<-649
dataset<-df_data_wd_10
last_i<-nrow(dataset)-n

df_backtest<-data.frame(Date = as.POSIXct('1970-01-01'), Loss = numeric(last_i), VaR_evt = numeric(last_i), VaR_norm = numeric(last_i), Sigma_f = numeric(last_i))

for (i in 1:last_i)
{
  # defining which model to be fitted
  model=ugarchspec(variance.model=list(model='gjrGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model='norm')
  # fitting the model to the dataset
  modelfit=ugarchfit(spec=model, data=dataset$Return[i:(i+n)]*(-1), solver.control = solvercontrol, solver=solver)
  # forecasting the sd
  foref<-ugarchforecast(modelfit, n.ahead=1)
  # getting the std residuals
  res<-residuals(modelfit, standardize=TRUE)
  # fitting the gpd to the excesses (std residuals-threshold)
  gpd<-gpdFit(as.numeric(res), u=quantile(res, 0.85))
  
  n_exc[i]<-length(res[res>quantile(res, 0.85)])
  xi_roll[i]<-gpd@fit$par.ests[1]
  mu_for[i]<-foref@forecast$seriesFor[1]
  sigma_for[i]<-foref@forecast$sigmaFor[1]
  
  if(is.na(sigma_for[i])==TRUE)
  {
    sigma_for[i]<-mean(sigma_for[(i-2):(i-1)])
  }
  
  risk<-evt_quant(gpd, 0.01)[[1]]
  quant[i]<-gpdRiskMeasures(gpd, prob=0.99)$quantile
  #quant[i]<-risk[1]
  VaR_evt[i]<-mu_for[i]+quant[i]*sigma_for[i]
  VaR_norm[i]<-mu_for[i]+qnorm(0.99)*sigma_for[i]
  
  # Filling the dataframe with results
  df_backtest['Date'][i,]<-as.POSIXct(dataset$Data[i+n+1])
  df_backtest['Loss'][i,]<-dataset$Return[i+n+1]*-1
  df_backtest['VaR_evt'][i,]<-VaR_evt[i]
  df_backtest['VaR_norm'][i,]<-VaR_norm[i]
  df_backtest['Sigma_f'][i,]<-sigma_for[i]
  
  print((i/last_i)*100)
}

# Defining the losses out of sample (test set)
loss<-df_backtest$Loss[1:(length(df_backtest$Loss)-1)]

# calculating vaR violations
VaR_evt_viol<-loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]
nv_evt<-sum(VaR_evt_viol)
VaR_norm_viol<-loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]
nv_norm<-sum(VaR_norm_viol)

VaR_evt_viol[VaR_evt_viol==TRUE]<-loss[loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]]
VaR_evt_viol[VaR_evt_viol==0]<-NA

VaR_norm_viol[VaR_norm_viol==TRUE]<-loss[loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]]-0.02
VaR_norm_viol[VaR_norm_viol==0]<-NA



plot(df_backtest$Date[1:(length(df_backtest$Loss)-1)],
     loss,
     xaxt="n",
     type='l', 
     ylim=c(min(loss)-0.1, max(df_backtest$Loss[1:(length(df_backtest$Loss)-1)])+0.1),
     xlab='Dates',
     ylab='Losses/VaR')
axis.Date(1,at=seq(df_backtest$Date[1],df_backtest$Date[(length(df_backtest$Loss)-1)],by="1 month"),format="%m/%Y")
lines(df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)], col='red', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
lines(df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)], col='green', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_evt_viol, lwd=4, col='red')
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_norm_viol, lwd=4, col='green')
title(main='Prezzo Unico Nazionale: 10:00 AM')
legend.txt<-c(paste("Violations gjrGARCH + POT(EVT): [",nv_evt,"]"),paste("Violations gjrGARCH + NORM: [",nv_norm,"]"))
legend('topright',legend.txt,lty=c(NA,NA),col=c("red","green"),lwd=c(3,3),bg="white",cex=0.75,bty="n",x.intersp=0,y.intersp=1.5,pch=c(21,21),merge=T)


VaRevt_010<-VaR_evt
VaRnorm_010<-VaR_norm










## 11 ##
xi_roll<-c()

mu_for<-c()
sigma_for<-c()
quant<-c()
VaR_evt<-c()
VaR_norm<-c()
ES<-c()
datepred<-c()
n_exc<-c()

solver<-'hybrid'
solvercontrol<-list("solver" = 1)

n<-649
dataset<-df_data_wd_11
last_i<-nrow(dataset)-n

df_backtest<-data.frame(Date = as.POSIXct('1970-01-01'), Loss = numeric(last_i), VaR_evt = numeric(last_i), VaR_norm = numeric(last_i), Sigma_f = numeric(last_i))

for (i in 1:last_i)
{
  # defining which model to be fitted
  model=ugarchspec(variance.model=list(model='gjrGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model='norm')
  # fitting the model to the dataset
  modelfit=ugarchfit(spec=model, data=dataset$Return[i:(i+n)]*(-1), solver.control = solvercontrol, solver=solver)
  # forecasting the sd
  foref<-ugarchforecast(modelfit, n.ahead=1)
  # getting the std residuals
  res<-residuals(modelfit, standardize=TRUE)
  # fitting the gpd to the excesses (std residuals-threshold)
  gpd<-gpdFit(as.numeric(res), u=quantile(res, 0.85))
  
  n_exc[i]<-length(res[res>quantile(res, 0.85)])
  xi_roll[i]<-gpd@fit$par.ests[1]
  mu_for[i]<-foref@forecast$seriesFor[1]
  sigma_for[i]<-foref@forecast$sigmaFor[1]
  
  if(is.na(sigma_for[i])==TRUE)
  {
    sigma_for[i]<-mean(sigma_for[(i-2):(i-1)])
  }
  
  risk<-evt_quant(gpd, 0.01)[[1]]
  quant[i]<-gpdRiskMeasures(gpd, prob=0.99)$quantile
  #quant[i]<-risk[1]
  VaR_evt[i]<-mu_for[i]+quant[i]*sigma_for[i]
  VaR_norm[i]<-mu_for[i]+qnorm(0.99)*sigma_for[i]
  
  # Filling the dataframe with results
  df_backtest['Date'][i,]<-as.POSIXct(dataset$Data[i+n+1])
  df_backtest['Loss'][i,]<-dataset$Return[i+n+1]*-1
  df_backtest['VaR_evt'][i,]<-VaR_evt[i]
  df_backtest['VaR_norm'][i,]<-VaR_norm[i]
  df_backtest['Sigma_f'][i,]<-sigma_for[i]
  
  print((i/last_i)*100)
}

# Defining the losses out of sample (test set)
loss<-df_backtest$Loss[1:(length(df_backtest$Loss)-1)]

# calculating vaR violations
VaR_evt_viol<-loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]
nv_evt<-sum(VaR_evt_viol)
VaR_norm_viol<-loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]
nv_norm<-sum(VaR_norm_viol)

VaR_evt_viol[VaR_evt_viol==TRUE]<-loss[loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]]
VaR_evt_viol[VaR_evt_viol==0]<-NA

VaR_norm_viol[VaR_norm_viol==TRUE]<-loss[loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]]-0.02
VaR_norm_viol[VaR_norm_viol==0]<-NA



plot(df_backtest$Date[1:(length(df_backtest$Loss)-1)],
     loss,
     xaxt="n",
     type='l', 
     ylim=c(min(loss)-0.1, max(df_backtest$Loss[1:(length(df_backtest$Loss)-1)])+0.1),
     xlab='Dates',
     ylab='Losses/VaR')
axis.Date(1,at=seq(df_backtest$Date[1],df_backtest$Date[(length(df_backtest$Loss)-1)],by="1 month"),format="%m/%Y")
lines(df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)], col='red', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
lines(df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)], col='green', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_evt_viol, lwd=4, col='red')
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_norm_viol, lwd=4, col='green')
title(main='Prezzo Unico Nazionale: 11:00 AM')
legend.txt<-c(paste("Violations gjrGARCH + POT(EVT): [",nv_evt,"]"),paste("Violations gjrGARCH + NORM: [",nv_norm,"]"))
legend('topright',legend.txt,lty=c(NA,NA),col=c("red","green"),lwd=c(3,3),bg="white",cex=0.75,bty="n",x.intersp=0,y.intersp=1.5,pch=c(21,21),merge=T)


VaRevt_011<-VaR_evt
VaRnorm_011<-VaR_norm










## 12 ##
xi_roll<-c()

mu_for<-c()
sigma_for<-c()
quant<-c()
VaR_evt<-c()
VaR_norm<-c()
ES<-c()
datepred<-c()
n_exc<-c()

solver<-'hybrid'
solvercontrol<-list("solver" = 1)

n<-649
dataset<-df_data_wd_12
last_i<-nrow(dataset)-n

df_backtest<-data.frame(Date = as.POSIXct('1970-01-01'), Loss = numeric(last_i), VaR_evt = numeric(last_i), VaR_norm = numeric(last_i), Sigma_f = numeric(last_i))

for (i in 1:last_i)
{
  # defining which model to be fitted
  model=ugarchspec(variance.model=list(model='gjrGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model='norm')
  # fitting the model to the dataset
  modelfit=ugarchfit(spec=model, data=dataset$Return[i:(i+n)]*(-1), solver.control = solvercontrol, solver=solver)
  # forecasting the sd
  foref<-ugarchforecast(modelfit, n.ahead=1)
  # getting the std residuals
  res<-residuals(modelfit, standardize=TRUE)
  # fitting the gpd to the excesses (std residuals-threshold)
  gpd<-gpdFit(as.numeric(res), u=quantile(res, 0.85))
  
  n_exc[i]<-length(res[res>quantile(res, 0.85)])
  xi_roll[i]<-gpd@fit$par.ests[1]
  mu_for[i]<-foref@forecast$seriesFor[1]
  sigma_for[i]<-foref@forecast$sigmaFor[1]
  
  if(is.na(sigma_for[i])==TRUE)
  {
    sigma_for[i]<-mean(sigma_for[(i-2):(i-1)])
  }
  
  risk<-evt_quant(gpd, 0.01)[[1]]
  quant[i]<-gpdRiskMeasures(gpd, prob=0.99)$quantile
  #quant[i]<-risk[1]
  VaR_evt[i]<-mu_for[i]+quant[i]*sigma_for[i]
  VaR_norm[i]<-mu_for[i]+qnorm(0.99)*sigma_for[i]
  
  # Filling the dataframe with results
  df_backtest['Date'][i,]<-as.POSIXct(dataset$Data[i+n+1])
  df_backtest['Loss'][i,]<-dataset$Return[i+n+1]*-1
  df_backtest['VaR_evt'][i,]<-VaR_evt[i]
  df_backtest['VaR_norm'][i,]<-VaR_norm[i]
  df_backtest['Sigma_f'][i,]<-sigma_for[i]
  
  print((i/last_i)*100)
}

# Defining the losses out of sample (test set)
loss<-df_backtest$Loss[1:(length(df_backtest$Loss)-1)]

# calculating vaR violations
VaR_evt_viol<-loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]
nv_evt<-sum(VaR_evt_viol)
VaR_norm_viol<-loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]
nv_norm<-sum(VaR_norm_viol)

VaR_evt_viol[VaR_evt_viol==TRUE]<-loss[loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]]
VaR_evt_viol[VaR_evt_viol==0]<-NA

VaR_norm_viol[VaR_norm_viol==TRUE]<-loss[loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]]-0.02
VaR_norm_viol[VaR_norm_viol==0]<-NA



plot(df_backtest$Date[1:(length(df_backtest$Loss)-1)],
     loss,
     xaxt="n",
     type='l', 
     ylim=c(min(loss)-0.1, max(df_backtest$Loss[1:(length(df_backtest$Loss)-1)])+0.1),
     xlab='Dates',
     ylab='Losses/VaR')
axis.Date(1,at=seq(df_backtest$Date[1],df_backtest$Date[(length(df_backtest$Loss)-1)],by="1 month"),format="%m/%Y")
lines(df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)], col='red', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
lines(df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)], col='green', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_evt_viol, lwd=4, col='red')
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_norm_viol, lwd=4, col='green')
title(main='Prezzo Unico Nazionale: 3:00 AM')
legend.txt<-c(paste("Violations gjrGARCH + POT(EVT): [",nv_evt,"]"),paste("Violations gjrGARCH + NORM: [",nv_norm,"]"))
legend('topright',legend.txt,lty=c(NA,NA),col=c("red","green"),lwd=c(3,3),bg="white",cex=0.75,bty="n",x.intersp=0,y.intersp=1.5,pch=c(21,21),merge=T)


VaRevt_012<-VaR_evt
VaRnorm_012<-VaR_norm










## 13 ##
xi_roll<-c()

mu_for<-c()
sigma_for<-c()
quant<-c()
VaR_evt<-c()
VaR_norm<-c()
ES<-c()
datepred<-c()
n_exc<-c()

solver<-'hybrid'
solvercontrol<-list("solver" = 1)

n<-649
dataset<-df_data_wd_13
last_i<-nrow(dataset)-n

df_backtest<-data.frame(Date = as.POSIXct('1970-01-01'), Loss = numeric(last_i), VaR_evt = numeric(last_i), VaR_norm = numeric(last_i), Sigma_f = numeric(last_i))

for (i in 1:last_i)
{
  # defining which model to be fitted
  model=ugarchspec(variance.model=list(model='gjrGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model='norm')
  # fitting the model to the dataset
  modelfit=ugarchfit(spec=model, data=dataset$Return[i:(i+n)]*(-1), solver.control = solvercontrol, solver=solver)
  # forecasting the sd
  foref<-ugarchforecast(modelfit, n.ahead=1)
  # getting the std residuals
  res<-residuals(modelfit, standardize=TRUE)
  # fitting the gpd to the excesses (std residuals-threshold)
  gpd<-gpdFit(as.numeric(res), u=quantile(res, 0.85))
  
  n_exc[i]<-length(res[res>quantile(res, 0.85)])
  xi_roll[i]<-gpd@fit$par.ests[1]
  mu_for[i]<-foref@forecast$seriesFor[1]
  sigma_for[i]<-foref@forecast$sigmaFor[1]
  
  if(is.na(sigma_for[i])==TRUE)
  {
    sigma_for[i]<-mean(sigma_for[(i-2):(i-1)])
  }
  
  risk<-evt_quant(gpd, 0.01)[[1]]
  quant[i]<-gpdRiskMeasures(gpd, prob=0.99)$quantile
  #quant[i]<-risk[1]
  VaR_evt[i]<-mu_for[i]+quant[i]*sigma_for[i]
  VaR_norm[i]<-mu_for[i]+qnorm(0.99)*sigma_for[i]
  
  # Filling the dataframe with results
  df_backtest['Date'][i,]<-as.POSIXct(dataset$Data[i+n+1])
  df_backtest['Loss'][i,]<-dataset$Return[i+n+1]*-1
  df_backtest['VaR_evt'][i,]<-VaR_evt[i]
  df_backtest['VaR_norm'][i,]<-VaR_norm[i]
  df_backtest['Sigma_f'][i,]<-sigma_for[i]
  
  print((i/last_i)*100)
}

# Defining the losses out of sample (test set)
loss<-df_backtest$Loss[1:(length(df_backtest$Loss)-1)]

# calculating vaR violations
VaR_evt_viol<-loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]
nv_evt<-sum(VaR_evt_viol)
VaR_norm_viol<-loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]
nv_norm<-sum(VaR_norm_viol)

VaR_evt_viol[VaR_evt_viol==TRUE]<-loss[loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]]
VaR_evt_viol[VaR_evt_viol==0]<-NA

VaR_norm_viol[VaR_norm_viol==TRUE]<-loss[loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]]-0.02
VaR_norm_viol[VaR_norm_viol==0]<-NA



plot(df_backtest$Date[1:(length(df_backtest$Loss)-1)],
     loss,
     xaxt="n",
     type='l', 
     ylim=c(min(loss)-0.1, max(df_backtest$Loss[1:(length(df_backtest$Loss)-1)])+0.1),
     xlab='Dates',
     ylab='Losses/VaR')
axis.Date(1,at=seq(df_backtest$Date[1],df_backtest$Date[(length(df_backtest$Loss)-1)],by="1 month"),format="%m/%Y")
lines(df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)], col='red', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
lines(df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)], col='green', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_evt_viol, lwd=4, col='red')
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_norm_viol, lwd=4, col='green')
title(main='Prezzo Unico Nazionale: 13:00 AM')
legend.txt<-c(paste("Violations gjrGARCH + POT(EVT): [",nv_evt,"]"),paste("Violations gjrGARCH + NORM: [",nv_norm,"]"))
legend('topright',legend.txt,lty=c(NA,NA),col=c("red","green"),lwd=c(3,3),bg="white",cex=0.75,bty="n",x.intersp=0,y.intersp=1.5,pch=c(21,21),merge=T)


VaRevt_013<-VaR_evt
VaRnorm_013<-VaR_norm










## 14 ##
xi_roll<-c()

mu_for<-c()
sigma_for<-c()
quant<-c()
VaR_evt<-c()
VaR_norm<-c()
ES<-c()
datepred<-c()
n_exc<-c()

solver<-'hybrid'
solvercontrol<-list("solver" = 1)

n<-649
dataset<-df_data_wd_14
last_i<-nrow(dataset)-n

df_backtest<-data.frame(Date = as.POSIXct('1970-01-01'), Loss = numeric(last_i), VaR_evt = numeric(last_i), VaR_norm = numeric(last_i), Sigma_f = numeric(last_i))

for (i in 1:last_i)
{
  # defining which model to be fitted
  model=ugarchspec(variance.model=list(model='gjrGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model='norm')
  # fitting the model to the dataset
  modelfit=ugarchfit(spec=model, data=dataset$Return[i:(i+n)]*(-1), solver.control = solvercontrol, solver=solver)
  # forecasting the sd
  foref<-ugarchforecast(modelfit, n.ahead=1)
  # getting the std residuals
  res<-residuals(modelfit, standardize=TRUE)
  # fitting the gpd to the excesses (std residuals-threshold)
  gpd<-gpdFit(as.numeric(res), u=quantile(res, 0.85))
  
  n_exc[i]<-length(res[res>quantile(res, 0.85)])
  xi_roll[i]<-gpd@fit$par.ests[1]
  mu_for[i]<-foref@forecast$seriesFor[1]
  sigma_for[i]<-foref@forecast$sigmaFor[1]
  
  if(is.na(sigma_for[i])==TRUE)
  {
    sigma_for[i]<-mean(sigma_for[(i-2):(i-1)])
  }
  
  risk<-evt_quant(gpd, 0.01)[[1]]
  quant[i]<-gpdRiskMeasures(gpd, prob=0.99)$quantile
  #quant[i]<-risk[1]
  VaR_evt[i]<-mu_for[i]+quant[i]*sigma_for[i]
  VaR_norm[i]<-mu_for[i]+qnorm(0.99)*sigma_for[i]
  
  # Filling the dataframe with results
  df_backtest['Date'][i,]<-as.POSIXct(dataset$Data[i+n+1])
  df_backtest['Loss'][i,]<-dataset$Return[i+n+1]*-1
  df_backtest['VaR_evt'][i,]<-VaR_evt[i]
  df_backtest['VaR_norm'][i,]<-VaR_norm[i]
  df_backtest['Sigma_f'][i,]<-sigma_for[i]
  
  print((i/last_i)*100)
}

# Defining the losses out of sample (test set)
loss<-df_backtest$Loss[1:(length(df_backtest$Loss)-1)]

# calculating vaR violations
VaR_evt_viol<-loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]
nv_evt<-sum(VaR_evt_viol)
VaR_norm_viol<-loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]
nv_norm<-sum(VaR_norm_viol)

VaR_evt_viol[VaR_evt_viol==TRUE]<-loss[loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]]
VaR_evt_viol[VaR_evt_viol==0]<-NA

VaR_norm_viol[VaR_norm_viol==TRUE]<-loss[loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]]-0.02
VaR_norm_viol[VaR_norm_viol==0]<-NA



plot(df_backtest$Date[1:(length(df_backtest$Loss)-1)],
     loss,
     xaxt="n",
     type='l', 
     ylim=c(min(loss)-0.1, max(df_backtest$Loss[1:(length(df_backtest$Loss)-1)])+0.1),
     xlab='Dates',
     ylab='Losses/VaR')
axis.Date(1,at=seq(df_backtest$Date[1],df_backtest$Date[(length(df_backtest$Loss)-1)],by="1 month"),format="%m/%Y")
lines(df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)], col='red', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
lines(df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)], col='green', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_evt_viol, lwd=4, col='red')
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_norm_viol, lwd=4, col='green')
title(main='Prezzo Unico Nazionale: 14:00 AM')
legend.txt<-c(paste("Violations gjrGARCH + POT(EVT): [",nv_evt,"]"),paste("Violations gjrGARCH + NORM: [",nv_norm,"]"))
legend('topright',legend.txt,lty=c(NA,NA),col=c("red","green"),lwd=c(3,3),bg="white",cex=0.75,bty="n",x.intersp=0,y.intersp=1.5,pch=c(21,21),merge=T)


VaRevt_014<-VaR_evt
VaRnorm_014<-VaR_norm










## 15 ##
xi_roll<-c()

mu_for<-c()
sigma_for<-c()
quant<-c()
VaR_evt<-c()
VaR_norm<-c()
ES<-c()
datepred<-c()
n_exc<-c()

solver<-'hybrid'
solvercontrol<-list("solver" = 1)

n<-649
dataset<-df_data_wd_15
last_i<-nrow(dataset)-n

df_backtest<-data.frame(Date = as.POSIXct('1970-01-01'), Loss = numeric(last_i), VaR_evt = numeric(last_i), VaR_norm = numeric(last_i), Sigma_f = numeric(last_i))

for (i in 1:last_i)
{
  # defining which model to be fitted
  model=ugarchspec(variance.model=list(model='gjrGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model='norm')
  # fitting the model to the dataset
  modelfit=ugarchfit(spec=model, data=dataset$Return[i:(i+n)]*(-1), solver.control = solvercontrol, solver=solver)
  # forecasting the sd
  foref<-ugarchforecast(modelfit, n.ahead=1)
  # getting the std residuals
  res<-residuals(modelfit, standardize=TRUE)
  # fitting the gpd to the excesses (std residuals-threshold)
  gpd<-gpdFit(as.numeric(res), u=quantile(res, 0.85))
  
  n_exc[i]<-length(res[res>quantile(res, 0.85)])
  xi_roll[i]<-gpd@fit$par.ests[1]
  mu_for[i]<-foref@forecast$seriesFor[1]
  sigma_for[i]<-foref@forecast$sigmaFor[1]
  
  if(is.na(sigma_for[i])==TRUE)
  {
    sigma_for[i]<-mean(sigma_for[(i-2):(i-1)])
  }
  
  risk<-evt_quant(gpd, 0.01)[[1]]
  quant[i]<-gpdRiskMeasures(gpd, prob=0.99)$quantile
  #quant[i]<-risk[1]
  VaR_evt[i]<-mu_for[i]+quant[i]*sigma_for[i]
  VaR_norm[i]<-mu_for[i]+qnorm(0.99)*sigma_for[i]
  
  # Filling the dataframe with results
  df_backtest['Date'][i,]<-as.POSIXct(dataset$Data[i+n+1])
  df_backtest['Loss'][i,]<-dataset$Return[i+n+1]*-1
  df_backtest['VaR_evt'][i,]<-VaR_evt[i]
  df_backtest['VaR_norm'][i,]<-VaR_norm[i]
  df_backtest['Sigma_f'][i,]<-sigma_for[i]
  
  print((i/last_i)*100)
}

# Defining the losses out of sample (test set)
loss<-df_backtest$Loss[1:(length(df_backtest$Loss)-1)]

# calculating vaR violations
VaR_evt_viol<-loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]
nv_evt<-sum(VaR_evt_viol)
VaR_norm_viol<-loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]
nv_norm<-sum(VaR_norm_viol)

VaR_evt_viol[VaR_evt_viol==TRUE]<-loss[loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]]
VaR_evt_viol[VaR_evt_viol==0]<-NA

VaR_norm_viol[VaR_norm_viol==TRUE]<-loss[loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]]-0.02
VaR_norm_viol[VaR_norm_viol==0]<-NA



plot(df_backtest$Date[1:(length(df_backtest$Loss)-1)],
     loss,
     xaxt="n",
     type='l', 
     ylim=c(min(loss)-0.1, max(df_backtest$Loss[1:(length(df_backtest$Loss)-1)])+0.1),
     xlab='Dates',
     ylab='Losses/VaR')
axis.Date(1,at=seq(df_backtest$Date[1],df_backtest$Date[(length(df_backtest$Loss)-1)],by="1 month"),format="%m/%Y")
lines(df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)], col='red', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
lines(df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)], col='green', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_evt_viol, lwd=4, col='red')
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_norm_viol, lwd=4, col='green')
title(main='Prezzo Unico Nazionale: 15:00 AM')
legend.txt<-c(paste("Violations gjrGARCH + POT(EVT): [",nv_evt,"]"),paste("Violations gjrGARCH + NORM: [",nv_norm,"]"))
legend('topright',legend.txt,lty=c(NA,NA),col=c("red","green"),lwd=c(3,3),bg="white",cex=0.75,bty="n",x.intersp=0,y.intersp=1.5,pch=c(21,21),merge=T)


VaRevt_015<-VaR_evt
VaRnorm_015<-VaR_norm










## 16 ##
xi_roll<-c()

mu_for<-c()
sigma_for<-c()
quant<-c()
VaR_evt<-c()
VaR_norm<-c()
ES<-c()
datepred<-c()
n_exc<-c()

solver<-'hybrid'
solvercontrol<-list("solver" = 1)

n<-649
dataset<-df_data_wd_16
last_i<-nrow(dataset)-n

df_backtest<-data.frame(Date = as.POSIXct('1970-01-01'), Loss = numeric(last_i), VaR_evt = numeric(last_i), VaR_norm = numeric(last_i), Sigma_f = numeric(last_i))

for (i in 1:last_i)
{
  # defining which model to be fitted
  model=ugarchspec(variance.model=list(model='gjrGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model='norm')
  # fitting the model to the dataset
  modelfit=ugarchfit(spec=model, data=dataset$Return[i:(i+n)]*(-1), solver.control = solvercontrol, solver=solver)
  # forecasting the sd
  foref<-ugarchforecast(modelfit, n.ahead=1)
  # getting the std residuals
  res<-residuals(modelfit, standardize=TRUE)
  # fitting the gpd to the excesses (std residuals-threshold)
  gpd<-gpdFit(as.numeric(res), u=quantile(res, 0.85))
  
  n_exc[i]<-length(res[res>quantile(res, 0.85)])
  xi_roll[i]<-gpd@fit$par.ests[1]
  mu_for[i]<-foref@forecast$seriesFor[1]
  sigma_for[i]<-foref@forecast$sigmaFor[1]
  
  if(is.na(sigma_for[i])==TRUE)
  {
    sigma_for[i]<-mean(sigma_for[(i-2):(i-1)])
  }
  
  risk<-evt_quant(gpd, 0.01)[[1]]
  quant[i]<-gpdRiskMeasures(gpd, prob=0.99)$quantile
  #quant[i]<-risk[1]
  VaR_evt[i]<-mu_for[i]+quant[i]*sigma_for[i]
  VaR_norm[i]<-mu_for[i]+qnorm(0.99)*sigma_for[i]
  
  # Filling the dataframe with results
  df_backtest['Date'][i,]<-as.POSIXct(dataset$Data[i+n+1])
  df_backtest['Loss'][i,]<-dataset$Return[i+n+1]*-1
  df_backtest['VaR_evt'][i,]<-VaR_evt[i]
  df_backtest['VaR_norm'][i,]<-VaR_norm[i]
  df_backtest['Sigma_f'][i,]<-sigma_for[i]
  
  print((i/last_i)*100)
}

# Defining the losses out of sample (test set)
loss<-df_backtest$Loss[1:(length(df_backtest$Loss)-1)]

# calculating vaR violations
VaR_evt_viol<-loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]
nv_evt<-sum(VaR_evt_viol)
VaR_norm_viol<-loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]
nv_norm<-sum(VaR_norm_viol)

VaR_evt_viol[VaR_evt_viol==TRUE]<-loss[loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]]
VaR_evt_viol[VaR_evt_viol==0]<-NA

VaR_norm_viol[VaR_norm_viol==TRUE]<-loss[loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]]-0.02
VaR_norm_viol[VaR_norm_viol==0]<-NA



plot(df_backtest$Date[1:(length(df_backtest$Loss)-1)],
     loss,
     xaxt="n",
     type='l', 
     ylim=c(min(loss)-0.1, max(df_backtest$Loss[1:(length(df_backtest$Loss)-1)])+0.1),
     xlab='Dates',
     ylab='Losses/VaR')
axis.Date(1,at=seq(df_backtest$Date[1],df_backtest$Date[(length(df_backtest$Loss)-1)],by="1 month"),format="%m/%Y")
lines(df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)], col='red', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
lines(df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)], col='green', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_evt_viol, lwd=4, col='red')
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_norm_viol, lwd=4, col='green')
title(main='Prezzo Unico Nazionale: 16:00 AM')
legend.txt<-c(paste("Violations gjrGARCH + POT(EVT): [",nv_evt,"]"),paste("Violations gjrGARCH + NORM: [",nv_norm,"]"))
legend('topright',legend.txt,lty=c(NA,NA),col=c("red","green"),lwd=c(3,3),bg="white",cex=0.75,bty="n",x.intersp=0,y.intersp=1.5,pch=c(21,21),merge=T)


VaRevt_016<-VaR_evt
VaRnorm_016<-VaR_norm










## 17 ##
xi_roll<-c()

mu_for<-c()
sigma_for<-c()
quant<-c()
VaR_evt<-c()
VaR_norm<-c()
ES<-c()
datepred<-c()
n_exc<-c()

solver<-'hybrid'
solvercontrol<-list("solver" = 1)

n<-649
dataset<-df_data_wd_17
last_i<-nrow(dataset)-n

df_backtest<-data.frame(Date = as.POSIXct('1970-01-01'), Loss = numeric(last_i), VaR_evt = numeric(last_i), VaR_norm = numeric(last_i), Sigma_f = numeric(last_i))

for (i in 1:last_i)
{
  # defining which model to be fitted
  model=ugarchspec(variance.model=list(model='gjrGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model='norm')
  # fitting the model to the dataset
  modelfit=ugarchfit(spec=model, data=dataset$Return[i:(i+n)]*(-1), solver.control = solvercontrol, solver=solver)
  # forecasting the sd
  foref<-ugarchforecast(modelfit, n.ahead=1)
  # getting the std residuals
  res<-residuals(modelfit, standardize=TRUE)
  # fitting the gpd to the excesses (std residuals-threshold)
  gpd<-gpdFit(as.numeric(res), u=quantile(res, 0.85))
  
  n_exc[i]<-length(res[res>quantile(res, 0.85)])
  xi_roll[i]<-gpd@fit$par.ests[1]
  mu_for[i]<-foref@forecast$seriesFor[1]
  sigma_for[i]<-foref@forecast$sigmaFor[1]
  
  if(is.na(sigma_for[i])==TRUE)
  {
    sigma_for[i]<-mean(sigma_for[(i-2):(i-1)])
  }
  
  risk<-evt_quant(gpd, 0.01)[[1]]
  quant[i]<-gpdRiskMeasures(gpd, prob=0.99)$quantile
  #quant[i]<-risk[1]
  VaR_evt[i]<-mu_for[i]+quant[i]*sigma_for[i]
  VaR_norm[i]<-mu_for[i]+qnorm(0.99)*sigma_for[i]
  
  # Filling the dataframe with results
  df_backtest['Date'][i,]<-as.POSIXct(dataset$Data[i+n+1])
  df_backtest['Loss'][i,]<-dataset$Return[i+n+1]*-1
  df_backtest['VaR_evt'][i,]<-VaR_evt[i]
  df_backtest['VaR_norm'][i,]<-VaR_norm[i]
  df_backtest['Sigma_f'][i,]<-sigma_for[i]
  
  print((i/last_i)*100)
}

# Defining the losses out of sample (test set)
loss<-df_backtest$Loss[1:(length(df_backtest$Loss)-1)]

# calculating vaR violations
VaR_evt_viol<-loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]
nv_evt<-sum(VaR_evt_viol)
VaR_norm_viol<-loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]
nv_norm<-sum(VaR_norm_viol)

VaR_evt_viol[VaR_evt_viol==TRUE]<-loss[loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]]
VaR_evt_viol[VaR_evt_viol==0]<-NA

VaR_norm_viol[VaR_norm_viol==TRUE]<-loss[loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]]-0.02
VaR_norm_viol[VaR_norm_viol==0]<-NA



plot(df_backtest$Date[1:(length(df_backtest$Loss)-1)],
     loss,
     xaxt="n",
     type='l', 
     ylim=c(min(loss)-0.1, max(df_backtest$Loss[1:(length(df_backtest$Loss)-1)])+0.1),
     xlab='Dates',
     ylab='Losses/VaR')
axis.Date(1,at=seq(df_backtest$Date[1],df_backtest$Date[(length(df_backtest$Loss)-1)],by="1 month"),format="%m/%Y")
lines(df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)], col='red', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
lines(df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)], col='green', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_evt_viol, lwd=4, col='red')
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_norm_viol, lwd=4, col='green')
title(main='Prezzo Unico Nazionale: 17:00 AM')
legend.txt<-c(paste("Violations gjrGARCH + POT(EVT): [",nv_evt,"]"),paste("Violations gjrGARCH + NORM: [",nv_norm,"]"))
legend('topright',legend.txt,lty=c(NA,NA),col=c("red","green"),lwd=c(3,3),bg="white",cex=0.75,bty="n",x.intersp=0,y.intersp=1.5,pch=c(21,21),merge=T)


VaRevt_017<-VaR_evt
VaRnorm_017<-VaR_norm










## 18 ##
xi_roll<-c()

mu_for<-c()
sigma_for<-c()
quant<-c()
VaR_evt<-c()
VaR_norm<-c()
ES<-c()
datepred<-c()
n_exc<-c()

solver<-'hybrid'
solvercontrol<-list("solver" = 1)

n<-649
dataset<-df_data_wd_18
last_i<-nrow(dataset)-n

df_backtest<-data.frame(Date = as.POSIXct('1970-01-01'), Loss = numeric(last_i), VaR_evt = numeric(last_i), VaR_norm = numeric(last_i), Sigma_f = numeric(last_i))

for (i in 1:last_i)
{
  # defining which model to be fitted
  model=ugarchspec(variance.model=list(model='gjrGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model='norm')
  # fitting the model to the dataset
  modelfit=ugarchfit(spec=model, data=dataset$Return[i:(i+n)]*(-1), solver.control = solvercontrol, solver=solver)
  # forecasting the sd
  foref<-ugarchforecast(modelfit, n.ahead=1)
  # getting the std residuals
  res<-residuals(modelfit, standardize=TRUE)
  # fitting the gpd to the excesses (std residuals-threshold)
  gpd<-gpdFit(as.numeric(res), u=quantile(res, 0.85))
  
  n_exc[i]<-length(res[res>quantile(res, 0.85)])
  xi_roll[i]<-gpd@fit$par.ests[1]
  mu_for[i]<-foref@forecast$seriesFor[1]
  sigma_for[i]<-foref@forecast$sigmaFor[1]
  
  if(is.na(sigma_for[i])==TRUE)
  {
    sigma_for[i]<-mean(sigma_for[(i-2):(i-1)])
  }
  
  risk<-evt_quant(gpd, 0.01)[[1]]
  quant[i]<-gpdRiskMeasures(gpd, prob=0.99)$quantile
  #quant[i]<-risk[1]
  VaR_evt[i]<-mu_for[i]+quant[i]*sigma_for[i]
  VaR_norm[i]<-mu_for[i]+qnorm(0.99)*sigma_for[i]
  
  # Filling the dataframe with results
  df_backtest['Date'][i,]<-as.POSIXct(dataset$Data[i+n+1])
  df_backtest['Loss'][i,]<-dataset$Return[i+n+1]*-1
  df_backtest['VaR_evt'][i,]<-VaR_evt[i]
  df_backtest['VaR_norm'][i,]<-VaR_norm[i]
  df_backtest['Sigma_f'][i,]<-sigma_for[i]
  
  print((i/last_i)*100)
}

# Defining the losses out of sample (test set)
loss<-df_backtest$Loss[1:(length(df_backtest$Loss)-1)]

# calculating vaR violations
VaR_evt_viol<-loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]
nv_evt<-sum(VaR_evt_viol)
VaR_norm_viol<-loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]
nv_norm<-sum(VaR_norm_viol)

VaR_evt_viol[VaR_evt_viol==TRUE]<-loss[loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]]
VaR_evt_viol[VaR_evt_viol==0]<-NA

VaR_norm_viol[VaR_norm_viol==TRUE]<-loss[loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]]-0.02
VaR_norm_viol[VaR_norm_viol==0]<-NA



plot(df_backtest$Date[1:(length(df_backtest$Loss)-1)],
     loss,
     xaxt="n",
     type='l', 
     ylim=c(min(loss)-0.1, max(df_backtest$Loss[1:(length(df_backtest$Loss)-1)])+0.1),
     xlab='Dates',
     ylab='Losses/VaR')
axis.Date(1,at=seq(df_backtest$Date[1],df_backtest$Date[(length(df_backtest$Loss)-1)],by="1 month"),format="%m/%Y")
lines(df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)], col='red', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
lines(df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)], col='green', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_evt_viol, lwd=4, col='red')
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_norm_viol, lwd=4, col='green')
title(main='Prezzo Unico Nazionale: 18:00 AM')
legend.txt<-c(paste("Violations gjrGARCH + POT(EVT): [",nv_evt,"]"),paste("Violations gjrGARCH + NORM: [",nv_norm,"]"))
legend('topright',legend.txt,lty=c(NA,NA),col=c("red","green"),lwd=c(3,3),bg="white",cex=0.75,bty="n",x.intersp=0,y.intersp=1.5,pch=c(21,21),merge=T)


VaRevt_018<-VaR_evt
VaRnorm_018<-VaR_norm










## 19 ##
xi_roll<-c()

mu_for<-c()
sigma_for<-c()
quant<-c()
VaR_evt<-c()
VaR_norm<-c()
ES<-c()
datepred<-c()
n_exc<-c()

solver<-'hybrid'
solvercontrol<-list("solver" = 1)

n<-649
dataset<-df_data_wd_19
last_i<-nrow(dataset)-n

df_backtest<-data.frame(Date = as.POSIXct('1970-01-01'), Loss = numeric(last_i), VaR_evt = numeric(last_i), VaR_norm = numeric(last_i), Sigma_f = numeric(last_i))

for (i in 1:last_i)
{
  # defining which model to be fitted
  model=ugarchspec(variance.model=list(model='gjrGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model='norm')
  # fitting the model to the dataset
  modelfit=ugarchfit(spec=model, data=dataset$Return[i:(i+n)]*(-1), solver.control = solvercontrol, solver=solver)
  # forecasting the sd
  foref<-ugarchforecast(modelfit, n.ahead=1)
  # getting the std residuals
  res<-residuals(modelfit, standardize=TRUE)
  # fitting the gpd to the excesses (std residuals-threshold)
  gpd<-gpdFit(as.numeric(res), u=quantile(res, 0.85))
  
  n_exc[i]<-length(res[res>quantile(res, 0.85)])
  xi_roll[i]<-gpd@fit$par.ests[1]
  mu_for[i]<-foref@forecast$seriesFor[1]
  sigma_for[i]<-foref@forecast$sigmaFor[1]
  
  if(is.na(sigma_for[i])==TRUE)
  {
    sigma_for[i]<-mean(sigma_for[(i-2):(i-1)])
  }
  
  risk<-evt_quant(gpd, 0.01)[[1]]
  quant[i]<-gpdRiskMeasures(gpd, prob=0.99)$quantile
  #quant[i]<-risk[1]
  VaR_evt[i]<-mu_for[i]+quant[i]*sigma_for[i]
  VaR_norm[i]<-mu_for[i]+qnorm(0.99)*sigma_for[i]
  
  # Filling the dataframe with results
  df_backtest['Date'][i,]<-as.POSIXct(dataset$Data[i+n+1])
  df_backtest['Loss'][i,]<-dataset$Return[i+n+1]*-1
  df_backtest['VaR_evt'][i,]<-VaR_evt[i]
  df_backtest['VaR_norm'][i,]<-VaR_norm[i]
  df_backtest['Sigma_f'][i,]<-sigma_for[i]
  
  print((i/last_i)*100)
}

# Defining the losses out of sample (test set)
loss<-df_backtest$Loss[1:(length(df_backtest$Loss)-1)]

# calculating vaR violations
VaR_evt_viol<-loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]
nv_evt<-sum(VaR_evt_viol)
VaR_norm_viol<-loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]
nv_norm<-sum(VaR_norm_viol)

VaR_evt_viol[VaR_evt_viol==TRUE]<-loss[loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]]
VaR_evt_viol[VaR_evt_viol==0]<-NA

VaR_norm_viol[VaR_norm_viol==TRUE]<-loss[loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]]-0.02
VaR_norm_viol[VaR_norm_viol==0]<-NA



plot(df_backtest$Date[1:(length(df_backtest$Loss)-1)],
     loss,
     xaxt="n",
     type='l', 
     ylim=c(min(loss)-0.1, max(df_backtest$Loss[1:(length(df_backtest$Loss)-1)])+0.1),
     xlab='Dates',
     ylab='Losses/VaR')
axis.Date(1,at=seq(df_backtest$Date[1],df_backtest$Date[(length(df_backtest$Loss)-1)],by="1 month"),format="%m/%Y")
lines(df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)], col='red', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
lines(df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)], col='green', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_evt_viol, lwd=4, col='red')
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_norm_viol, lwd=4, col='green')
title(main='Prezzo Unico Nazionale: 19:00 AM')
legend.txt<-c(paste("Violations gjrGARCH + POT(EVT): [",nv_evt,"]"),paste("Violations gjrGARCH + NORM: [",nv_norm,"]"))
legend('topright',legend.txt,lty=c(NA,NA),col=c("red","green"),lwd=c(3,3),bg="white",cex=0.75,bty="n",x.intersp=0,y.intersp=1.5,pch=c(21,21),merge=T)


VaRevt_019<-VaR_evt
VaRnorm_019<-VaR_norm










## 20 ##
xi_roll<-c()

mu_for<-c()
sigma_for<-c()
quant<-c()
VaR_evt<-c()
VaR_norm<-c()
ES<-c()
datepred<-c()
n_exc<-c()

solver<-'hybrid'
solvercontrol<-list("solver" = 1)

n<-649
dataset<-df_data_wd_20
last_i<-nrow(dataset)-n

df_backtest<-data.frame(Date = as.POSIXct('1970-01-01'), Loss = numeric(last_i), VaR_evt = numeric(last_i), VaR_norm = numeric(last_i), Sigma_f = numeric(last_i))

for (i in 1:last_i)
{
  # defining which model to be fitted
  model=ugarchspec(variance.model=list(model='gjrGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model='norm')
  # fitting the model to the dataset
  modelfit=ugarchfit(spec=model, data=dataset$Return[i:(i+n)]*(-1), solver.control = solvercontrol, solver=solver)
  # forecasting the sd
  foref<-ugarchforecast(modelfit, n.ahead=1)
  # getting the std residuals
  res<-residuals(modelfit, standardize=TRUE)
  # fitting the gpd to the excesses (std residuals-threshold)
  gpd<-gpdFit(as.numeric(res), u=quantile(res, 0.85))
  
  n_exc[i]<-length(res[res>quantile(res, 0.85)])
  xi_roll[i]<-gpd@fit$par.ests[1]
  mu_for[i]<-foref@forecast$seriesFor[1]
  sigma_for[i]<-foref@forecast$sigmaFor[1]
  
  if(is.na(sigma_for[i])==TRUE)
  {
    sigma_for[i]<-mean(sigma_for[(i-2):(i-1)])
  }
  
  risk<-evt_quant(gpd, 0.01)[[1]]
  quant[i]<-gpdRiskMeasures(gpd, prob=0.99)$quantile
  #quant[i]<-risk[1]
  VaR_evt[i]<-mu_for[i]+quant[i]*sigma_for[i]
  VaR_norm[i]<-mu_for[i]+qnorm(0.99)*sigma_for[i]
  
  # Filling the dataframe with results
  df_backtest['Date'][i,]<-as.POSIXct(dataset$Data[i+n+1])
  df_backtest['Loss'][i,]<-dataset$Return[i+n+1]*-1
  df_backtest['VaR_evt'][i,]<-VaR_evt[i]
  df_backtest['VaR_norm'][i,]<-VaR_norm[i]
  df_backtest['Sigma_f'][i,]<-sigma_for[i]
  
  print((i/last_i)*100)
}

# Defining the losses out of sample (test set)
loss<-df_backtest$Loss[1:(length(df_backtest$Loss)-1)]

# calculating vaR violations
VaR_evt_viol<-loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]
nv_evt<-sum(VaR_evt_viol)
VaR_norm_viol<-loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]
nv_norm<-sum(VaR_norm_viol)

VaR_evt_viol[VaR_evt_viol==TRUE]<-loss[loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]]
VaR_evt_viol[VaR_evt_viol==0]<-NA

VaR_norm_viol[VaR_norm_viol==TRUE]<-loss[loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]]-0.02
VaR_norm_viol[VaR_norm_viol==0]<-NA



plot(df_backtest$Date[1:(length(df_backtest$Loss)-1)],
     loss,
     xaxt="n",
     type='l', 
     ylim=c(min(loss)-0.1, max(df_backtest$Loss[1:(length(df_backtest$Loss)-1)])+0.1),
     xlab='Dates',
     ylab='Losses/VaR')
axis.Date(1,at=seq(df_backtest$Date[1],df_backtest$Date[(length(df_backtest$Loss)-1)],by="1 month"),format="%m/%Y")
lines(df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)], col='red', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
lines(df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)], col='green', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_evt_viol, lwd=4, col='red')
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_norm_viol, lwd=4, col='green')
title(main='Prezzo Unico Nazionale: 20:00 AM')
legend.txt<-c(paste("Violations gjrGARCH + POT(EVT): [",nv_evt,"]"),paste("Violations gjrGARCH + NORM: [",nv_norm,"]"))
legend('topright',legend.txt,lty=c(NA,NA),col=c("red","green"),lwd=c(3,3),bg="white",cex=0.75,bty="n",x.intersp=0,y.intersp=1.5,pch=c(21,21),merge=T)


VaRevt_020<-VaR_evt
VaRnorm_020<-VaR_norm










## 21 ##
xi_roll<-c()

mu_for<-c()
sigma_for<-c()
quant<-c()
VaR_evt<-c()
VaR_norm<-c()
ES<-c()
datepred<-c()
n_exc<-c()

solver<-'hybrid'
solvercontrol<-list("solver" = 1)

n<-649
dataset<-df_data_wd_21
last_i<-nrow(dataset)-n

df_backtest<-data.frame(Date = as.POSIXct('1970-01-01'), Loss = numeric(last_i), VaR_evt = numeric(last_i), VaR_norm = numeric(last_i), Sigma_f = numeric(last_i))

for (i in 1:last_i)
{
  # defining which model to be fitted
  model=ugarchspec(variance.model=list(model='gjrGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model='norm')
  # fitting the model to the dataset
  modelfit=ugarchfit(spec=model, data=dataset$Return[i:(i+n)]*(-1), solver.control = solvercontrol, solver=solver)
  # forecasting the sd
  foref<-ugarchforecast(modelfit, n.ahead=1)
  # getting the std residuals
  res<-residuals(modelfit, standardize=TRUE)
  # fitting the gpd to the excesses (std residuals-threshold)
  gpd<-gpdFit(as.numeric(res), u=quantile(res, 0.85))
  
  n_exc[i]<-length(res[res>quantile(res, 0.85)])
  xi_roll[i]<-gpd@fit$par.ests[1]
  mu_for[i]<-foref@forecast$seriesFor[1]
  sigma_for[i]<-foref@forecast$sigmaFor[1]
  
  if(is.na(sigma_for[i])==TRUE)
  {
    sigma_for[i]<-mean(sigma_for[(i-2):(i-1)])
  }
  
  risk<-evt_quant(gpd, 0.01)[[1]]
  quant[i]<-gpdRiskMeasures(gpd, prob=0.99)$quantile
  #quant[i]<-risk[1]
  VaR_evt[i]<-mu_for[i]+quant[i]*sigma_for[i]
  VaR_norm[i]<-mu_for[i]+qnorm(0.99)*sigma_for[i]
  
  # Filling the dataframe with results
  df_backtest['Date'][i,]<-as.POSIXct(dataset$Data[i+n+1])
  df_backtest['Loss'][i,]<-dataset$Return[i+n+1]*-1
  df_backtest['VaR_evt'][i,]<-VaR_evt[i]
  df_backtest['VaR_norm'][i,]<-VaR_norm[i]
  df_backtest['Sigma_f'][i,]<-sigma_for[i]
  
  print((i/last_i)*100)
}

# Defining the losses out of sample (test set)
loss<-df_backtest$Loss[1:(length(df_backtest$Loss)-1)]

# calculating vaR violations
VaR_evt_viol<-loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]
nv_evt<-sum(VaR_evt_viol)
VaR_norm_viol<-loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]
nv_norm<-sum(VaR_norm_viol)

VaR_evt_viol[VaR_evt_viol==TRUE]<-loss[loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]]
VaR_evt_viol[VaR_evt_viol==0]<-NA

VaR_norm_viol[VaR_norm_viol==TRUE]<-loss[loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]]-0.02
VaR_norm_viol[VaR_norm_viol==0]<-NA



plot(df_backtest$Date[1:(length(df_backtest$Loss)-1)],
     loss,
     xaxt="n",
     type='l', 
     ylim=c(min(loss)-0.1, max(df_backtest$Loss[1:(length(df_backtest$Loss)-1)])+0.1),
     xlab='Dates',
     ylab='Losses/VaR')
axis.Date(1,at=seq(df_backtest$Date[1],df_backtest$Date[(length(df_backtest$Loss)-1)],by="1 month"),format="%m/%Y")
lines(df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)], col='red', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
lines(df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)], col='green', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_evt_viol, lwd=4, col='red')
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_norm_viol, lwd=4, col='green')
title(main='Prezzo Unico Nazionale: 21:00 AM')
legend.txt<-c(paste("Violations gjrGARCH + POT(EVT): [",nv_evt,"]"),paste("Violations gjrGARCH + NORM: [",nv_norm,"]"))
legend('topright',legend.txt,lty=c(NA,NA),col=c("red","green"),lwd=c(3,3),bg="white",cex=0.75,bty="n",x.intersp=0,y.intersp=1.5,pch=c(21,21),merge=T)


VaRevt_021<-VaR_evt
VaRnorm_021<-VaR_norm










## 22 ##
xi_roll<-c()

mu_for<-c()
sigma_for<-c()
quant<-c()
VaR_evt<-c()
VaR_norm<-c()
ES<-c()
datepred<-c()
n_exc<-c()

solver<-'hybrid'
solvercontrol<-list("solver" = 1)

n<-649
dataset<-df_data_wd_22
last_i<-nrow(dataset)-n

df_backtest<-data.frame(Date = as.POSIXct('1970-01-01'), Loss = numeric(last_i), VaR_evt = numeric(last_i), VaR_norm = numeric(last_i), Sigma_f = numeric(last_i))

for (i in 1:last_i)
{
  # defining which model to be fitted
  model=ugarchspec(variance.model=list(model='gjrGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model='norm')
  # fitting the model to the dataset
  modelfit=ugarchfit(spec=model, data=dataset$Return[i:(i+n)]*(-1), solver.control = solvercontrol, solver=solver)
  # forecasting the sd
  foref<-ugarchforecast(modelfit, n.ahead=1)
  # getting the std residuals
  res<-residuals(modelfit, standardize=TRUE)
  # fitting the gpd to the excesses (std residuals-threshold)
  gpd<-gpdFit(as.numeric(res), u=quantile(res, 0.85))
  
  n_exc[i]<-length(res[res>quantile(res, 0.85)])
  xi_roll[i]<-gpd@fit$par.ests[1]
  mu_for[i]<-foref@forecast$seriesFor[1]
  sigma_for[i]<-foref@forecast$sigmaFor[1]
  
  if(is.na(sigma_for[i])==TRUE)
  {
    sigma_for[i]<-mean(sigma_for[(i-2):(i-1)])
  }
  
  risk<-evt_quant(gpd, 0.01)[[1]]
  quant[i]<-gpdRiskMeasures(gpd, prob=0.99)$quantile
  #quant[i]<-risk[1]
  VaR_evt[i]<-mu_for[i]+quant[i]*sigma_for[i]
  VaR_norm[i]<-mu_for[i]+qnorm(0.99)*sigma_for[i]
  
  # Filling the dataframe with results
  df_backtest['Date'][i,]<-as.POSIXct(dataset$Data[i+n+1])
  df_backtest['Loss'][i,]<-dataset$Return[i+n+1]*-1
  df_backtest['VaR_evt'][i,]<-VaR_evt[i]
  df_backtest['VaR_norm'][i,]<-VaR_norm[i]
  df_backtest['Sigma_f'][i,]<-sigma_for[i]
  
  print((i/last_i)*100)
}

# Defining the losses out of sample (test set)
loss<-df_backtest$Loss[1:(length(df_backtest$Loss)-1)]

# calculating vaR violations
VaR_evt_viol<-loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]
nv_evt<-sum(VaR_evt_viol)
VaR_norm_viol<-loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]
nv_norm<-sum(VaR_norm_viol)

VaR_evt_viol[VaR_evt_viol==TRUE]<-loss[loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]]
VaR_evt_viol[VaR_evt_viol==0]<-NA

VaR_norm_viol[VaR_norm_viol==TRUE]<-loss[loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]]-0.02
VaR_norm_viol[VaR_norm_viol==0]<-NA



plot(df_backtest$Date[1:(length(df_backtest$Loss)-1)],
     loss,
     xaxt="n",
     type='l', 
     ylim=c(min(loss)-0.1, max(df_backtest$Loss[1:(length(df_backtest$Loss)-1)])+0.1),
     xlab='Dates',
     ylab='Losses/VaR')
axis.Date(1,at=seq(df_backtest$Date[1],df_backtest$Date[(length(df_backtest$Loss)-1)],by="1 month"),format="%m/%Y")
lines(df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)], col='red', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
lines(df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)], col='green', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_evt_viol, lwd=4, col='red')
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_norm_viol, lwd=4, col='green')
title(main='Prezzo Unico Nazionale: 22:00 AM')
legend.txt<-c(paste("Violations gjrGARCH + POT(EVT): [",nv_evt,"]"),paste("Violations gjrGARCH + NORM: [",nv_norm,"]"))
legend('topright',legend.txt,lty=c(NA,NA),col=c("red","green"),lwd=c(3,3),bg="white",cex=0.75,bty="n",x.intersp=0,y.intersp=1.5,pch=c(21,21),merge=T)


VaRevt_022<-VaR_evt
VaRnorm_022<-VaR_norm










## 23 ##
xi_roll<-c()

mu_for<-c()
sigma_for<-c()
quant<-c()
VaR_evt<-c()
VaR_norm<-c()
ES<-c()
datepred<-c()
n_exc<-c()

solver<-'hybrid'
solvercontrol<-list("solver" = 1)

n<-649
dataset<-df_data_wd_23
last_i<-nrow(dataset)-n

df_backtest<-data.frame(Date = as.POSIXct('1970-01-01'), Loss = numeric(last_i), VaR_evt = numeric(last_i), VaR_norm = numeric(last_i), Sigma_f = numeric(last_i))

for (i in 1:last_i)
{
  # defining which model to be fitted
  model=ugarchspec(variance.model=list(model='gjrGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model='norm')
  # fitting the model to the dataset
  modelfit=ugarchfit(spec=model, data=dataset$Return[i:(i+n)]*(-1), solver.control = solvercontrol, solver=solver)
  # forecasting the sd
  foref<-ugarchforecast(modelfit, n.ahead=1)
  # getting the std residuals
  res<-residuals(modelfit, standardize=TRUE)
  # fitting the gpd to the excesses (std residuals-threshold)
  gpd<-gpdFit(as.numeric(res), u=quantile(res, 0.85))
  
  n_exc[i]<-length(res[res>quantile(res, 0.85)])
  xi_roll[i]<-gpd@fit$par.ests[1]
  mu_for[i]<-foref@forecast$seriesFor[1]
  sigma_for[i]<-foref@forecast$sigmaFor[1]
  
  if(is.na(sigma_for[i])==TRUE)
  {
    sigma_for[i]<-mean(sigma_for[(i-2):(i-1)])
  }
  
  risk<-evt_quant(gpd, 0.01)[[1]]
  quant[i]<-gpdRiskMeasures(gpd, prob=0.99)$quantile
  #quant[i]<-risk[1]
  VaR_evt[i]<-mu_for[i]+quant[i]*sigma_for[i]
  VaR_norm[i]<-mu_for[i]+qnorm(0.99)*sigma_for[i]
  
  # Filling the dataframe with results
  df_backtest['Date'][i,]<-as.POSIXct(dataset$Data[i+n+1])
  df_backtest['Loss'][i,]<-dataset$Return[i+n+1]*-1
  df_backtest['VaR_evt'][i,]<-VaR_evt[i]
  df_backtest['VaR_norm'][i,]<-VaR_norm[i]
  df_backtest['Sigma_f'][i,]<-sigma_for[i]
  
  print((i/last_i)*100)
}

# Defining the losses out of sample (test set)
loss<-df_backtest$Loss[1:(length(df_backtest$Loss)-1)]

# calculating vaR violations
VaR_evt_viol<-loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]
nv_evt<-sum(VaR_evt_viol)
VaR_norm_viol<-loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]
nv_norm<-sum(VaR_norm_viol)

VaR_evt_viol[VaR_evt_viol==TRUE]<-loss[loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]]
VaR_evt_viol[VaR_evt_viol==0]<-NA

VaR_norm_viol[VaR_norm_viol==TRUE]<-loss[loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]]-0.02
VaR_norm_viol[VaR_norm_viol==0]<-NA



plot(df_backtest$Date[1:(length(df_backtest$Loss)-1)],
     loss,
     xaxt="n",
     type='l', 
     ylim=c(min(loss)-0.1, max(df_backtest$Loss[1:(length(df_backtest$Loss)-1)])+0.1),
     xlab='Dates',
     ylab='Losses/VaR')
axis.Date(1,at=seq(df_backtest$Date[1],df_backtest$Date[(length(df_backtest$Loss)-1)],by="1 month"),format="%m/%Y")
lines(df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)], col='red', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
lines(df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)], col='green', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_evt_viol, lwd=4, col='red')
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_norm_viol, lwd=4, col='green')
title(main='Prezzo Unico Nazionale: 23:00 AM')
legend.txt<-c(paste("Violations gjrGARCH + POT(EVT): [",nv_evt,"]"),paste("Violations gjrGARCH + NORM: [",nv_norm,"]"))
legend('topright',legend.txt,lty=c(NA,NA),col=c("red","green"),lwd=c(3,3),bg="white",cex=0.75,bty="n",x.intersp=0,y.intersp=1.5,pch=c(21,21),merge=T)


VaRevt_023<-VaR_evt
VaRnorm_023<-VaR_norm










## 24 ##
xi_roll<-c()

mu_for<-c()
sigma_for<-c()
quant<-c()
VaR_evt<-c()
VaR_norm<-c()
ES<-c()
datepred<-c()
n_exc<-c()

solver<-'hybrid'
solvercontrol<-list("solver" = 1)

n<-649
dataset<-df_data_wd_24
last_i<-nrow(dataset)-n

df_backtest<-data.frame(Date = as.POSIXct('1970-01-01'), Loss = numeric(last_i), VaR_evt = numeric(last_i), VaR_norm = numeric(last_i), Sigma_f = numeric(last_i))

for (i in 1:last_i)
{
  # defining which model to be fitted
  model=ugarchspec(variance.model=list(model='gjrGARCH', garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1), include.mean=TRUE), distribution.model='norm')
  # fitting the model to the dataset
  modelfit=ugarchfit(spec=model, data=dataset$Return[i:(i+n)]*(-1), solver.control = solvercontrol, solver=solver)
  # forecasting the sd
  foref<-ugarchforecast(modelfit, n.ahead=1)
  # getting the std residuals
  res<-residuals(modelfit, standardize=TRUE)
  # fitting the gpd to the excesses (std residuals-threshold)
  gpd<-gpdFit(as.numeric(res), u=quantile(res, 0.85))
  
  n_exc[i]<-length(res[res>quantile(res, 0.85)])
  xi_roll[i]<-gpd@fit$par.ests[1]
  mu_for[i]<-foref@forecast$seriesFor[1]
  sigma_for[i]<-foref@forecast$sigmaFor[1]
  
  if(is.na(sigma_for[i])==TRUE)
  {
    sigma_for[i]<-mean(sigma_for[(i-2):(i-1)])
  }
  
  risk<-evt_quant(gpd, 0.01)[[1]]
  quant[i]<-gpdRiskMeasures(gpd, prob=0.99)$quantile
  #quant[i]<-risk[1]
  VaR_evt[i]<-mu_for[i]+quant[i]*sigma_for[i]
  VaR_norm[i]<-mu_for[i]+qnorm(0.99)*sigma_for[i]
  
  # Filling the dataframe with results
  df_backtest['Date'][i,]<-as.POSIXct(dataset$Data[i+n+1])
  df_backtest['Loss'][i,]<-dataset$Return[i+n+1]*-1
  df_backtest['VaR_evt'][i,]<-VaR_evt[i]
  df_backtest['VaR_norm'][i,]<-VaR_norm[i]
  df_backtest['Sigma_f'][i,]<-sigma_for[i]
  
  print((i/last_i)*100)
}

# Defining the losses out of sample (test set)
loss<-df_backtest$Loss[1:(length(df_backtest$Loss)-1)]

# calculating vaR violations
VaR_evt_viol<-loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]
nv_evt<-sum(VaR_evt_viol)
VaR_norm_viol<-loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]
nv_norm<-sum(VaR_norm_viol)

VaR_evt_viol[VaR_evt_viol==TRUE]<-loss[loss>df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)]]
VaR_evt_viol[VaR_evt_viol==0]<-NA

VaR_norm_viol[VaR_norm_viol==TRUE]<-loss[loss>df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)]]-0.02
VaR_norm_viol[VaR_norm_viol==0]<-NA



plot(df_backtest$Date[1:(length(df_backtest$Loss)-1)],
     loss,
     xaxt="n",
     type='l', 
     ylim=c(min(loss)-0.1, max(df_backtest$Loss[1:(length(df_backtest$Loss)-1)])+0.1),
     xlab='Dates',
     ylab='Losses/VaR')
axis.Date(1,at=seq(df_backtest$Date[1],df_backtest$Date[(length(df_backtest$Loss)-1)],by="1 month"),format="%m/%Y")
lines(df_backtest$VaR_evt[1:(length(df_backtest$Loss)-1)], col='red', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
lines(df_backtest$VaR_norm[1:(length(df_backtest$Loss)-1)], col='green', x=df_backtest$Date[1:(length(df_backtest$Loss)-1)])
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_evt_viol, lwd=4, col='red')
points(df_backtest$Date[1:(length(df_backtest$Loss)-1)], VaR_norm_viol, lwd=4, col='green')
title(main='Prezzo Unico Nazionale: 24:00 AM')
legend.txt<-c(paste("Violations gjrGARCH + POT(EVT): [",nv_evt,"]"),paste("Violations gjrGARCH + NORM: [",nv_norm,"]"))
legend('topright',legend.txt,lty=c(NA,NA),col=c("red","green"),lwd=c(3,3),bg="white",cex=0.75,bty="n",x.intersp=0,y.intersp=1.5,pch=c(21,21),merge=T)



VaRevt_024<-VaR_evt
VaRnorm_024<-VaR_norm



################################################################################
# 4) Performing some aggregation and operation on the dataset
################################################################################

# Rearranging the dataset, estimates and dataframes
for(i in length(VaRevt_017):(length(VaRevt_001)))
{
  VaRevt_017[i]<-NA
  VaRnorm_017[i]<-NA
}

VaRevt_017<-VaRevt_017[1:2160]

df_var<-data.frame(df_backtest$Date[1:(length(df_backtest$Date)-1)], 
                   loss, 
                   VaRevt_001[1:(length(VaRevt_001)-1)], 
                   VaRevt_002[1:(length(VaRevt_002)-1)], 
                   VaRevt_003[1:(length(VaRevt_003)-1)],
                   VaRevt_004[1:(length(VaRevt_004)-1)], 
                   VaRevt_005[1:(length(VaRevt_005)-1)], 
                   VaRevt_006[1:(length(VaRevt_006)-1)], 
                   VaRevt_007[1:(length(VaRevt_007)-1)],
                   VaRevt_008[1:(length(VaRevt_008)-1)], 
                   VaRevt_009[1:(length(VaRevt_009)-1)], 
                   VaRevt_010[1:(length(VaRevt_010)-1)], 
                   VaRevt_011[1:(length(VaRevt_011)-1)],
                   VaRevt_012[1:(length(VaRevt_012)-1)], 
                   VaRevt_013[1:(length(VaRevt_013)-1)], 
                   VaRevt_014[1:(length(VaRevt_014)-1)], 
                   VaRevt_015[1:(length(VaRevt_015)-1)],
                   VaRevt_016[1:(length(VaRevt_016)-1)], 
                   VaRevt_017[1:(length(VaRevt_017)-1)], 
                   VaRevt_018[1:(length(VaRevt_018)-1)], 
                   VaRevt_019[1:(length(VaRevt_019)-1)],
                   VaRevt_020[1:(length(VaRevt_020)-1)], 
                   VaRevt_021[1:(length(VaRevt_021)-1)], 
                   VaRevt_022[1:(length(VaRevt_022)-1)], 
                   VaRevt_023[1:(length(VaRevt_023)-1)],
                   VaRevt_024[1:(length(VaRevt_024)-1)],
                   VaRnorm_001[1:(length(VaRnorm_001)-1)], 
                   VaRnorm_002[1:(length(VaRnorm_002)-1)], 
                   VaRnorm_003[1:(length(VaRnorm_003)-1)],
                   VaRnorm_004[1:(length(VaRnorm_004)-1)], 
                   VaRnorm_005[1:(length(VaRnorm_005)-1)], 
                   VaRnorm_006[1:(length(VaRnorm_006)-1)], 
                   VaRnorm_007[1:(length(VaRnorm_007)-1)],
                   VaRnorm_008[1:(length(VaRnorm_008)-1)], 
                   VaRnorm_009[1:(length(VaRnorm_009)-1)], 
                   VaRnorm_010[1:(length(VaRnorm_010)-1)], 
                   VaRnorm_011[1:(length(VaRnorm_011)-1)],
                   VaRnorm_012[1:(length(VaRnorm_012)-1)], 
                   VaRnorm_013[1:(length(VaRnorm_013)-1)], 
                   VaRnorm_014[1:(length(VaRnorm_014)-1)], 
                   VaRnorm_015[1:(length(VaRnorm_015)-1)],
                   VaRnorm_016[1:(length(VaRnorm_016)-1)], 
                   VaRnorm_017[1:(length(VaRnorm_017)-1)], 
                   VaRnorm_018[1:(length(VaRnorm_018)-1)], 
                   VaRnorm_019[1:(length(VaRnorm_019)-1)],
                   VaRnorm_020[1:(length(VaRnorm_020)-1)], 
                   VaRnorm_021[1:(length(VaRnorm_021)-1)], 
                   VaRnorm_022[1:(length(VaRnorm_022)-1)], 
                   VaRnorm_023[1:(length(VaRnorm_023)-1)],
                   VaRnorm_024[1:(length(VaRnorm_024)-1)])

names(df_var)<-c('Data', 'Loss', 
                 'VaRevt01', 
                 'VaRevt02',
                 'VaRevt03',
                 'VaRevt04',
                 'VaRevt05',
                 'VaRevt06',
                 'VaRevt07',
                 'VaRevt08',
                 'VaRevt09',
                 'VaRevt10',
                 'VaRevt11',
                 'VaRevt12',
                 'VaRevt13',
                 'VaRevt14',
                 'VaRevt15',
                 'VaRevt16',
                 'VaRevt17',
                 'VaRevt18',
                 'VaRevt19',
                 'VaRevt20',
                 'VaRevt21',
                 'VaRevt22',
                 'VaRevt23',
                 'VaRevt24',
                 'VaRnorm01', 
                 'VaRnorm02',
                 'VaRnorm03',
                 'VaRnorm04',
                 'VaRnorm05',
                 'VaRnorm06',
                 'VaRnorm07',
                 'VaRnorm08',
                 'VaRnorm09',
                 'VaRnorm10',
                 'VaRnorm11',
                 'VaRnorm12',
                 'VaRnorm13',
                 'VaRnorm14',
                 'VaRnorm15',
                 'VaRnorm16',
                 'VaRnorm17',
                 'VaRnorm18',
                 'VaRnorm19',
                 'VaRnorm20',
                 'VaRnorm21',
                 'VaRnorm22',
                 'VaRnorm23',
                 'VaRnorm24')


# Builing the dataset for backtesting
df_backtesting<-subset(df_data_wd, Data>=as.POSIXct('2012-07-01'))
df_backtesting<-df_backtesting[with(df_backtesting, order(Data, Ora)),]

df_Prices<-subset(df_data, Data>=as.POSIXct('2012-06-30') & Data<=as.POSIXct('2020-10-07'))[,c(1,3,4)]

write.table(df_Prices, 'F:/Progetti/MGP/df_Prices.csv', sep='|', dec='.')
write.table(df_backtesting, 'F:/Progetti/MGP/df_backtesting.csv', sep='|', dec='.')
write.table(df_var, 'F:/Progetti/MGP/df_var.csv', sep='|', dec='.')
write.table(dataset$Data, 'F:/Progetti/MGP/datasetData.csv', sep='|', dec='.')


test<-subset(df_Prices, Ora==1)
nrow(test)
nrow(df_var)


# Binding togheter all var estimates (evt, norm)
df<-data.frame()


for(i in 1:nrow(df_var))
{
  if(i==1)
  {
    df<-cbind(as.POSIXlt(as.character(df_var$Data[i])), stack(df_var[1,c(3:26)]), 1:24, stack(df_var[1,c(27:50)]))
  }
  else
  {
    df<-rbind(df, cbind(as.POSIXlt(as.character(df_var$Data[i])), stack(df_var[i,c(3:26)]), 1:24, stack(df_var[i,c(27:50)])))
  }
}

df<-as.data.frame(df[,c(1,4,2,5)])
names(df)<-c('Data', 'Ora', 'VaRevt', 'VaRnorm')
df['Losses']<-df_backtesting$Return*(-1)
df$Data<-as.POSIXlt(df$Data)

# Fixing for Nan's: replacing 0 both for losses and for VaR and convert dates from POSIXct to POSIXlt, adding hours to format
for(i in 1:nrow(df))
{
  if(is.na(df$VaRevt[i])==TRUE | is.na(df$VaRnorm[i])==TRUE | is.na(df$Losses[i])==TRUE)
  {
    df$VaRevt[i]<-0
    df$VaRnorm[i]<-0
    df$Losses[i]<-0
  }
  df$Data$hour[i]<-df$Ora[i]
}

write.table(df, 'F:/Progetti/MGP/df.csv', sep='|', dec='.')

################################################################################
# 5) FINAL RESULTS: number of violations and chart
################################################################################
n_evt<-sum(df$Losses>df$VaRevt)
n_norm<-sum(df$Losses>df$VaRnorm)

VaRTest(alpha=0.01, df$Losses, df$VaRnorm, conf.level=0.95)

# 45097
# 45216

# Lockdown
# 48098
# 48216

# Hot summer
# 25657
# 25776

# Snow and cold
# 3841
# 3959


# Visual sample
start<-34000       #45097
end<-34590     #45216
n_evt_vs<-sum(df$Losses[start:end]>df$VaRevt[start:end])
n_norm_vs<-sum(df$Losses[start:end]>df$VaRnorm[start:end])

viol_evt<-df$Losses>df$VaRevt
viol_norm<-df$Losses>df$VaRnorm

viol_evt[viol_evt==TRUE]<-df$Losses[df$Losses>df$VaRevt]
viol_evt[viol_evt==0]<-NA

viol_norm[viol_norm==TRUE]<-df$Losses[df$Losses>df$VaRnorm]-0.05
viol_norm[viol_norm==0]<-NA


plot(df$Losses[start:end], type='l', lwd=1, ylim=c(-0.7,0.9))
lines(df$VaRevt[start:end], col='red', lwd=1)
lines(df$VaRnorm[start:end], col='green', lwd=1)

dev.off()
plot(x=df$Data[start:end],
     y=df$Losses[start:end],
     xaxt="n",
     type='l', 
     ylim=c(-0.6,0.6),
     xlab='',
     ylab='Losses/VaR', lwd=2, las=2, cex=0.5)
grid(nx=NULL, ny=NULL, lwd = 1)
axis.POSIXct(1,at=seq(as.POSIXlt(df$Data[start]),as.POSIXlt(df$Data[end]),by="12 hour"),format="%d/%m/%Y(%H)", las=3, cex.axis=0.7)
lines(df$VaRevt[start:end], col='red', x=df$Data[start:end], lwd=2)
lines(df$VaRnorm[start:end], col='green', x=df$Data[start:end], lwd=2)
points(df$Data[start:end], viol_evt[start:end], lwd=4, col='red')
points(df$Data[start:end], viol_norm[start:end], lwd=4, col='green')
title(main='Hourly % Losses - Prezzo Unico Nazionale')
legend.txt<-c(paste("Violations gjrGARCH + POT(EVT): [",n_evt_vs,"]"),paste("Violations gjrGARCH + NORM: [",n_norm_vs,"]"))
legend('topright',legend.txt,lty=c(NA,NA),col=c("red","green"),lwd=c(3,3),bg="white",cex=0.75,bty="n",x.intersp=0,y.intersp=1.5,pch=c(21,21),merge=T)





# Binomial test
N_evt<-length(df$Losses)
x_evt<-sum(df$Losses>df$VaRevt)
P=0.01

z_evt = (x_evt-N_evt*P)/sqrt(N_evt*P*(1-P))



# Binomial test
N_norm<-length(df$Losses)
x_norm<-sum(df$Losses>df$VaRnorm)
P=0.01

z_norm = (x_norm-N_norm*P)/sqrt(N_norm*P*(1-P))


VaRTest(alpha=0.01, df[df$Losses>df$VaR,]$Losses, df[df$Losses>df$VaR,]$VaRevt, conf.level=0.95)
