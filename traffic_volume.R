
#Project Statement
#Estimate the Count of Traffic flow at the given hour with the features like tempearture,
#holiday, weather condition, etc.....
#----------------------------------------------------------------------------------------------------------     
#Data importing

setwd("E:\\Data science\\assignments")
dir()
traffic1<- read.csv("Traffic_volume.csv",header = TRUE)
View(traffic1)
#----------------------------------------------------------------------------------------------------------
#input - holiday, temp,rain_1h,snow_1h,clouds_all,weather_main,weather_description , date_time"  
#output - traffic_volume

head(traffic1)
names(traffic1)
str(traffic1)

#Data understands
# holiday,weather_main,data_time and weather description columns  are of  character data type
# temp,snow_1h,rain_1h,clouds all are  of continous data type
#output column is a continous data type- so we use regression models

#----------------------------------------------------------------------------------------------------------
#Summary or basic statiscs of the data

library(fBasics)

basicStats(traffic1[ ,c(2,3,4,5)])  ## basicstats works only on numeric datatype

summary(traffic1)

#---------------------------------------------------------------------------------------------------------
#Missing values identification and treatment

colSums(is.na(traffic1))  ## temp,rain_1h,snow_1h has missing values all are numeric variables

traffic1$temp[is.na(traffic1$temp)]<- median(traffic1$temp,na.rm = TRUE)
traffic1$rain_1h[is.na(traffic1$rain_1h)]<- median(traffic1$rain_1h,na.rm = TRUE)
traffic1$snow_1h[is.na(traffic1$snow_1h)]<- median(traffic1$snow_1h,na.rm = TRUE)

library(Amelia)
missmap(traffic)  ##missingnes map-shows the missing values through graphical representation

#===========================================================================================================
##EDA - Analysis

###--------------------------uni- variate analsis for columns------------------------------------------------
library(e1071)
attach(traffic1)

#                  --------For column temperature--------------

mean(temp)       #281.2067
median(temp)     #282.46
min(temp)        #0
max(temp)        #310.07
IQR(temp)        #19.62
quantile(temp)   # 0%    25%    50%    75%   100% 
# 0.00 272.18 282.46 291.80 310.07  
skewness(temp)  #-2.248258   negatively skewed1
kurtosis(temp)  #39.93298  = high peak,its a leptokurtic curve

#                    ---------For column rain_1h-------------

mean(rain_1h)     # 0.334264
median(rain_1h)   # 0
min(rain_1h)      # 0
max(rain_1h)      # 9831.3
IQR(rain_1h)      # 0
quantile(rain_1h) # 0%    25%    50%    75%   100% 
# 0.0    0.0    0.0    0.0 9831.3
skewness(rain_1h) # 219.375
kurtosis(rain_1h) # 48148.63

#         --------------For column snow_1h----------------------

mean(snow_1h)     #0.0002223882
median(snow_1h)   #0
min(snow_1h)      #0
max(snow_1h)      #0.51
IQR(snow_1h)      #0
quantile(snow_1h) # 0%  25%  50%  75% 100% 
# 0.00 0.00 0.00 0.00 0.51 

skewness(snow_1h) #48.36
kurtosis(snow_1h) #2620.085

#                -------------For column clouds_all-----------------

mean(clouds_all)     #49.366
median(clouds_all)   #64
min(clouds_all)      #0
max(clouds_all)      #100
IQR(clouds_all)      # 89
quantile(clouds_all) #0%  25%  50%  75% 100% 
#0    1   64   90  100 

skewness(clouds_all) #-0.19721
kurtosis(clouds_all) # -1.7422

#----------------Bi- variate analysis btwn input and output column-------------------------------------------

cor(temp,traffic_volume)       # 0.13 low correlated
cor(rain_1h,traffic_volume)    # 0.004 low correlated
cor(snow_1h,traffic_volume)    # 0.0007 low
cor(clouds_all,traffic_volume) # 0.067 low

##corr matrix cannot be obtained on non-numeric data,so using visualization for correlation
library(psych)
pairs.panels(traffic)

#output - date_time         =  -0.01
#output - weatherdescription = -0.06
#output - weather main        = -0.04
#output - holiday            =  0.02
#===========================================================================================================
#Visualizations

library(ggplot2)
library(graphics)

#o Check the relation b/w holiday vs traffic volume
qplot(traffic_volume,holiday)           # volume is more on None of holiday

#o Temp vs traffic volume
qplot(traffic_volume,temp)              # volume is more when temp ranges btwn 240 - 300 F
plot(traffic_volume,temp)             # fit line is parallel to x- axis(volume),with intercept range of temp

#o Weather vs traffic volume
qplot(traffic_volume,weather_main)             #when clouds more traffic,squall less traffic
qplot(traffic_volume,weather_description)       #showersnow -almost no traffic,scatterclouds -more traffic

ggplot(data = traffic1,aes(x = weather_main,y = holiday,fill = traffic_volume))+geom_tile()
ggplot(data = traffic1,aes(x = weather_description,y = holiday,fill = traffic_volume))+geom_tile()
#========================================================================================================
#Correlation using 

library(psych)

pairs.panels(traffic)

#output of above shows,correlation values,strength,direction
#temp,traffic_volume          = 0.13 low correlated
#rain_1h,traffic_volume       = 0.004 low correlated
#snow_1h,traffic_volume       = 0.0007 low
#clouds_all,traffic_volume    = 0.067 low
#output - date_time           =-0.01
#output - weatherdescription  = -0.06
#output - weather main        = -0.04
#output - holiday             = 0.02

#==========================================================================================================
### outliers replacing with median

boxplot(traffic1$temp)$out
mean(traffic1$temp,na.rm = TRUE)

traffic1$temp[traffic1$temp == 0]<- median(traffic1$temp,na.rm =TRUE)

boxplot(traffic1$rain_1h)$out
traffic1$rain_1h <- ifelse(traffic1$rain_1h > 0,0,traffic1$rain_1h)

boxplot(traffic1$snow_1h)$out
median(traffic$1snow_1h,na.rm = TRUE)

traffic1$snow_1h<- ifelse(traffic1$snow_1h > 0,0,traffic1$snow_1h)

boxplot(traffic1)

#====================================================================================================
#Feature selection

# since all the inputs have no correlation,we cannot remove all,so considering all columns,except data_time
attach(traffic1)
head(traffic1)

table(holiday) ## holidays are just 0.1% of non holidays,remove that col,all comes under non-holiday

table(rain_1h) ## all are zeros,we can remove that column(its a numeric datatype)

table(snow_1h) ## all are zeros,we can remove that column(its a numeric datatype)

table(clouds_all) ## can consider for model,since data is randomly distributed

table(weather_main) ## data is distributed in all classes

table(weather_description)  ## distributed in all classes
# removing weather_description,because ML predictions should be in a generalized way,but not specifi

## considering all columns except holiday and weather description,becasue r square is effecting by dropping
traffic1<- traffic1[ ,-c(1,7)]
head(traffic1)

library(stringr)
attach(traffic1)
## since traffic is effected by timings ,sepearet time from data and used for analysis

str_split_fixed(traffic1$date_time," ",2)->x
time<- x[ ,2]

View(time)
## time has 12 factor levels ,reducing to 5

time[time == c("07:00")]<- "MORNING"
time[time == c("08:00")]<- "MORNING"
time[time == c("09:00")]<- "MORNING"
time[time == c("10:00")]<- "MORNING"
time[time == c("11:00")]<- "MORNING"
time[time == c("12:00")]<- "MORNING"

time[time == c("13:00")]<- "AFTNOON"
time[time == c("14:00")]<- "AFTNOON"
time[time == c("15:00")]<- "AFTNOON"
time[time == c("16:00")]<- "AFTNOON"

time[time == c("17:00")]<- "EVENING"
time[time == c("18:00")]<- "EVENING"
time[time == c("19:00")]<- "EVENING"
time[time == c("20:00")]<- "EVENING"

time[time == c("21:00")]<- "NIGHT"
time[time == c("22:00")]<- "NIGHT"
time[time == c("23:00")]<- "NIGHT"

time[time == c("00:00")]<- "NIGHT"
time[time == c("01:00")]<- "NIGHT"
time[time == c("02:00")]<- "NIGHT"

time[time == c("03:00")]<- "EMORNING"
time[time == c("04:00")]<- "EMORNING"
time[time == c("05:00")]<- "EMORNING"
time[time == c("06:00")]<- "EMORNING"

rm(traffic)
traffic<-cbind(time,traffic1[ ,-6])
head(traffic)
str(traffic)
#======================================================================================================
### one hot encoding for holiday,weather_main,weather_description
library(fastDummies)

tim<- fastDummies::dummy_cols(traffic$time)

cloud<- dummy_cols(traffic$weather_main)

Mdata<- cbind(traffic[ ,-c(1,6)],cloud[ ,-1],tim[ ,-1])

head(Mdata)

dim(Mdata)  # dim size = 48204*69

str(Mdata)
#===========================================================================================================

#Normalizing the data

norma<-function(x){(x-min(x))/(max(x)-min(x))}

Mdata[ ,-5]<- norma(Mdata[ ,-5])
head(Mdata)
#=======================================================================================================
#Data partition

df = sample(2,nrow(Mdata),replace = TRUE,prob = c(0.7,0.3))
training<- as.data.frame(Mdata[df == 1, ])
testing<- as.data.frame(Mdata[df == 2, ])

dim(training)
dim(testing)
#==========================================================================================================
#APPLYING REGRESSION MODELS

#-------------------------LINEAR REGRESSION---------------------------------------------------------------
attach(training)
reg<- lm(traffic_volume~ ., data = training)
summary(reg)

## from the model we can remove columns 2,3,16,21 are neglible for model ...4,5,9,13,14,15,

reg1<- lm(traffic_volume~ ., data = training[ ,-c(2,3,16,21)])
summary(reg1)

###considering only important columns

regimp<- lm(traffic_volume~ ., data = training[ ,c(1,6,7,8,10,21,20,19,17)])
summary(regimp)

## by reducing the colums the expalnation of variance by model is decreasing so,considering all columns is good
tpred<- predict(reg,training[ ,-5])

los<- training$traffic_volume - tpred
trainacc<- sqrt(mean(los*los))
trainacc


head(testing)
pred<-predict(reg,testing[ ,-5])

error = testing$traffic_volume - pred

head(cbind(acutal = testing$traffic_volume,pred),30) ## actual and pred comparision

Mloss = mean(error*error)
Mloss

sqrt(Mloss)

#--------------------------- USing decison tree regression------------------------------------------------------------

library(rpart)

rpt<- rpart(traffic_volume ~ ., data = training)
plot(rpt)
summary(rpt)

sqrt(3952972) # 1988.08
#MSE = 3952972

pred1<- predict(rpt,testing[ ,-5])
err<- testing$traffic_volume - pred1

sqrt(mean(err*err))


#-------------------------------------randomforest---------------------------------------------------------------------
library(randomForest)

rdm<- randomForest(traffic_volume ~ ., data = training)
plot(rdm)
summary(rdm)

sqrt(1586601)  #1259.60
#MSE = 1586601 , ntress = 500

pred2<- predict(rdm,testing[ ,-5])
err<- testing$traffic_volume - pred2

sqrt(mean(err*err))

## 1240.524 RMsE fro testing less than training,computation times is more

#------------------------------using svm-------------------------------------------------------------------------------
library(e1071)

svm<-svm(traffic_volume ~ ., data = training)
summary(svm)

sqrt(3952972) # 1988.08
#MSE = 3952972

pred3<- predict(svm,testing[ ,-5])
err<- testing$traffic_volume - pred3

sqrt(mean(err*err))

##1985.145 test RMSE is less than train ,computational time is more





