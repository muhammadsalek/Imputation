#Libraries
library(dplyr)
library(ggplot2)

####Data Exploration####
df<-airquality

#Structure
str(df)

#Basic Summary
summary(df)


#Visualisation
ggplot(df,aes(x=Ozone))+
  geom_histogram(bins = 15,fill="darkblue",color="white")


####Missing Value####
#Identify
colSums(is.na(df))

mean(df$Ozone,na.rm = T)

#Mean/Median Imputation
df$Ozone_v2<-ifelse(is.na(df$Ozone),mean(df$Ozone,na.rm = T),df$Ozone)
df$Ozone_v3<-ifelse(is.na(df$Ozone),median(df$Ozone,na.rm = T),df$Ozone)

#Regression Imputation
model<-lm(Ozone~Wind+Temp+Month+Day,data = df)
df$Ozone_v4<-df$Ozone
df$Ozone_v4[is.na(df$Ozone_v4)]<-predict(model,newdata = df[is.na(df$Ozone),])

#KNN Imputation
library(VIM)
dt<-airquality

dt_knn<-kNN(dt,k=5)

####Outlier####
Q1<-quantile(dt$Ozone,0.25,na.rm = T)
Q3<-quantile(dt$Ozone,0.75,na.rm = T)
IQR<-Q3-Q1

lower_bound<-Q1-1.5*IQR
upper_bound<-Q3+1.5*IQR

na.omit(dt[dt$Ozone<lower_bound|dt$Ozone>upper_bound,])
dt_2<-dt[dt$Ozone>lower_bound&dt$Ozone<upper_bound,]

#Winsorization
dt$Ozone_Win<-ifelse(dt$Ozone<lower_bound,lower_bound,
                     ifelse(dt$Ozone>upper_bound,upper_bound,
                            dt$Ozone))

#Log Transformation
dt$Ozone_log<-log1p(dt$Ozone)

ggplot(dt,aes(x="",y=Ozone))+
  geom_boxplot(fill="orange")

ggplot(dt,aes(x="",y=Ozone_Win))+
  geom_boxplot(fill="orange")

ggplot(dt,aes(x="",y=Ozone_log))+
  geom_boxplot(fill="orange")
