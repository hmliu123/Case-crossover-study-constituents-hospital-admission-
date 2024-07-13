##########################################################################################################
#Codes for "Associations of Improved Fine Particulate Air Pollution with Cause-specific Hospital Admissions"
#Authors for codes: Huimeng Liu, Jian Lei, Yunxing Jiang, Lijun Bai et.al.
#Correspondence to Shaowei Wu, Yuewei Liu.
###########################################################################################################

##########################################################################################################
# Exposure-response curve (Figure S8)
###########################################################################################################
rm(list=ls())

library(dplyr)
library(tidyverse)
library(lubridate) 
library(splines) 
library(survival)
library(mvmeta)
library("RcppRoll")
library(forestploter)
library(patchwork)

#########################################
#Read data
subtype <- c("Asthma","LRI", "Cardiac arrhythmias","CHD","Heart failure","Stroke",
             "Depression","Schizophrenia","Parkinson's disease","CKD")

#Loop for cause-specific hospital admission
for (m in subtype) {
cat(m," ")
  ckd_inc <- import(paste0("Analysis\\All disease\\",
                           m,"_compont.csv")) 

#Region number in each cause-specific hospital admissions  
regions <- unique(ckd_inc$district)
#Arrange the data as a list of data sets
data <- lapply(regions,function(x) ckd_inc[ckd_inc$district==x,])
names(data) <- regions

#Generate matrix to store the results
ymat <- matrix(NA,length(data),4,dimnames=list(regions,paste("b",seq(4),sep="")))
knot <- matrix(NA,length(data),3,dimnames=list(regions,paste("b",seq(3),sep="")))

#（Co）variance matrices
Slist <- vector("list",length(data))
names(Slist) <- regions

#Loop for cities
for(i in regions) {
  
  # Print
  cat(i,"")
  
  # LOAD city data
  sub <- data[[i]]
  sub <- sub[order(sub$city_code,sub$date),]

  #Mean constituents' concentration at lag01
  sub$SO4lag01 = roll_mean(sub$SO4, n = 2, align = "right", fill = NA)
  sub$NO3lag01 = roll_mean(sub$NO3, n = 2, align = "right", fill = NA)
  sub$NH4lag01 = roll_mean(sub$NH4, n = 2, align = "right", fill = NA)
  sub$OMlag01 = roll_mean(sub$OM, n = 2, align = "right", fill = NA)
  sub$BClag01 = roll_mean(sub$BC, n = 2, align = "right", fill = NA)
  sub$PM2.5lag01 = roll_mean(sub$PM2.5, n = 2, align = "right", fill = NA)
  ################################################################
  #Function that can transform data into case crossover dataset 
  source("funccmake.R")
  Sys.setlocale("LC_TIME", "English")
  #Generate time stratum
  sub$month  <- as.factor(months(sub$date))
  sub$year   <- as.factor(format(sub$date, format="%Y") )
  sub$dow    <- as.factor(weekdays(sub$date))
  sub$stratum <- as.factor(sub$year:sub$month:sub$dow)
  #Temperature and relative humidity at lag 0-21
  sub$templag21 = roll_mean(sub$meantem/10, n = 22, align = "right", fill = NA)
  sub$humlag21 = roll_mean(sub$rh, n = 22, align = "right", fill = NA)
  #Generate case crossover dataset for each city
  data_case_crossover <- funccmake(sub$stratum,sub$PSN_NO,
                                   vars=cbind(temperature=sub$templag21, humidity = sub$humlag21, Holiday = sub$Holiday,
                                              city_code = sub$city_code,
                                              BClag01=sub$BClag01
                                   ))
  
  
  #Predicted bound range for BC concentration
  bound <- range(data_case_crossover$BClag01,na.rm=T)
  #Natural cubic spline for BC with knots specified at the 25th, 50th, and 75th percentiles
  knotperc <- c(25,50,75)
  knots<- quantile(data_case_crossover$BClag01,boundary=bound,knotperc/100,na.rm=T)
  btpm<- onebasis(data_case_crossover$BClag01,fun = "ns",knots=knots)
  
  #Run conditioanl logistic regression model
  timeout <- as.numeric(factor(data_case_crossover$stratum))
  timein <- timeout-0.1
  model_clr <- coxph(Surv(timein,timeout,status) ~ btpm +  ns(temperature, df=6) + ns(humidity, df=3) + as.factor(Holiday), 
                     weights=weights, data_case_crossover)
  
  x <- summary(model_clr)
  
  #Reduction of alternative models to the space of the predictor returns

  pred<- crosspred(btpm,model_clr,cen = 0)
  #Save the city-specific coefficients
  ymat[i,] <- coef(pred)
  Slist[[i]] <- vcov(pred)
  
  knot[i,] <- knots
  data[[i]] <- sub
}

#Random-effect models to pool the results
mv <- mvmeta(ymat,Slist,method = "reml")

#Restricted BC concentration in the ER plot
ranges <- t(sapply(data,function(x) range(x$BClag01,na.rm=T)))
bound <- colMeans(ranges)
knots <- colMeans(knot)
#Mean 99th percentile of 292 cities
quantile_all_city_BC <- as.data.frame(sapply(data,function(x) quantile(x$BClag01,0.99,na.rm=T)))
quantile_BC <- colMeans(quantile_all_city_BC)

#Generate basis variable
Pollu_set <- seq(0, bound[2], length = 5)
dl_basis <- onebasis(Pollu_set, fun = 'ns', knots=knots)#

#Overall cumulative summary association
p <- crosspred(dl_basis, coef=coef(mv), vcov=vcov(mv), model.link="log", by=0.01, cen=0)

#Asign plot name for each disease                     
assign(paste0(paste0("cp_BC_",m)),p)
}

#Plot ER curve
plot(cp_BC_LRI,"overall",col=2,lwd=2,ylab="Percent change (%)", xlab=expression(paste("BC"," (", μ, g, "/", m^3, ")", sep = "")),
     yaxt="n",xlim=c(0,quantile_BC_LRI),main = "LRI")
axis(side=2,
  at=c(1,1.05,1.1,1.15),
  labels=c("0","5","10","15"))