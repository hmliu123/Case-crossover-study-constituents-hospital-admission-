##########################################################################################################
#Codes for "Associations of Improved Fine Particulate Air Pollution with Cause-specific Hospital Admissions"
#Authors for codes: Huimeng Liu, Jian Lei, Yunxing Jiang, Lijun Bai et.al.
#Correspondence to Shaowei Wu, Yuewei Liu.
###########################################################################################################


####################################################################################################################################################
#Annual and total attributable fractions for hospital admissions and Length of hospital stay (Table 2, Figure 3, Figure S3, Table S3, Table S4)
##################################################################################################################################################
rm(list=ls())

library(tidyverse)
library(lubridate) 
library(mvmeta) 
library(splines) 
library(survival)
library(metafor)
library("RcppRoll")
library(ggplot2)
library(scales)
library(ggsci)

#Import pollution data
a <- read.csv("Data\\Pol data\\Wei_clean.csv")
a <- a %>% 
  mutate(date=ymd(date))

###########################################################################
#1.Annual and total attributable fractions of major cause-specific hoapital admissions 
############################################################################

#Functions for get city-specific attributable hospital admission numbers for each pollutant
num_fun <- function(dataset,city_code_i,pollution_i,disease_cat_i) {
  final_result <- tibble()

  #Filter city-specific health data
  full_data <- dataset %>% 
    filter(city_code == city_code_i) %>% 
    select(date,PSN_NO,pollution_i,city_code)
  
 
  
  #Annual hospital admissions
  annual_sum <- aggregate(full_data$PSN_NO, list(format(full_data$date, "%Y")), FUN=sum, na.rm=T)
  #Annual average pollution levels
  pollutant_avg <- aggregate(full_data[,pollution_i], list(format(full_data$date, "%Y")), FUN=mean, na.rm=T)
  
  #Filter effect esitmations of each constituent and cause-specific hospital admissions based on single constituent model
  rr_use <- subset(rr, disease_group==disease_cat_i & pollution == pollution_i )
  #Atributable number of hospital admission associted with constituent in city i
  rri <- exp(log(rr_use$RR) * pollutant_avg[,pollution_i])          
  all_ann <- (rri-1)/rri * annual_sum$x
  #95% CI for atributable number of hospital admission associted with constituent in city i
  rril <- exp(log(rr_use$RR_L) * pollutant_avg[,pollution_i])
  low <- (rril-1)/rril * annual_sum$x
  rrih <- exp(log(rr_use$RR_H) * pollutant_avg[,pollution_i])
  high <- (rrih-1)/rrih * annual_sum$x
  
  result <- cbind(city_code=city_code_i, all_ann=all_ann, low=low, high=high,
                  disease = disease_cat_i,pollution_con = pollutant_avg[,pollution_i],year = seq(2013,2017),
                  annual_sum = annual_sum)
  
  final_result <- rbind(final_result, result)
  
  return(final_result)
}

#Import exposure-response relationships obtained form single constituent model
meta_relag <- read_csv("\\Result\\Single consituents model 10 disease.csv") 
rr <- as.data.frame(meta_relag, stringsAsFactors = F)
#Generate empty list to store the results
temp_vars <- c("RR", "RR_L", "RR_H")
rr[temp_vars] <- sapply(rr[temp_vars], as.numeric)


#Read data and apply function
re_HAN <- NULL
pollution_i <- c("BClag01") #PM2.5lag01 
disease_cat <- c("Asthma","LRI","Cardiac arrhythmias","CHD","Heart failure","Stroke",
                 "Depression","Schizophrenia","Parkinson's disease","CKD")

#Loop for cause-specific hospital admissions
for (disease_cat_i in disease_cat){
  cat(disease_cat_i," ")
  file<-list.files("/Analysis//All disease/")
  
  dataset = import(paste0("/Analysis//All disease/",disease_cat_i,"_compont.csv")) meantem,rh,city_code,district) 
  
  
  temp <- tibble()
  #Generate city list for each disease
  city_list <- unique(dataset$city_code)
  
  
  #Loop for cities
  for (city_code_i in city_list) {
    #Calculate attibutable number in each city
    temp <- rbind(temp, num_fun(dataset,city_code_i,pollution_i,disease_cat_i)) %>% 
      distinct() 
  }
  re_HAN  <- rbind(temp,re_HAN)
}


#Annual attributable number of hospital admissions for all included cities
fin_HAN <- aggregate(re_HAN[, c(2:4,9)], list(re_HAN$disease,re_HAN$year), sum,na.rm = T)



#Calculate annual attributable fractions and its 95% CIs
#Annual attributable fraction = Annual attributable numbers/Annual total number of hospital admissions 
fin_HAN["AF_all"] <- (fin_HAN["all_ann"] / fin_HAN$annual_sum.x) * 100
fin_HAN["AF_low"] <- (fin_HAN["low"] / fin_HAN$annual_sum.x) * 100
fin_HAN["AF_high"] <- (fin_HAN["high"] / fin_HAN$annual_sum.x) * 100

#Calculate total attributable fractions and its 95% CIs
#Total attributable fractions =Total attributable numbers/Total number of hospital admissions
fin_HAN_total <- aggregate(fin_HAN[, c(3:6)], list(fin_HAN$Group.1), sum,na.rm = T)
fin_HAN_total["AF_all"] <- fin_HAN_total["all_ann"] / fin_HAN_total$sum_total * 100
fin_HAN_total["AF_low"] <- fin_HAN_total["low"] / fin_HAN_total$sum_total * 100
fin_HAN_total["AF_high"] <- fin_HAN_total["high"] / fin_HAN_total$sum_total * 100



##################################################################################
#2.Annual attributable fractions of  major cause-specific length of hospital stay(LOS)
#################################################################################

# Fuction for city-specific attributable LOS
day_fun <- function(data,city_code_i,pollution_i,disease_cat_i) {
  final_result <- tibble()
  #Filter city-specific health data
  full_data <- data %>% 
     filter(city_code == city_code_i) 
    
#Annual total hospital addmissions
  annual_sum <- aggregate(full_data$PSN_NO, list(format(full_data$date, "%Y")), FUN=sum, na.rm=T) 
#LOS
  avg_day <- sum(full_data$TOT_INP_DAYS2, na.rm=T)/ sum(full_data$PSN_NO, na.rm=T) 
  
#Annual mean pollution level
  pollutant_avg <- aggregate(full_data[,pollution_i], list(format(full_data$date, "%Y")), FUN=mean, na.rm=T) #########################
  
  #Effects esitmation of each constituents and cause-specific hospital admissions based on single constituent model
  rr_use <- subset(rr, disease_group==disease_cat_i & pollution==pollution_i)
  
   #Atributable LOS associted with constituent in city i
  rri <- exp(log(rr_use$RR) * pollutant_avg[,pollution_i])
  all_ann <- (rri-1)/rri * annual_sum$x * avg_day

  #95% CI for atributable LOS associted with constituent in city i
  rril <- exp(log(rr_use$RR_L) *  pollutant_avg[,pollution_i])
  low <- (rril-1)/rril * annual_sum$x * avg_day
  rrih <- exp(log(rr_use$RR_H) *  pollutant_avg[,pollution_i])
  high <- (rrih-1)/rrih * annual_sum$x * avg_day

  #Annual LOS for each disease in city i
  final_total_num <- aggregate(full_data$TOT_INP_DAYS2, list(format(full_data$date, "%Y")), FUN=sum, na.rm=T) 
  
  result <- cbind(city_code=city_code_i, all_ann=all_ann, low=low, high=high,
                  disease = disease_cat_i,pollution_con = pollutant_avg[,pollution_i],year = seq(2013,2017),
                  annual_sum = annual_sum,final_total_num = final_total_num)
  final_result <- rbind(final_result, result)
  return(final_result)
}

#Import exposure-response relationships obtained form single constituent model
meta_relag <- read_csv("\\Result\\Single consituents model 10 disease.csv") 
rr <- as.data.frame(meta_relag, stringsAsFactors = F)
#Generate empty list to store the results
temp_vars <- c("RR", "RR_L", "RR_H")
rr[temp_vars] <- sapply(rr[temp_vars], as.numeric)

#Read data and apply fuction
re_HAN <- NULL
pollution_i <- c("BClag01") 
disease_cat <- c("Asthma","LRI",
                 "Cardiac arrhythmias","CHD","Heart failure","Stroke",
                 "Depression","Schizophrenia","Parkinson's disease","CKD")
#Loop for cause-specific hospital admissions
for (disease_cat_i in disease_cat){
  cat(disease_cat_i," ")
    
  data = import(paste0("/Analysis//All disease/",disease_cat_i,"_compont.csv")) 
  

  temp <- tibble()
#Generate city list for each disease
  city_list <- unique(data$city_code)
#Loop for cities
  for (city_code_i in city_list) {
    #Calculate attibutable number in each city
    temp <- rbind(temp, day_fun(data,city_code_i,pollution_i,disease_cat_i)) %>% 
      distinct() 
  }
  re_HAN  <- rbind(temp,re_HAN) 
  
    
}
#Total attributable LOS
fin_HAN <- aggregate(re_HAN[, c(2:4,11)], list(re_HAN$disease,re_HAN$year), sum,na.rm = T)


#Calculate annual attributable fractions of LOS and its 95% CIs
#Annual attributable fraction = Annual attributable numbers of LOS/Annual total number of LOS
fin_HAN["AF_all"] <- fin_HAN["all_ann"] / fin_HAN["final_total_num.x"] * 100
fin_HAN["AF_low"] <- fin_HAN["low"] / fin_HAN["final_total_num.x"]  * 100
fin_HAN["AF_high"] <- fin_HAN["high"] / fin_HAN["final_total_num.x"]  * 100

#Calculate total attributable fractions for LOS and its 95% CIs
#Total attributable fractions =Total attributable numbers of LOS/Total number of LOS
fin_HAN_total <- aggregate(fin_HAN[, c(3:6)], list(fin_HAN$Group.1), sum,na.rm = T)
fin_HAN_total["AF_all"] <- fin_HAN_total["all_ann"] / fin_HAN_total["final_total_num.x"] * 100
fin_HAN_total["AF_low"] <- fin_HAN_total["low"] / fin_HAN_total["final_total_num.x"] * 100
fin_HAN_total["AF_high"] <- fin_HAN_total["high"] / fin_HAN_total["final_total_num.x"] * 100
