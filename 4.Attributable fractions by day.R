##########################################################################################################
#Codes for "Associations of Improved Fine Particulate Air Pollution with Cause-specific Hospital Admissions"
#Authors for codes: Huimeng Liu, Jian Lei, Yunxing Jiang, Lijun Bai et.al.
#Correspondence to Shaowei Wu, Yuewei Liu.
###########################################################################################################


####################################################################################################################################################
#Annual and total attributable fractions for hospital admissions (Table 2, Figure 3, Table S4, Table S5)
##################################################################################################################################################
rm(list=ls())

library(tidyverse)
library(lubridate)
library(skimr) 
library(janitor) 
library(rio)
library(dlnm)
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
 
#Import exposure-response relationships obtained form single constituent model
meta_relag <- read_csv("\\Result\\Single consituents model 10 disease.csv") 
rr <- as.data.frame(meta_relag, stringsAsFactors = F)
#Generate empty list to store the results
temp_vars <- c("RR", "RR_L", "RR_H")
rr[temp_vars] <- sapply(rr[temp_vars], as.numeric)
re_HAN <- NULL

pollution_i <- c("PM2.5lag01") #PM2.5lag01 OMlag01 BClag01 SO4lag01 NO3lag01 NH4lag01 cllag01
disease_cat <- c("Asthma","LRI",
                 "Cardiac arrhythmias","CHD","Heart failure","Stroke",
                 "Depression","Schizophrenia","Parkinson's disease","CKD")


#Loop for cause-specific hospital admissions
for (disease_cat_i in disease_cat){
     cat(disease_cat_i," ")
    file<-list.files("/Analysis//All disease/")
    
     
   dataset = import(paste0("/Analysis//All disease/",disease_cat_i,"_compont.csv")) meantem,rh,city_code,district) 
    #Generate city list for each disease
     city_list <- unique(dataset$city_code)
    
     ann_final <- tibble()
     for (city_code_i in city_list) {
          cat(city_code_i," ")
          #Extract data of city_code_i
          sub <- dataset %>% 
               filter(city_code == city_code_i)
         #Calculate concentrations of PM2.5 and its major constituents at lag01
          sub$PM2.5lag01 = roll_mean(sub$PM2.5, n = 2, align = "right", fill = NA)
          sub$SO4lag01 = roll_mean(sub$SO4, n = 2, align = "right", fill = NA)
          sub$NO3lag01 = roll_mean(sub$NO3, n = 2, align = "right", fill = NA)
          sub$NH4lag01 = roll_mean(sub$NH4, n = 2, align = "right", fill = NA)
          sub$OMlag01 = roll_mean(sub$OM, n = 2, align = "right", fill = NA)
          sub$BClag01 = roll_mean(sub$BC, n = 2, align = "right", fill = NA)
          
          #Annual hospital admissions
          sum_add <- aggregate(sub$PSN_NO, list(format(sub$date, "%Y")), FUN=sum, na.rm=T) %>% 
               rename(PSN_NO = x)
          
          #Filter effect esitmations of each constituent and cause-specific hospital admissions based on single constituent model
          rr_use <- subset(rr, disease_group==disease_cat_i & pollution == pollution_i )
          sub["pol"] <- sub[,pollution_i]

          #Daily atributable number and its 95% CI of hospital admissions associted with constituent "pol" in city i
 
          sub <- sub %>% 
               mutate(AN = (exp(log(rr_use$RR)*pol)-1)/exp(log(rr_use$RR)*pol)*PSN_NO,
                      AN_L = (exp(log(rr_use$RR_L)*pol)-1)/exp(log(rr_use$RR_L)*pol)*PSN_NO,
                      AN_H = (exp(log(rr_use$RR_H)*pol)-1)/exp(log(rr_use$RR_H)*pol)*PSN_NO)
          
          #Annual attributable number in city_code_i
          sub_ann <- aggregate(sub[, c(28:30)], list(format(sub$date, "%Y")), FUN=sum, na.rm=T)
          sub_ann <- sub_ann %>% 
               mutate(city_code = city_code_i,
                      disease = disease_cat_i)
          sub_ann <- left_join(sub_ann,sum_add)
          
          ann_final  <- rbind(ann_final,sub_ann)
          
          
     }
     
     
     #Annual attributable numbers for each cause-specific hospital admissions of the included cities
     af_all <- ann_final %>% 
          group_by(disease,`year(date)`) %>% 
          summarise(AN = sum(AN,na.rm = T),PSN_NO = sum(PSN_NO,na.rm=T),
                    AN_L = sum(AN_L,na.rm = T),AN_H = sum(AN_H,na.rm = T))
    
    #Annual attributable fractions for each cause-specific hospital admissions of the included cities
     af_all <- af_all %>% 
          mutate(AF = (AN/PSN_NO)*100,
                 AF_L = (AN_L/PSN_NO)*100,
                 AF_H = (AN_H/PSN_NO)*100)
     #Total attributable fractions for each cause-specific hospital admissions of the included cities
      ann_final <- ann_final %>% 
          mutate(AF = (AN/PSN_NO)*100,
                 AF_L = (AN_L/PSN_NO)*100,
                 AF_H = (AN_H/PSN_NO)*100)
      
}
