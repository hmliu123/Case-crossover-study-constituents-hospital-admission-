##########################################################################################################
#Codes for "Cause-Specific Hospital Admissions Attributable to Reduced Fine Particulate Air Pollution"
#Authors for codes: Huimeng Liu, Jian Lei, Yunxing Jiang, Lijun Bai et.al.
#Correspondence to Shaowei Wu, Yuewei Liu.
###########################################################################################################

##########################################################################################################
# Case cross-over analysis for PM2.5 minus constituents model (Supplementary Tbale 4)
###########################################################################################################
rm(list=ls())
library(dplyr)
library(tidyverse)
library(lubridate) 
library(splines) 
library(survival)
library(metafor)
library(RcppRoll)
library(rio)


#Import data
b <- read.csv("test_data_pol.csv") %>% 
     mutate(date = as.Date(date))

ckd_com <- import("test_data.csv") %>% 
     mutate(date = as.Date(date))

# Fuction for running model in each city
city_RR <-  function(dataset,pollution_i){
  c <- as.character(pollution_i)
 
  lag01<-paste0(c,"lag01")
  
   # Loop for cities
  
  for(i in regions) {
    
    # PRINT
    cat(i,"")
    
    # LOAD
    sub <- data[[i]]
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
    sub$templag21 = roll_mean(sub$meantem, n = 22, align = "right", fill = NA)
    sub$humlag21 = roll_mean(sub$rh, n = 22, align = "right", fill = NA)
    
    #Generate case crossover dataset for each city
    data_case_crossover <- funccmake(sub$stratum,sub$PSN_NO,   
                                     vars=cbind(temperature=sub$templag21, humidity = sub$humlag21, Holiday = sub$Holiday,
                                                city_code = sub$city_code,
                                                lag01=sub[,lag01],
                                                PM2.5lag01 = sub$PM2.5lag01))
    
    #Calculate PM2.5 minus constituents at lag01
    data_case_crossover$other = data_case_crossover$PM2.5lag01-data_case_crossover[,paste0("lag01")] 
    #Empty tibble to store the results 
    final_result = tibble()
    lag_i <- "lag01"

    data_case_crossover["analysis"] <- data_case_crossover[paste0(lag_i)]  
    data_case_crossover["PM2.5"] <- data_case_crossover$PM2.5lag01  
    
      
      ################################################################################# 
      # Run conditioanl logistic regression model
      
      timeout <- as.numeric(factor(data_case_crossover$stratum))
      timein <- timeout-0.1
      model_clr <- coxph(Surv(timein,timeout,status) ~ analysis + other+PM2.5+ ns(temperature, df=6) + ns(humidity, df=3) + as.factor(Holiday), 
                         weights=weights, data_case_crossover)
      #Extract results
      x <- summary(model_clr)
      beta<-signif(x$coef[1], digits=5)
      coefss = x$coefficients
      se = coefss[1,3]
      p.value<-coefss[1,5]
      result <- data.frame(cbind(indicator = lag_i,
                                 se = se, 
                                 p_value = p.value,
                                 n = x$n,
                                 beta = beta,
                                 event = x$nevent,
                                 district = i,
                                 pollution = pollution_i))
      final_result <- rbind(final_result, result)
      
      model_re[[i]] <- final_result
    }
  
  all_city_result <- data.table::rbindlist(model_re,use.names = T)
  all_city_result$beta <- as.numeric(all_city_result$beta)
  all_city_result$se <- as.numeric(all_city_result$se)
  return(all_city_result)
}

#####################################################################################
#Fuction for random-effects model
meta <- function(dataset,pollution_i){
  #Filter city-specific estimations for each constituents
  temp1 <- dataset %>% 
    filter(pollution == pollution_i)
  #Use lag01 as the main time window
  lag_indicator <- "lag01"
  temp <- lapply(lag_indicator,function(x) temp1[temp1$indicator==x,])
  names(temp) <- lag_indicator

  #Loop for lags
  for (lag_indicator_i in lag_indicator) {
    cat(lag_indicator_i,"")
    
   
    sub <- temp[[lag_indicator_i]]
    IQR_p <- quantile(b[,paste0(pollution_i)], 0.75,na.rm = T) - 
      quantile(b[,paste0(pollution_i)], 0.25,na.rm = T)
    #Random effects model
    summary <- summary(rma(yi = beta,
                           sei = se,
                           data = sub,
                           method="DL")) 
    beta=summary$b
    se = summary$se
    RR <-exp(beta*IQR_p)
    RR_L <- exp((beta-1.96*se)*IQR_p)
    RR_H <- exp((beta+1.96*se)*IQR_p)
    percent_change <- RR-1
    percent_change_L <- RR_L-1
    percent_change_H <- RR_H-1
    res <- data.frame(cbind(indicator = lag_indicator_i,
                            RR = RR,
                            RR_L = RR_L,
                            RR_H = RR_H,
                            beta = beta,
                            se = se,
                            P_value = summary$pval,
                            percent_change = percent_change*100,
                            percent_change_L = percent_change_L*100,
                            percent_change_H = percent_change_H*100,
                            pollution = pollution_i,
                            IQR = IQR_p
                            
    ))
    final_result <- rbind(final_result,res)
  }
  
  return(final_result)
}



pollution <- c("BC","OM","SO4","NO3","NH4","cl")

#Loop for constituents

  final_result = tibble()
  all_city_result  = tibble()
  
   #Run the loop
  for (pollution_i in pollution) { 
  #City number in each cause-specific hospital admissions
    regions <- unique(ckd_com$district)
  
  #Arrange the data as a list of data sets  
    data <- lapply(regions,function(x) ckd_com[ckd_com$district==x,])
    names(data) <- regions
    
    #Generate a vector to store model results    
    model_re <- vector("list",length(data))
    names(model_re) <- regions
   
    #City-specific results 
    temp_city <- city_RR(ckd_com,pollution_i)
    #Bind city-specific results into one file 
    all_city_result <- rbind(all_city_result,temp_city)
    #Randome effects model to pool the results for each constituents
    temp <- meta(all_city_result,pollution_i)
    #Bind results of all constituents
    final_result <- rbind(final_result,temp)
  }
  
 #Calculate percent change 
  
  final_result <- final_result %>% 
    rename(RR = V2,
           RR_L = V3,
           RR_H = V4,
           beta = V5,
           percent_change = V8,
           percent_change_L = V9,
           percent_change_H = V10) 
  final_result <- final_result %>%   
    mutate(percent_change = round(as.numeric(percent_change),2),
           percent_change_L = round(as.numeric(percent_change_L),2),
           percent_change_H = round(as.numeric(percent_change_H),2))
  final_result <- final_result %>%        
    mutate(value = sprintf("%.2f (%.2f, %.2f)",
                           final_result$percent_change, 
                           final_result$percent_change_L, 
                           final_result$percent_change_H)) %>% 
    distinct()
   

