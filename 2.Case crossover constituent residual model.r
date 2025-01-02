##########################################################################################################
#Codes for "Hospital admissions attributable to reduced air pollution due to clean-air policies in China"
#Authors for codes: Huimeng Liu, Jian Lei, Yunxing Jiang, Lijun Bai, et.al.
#Correspondence to Shaowei Wu, Yuewei Liu.
###########################################################################################################

##########################################################################################################
# Case-crossover analysis for constituent residual model (Supplementary Table 4)
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

ckd_com <- left_join(ckd_com,b,by = c("city_code","date","district"))
#Calculate residuals for 292 cities to further obtain IQR of constituent residuals
pollution <- c("BC","OM","SO4","NO3","NH4","cl")
city <- unique(b$district)
final <- tibble()
for (i in city){
  sub <- b %>% 
    filter(district == i)
  
  # Concentrations of PM2.5 and its constituents at lag01 time window 
  sub$PM2.5lag01 = roll_mean(sub$PM2.5, n = 2, align = "right", fill = NA)
  sub$BClag01 = roll_mean(sub$BC, n = 2, align = "right", fill = NA)
  sub$OMlag01 = roll_mean(sub$OM, n = 2, align = "right", fill = NA)
  sub$SO4lag01 = roll_mean(sub$SO4, n = 2, align = "right", fill = NA)
  sub$NO3lag01 = roll_mean(sub$NO3, n = 2, align = "right", fill = NA)
  sub$NH4lag01 = roll_mean(sub$NH4, n = 2, align = "right", fill = NA)
  sub$cllag01 = roll_mean(sub$cl, n = 2, align = "right", fill = NA)
  
  #Loop for constituents
  for (pollution_i in pollution) {
    x <- "PM2.5"
    #Linear model for PM2.5 and constituents
    lin_reg <- lm(unlist(sub[,pollution_i])~unlist(sub[,x]),na.action = na.exclude)
    
    #Constituent residual
    sub[,paste0(pollution_i,"res")] <- resid(lin_reg)
    sub[,paste0(pollution_i,"reslag01")] <- roll_mean(sub[,paste0(pollution_i,"res")], n = 2, align = "right", fill = NA)
    
  }
  final <- rbind(final,sub)
}

# Fuction for running model in each city
city_RR <-  function(dataset,pollution_i){
  # LOOP FOR CITIES
  c <- as.character(pollution_i)
  
  lag01_res<-paste0(c,"lag01","res")
  lag01<-paste0(c,"lag01")
 
 #Loop for cities
  for(i in regions) {
    
    # Print
    cat(i,"")
    
    # Load city data
    sub <- data[[i]]
    ################################################################
    lag_indicator <- "lag01"
    
    #Generate constituent residuals at main time window
    for (lag_i in lag_indicator){
      x <- paste0("PM2.5")
      #Linear regression model for PM2.5 and its constituents
      lin_reg <- lm(unlist(sub[,paste0(pollution_i)])~unlist(sub[,x]),na.action = na.exclude)
      #Constituent residual
      sub[,paste0(pollution_i,"res")] <- resid(lin_reg)
      #Consituent residual at lag01
      sub[,paste0(pollution_i,"lag01res")] <- roll_mean(unlist(sub[,paste0(pollution_i,"res")]), n = 2, align = "right", fill = NA)
      
    }
    #Function that can transform data into case-crossover dataset 
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
    #Generate case-crossover dataset for each city
    data_case_crossover <- funccmake(sub$stratum,sub$PSN_NO,   
                                     vars=cbind(temperature=sub$templag21, humidity = sub$humlag21, Holiday = sub$Holiday,
                                                city_code = sub$city_code,PM2.5lag01 = sub$PM2.5lag01,PM2.5lag0 = sub$PM2.5lag0,
                                                lag01=sub[,lag01],
                                                lag01_res=sub[,lag01_res]))
   #Empty tibble to store the results 
    final_result = tibble()
    lag_indicator <- "lag01"
    for (lag_i in lag_indicator) {
      
      data_case_crossover["analysis"] <- data_case_crossover[paste0("lag01","_res")] 
      
      data_case_crossover["analysis2"] <- data_case_crossover[paste0("PM2.5","lag01")] 
      
      ################################################################################# 
      #Run conditioanl logistic regression model
      
      timeout <- as.numeric(factor(data_case_crossover$stratum))
      timein <- timeout-0.1
      model_clr <- coxph(Surv(timein,timeout,status) ~ analysis+ analysis2 + ns(temperature, df=6) + ns(humidity, df=3) + as.factor(Holiday), 
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
  }
  all_city_result <- data.table::rbindlist(model_re,use.names = T)
  all_city_result$beta <- as.numeric(all_city_result$beta)
  all_city_result$se <- as.numeric(all_city_result$se)
  return(all_city_result)
}

#####################################################################################
#Fuction for random-effects model of each constituent
meta <- function(dataset,pollution_i){
 #Filter city-specific estimations for each constituent
  temp1 <- dataset %>% 
    filter(pollution == pollution_i)
 #Use lag01 as the main time window
  lag_indicator <- "lag01"
  temp <- lapply(lag_indicator,function(x) temp1[temp1$indicator==x,])
  names(temp) <- lag_indicator
  
  #Loop for lags
  for (lag_indicator_i in lag_indicator) {
    cat(lag_indicator_i,"")
    
    # Load
    sub <- temp[[lag_indicator_i]]
    #Calculate IQR for constituent residuals
    IQR_p <- quantile(final[,paste0(pollution_i,"res")], 0.75,na.rm = T) -
      quantile(final[,paste0(pollution_i,"res")], 0.25,na.rm = T) 
    
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
 
  #Run the loop
  for (pollution_i in pollution) { 
    #City number in each cause-specific hospital admission  
    regions <- unique(ckd_com$district)
    #Arrange the data as a list of data sets
    data <- lapply(regions,function(x) ckd_com[ckd_com$district==x,])
    names(data) <- regions
    
    #Generate a vector to store model results    
    model_re <- vector("list",length(data))
    names(model_re) <- regions
    
    #city-specific results for each constituent
    all_city_result <- city_RR(ckd_com,pollution_i)
    #Random-effects model to pool the results for each constituent
    temp <- meta(all_city_result,pollution_i)
    #Bind results of all constituents
    final_result <- rbind(final_result,temp)
  }
 #Calculate percent change 
  
  final_result <- final_result %>% 
    distinct()
  
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
  write.csv(final_result,"Result\\res_pol.csv")
