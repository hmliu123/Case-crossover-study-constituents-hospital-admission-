##########################################################################################################
#Codes for "Hospital admissions attributable to reduced air pollution due to clean-air policies in China"
#Authors for codes: Huimeng Liu, Jian Lei, Yunxing Jiang, Lijun Bai, et.al.
#Correspondence to Shaowei Wu, Yuewei Liu.
###########################################################################################################


####################################################################################################################################################
#Total avoided numbers of cause-specific hospital admissions attributable to cumulative reductions of PM2.5 and 
#black carbon during 2014-2017 compared with 2013. (Figure 3)
##################################################################################################################################################
rm(list = ls())

library(rio)
library(tidyverse)
final <- NULL


################################################################################################
#Total avoided numbers attributable to cumulative reductions in PM2.5 and black carbon
#（cumulative reductions were estimated according to annual absolute declining rates (μg/m3 per year) 
#############################################################################################

pollution_i <- c("PM2.5lag01")

final_pol <- NULL
final_dis <- NULL

#Import estimation from single-constituent model
    data1 <- read.csv(paste0("Result\\single_pol_test.csv"))
#Import annual hospital admission data     
    ann <- import("Result/Annual_num.xlsx") 
     #Loop for PM2.5 and BC
test <- data1 %>% 
     filter(pollution == pollution_i) 
#Annual absolute decline rate was 4.23 μg/m3 per year for PM2.5 and 0.17 μg/m3 per year for BC
test <- test %>% 
  mutate(H = (exp(test$beta*4.23)-1)*ann[2,2]+
           (exp(test$beta*4.23*2)-1)*ann[3,2]+
           (exp(test$beta*4.23*3)-1)*ann[4,2]+
           (exp(test$beta*4.23*4)-1)*ann[5,2],
         H_L = (exp((test$beta-1.96*se)*4.23)-1)*ann[2,2]+
                (exp((test$beta-1.96*se)*4.23*2)-1)*ann[3,2]+
                (exp((test$beta-1.96*se)*4.23*3)-1)*ann[4,2]+
                (exp((test$beta-1.96*se)*4.23*4)-1)*ann[5,2],
        H_H = (exp((test$beta+1.96*se)*4.23)-1)*ann[2,2]+
              (exp((test$beta+1.96*se)*4.23*2)-1)*ann[3,2]+
              (exp((test$beta+1.96*se)*4.23*3)-1)*ann[4,2]+
              (exp((test$beta+1.96*se)*4.23*4)-1)*ann[5,2])
          
          
          
          

          

