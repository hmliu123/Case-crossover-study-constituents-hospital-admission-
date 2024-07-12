##########################################################################################################
#Codes for "Associations of Improved Fine Particulate Air Pollution with Cause-specific Hospital Admissions"
#Authors for codes: Huimeng Liu, Jian Lei, Yunxing Jiang, Lijun Bai et.al.
#Table 1: Descriptive statistics of daily environmental data
###########################################################################################################


remove(list = ls())
library(tidyverse)
library(lubridate) 

#Read pollution data
main_data <- read.csv("Data/292city_pol_wei.csv")
#Mean IQR SD quantile for environmental variables
result <- t(data.frame(`PM2.5_mean` = mean(main_data$PM2.5,na.rm = T),
                       `BC mean` = mean(main_data$BC,na.rm = T),
                       `OM mean` = mean(main_data$OM,na.rm = T),
                       `SO4_mean` = mean(main_data$SO4,na.rm = T),
                       `NO3_mean` = mean(main_data$NO3,na.rm = T),
                       `NH4_mean` = mean(main_data$NH4,na.rm = T),
                       `cl_mean` = mean(main_data$cl,na.rm = T),
                       `tem_mean` = mean(main_data$meantem,na.rm = T)/10,
                       `rh_mean` = mean(main_data$rh,na.rm = T),
                       
                       
                       `PM2.5_IQR` = IQR(main_data$PM2.5,na.rm = T),
                       `BC IQR` = IQR(main_data$BC,na.rm = T),
                       `OM IQR` = IQR(main_data$OM,na.rm = T),
                       `SO4_IQR` = IQR(main_data$SO4,na.rm = T),
                       `NO3_IQR` = IQR(main_data$NO3,na.rm = T),
                       `NH4_IQR` = IQR(main_data$NH4,na.rm = T),
                       `cl_IQR` = IQR(main_data$cl,na.rm = T),
                       `tem_IQR` = IQR(main_data$meantem,na.rm = T)/10,
                       `rh_IQR` = IQR(main_data$rh,na.rm = T),
                       
                       
                       `PM2.5_sd` = sd(main_data$PM2.5,na.rm = T),
                       `BC sd` = sd(main_data$BC,na.rm = T),
                       `OM sd` = sd(main_data$OM,na.rm = T),
                       `SO4_sd` = sd(main_data$SO4,na.rm = T),
                       `NO3_sd` = sd(main_data$NO3,na.rm = T),
                       `NH4_sd` = sd(main_data$NH4,na.rm = T),
                       `cl_sd` = sd(main_data$cl,na.rm = T),
                       `tem_sd` = sd(main_data$meantem,na.rm = T)/10,
                       `rh_sd` = sd(main_data$rh,na.rm = T),
                       
                       
                       `PM2.5_Q1` = quantile(main_data$PM2.5,0.01,na.rm = T),
                       `BC Q1` = quantile(main_data$BC,0.01,na.rm = T),
                       `OM Q1` = quantile(main_data$OM,0.01,na.rm = T),
                       `SO4_Q1` = quantile(main_data$SO4,0.01,na.rm = T),
                       `NO3_Q1` = quantile(main_data$NO3,0.01,na.rm = T),
                       `NH4_Q1` = quantile(main_data$NH4,0.01,na.rm = T),
                       `cl_Q1` = quantile(main_data$cl,0.01,na.rm = T),
                       `tem_Q1` = quantile(main_data$meantem,0.01,na.rm = T)/10,
                       `rh_Q1` = quantile(main_data$rh,0.01,na.rm = T),
                       
                       
                       `PM2.5_Q99` = quantile(main_data$PM2.5,0.99,na.rm = T),
                       `BC Q99` = quantile(main_data$BC,0.99,na.rm = T),
                       `OM Q99` = quantile(main_data$OM,0.99,na.rm = T),
                       `SO4_Q99` = quantile(main_data$SO4,0.99,na.rm = T),
                       `NO3_Q99` = quantile(main_data$NO3,0.99,na.rm = T),
                       `NH4_Q99` = quantile(main_data$NH4,0.99,na.rm = T),
                       `cl_Q99` = quantile(main_data$cl,0.99,na.rm = T),
                       `tem_Q99` = quantile(main_data$meantem,0.99,na.rm = T)/10,
                       `rh_Q99` = quantile(main_data$rh,0.99,na.rm = T),
                       
                       
                       `PM2.5_Q50` = quantile(main_data$PM2.5,0.50,na.rm = T),
                       `BC Q50` = quantile(main_data$BC,0.50,na.rm = T),
                       `OM Q50` = quantile(main_data$OM,0.50,na.rm = T),
                       `SO4_Q50` = quantile(main_data$SO4,0.50,na.rm = T),
                       `NO3_Q50` = quantile(main_data$NO3,0.50,na.rm = T),
                       `NH4_Q50` = quantile(main_data$NH4,0.50,na.rm = T),
                       `cl_Q50` = quantile(main_data$cl,0.50,na.rm = T),
                       `tem_Q50` = quantile(main_data$meantem,0.50,na.rm = T)/10,
                       `rh_Q50` = quantile(main_data$rh,0.50,na.rm = T)
))


