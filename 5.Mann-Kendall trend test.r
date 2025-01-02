##########################################################################################################
#Codes for "Hospital admissions attributable to reduced air pollution due to clean-air policies in China"
#Authors for codes: Huimeng Liu, Jian Lei, Yunxing Jiang, Lijun Bai, et.al.
#Correspondence to Shaowei Wu, Yuewei Liu.
###########################################################################################################
library(wql)
####################Theil median slope estimation and Mann-Kendall trend test for annual attributable fractions and air pollution (Table 2,Figure 1)######################
disease <- c("LRI",
             "Cardiac arrhythmias","CHD","Heart failure","Stroke",
             "Depression","Schizophrenia","Parkinson's disease","CKD")

pollution <- c("BC")
test_res <- tibble()
for (i in disease) {
  for (pol_i in pollution){
  data <- import("Result//BC_annual_AF.csv") %>% 
    select(disease, Year,AF,pollution)  %>% #AF
    filter(disease == i & pollution == pol_i)
  
  AF <- data$AF
  
  ts_data <- ts(AF, start=c(2013), frequency=1)
  
  res <- mannKen(ts_data)
  
  p <- res$p.value
  sen_slope <- round(res$sen.slope,2)
  
  temp <- data.frame(disease = i,
                     pollution = pol_i,
                     sen_slope = sen_slope,
                     p = p)
  
  test_res <- rbind(test_res,temp)
  }
}
