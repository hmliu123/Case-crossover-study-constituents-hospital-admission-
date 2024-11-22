##########################################################################################################
#Codes for "Associations of reduced Fine Particulate Air Pollution with Cause-specific Hospital Admissions"
#Authors for codes: Huimeng Liu, Jian Lei, Yunxing Jiang, Lijun Bai et.al.
#Correspondence to Shaowei Wu, Yuewei Liu.
###########################################################################################################
library(wql)
####################Mann-Kendall trend test for pollution concentration in 292 cities######################

pollution <- c("PM2.5","BC","OM","SO4","NO3","NH4","cl")
test_res <- tibble()

data <- read.csv("5city_pol_2013_2017.csv")

city <- unique(data$city_code)

for (pol_i in pollution){
  for (city_i in city) {
    cat(city_i," ")
    sub <- data %>% 
      filter(city_code == city_i)
  pol <- sub[,pol_i]
  
  
  start_date <- as.Date("2013-01-01")
  end_date <- as.Date("2017-12-31")
  ts_data <- ts(pol, start = start_date, frequency = 365)
  
  res <- mannKen(ts_data)
  
  p <- res$p.value
  sen_slope <- round(res$sen.slope,2)
  
  temp <- data.frame(pollution = pol_i,
                     sen_slope = sen_slope,
                     p = p,
                     city= city_i)
  
  test_res <- rbind(test_res,temp)
  }
}
