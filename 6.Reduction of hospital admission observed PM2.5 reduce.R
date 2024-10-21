############################Reduction of hospital admission#############################
#计算组分浓度每降低1μg/m3所对应降低的住院人数:H =(exp(β × Δx) − 1) × N,
#8.8 计算组分浓度每降低1μg/m3,PM2.5每降低10，所对应降低的住院人数:H =(exp(β × Δx) − 1) × N,
#9.17 按照每年平均下降浓度（Theil estimator）计算
#9.28 按照每年平均下降浓度（Theil estimator）计算，住院人数用5年的平均人数
#10.9污染物的累积下降
rm(list = ls())

library(rio)
library(grid)
library(tidyverse)
library(ggsci)#sci配色方案
library(scales) #调用show_col函数显示颜色
setwd("E:\\OneDrive\\11_Doctor research\\17.PM2.5组分健康效应")
final <- NULL

#计算5年平均住院人数

subtype <- c("Asthma","LRI",
             "Cardiac arrhythmias","CHD","Heart failure","Stroke",
             "Depression","Schizophrenia","Parkinson's disease","CKD")

for (m in subtype){
     cat(m,"")
     #导入数据
     ckd_com <- read.csv(paste0("住院//Analysis//All disease//",m,"_compont.csv")) %>% 
          mutate(date = ymd(date)) 
     
     ann <-   aggregate(PSN_NO ~ year(date), sum,data = ckd_com) %>% 
          mutate(disease = m)
     
     
     
     final <- rbind(final,ann)
}

#write.csv(final,"住院\\组分\\Result\\V8_NM返修1\\Annual reduction\\5年平均住院人数.csv")
write.csv(final,"住院\\组分\\Result\\V8_NM返修1\\Annual reduction\\每年总住院人数.csv")

#############################################
#住院人数
#PM2.5每降低10,组分每降低1所降低的住院数
#导入各疾病主效应结果（beta se）
###############################

pollution <- c("PM2.5")
final <- read.csv("住院\\组分\\Result\\V8_NM返修1\\Annual reduction\\每年总住院人数.csv")

disease <- c("Asthma","LRI",
             "Cardiac arrhythmias","CHD","Heart failure","Stroke",
             "Depression","Schizophrenia","Parkinson's disease","CKD")
final_pol <- NULL
final_dis <- NULL

for (a in disease){
     dir <- "住院\\组分\\Result\\V6.0\\Table\\单组分模型\\"
     
     cat (a,"")
     
     data1 = import(paste0(dir,a,"_","单组分模型结果-Weilag21.csv")) %>% 
          filter(indicator == "lag01") %>% 
          mutate(disease = a) %>% 
          select(disease,pollution,beta,se) #数据导入
     
     #data1 <- left_join(data1,final)
     ann <- final %>% 
          filter(disease == a)
     
     for (pollution_i in pollution){
          test <- data1 %>% 
               filter(pollution == pollution_i) 
          test <- test %>% 
               mutate(H = (exp(test$beta*4.23)-1)*ann[2,3]+
                           (exp(test$beta*4.23*2)-1)*ann[3,3]+
                           (exp(test$beta*4.23*3)-1)*ann[4,3]+
                           (exp(test$beta*4.23*3)-1)*ann[5,3],
                      H_L = (exp((test$beta-1.96*se)*4.23)-1)*ann[2,3]+
                           (exp((test$beta-1.96*se)*4.23*2)-1)*ann[3,3]+
                           (exp((test$beta-1.96*se)*4.23*3)-1)*ann[4,3]+
                           (exp((test$beta-1.96*se)*4.23*4)-1)*ann[5,3],
                      H_H = (exp((test$beta+1.96*se)*4.23)-1)*ann[2,3]+
                           (exp((test$beta+1.96*se)*4.23*2)-1)*ann[3,3]+
                           (exp((test$beta+1.96*se)*4.23*3)-1)*ann[4,3]+
                           (exp((test$beta+1.96*se)*4.23*4)-1)*ann[5,3])
          
          
          
          
          final_pol <- rbind(final_pol,test)
          test <- NULL
     }
     
     final_dis <- rbind(final_dis,final_pol)
}

final_dis <- final_dis %>% 
     distinct()



pollution <- c("BC") 
for (a in disease){
     dir <- "住院\\组分\\Result\\V6.0\\Table\\单组分模型\\"
     
     cat (a,"")
     
     data1 = import(paste0(dir,a,"_","单组分模型结果-Weilag21.csv")) %>% 
          filter(indicator == "lag01") %>% 
          mutate(disease = a) %>% 
          select(disease,pollution,beta,se) #数据导入
     
     #data1 <- left_join(data1,final)
     ann <- final %>% 
          filter(disease == a)
     
     for (pollution_i in pollution){
          test <- data1 %>% 
               filter(pollution == pollution_i) 
          test <- test %>% 
               mutate(H = (exp(test$beta*0.17)-1)*ann[2,3]+
                           (exp(test$beta*0.17*2)-1)*ann[3,3]+
                           (exp(test$beta*0.17*3)-1)*ann[4,3]+
                           (exp(test$beta*0.17*3)-1)*ann[5,3],
                      H_L = (exp((test$beta-1.96*se)*0.17)-1)*ann[2,3]+
                           (exp((test$beta-1.96*se)*0.17*2)-1)*ann[3,3]+
                           (exp((test$beta-1.96*se)*0.17*3)-1)*ann[4,3]+
                           (exp((test$beta-1.96*se)*0.17*4)-1)*ann[5,3],
                      H_H = (exp((test$beta+1.96*se)*0.17)-1)*ann[2,3]+
                           (exp((test$beta+1.96*se)*0.17*2)-1)*ann[3,3]+
                           (exp((test$beta+1.96*se)*0.17*3)-1)*ann[4,3]+
                           (exp((test$beta+1.96*se)*0.17*4)-1)*ann[5,3])
          
          
          final_pol <- rbind(final_pol,test)
          test <- NULL
     }
     
     final_dis <- rbind(final_dis,final_pol)
}

final_dis <- final_dis %>% 
     distinct()


write.csv(final_dis,"住院\\组分\\Result\\V8_NM返修1\\Annual reduction\\Theil_pol_总减少的住院人数(累计).csv")

###################################画图#####################################
#选择配色方案
library(ggsci)
library(scales)

################################################
plot_data <- read.csv("住院\\组分\\Result\\V8_NM返修1\\Annual reduction\\Theil_pol_总减少的住院人数(累计).csv") %>% 
     mutate(RR = exp(beta)) %>% 
     select(X,pollution,disease,H,H_L,H_H) %>% 
     mutate(H = if_else(H_L<0,0,H),
            H_L = if_else(H_L<0,0,H_L),
            H_H = if_else(H_L==0,0,H_H))

plot_data <- plot_data %>% 
     filter(disease != "Asthma") 

plot_data$pollution <- factor(plot_data$pollution,levels= c("PM2.5","BC"))

disease <- c("LRI",
             "Cardiac arrhythmias","CHD","Heart failure","Stroke",
             "Depression","Schizophrenia","Parkinson's disease","CKD")

plot_data$disease <- factor(plot_data$disease,levels= disease)
plot_data <- plot_data[order(plot_data$disease),]

p <- plot_data %>% 
     ggplot( aes(x=disease, y=H,fill = pollution)) +
     geom_bar(stat="identity", width=0.8,position = "dodge") +
     geom_errorbar(aes(ymin=H_L, ymax=H_H),stat="identity",
                   position = position_dodge(width=0.8),
                   linewidth=0.3,width = 0.3)+  #width 控制上下短线的长度
     ylab(expression(paste("Avoided number of hospital admissions"))) +
     #title("PM2.5")+
     xlab("Cause-specific hospital admissions")+
     theme_light() +
     theme(plot.title = element_text(hjust = 0.5))+
     scale_fill_lancet()+
     #scale_fill_manual(values =mycolor)+
     theme(plot.title = element_text(hjust = 0.5,size = 22)) +
     theme(axis.title.x = element_text(size =15)) +
     theme(axis.title.y = element_text(size = 15)) +
     theme(axis.text.x = element_text(size = 13)) +
     theme(axis.text.y = element_text(size = 13))+
     theme(legend.text =  element_text(size = 18))+
     theme(strip.background = element_rect(fill = "white"))+
     theme(strip.text.x = element_text(color = "black"))
p

