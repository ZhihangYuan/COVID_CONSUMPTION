#set the directory to your current working directory
setwd("E:/Pan/sharingtowho")
rm(list=ls())
# 
# library(remotes)
# 
# library(nCov2019)
library(ggplot2)
library(reshape2)
library(stargazer)
library(AER)
library(ivpack)
library(plm)
library(dplyr)
library(tidyverse)
library(splm)
library(spdep)
library(spatialreg)
library(RColorBrewer)

# 

#read data for plotting
data_0 = read.csv("parallel_data.csv")

#ggplot for figure 1 in the manuscript
ggplot(aes(x=date_num, y= value, group = variable, color = variable), data = data_0[data_0$date_num<=51,]) +
  geom_line() + geom_point() + theme_bw()  + ylim(-2.5, 2) +
  theme(text = element_text(size = 15), axis.text = element_text(size = 15), 
        legend.text = element_text(size = 15)) + 
  geom_vline(xintercept=10, linetype="dashed", color = "black", size=1) +
  geom_vline(xintercept=20, linetype="dashed", color = "black", size=1) +
  geom_vline(xintercept=23, linetype="dashed", color = "black", size=1) +
  geom_vline(xintercept=25, linetype="dashed", color = "black", size=1) +
  geom_vline(xintercept=31, linetype="dashed", color = "black", size=1) +
  geom_vline(xintercept=35, linetype="dashed", color = "black", size=1)   


########################################
##############DID Models##########################
############################################
# This part shows the DID and DDD model to derive numbers for figure 2b and 3b. For figure 2a and 4a that only use data for January, you can simply add 
# a new constraint that new_data = test_data[test_data$date_num<=31,] and run the same models

# read and write ID-masked and normalized data
test_data = read.csv("feb_data.csv")

#data for figure 2b post "human to human transmission confirmation", using "total transaction" as an example
mod_did_ph = lm(log(total) ~ as.factor(year) + as.factor(post_zhong) + as.factor(year)*as.factor(post_zhong), 
                data = test_data)

summary(mod_did_ph)


#data for figure 2b post "wuhan lockdown", using "total transaction" as an example
mod_did_ps = lm(log(total) ~ as.factor(year) + as.factor(post_shut) + as.factor(year)*as.factor(post_shut), 
                data = test_data)

summary(mod_did_ps)

#data for figure 2b post "spring festival", using "total transaction" as an example
mod_did_pp = lm(log(total) ~ as.factor(year) + as.factor(post_sp) + as.factor(year)*as.factor(post_sp), 
                data = test_data)

summary(mod_did_pp)

#data for figure 3b post "human to human transmissions", using "total transaction" as an example
mod_did_1 = lm(log(total) ~ as.factor(year) + as.factor(post_zhong) + as.factor(year)*as.factor(post_zhong)
               +as.factor(hubei) + as.factor(year)*as.factor(hubei) + as.factor(post_zhong)*as.factor(hubei) + 
                 as.factor(post_zhong)*as.factor(hubei)*as.factor(year) +as.factor(cityname) -1  , 
               data = test_data)

summary(mod_did_1)$coefficient["as.factor(year)2020:as.factor(post_zhong)1:as.factor(hubei)1",]


#data for figure 3b post "hubei lockdown", using "total transaction" as an example
mod_did_1 = lm(log(total) ~ as.factor(year) + as.factor(post_shut) + as.factor(year)*as.factor(post_shut)
               +as.factor(hubei) + as.factor(year)*as.factor(hubei) + as.factor(post_shut)*as.factor(hubei) + 
                 as.factor(post_shut)*as.factor(hubei)*as.factor(year) +as.factor(cityname) -1  , 
               data = test_data)

#data for figure 3b post "spring festival", using "total transaction" as an example
summary(mod_did_1)$coefficient["as.factor(year)2020:as.factor(post_shut)1:as.factor(hubei)1",]

mod_did_1 = lm(log(total) ~ as.factor(year) + as.factor(post_sp) + as.factor(year)*as.factor(post_sp)
               +as.factor(hubei) + as.factor(year)*as.factor(hubei) + as.factor(post_sp)*as.factor(hubei) + 
                 as.factor(post_sp)*as.factor(hubei)*as.factor(year) +as.factor(cityname) -1  , 
               data = test_data)

summary(mod_did_1)$coefficient["as.factor(year)2020:as.factor(post_sp)1:as.factor(hubei)1",]

#data for figure 3b post "resume working", using "total transaction" as an example
mod_did_1 = lm(log(total) ~ as.factor(year) + as.factor(post_rw) + as.factor(year)*as.factor(post_rw)
               +as.factor(hubei) + as.factor(year)*as.factor(hubei) + as.factor(post_rw)*as.factor(hubei) + 
                 as.factor(post_rw)*as.factor(hubei)*as.factor(year) +as.factor(cityname) -1  , 
               data = test_data)

summary(mod_did_1)$coefficient["as.factor(year)2020:as.factor(post_rw)1:as.factor(hubei)1",]

#data for figure 3b post "resume delivery", using "total transaction" as an example
mod_did_1 = lm(log(total) ~ as.factor(year) + as.factor(post_rd) + as.factor(year)*as.factor(post_rd)
               +as.factor(hubei) + as.factor(year)*as.factor(hubei) + as.factor(post_rd)*as.factor(hubei) + 
                 as.factor(post_rd)*as.factor(hubei)*as.factor(year) +as.factor(cityname) -1  , 
               data = test_data)

summary(mod_did_1)$coefficient["as.factor(year)2020:as.factor(post_rd)1:as.factor(hubei)1",]

########################################
##############correlation plot##########################
############################################
#This part demonstrates the model to make figure 4 (figure 5 uses the same data but plotted with ArcGIS)
# note that the provname and cityname ID may not be the same match to the other datasets in this code (because city case numbers can be matched to masked city names)
# to avoid outlier numbers, the city of Wuhan and cities with less than 10 cases are removed. 

#read plot data
plot_data = read.csv("plot_data_masked.csv")



ggplot(aes(x = log(Case), y = Total), 
       data = plot_data) + geom_point() +
  theme_bw() + ylim(0.5,2)+
  theme(text = element_text(size = 20), axis.text = element_text(size = 20),
        legend.text = element_text(size = 20)) +
  # stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x) + labs(x ="Logarithm of total case number", y = "Pre and post Jan 20 \n average daily transaction ratio")


ggplot(aes(x = log(Case), y = Total, color = as.factor(hubei)), 
       data = plot_data) + geom_point() +
  theme_bw() + ylim(0.5,2)+
  theme(text = element_text(size = 20), axis.text = element_text(size = 20),
        legend.text = element_text(size = 20)) +
  # stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x, se = F) + labs(x ="Logarithm of total case number", y = "Pre and post Jan 20 \n average daily transaction ratio") +
  theme(legend.position = "none") 

########################################
##############conterfactual##########################
############################################
#code for drawing figure 6
test_data = read.csv("feb_data.csv")

#this is to remove cities that case numbers are not identify if UnionPay city identification has minor difference with the official COVID case release.
# we detect 12 cities out of total of 373
#levelx.x is the level of case as of Feb.28 (when China domestic case numbers have been stable) 0: Wuhan; 1: other cities in Hubei; 2: cities with >=100 cases;
#3: cities with >=50 cases; 4: cities with >=10 cases. 4: cities with <10 cases.
h_names = c("total","food")
ah_names = paste0(h_names,"_ratio") 

h_data = test_data[test_data$hubei == 1,]
h_data[,ah_names] = NA
h_data = test_data[is.na(test_data$Levelx.x)==F ,]
ah_names = paste0(h_names,"_ratio") 
h_data[,ah_names] = NA

for (i in 10:51){
  h_data[(h_data$date_num == i)& (h_data$year == 2020) ,ah_names] = 
    h_data[(h_data$date_num == i)& (h_data$year == 2020), paste0(h_names)]/
    h_data[(h_data$date_num == i) & (h_data$year == 2019), paste0(h_names)]  
} 



fact_10 = sapply(h_data[(h_data$date_num == 10)& (h_data$year == 2020), ah_names] ,  FUN = mean, na.rm = T)
fact_10
fact_10 = c(fact_10,10)
names(fact_10)[length(names(fact_10))] = "day_number"
fact_10

fact_11 = sapply(h_data[(h_data$date_num == 11)& (h_data$year == 2020), ah_names] ,  FUN = mean, na.rm = T)
fact_11
fact_11 = c(fact_11,11)
names(fact_11)[length(names(fact_11))] = "day_number"
fact_11

fact_data = data.frame(rbind(fact_10,fact_11))

for (i in 12:51){
  fact_day = sapply(h_data[(h_data$date_num == i)& (h_data$year == 2020), ah_names] ,  FUN = mean, na.rm = T)
  fact_day = c(fact_day, i)
  names(fact_day)[length(names(fact_day))] = "day_number"
  fact_data = rbind(fact_data, fact_day)
} 

fact_data$fact = "fact2020"


fact_23 = sapply(h_data[(h_data$date_num == 23)& (h_data$year == 2020), ah_names] ,  FUN = mean, na.rm = T)
fact_23

fact_24 = sapply(h_data[(h_data$date_num == 24)& (h_data$year == 2020), ah_names] ,  FUN = mean, na.rm = T)
fact_24

test_data$d23 = rep(0,length(test_data$cityname))
test_data$d23[test_data$date_num == 23] = 1

test_data$d24 = rep(0,length(test_data$cityname))
test_data$d24[test_data$date_num == 24] = 1

new_data = test_data[test_data$hubei == 1,]
new_data$hubei = 0


new_data$post_shut = rep(0,length(new_data$cityname)) 
new_data$post_shut[new_data$date_num >= 25] = 1
ah_names = paste0(h_names,"_ratio") 
new_data[,ah_names] = NA
# 

mod_did_1 = lm(log(total) ~ as.factor(year) + as.factor(post_shut) + as.factor(year)*as.factor(post_shut)
               +as.factor(hubei) + as.factor(year)*as.factor(hubei) + as.factor(post_shut)*as.factor(hubei) + 
                 as.factor(post_shut)*as.factor(hubei)*as.factor(year) +as.factor(cityname) -1 + as.factor(date_num) + as.factor(year)*as.factor(date_num), 
               data = test_data)

# res_did = summary(mod_did_1)$coefficient
summary(mod_did_1)$coefficient["as.factor(year)2020:as.factor(post_shut)1:as.factor(hubei)1",]
summary(mod_did_1)$coefficient["as.factor(date_num)23",]
summary(mod_did_1)$coefficient["as.factor(date_num)24",]



###########################################################################################################
mod_did_2 = lm(log(total) ~ as.factor(year) + as.factor(post_zhong) + as.factor(year)*as.factor(post_zhong)
               +as.factor(hubei) + as.factor(year)*as.factor(hubei) + as.factor(post_zhong)*as.factor(hubei) + 
                 as.factor(post_zhong)*as.factor(hubei)*as.factor(year) +as.factor(cityname) -1 + as.factor(date_num) + as.factor(year)*as.factor(date_num), 
               data = test_data)

# res_did = summary(mod_did_1)$coefficient
summary(mod_did_2)$coefficient["as.factor(year)2020:as.factor(post_zhong)1:as.factor(hubei)1",]
summary(mod_did_2)$coefficient["as.factor(date_num)23",]
summary(mod_did_2)$coefficient["as.factor(date_num)24",]

new_data$total2 = new_data$total

for (i in 21:22) {
  new_data[new_data$date_num == i,"total2"] = predict(mod_did_2, new_data[new_data$date_num == i,])
  new_data[new_data$date_num == i,"total2"] = exp(new_data[new_data$date_num == i,"total2"])
}


cf_data = fact_data
cf_data$fact = "counterfactual"

##################################################################################


for (i in 23:24) {
  new_data[new_data$date_num == i,"total2"] = predict(mod_did_1, new_data[new_data$date_num == i,])
  new_data[new_data$date_num == i,"total2"] = exp(new_data[new_data$date_num == i,"total2"])
}

for (i in 10:51){
  new_data[(new_data$date_num == i)& (new_data$year == 2020) ,"total_ratio"] = 
    new_data[(new_data$date_num == i)& (new_data$year == 2020), "total2"]/
    new_data[(new_data$date_num == i) & (new_data$year == 2019), "total2"]  
} 

cf_data = fact_data
cf_data$fact = "counterfactual"

for (i in 21:24){
  new_day = sapply(new_data[(new_data$date_num == i)& (new_data$year == 2020), ah_names] ,  FUN = mean, na.rm = T)
  new_day = c(new_day, i)
  names(new_day)[length(names(new_day))] = "day_number"
  cf_data[(cf_data$day_number == i),"total_ratio"] = new_day["total_ratio"]
} 

draw_data =fact_data
# draw_data = rbind(draw_data, old_data)
draw_data = rbind(draw_data, cf_data)

draw_data = melt(draw_data, id.vars = c("day_number","fact"), measure.vars = "total_ratio")


# draw figure 6a 
ggplot(aes(x=day_number, y= value, group = fact, color = fact), data = draw_data) + ylim(0, 2) +  xlim(10, 25) +
  geom_line(size = 1.5, data = draw_data,aes(linetype =draw_data$fact)) + theme_bw() +
  theme(text = element_text(size = 15), axis.text = element_text(size = 15),
        legend.text = element_text(size = 15)) + 
  # geom_ribbon(data = draw_data, alpha = 0.1, aes(x=day_number, ymin = value*(sigma^(-1)),ymax = value*sigma)) +
  geom_vline(xintercept=10, linetype="dashed", color = "black", size=0.5) +
  geom_vline(xintercept=20, linetype="dashed", color = "black", size=0.5) +
  geom_vline(xintercept=23, linetype="dashed", color = "black", size=0.5) +
  geom_vline(xintercept=25, linetype="dashed", color = "black", size=0.5) +
  geom_vline(xintercept=31, linetype="dashed", color = "black", size=0.5) +
  geom_vline(xintercept=35, linetype="dashed", color = "black", size=0.5) 

#####################################################################
# draw figure 6c for the worst-case scenario conterfactual 
# 6b is omitted. The only difference is set change new_data[(new_data$date_num > 24) & (new_data$Levelx.x <= 5),"hubei"] = 1 
# to  new_data[(new_data$date_num > 24) & (new_data$Levelx.x <= 2),"hubei"] = 1 


test_data = test_data[(test_data$date_num <= 51) & (test_data$date_num >= 10) ,]
h_data = test_data[is.na(test_data$Levelx.x)==F ,]
ah_names = paste0(h_names,"_ratio") 
h_data[,ah_names] = NA


div_df =  h_data[which(h_data$year == 2019),c("cityname","date_num",h_names)]
colnames(div_df)=c("cityname","date_num",paste0(h_names,"_div"))
h_data = merge(h_data, div_df, by = c("cityname","date_num"))
h_data[which(h_data$year == 2020) ,ah_names] = h_data[which(h_data$year == 2020) ,h_names] /
  h_data[which(h_data$year == 2020) ,paste0(h_names,"_div")]
h_data$paste0(h_names,"_div") = NULL

fact_10 = sapply(h_data[(h_data$date_num == 10)& (h_data$year == 2020), ah_names] ,  FUN = mean, na.rm = T)
fact_10
fact_10 = c(fact_10,10)
names(fact_10)[length(names(fact_10))] = "day_number"
fact_10

fact_11 = sapply(h_data[(h_data$date_num == 11)& (h_data$year == 2020), ah_names] ,  FUN = mean, na.rm = T)
fact_11
fact_11 = c(fact_11,11)
names(fact_11)[length(names(fact_11))] = "day_number"
fact_11

fact_data = data.frame(rbind(fact_10,fact_11))

for (i in 12:51){
  fact_day = sapply(h_data[(h_data$date_num == i)& (h_data$year == 2020), ah_names] ,  FUN = mean, na.rm = T)
  fact_day = c(fact_day, i)
  names(fact_day)[length(names(fact_day))] = "day_number"
  fact_data = rbind(fact_data, fact_day)
} 

fact_data$fact = "fact2020"

new_data = test_data[is.na(test_data$Levelx.x)==F ,]
new_data[new_data$date_num <=24,"hubei"] = 0 
new_data[is.na(new_data$Levelx.x),"Levelx.x"]=5
new_data[(new_data$date_num > 24) & (new_data$Levelx.x <= 5),"hubei"] = 1 
new_data$post_zhong = rep(0,length(new_data$cityname)) 
ah_names = paste0(h_names,"_ratio") 
new_data[,ah_names] = NA

#fit original model
mod_did_1 = lm(log(total) ~ as.factor(year) + as.factor(post_zhong) + as.factor(year)*as.factor(post_zhong)
               +as.factor(hubei) + as.factor(year)*as.factor(hubei) + as.factor(post_zhong)*as.factor(hubei) + 
                 as.factor(post_zhong)*as.factor(hubei)*as.factor(year) +as.factor(cityname) -1 + as.factor(date_num) + as.factor(year)*as.factor(date_num), 
               data = test_data)

summary(mod_did_1)$coefficient["as.factor(year)2020:as.factor(post_zhong)1:as.factor(hubei)1",]
summary(mod_did_1)$coefficient["as.factor(date_num)23",]
summary(mod_did_1)$coefficient["as.factor(date_num)24",]


mod_did_2 = lm(log(total) ~ as.factor(year) + as.factor(post_sp) + as.factor(year)*as.factor(post_sp)
               +as.factor(hubei) + as.factor(year)*as.factor(hubei) + as.factor(post_sp)*as.factor(hubei) + 
                 as.factor(post_sp)*as.factor(hubei)*as.factor(year) +as.factor(cityname) -1 +
                 as.factor(date_num) + as.factor(year)*as.factor(date_num), 
               data = test_data)

summary(mod_did_2)$coefficient["as.factor(year)2020:as.factor(post_sp)1:as.factor(hubei)1",]


mod_did_3 = lm(log(total) ~ as.factor(year) + as.factor(post_rw) + as.factor(year)*as.factor(post_rw)
               +as.factor(hubei) + as.factor(year)*as.factor(hubei) + as.factor(post_rw)*as.factor(hubei) + 
                 as.factor(post_rw)*as.factor(hubei)*as.factor(year) +as.factor(cityname) -1  +
                 as.factor(date_num) + as.factor(year)*as.factor(date_num),
               data = test_data)

summary(mod_did_3)$coefficient["as.factor(year)2020:as.factor(post_rw)1:as.factor(hubei)1",]

mod_did_4 = lm(log(total) ~ as.factor(year) + as.factor(post_rd) + as.factor(year)*as.factor(post_rd)
               +as.factor(hubei) + as.factor(year)*as.factor(hubei) + as.factor(post_rd)*as.factor(hubei) + 
                 as.factor(post_rd)*as.factor(hubei)*as.factor(year) +as.factor(cityname) -1 + 
                 as.factor(date_num) + as.factor(year)*as.factor(date_num),
               data = test_data)

summary(mod_did_4)$coefficient["as.factor(year)2020:as.factor(post_rd)1:as.factor(hubei)1",]


#change data

new_data$total2 = new_data$total
new_data$sigma = NA


for (i in 21:24) {
  new_data[new_data$date_num == i,"total2"] = predict(mod_did_1, new_data[new_data$date_num == i,])
  new_data[new_data$date_num == i,"total2"] = exp(new_data[new_data$date_num == i,"total2"])
  new_data[new_data$date_num == i,"sigma"] = summary(mod_did_1)$sigma
  new_data[new_data$date_num == i,"sigma"] = exp(new_data[new_data$date_num == i,"sigma"]*1.645)
  
}

for (i in 25:30) {
  new_data[new_data$date_num == i,"total2"] = predict(mod_did_2, new_data[new_data$date_num == i,])
  new_data[new_data$date_num == i,"total2"] = exp(new_data[new_data$date_num == i,"total2"])
  new_data[new_data$date_num == i,"sigma"] = summary(mod_did_2)$sigma
  new_data[new_data$date_num == i,"sigma"] = exp(new_data[new_data$date_num == i,"sigma"]*1.645)
}

for (i in 31:35) {
  new_data[new_data$date_num == i,"total2"] = predict(mod_did_3, new_data[new_data$date_num == i,])
  new_data[new_data$date_num == i,"total2"] = exp(new_data[new_data$date_num == i,"total2"])
  new_data[new_data$date_num == i,"sigma"] = summary(mod_did_3)$sigma
  new_data[new_data$date_num == i,"sigma"] = exp(new_data[new_data$date_num == i,"sigma"]*1.645)
}

for (i in 36:51) {
  new_data[new_data$date_num == i,"total2"] = predict(mod_did_4, new_data[new_data$date_num == i,])
  new_data[new_data$date_num == i,"total2"] = exp(new_data[new_data$date_num == i,"total2"])
  new_data[new_data$date_num == i,"sigma"] = summary(mod_did_4)$sigma
  new_data[new_data$date_num == i,"sigma"] = exp(new_data[new_data$date_num == i,"sigma"]*1.645)
}


for (i in 10:51){
  new_data[(new_data$date_num == i)& (new_data$year == 2020) ,"total_ratio"] = 
    new_data[(new_data$date_num == i)& (new_data$year == 2020), "total2"]/
    new_data[(new_data$date_num == i) & (new_data$year == 2019), "total2"]  
} 

fact_data$sigma = NA
cf_data = fact_data
cf_data$fact = "counterfactual"

for (i in 21:51){
  new_day = sapply(new_data[(new_data$date_num == i)& (new_data$year == 2020), ah_names] ,  FUN = mean, na.rm = T)
  new_day = c(new_day, i)
  names(new_day)[length(names(new_day))] = "day_number"
  cf_data[(cf_data$day_number == i),"total_ratio"] = new_day["total_ratio"]
  cf_data[(cf_data$day_number == i),"sigma"] = sapply(new_data[(new_data$date_num == i)& (new_data$year == 2020), "sigma"] ,  FUN = mean, na.rm = T)[1]
} 

draw_data =fact_data
draw_data = rbind(draw_data, cf_data)

draw_data[is.na(draw_data$sigma),"sigma"] = 1

draw_data = melt(draw_data, id.vars = c("day_number","fact","sigma"), measure.vars = c("total_ratio"))


# draw figure 6c
ggplot(aes(x=day_number, y= value, group = fact, color = fact), data = draw_data) + ylim(0, 3) +  xlim(10, 50) +
  geom_line(size = 1.5, data = draw_data,aes(linetype =draw_data$fact)) + theme_bw() +
  theme(text = element_text(size = 15), axis.text = element_text(size = 15),
        legend.text = element_text(size = 15)) + 
  # geom_ribbon(data = draw_data, alpha = 0.1, aes(x=day_number, ymin = value*(sigma^(-1)),ymax = value*sigma)) +
  geom_vline(xintercept=10, linetype="dashed", color = "black", size=0.5) +
  geom_vline(xintercept=20, linetype="dashed", color = "black", size=0.5) +
  geom_vline(xintercept=23, linetype="dashed", color = "black", size=0.5) +
  geom_vline(xintercept=25, linetype="dashed", color = "black", size=0.5) +
  geom_vline(xintercept=31, linetype="dashed", color = "black", size=0.5) +
  geom_vline(xintercept=35, linetype="dashed", color = "black", size=0.5) 

