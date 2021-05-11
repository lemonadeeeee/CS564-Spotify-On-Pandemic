library(ggplot2)

dat_aus = read.csv("australia_week.csv", header = TRUE)
#dat_aus[,3] = dat_aus[,3]/25499884

dat_in = read.csv("india_week.csv", header = TRUE)
#dat_in[,3] = dat_in[,3]/1380004385

dat_br = read.csv("brazil_week.csv", header = TRUE)
#dat_br[,3] = dat_br[,3]/212559417

dat_us = read.csv("US_week.csv", header = TRUE)
#dat_us[,3] = dat_us[,3]/331002651

barplot(dat_aus[,3], names.arg = dat_aus[,1], col = "green", xlab = "Week Date", ylab = "New Cases", main = "Australia")
#Corona Date 31-Jul

barplot(dat_in[,3], names.arg = dat_in[,1], col = "yellow", xlab = "Week Date", ylab = "New Cases", main = "India")
#Corona Date 18-Sep

barplot(dat_br[,3], names.arg = dat_br[,1], col = "orange", xlab = "Week Date", ylab = "New Cases", main = "Brazil")
#Corona Date 7-Aug

barplot(dat_us[,3], names.arg = dat_us[,1], col = "lightblue", xlab = "Week Date", ylab = "New Cases", main = "US")
#Corona Date 17-Jul

covid_ratio = data.frame(c(dat_aus[,3],dat_in[,3],dat_br[,3],dat_us[,3]), c(rep("Australia", 40),rep("India", 40),rep("Brazil", 40),rep("US", 40)))
covid_ratio

boxplot(covid_ratio[,1]~covid_ratio[,2], data = covid_ratio, xlab = "Countries", ylab = "Covid cases as a ratio of population", col = c("cyan", "magenta", "yellow", "red"))

mus_aus = read.csv("au_waf.csv", header = TRUE)
mus_aus = mus_aus[c(10,11,13,15,16,18,19,20)]
#Separation Row is 3801
mus_aus_bef = mus_aus[c(1:3800),]
mus_aus_bef
mus_aus_aft = mus_aus[c(3801:5400),]

mus_in = read.csv("in_waf.csv", header = TRUE)
mus_in = mus_in[c(10,11,13,15,16,18,19,20)]
#Separation Row is 4501
mus_in_bef = mus_in[c(1:4500),]
mus_in_aft = mus_in[c(4501:5400),]

mus_br = read.csv("br_waf.csv", header = TRUE)
mus_br = mus_br[c(10,11,13,15,16,18,19,20)]
#Separation Row is 3901
mus_br_bef = mus_br[c(1:3900),]
mus_br_aft = mus_br[c(3901:5400),]

mus_us = read.csv("us_waf.csv", header = TRUE)
mus_us = mus_us[c(10,11,13,15,16,18,19,20)]
#Separation Row is 3601
mus_us_bef = mus_us[c(1:3600),]
mus_us_aft = mus_us[c(3601:5400),]

var.test(mus_br_bef[,2], mus_br_aft[,2])
t.test(mus_br_bef[,2], mus_br_aft[,2], alternative = "two.sided", var.equal = TRUE)

mus_aus["time"] = c(rep("bef", 3800), rep("aft", 1600))
mus_in["time"] = c(rep("bef", 4500), rep("aft", 900))
mus_br["time"] = c(rep("bef", 3900), rep("aft", 1500))
mus_us["time"] = c(rep("bef", 3600), rep("aft", 1800))

boxplot(acousticness~time, data = mus_br, xlab = "Before or After Covid", ylab = "Energy", col = c("cyan", "magenta"))