##Hopland Weather graphs

library(tidyverse)

setwd("C:/Users/linds/Dropbox/Documents (Selective Sync Conflict)/Dissertation")

data<-read.csv("C:/Users/linds/Dropbox/Documents (Selective Sync Conflict)/Dissertation/Dissertation DATA/Herb-removal plots/HoplandPrecipSeas.csv", header=TRUE)
data <- select(data, year:precip)
data <- data[1:20,]
data[,3] <- data[,3]*2.54

head(data)
data$season <- factor(data$season, levels = c("Fall","Winter","Spring","Summer","Annual"))

ggplot(data, aes(x=season)) +
  geom_bar(aes(y=precip, fill = year), stat="identity", position = position_dodge(), colour = "black") +
  scale_fill_grey(start = 0, end = .8, name = "Year") +
  theme_bw() +
  scale_y_continuous(name = "Precipitation (cm)", breaks=seq(0,120,20)) +
  scale_x_discrete(name = "Season")


##Temperature

data<-read.csv("C:/Users/linds/Dropbox/Documents (Selective Sync Conflict)/Dissertation/Dissertation DATA/Herb-removal plots/HoplandWeatherMon.csv", header=TRUE)
data[,4:5] <- (data[,4:5]-32)*(5/9)
data[,3] <- data[,3]*2.54
data <- data[13:48,]

head(data)

ggplot(data, aes(x=month)) + theme_bw() +
  geom_line(aes(y=max_tmp,linetype=year)) +
  geom_line(aes(y=min_tmp,linetype=year)) +
  scale_y_continuous(breaks=seq(0,30,5)) +
  scale_linetype_manual(name = "Year", values = c("solid","dashed","dotdash","dotted")) +
  scale_x_continuous(name = "Month", breaks=seq(0,12,1)) +
  ylab(expression("Temperature " ( degree*C)))

