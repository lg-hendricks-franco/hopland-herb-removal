getwd()
setwd("C:/Users/linds/Dropbox")
library(tidyverse)

##Let's Find All the Important Data Tables
##Condense to plot level where needed
herbs_rem<-read.csv("C:/Users/linds/Dropbox/Documents (Selective Sync Conflict)/Dissertation/Dissertation DATA/Herb-removal plots/herbs_removed_J.csv", header=TRUE)

head(herbs_rem)

##2017 biomass removed by treatment/meter-sq

herbs_rem %>% group_by(Plot,Treatment) %>%
  summarise(mass_rem = sum(biomass)) %>% 
  group_by(Treatment) %>% 
  summarise(mass = mean(mass_rem)/12) %>% 
  as.data.frame()
