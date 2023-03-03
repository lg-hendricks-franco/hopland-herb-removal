#Gross N Immobilization Index

#HopPull-vegetation

library(tidyverse)
library(emmeans)
library(lme4)
library(lmerTest)
library(MuMIn)



##Run Get Full Table by Running File HoplandTableJoin.R

head(hopwide)
head(df.Nmin_cut)

hopwide_pull <- hopwide %>% filter(site == "J", trt %in% c("PRE","CON","REM","FIX","NOF")) %>% 
  unite("date", c("year","month"),sep="_", remove = F) %>%
  mutate(date = as.factor(date))



Nmin.df <- df.Nmin_cut %>% 
  full_join(plot_trt,by=c("plot")) %>% 
  select(plot,trt,year,month,block:seas,everything()) %>% 
  filter(site == "J", ##These are half cages excluded
         trt %in% c("CON","REM","FIX","NOF")) %>% #get rid of PRE for rates 
  mutate(plot = as.factor(plot)) %>% #This fixes warning
  unite("date", c("year","month"),sep="_", remove = F) %>%
  mutate(date = as.factor(date)) %>%
  unite("key", c("plot","date"),sep="_", remove = F) %>%
  mutate(key = as.factor(key)) %>%
  #mutate(time = if_else(date %in% c("2017_3","2017_4","2018_2","2018_3"), "early", "late")) %>% #filter to lump dates
  filter(!key %in% c("J4E_2017_5")) %>% #mineralization
  filter(date %in% c("2018_3","2018_4","2018_5"))


head(df.Cmin_cut)
a <- df.Cmin_cut %>% filter(month %in% c("3","4","5")) %>%
  unite("date", c("year","month"),sep="_", remove = F) %>%
  mutate(date = as.factor(date)) %>%
  unite("key", c("plot","date"),sep="_", remove = F) %>%
  mutate(key = as.factor(key)) %>%
  select(key,plot, month, Resp_ugC_gsoil_day)
head(a)
nrow(a)


joined <- inner_join(Nmin.df,a, by = c("key")) %>% na.omit() %>% 
  mutate(imm_pot = Resp_ugC_gsoil_day/minRATE_ug) %>%
  filter(!key %in% c("J1E_2018_5")) %>%
  select(key, plot=plot.x, trt,month=month.x,imm_pot)

head(joined)
nrow(joined)

plot(data=joined, y = joined$imm_pot, x = joined$trt)
mod <- lm(data=joined, imm_pot ~ trt*month)
anova(mod)
plot(mod)

#Immobilizatoin potential in May. doesn't seem too useful.
