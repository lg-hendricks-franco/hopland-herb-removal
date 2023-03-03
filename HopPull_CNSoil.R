
#HopPull-vegetation

library(tidyverse)
library(emmeans)
library(lme4)
library(lmerTest)
library(MuMIn)



##Run Get Full Table by Running File HoplandTableJoin.R

head(hopwide)
head(df.CN_cut)

hopwide_pull <- hopwide %>% filter(site == "J", trt %in% c("PRE","CON","REM","FIX","NOF")) %>% 
  unite("date", c("year","month"),sep="_", remove = F) %>%
  mutate(date = as.factor(date))



#CN graphs

##Get total soil C and N in mg/g
head(df.CN_cut)
df.CN_cut1 <- df.CN_cut %>% mutate(N_mg_g = perN*10, C_mg_g = perC*10)


CN <- full_join(df.CN_cut1,plot_trt,by=c("plot")) %>%
  select(plot,trt,year,block:seas,everything()) %>% 
  filter(site == "J", ##These are half cages excluded
         trt %in% c("CON","REM","FIX","NOF")) %>% 
  mutate(plot = as.factor(plot))
  
  



summ <- CN %>%
  group_by(year,trt) %>% 
  summarize(mean_C = mean(C_mg_g), SE_C = sd(C_mg_g)/sqrt(length(C_mg_g)),
            mean_N = mean(N_mg_g), SE_N = sd(N_mg_g)/sqrt(length(N_mg_g)),
            mean_CN = mean(CNrat), SE_CN = sd(CNrat)/sqrt(length(CNrat))) %>%
  as.data.frame()

write.csv(summ,"summ.csv")

mod<-lmer(data=CN, C_mg_g ~ trt*year + (1|block) + (1|plot))
anova(mod)
emmeans(mod, pairwise ~ year|trt, adjust = "none")

plot(mod)

mean(summ[summ$year==2017,]$mean_CN)

hi_there<- summ %>% select(year,trt,mean_C) %>% 
  spread(key=year,value=mean_C) 

hi_there$`2018` - hi_there$`2017`

###Analyze differences in CN only
delC <- CN %>% select(plot:year,C_mg_g)%>% 
  spread(key=year,value=c(C_mg_g)) %>%
  rename(y1 = '2017',y2 = '2018') %>% 
  mutate(del_C = y2-y1) %>% select(plot,trt,del_C)

delN <- CN %>% select(plot:year,N_mg_g)%>% 
  spread(key=year,value=c(N_mg_g)) %>%
  rename(y1 = '2017',y2 = '2018') %>% 
  mutate(del_N = y2-y1) %>% select(plot,trt,del_N)

delCN <- CN %>% select(plot:year,CNrat)%>% 
  spread(key=year,value=c(CNrat)) %>%
  rename(y1 = '2017',y2 = '2018') %>% 
  mutate(del_CN = y2-y1) %>% select(plot,trt,del_CN)

delta_CN <- cbind(delC,delN[3],delCN[3])

mod<-lm(data=delta_CN, del_C ~ trt)
anova(mod)
emmeans(mod, pairwise ~ trt, adjust = "none")

summ_delta <- delta_CN %>%
  group_by(trt) %>% 
  summarize(mean_C = mean(del_C), SE_C = sd(del_C)/sqrt(length(del_C)),
            mean_N = mean(del_N), SE_N = sd(del_N)/sqrt(length(del_N)),
            mean_CN = mean(del_CN), SE_CN = sd(del_CN)/sqrt(length(del_CN))) %>%
  as.data.frame()

ggplot(delta_CN, aes(x=trt, y=del_C)) + 
  geom_boxplot()

ggplot(data=summ_delta, aes(x=trt, y=mean_C)) +
  geom_errorbar(aes(ymin=mean_C-SE_C,ymax=mean_C+SE_C), size=.6, width=.1, position = position_dodge(width=.3)) +
  #geom_point(aes(color = trt, shape = trt), 
            # position = position_dodge(width=.3), size = 3) +
  #scale_color_brewer(type = 'cat', palette = "Dark2", direction = 1) +
  #scale_color_manual(values = c("black","grey30","grey60","grey80"),name = "") +
  #scale_shape_manual(values=c(15,16,17,18),name = "") +
  #xlab("Date") + theme_bw() +
  #ylim(0,30) +
  theme(legend.justification=c(1,1),
        legend.position="bottom",
        axis.text=element_text(size=12), axis.title=element_text(size=12),
        legend.text=element_text(size=12))



###

ggplot(data=summ, aes(x=year, y=mean_C, color=trt,group=trt)) +
  geom_line(aes(color=trt), size=.3, position = position_dodge(width=.3)) +
  geom_errorbar(aes(ymin=mean_C-SE_C,ymax=mean_C+SE_C), size=.6, width=.1, position = position_dodge(width=.3)) +
  geom_point(aes(color = trt, shape = trt), 
             position = position_dodge(width=.3), size = 3) +
  #scale_color_brewer(type = 'cat', palette = "Dark2", direction = 1) +
  scale_color_manual(values = c("black","grey30","grey60","grey80"),name = "") +
  scale_shape_manual(values=c(15,16,17,18),name = "") +
  xlab("Date") + theme_bw() +
  ylim(0,30) +
  theme(legend.justification=c(1,1),
        legend.position="bottom",
        axis.text=element_text(size=12), axis.title=element_text(size=12),
        legend.text=element_text(size=12))


