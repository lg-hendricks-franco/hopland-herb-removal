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
  filter(!key %in% c("J4E_2017_5"))#mineralization


###mineralization by 

mod1 <- lm(data=Nmin.df, nitrRATE_ug ~minRATE_ug)
mod2 <- lm(data=Nmin.df, nitrRATE_ug ~minRATE_ug + minRATE_ug*trt)
summary(mod1)
anova(mod1,mod2)

ggplot(data = Nmin.df, aes(x = minRATE_ug, y = nitrRATE_ug, color = trt)) +
  geom_abline(intercept = 0, slope =0.6, color = "black", size=1) +
  geom_point() +
  geom_abline(intercept = 0.76, slope=1.34, color = "grey") +
  #geom_smooth(method = "lm") +
  theme_bw() +
  scale_color_manual(values = c("black","grey30","grey60","grey80"),name = "") +
  scale_shape_manual(values=c(15,16,17,18),name = "")



mod <- lmer(data=Nmin.df, nitrRATE_ug ~ trt*(month:year) + (1|block) + (1|plot))
anova(mod, type = 3)
emmeans(mod, pairwise ~ trt|month:year, adjust="none")$contrasts
plot(mod)

min_summ <- Nmin.df %>% 
  gather(ammon_ug:nitrRATE_ug,key="meas",value="value") %>% 
  group_by(year,month,trt,meas) %>%
  summarise(mn.val = mean(value), se = sd(value)/sqrt(length(value))) %>% 
  as.data.frame() %>% 
  filter(meas %in% c("minRATE_ug"))


new <- min_summ %>% 
  select(year:trt,mn.val) %>% 
  spread(key=trt,value = mn.val) %>%
  mutate(mn.con.nof = (CON+NOF)/2) %>% 
  mutate(fix.v.con.nof = FIX/mn.con.nof,
         rem.v.con.nof = REM/mn.con.nof)


ggplot(data=min_summ, aes(x=month, y=c(mn.val),group = trt)) + 
  geom_line(aes(linetype=trt, color = trt), size=.6, position = position_dodge(width=0)) +
  geom_errorbar(aes(ymin=mn.val-se,ymax=mn.val+se, color = trt), size=.3, width=.3, position = position_dodge(width=0)) +
  #geom_point(aes(color = trt, shape = trt), 
  #           position = position_dodge(width=0), size = 3) +
  scale_color_manual(values = c("black","grey50","black","grey50"),name = "") +
  scale_shape_manual(values=c(15,16,15,16),name = "") +
  scale_linetype_manual(values=c("solid","solid","dashed", "dashed"))+
  ylab(expression("("*mu*"g N/g soil/week)")) +
  xlab("Month") + theme_bw() +
  scale_y_continuous(limits=c(-1.1,9),breaks = seq(0,10,2)) +
  #ggtitle("Mineralization") +
  #geom_hline(yintercept=0, linetype=1, size=0.3, color="grey14") +
  theme(plot.title = element_text(hjust = 0.5, size=20), legend.position="bottom", 
        axis.text=element_text(size=12), axis.title=element_text(size=12),
        legend.text=element_text(size=12),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  facet_grid(~year)
