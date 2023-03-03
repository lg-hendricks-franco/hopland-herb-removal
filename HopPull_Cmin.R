#HopPull-vegetation

library(tidyverse)
library(emmeans)
library(lme4)
library(lmerTest)
library(MuMIn)



##Run Get Full Table by Running File HoplandTableJoin.R

head(hopwide)
head(df.Cmin_cut)

hopwide_pull <- hopwide %>% filter(site == "J", trt %in% c("PRE","CON","REM","FIX","NOF")) %>% 
  unite("date", c("year","month"),sep="_", remove = F) %>%
  mutate(date = as.factor(date))

head(df.Cmin_cut)



Cmin.df <- df.Cmin_cut %>% 
  full_join(plot_trt,by=c("plot")) %>% 
  select(plot,trt,year,month,block:seas,everything()) %>% 
  filter(site == "J", ##These are half cages excluded
         trt %in% c("CON","REM","FIX","NOF"),
         month != 2) %>% 
  mutate(plot = as.factor(plot)) %>% #This fixes warning
  rename(resp =Resp_ugC_gsoil_day) %>% 
  unite("date", c("year","month"),sep="_", remove = F) %>%
  mutate(date = as.factor(date)) %>%
  unite("key", c("plot","date"),sep="_", remove = F) %>%
  mutate(date = as.factor(date)) %>%
  filter(!key %in% c("J1E_2018_5"))#respiration


head(Cmin.df)

pull_summ_resp <- Cmin.df %>%
  group_by(month,trt) %>% 
  summarize(mean_resp = mean(resp), SE = sd(resp)/sqrt(length(resp))) %>%
  as.data.frame()


## Difference between removal and all other trr,
## averaged across months
new <- pull_summ_resp %>% 
  select(-SE) %>% 
  spread(key=trt,value = mean_resp) %>%
  mutate(mn.herbs = (CON+NOF+FIX)/3) %>% 
  mutate(diff = mn.herbs-REM, per_dec = diff/mn.herbs)

mean(new$diff)




###

ggplot(data=pull_summ_resp, aes(x=month, y=mean_resp, group=trt)) +
  geom_line(aes(linetype=trt,color=trt), size=.6, position = position_dodge(width=0)) +
  geom_errorbar(aes(ymin=mean_resp-SE,ymax=mean_resp+SE, color = trt), size=.3, width=.1, position = position_dodge(width=0)) +
  #geom_point(aes(color = trt, shape = trt), 
  #           position = position_dodge(width=0), size = 3) +
  scale_color_manual(values = c("black","grey50","black","grey50"),name = "") +
  scale_shape_manual(values=c(15,16,15,16),name = "") +
  scale_linetype_manual(values=c("solid","solid","dashed", "dashed"))+
  ylab(expression("("*mu*"g C/g soil/day)")) + 
  xlab("Month (2018)") + theme_bw() +
  ylim(0,23) +
  theme(legend.justification=c(1,1),
        legend.position="bottom",
        axis.text=element_text(size=12), axis.title=element_text(size=12),
        legend.text=element_text(size=12))


mod <- lmer(data = Cmin.df, resp ~ trt*month + (1|block) + (1|plot))
anova(mod)
emmeans(mod, pairwise ~ trt, adjust = "none")

head(hopwide)

new <- hopwide %>% filter(year == 2017, #month %in% c("3","4","5"),
                          block %in% c("J1","J2","J4","J5","J6","K1","K3","K5",
                                       "L1","L3","L5","L6","T1","T3","T4","T5","T6"),
                          !plot %in% c("J2A","J5F","K1F","K5F","T4A","T5F","T6F","L6E"), ##These are half cages excluded
                          trt %in% c("CON","HAL","EXC")) %>% 
  mutate(trt = replace(trt, trt == "HAL", "CON"),
         plot = as.factor(plot)) %>% filter(minRATE_ug < 30) %>% 
  rename(resp =Resp_ugC_gsoil_day)

ggplot(data=new, aes(y = minRATE_ug, x = herb.m, color = site)) +
  geom_point(size=2) +
  #scale_color_manual(values = c("Black", "grey40","grey"),name = "") +
  #scale_shape_manual(values=c(15,16,17),name = "") +
  ylab(expression("("*mu*"g C/g soil/day)")) +
  geom_smooth(method = "lm", se = F) +
  #xlab("Gravimetric Percent Water") + 
  theme_bw() +
  facet_grid(month~year)


mod1 <- lmer(data = new, minRATE_ug ~ herb.m*month*site + (1|block) + (1|plot))
mod2 <- lmer(data = new, resp ~ month*per_wat + (1|block) + (1|plot))
mod3 <- lmer(data = new, resp ~ month*per_wat*trt + (1|block) + (1|plot))

anova(mod1)
anova(mod2,mod3)

r.squaredGLMM(mod1)

################

#Specific respiration rates

new <- hopwide_pull %>%
  select(plot:block,resp=Resp_ugC_gsoil_day,perC) %>% 
  filter(month == 5, year == 2018) %>% 
  mutate(spec_resp = resp/perC) #in ug C respired/mg C in soil

mean(new$spec_resp)
sd(new$spec_resp)/sqrt(length(new$spec_resp))

plot(data = new, spec_resp ~ trt)

mod <- lm(data = new, spec_resp ~ trt)
summary(mod)
anova(mod)
plot(mod)
