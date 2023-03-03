library(tidyverse)
library(emmeans)
library(lme4)
library(lmerTest)
library(MuMIn)

#Correlational DataSets
head(hopcut)


hopcut_pull <- hopcut %>% filter(site == "J", trt %in% c("CON","REM","FIX","NOF")) %>% 
  unite("date", c("year","month"),sep="_", remove = F) %>%
  mutate(date = as.factor(date)) %>%
  mutate(time = if_else(date %in% c("2017_3","2017_4","2018_2","2018_3"), "early", "late")) %>%
  mutate(time = as.factor(time)) %>%
  unite("key", c("plot","date"),sep="_", remove = F) %>%
  mutate(key = as.factor(key)) %>%
  select(-site,-seas)




#filter(!key %in% c("J4E_2017_5"))#mineralization
#filter(!key %in% c("J1E_2018_5"))#respiration



#years side by side
cut17 <- hopcut_pull %>% filter(year == 2017) %>% select(-year)
cut18 <- hopcut_pull %>% filter(year == 2018) %>% select(-year)
pull_years <- full_join(cut17,cut18,by=c("plot","trt","block"),suffix = c(".17", ".18"))


new <- hopcut_pull %>%  filter(date %in% c("2017_4","2018_4"),
                               trt %in%  c("CON","FIX","NOF","REM")) %>%
                        filter(!key %in% c("J4E_2017_5"))#mineralization
                                 


new <- hopcut_pull %>%  filter(trt %in%  c("CON","FIX","NOF","REM")) %>% 
  filter(!key %in% c("J4E_2017_5"))#mineralization


  
ggplot(data=pull_years, aes(y=minRATE.18, x = herb.m.17+herb.m.18, color = trt)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("black","grey30","grey60","blue"),name = "") +
  scale_shape_manual(values=c(15,16,17,18),name = "") +
  geom_smooth(method = "lm", se = F) +
  theme_bw() +
  facet_grid(~month.18)




mod1 <- lmer(data = new, log(1+nitrRATE) ~ log(1+ammon)*trt*year*(year:date) + (1|plot) + (1|block))
mod2 <- lmer(data = new, log(1+nitrRATE) ~ trt*(date)+ (1|plot) + (1|block))
mod3 <- lmer(data = new, log(1+nitrRATE) ~ log(1+ammon)*year + (1|plot) + (1|block))
mod4 <- lmer(data = new, log(1+nitrRATE) ~ (month:year) + (1|plot) + (1|block))

anova(mod1,mod3)
anova(mod2,mod1)
anova(mod3)

r.squaredGLMM(mod3)



mod1 <- lmer(data = hopcut_pull, log(1+totminN) ~ (date)*trt*herb.m + (1|plot) + (1|block))
mod2 <- lmer(data = hopcut_pull, log(1+totminN) ~ (date)*trt (1|plot) + (1|block))
mod3 <- lmer(data = hopcut_pull, log(1+totminN) ~ (herb.m)*date)*(1|plot) + (1|block))
mod4 <- lmer(data = hopcut_pull, log(1+totminN) ~ (herb.m)**(time:year) + (1|plot) + (1|block))
mod5 <- lmer(data = hopcut_pull, log(1+totminN) ~ (herb.m)*trt*(time:year) + (1|plot) + (1|block))



anova(mod1,mod2)
anova(mod1,mod3)
anova(mod3,mod4)
anova(mod4,mod5)
emmeans(mod4, pairwise ~ trt|time:year, adjust = "none")

r.squaredGLMM(mod4)
