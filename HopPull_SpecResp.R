#HopPull-vegetation

library(tidyverse)
library(emmeans)
library(lme4)
library(lmerTest)
library(MuMIn)



##Run Get Full Table by Running File HoplandTableJoin.R

head(hopwide)
head(df.Cmin_cut)
a <- df.Cmin_cut %>% filter(month == "5") %>%
  select(plot, month, Resp_ugC_gsoil_day)
head(a)

head(df.CN_cut)
df.CN_cut1 <- df.CN_cut %>% mutate(N_mg_g = perN*10, C_mg_g = perC*10)


CN <- full_join(df.CN_cut1,plot_trt,by=c("plot")) %>%
  select(plot,trt,year,block:seas,everything()) %>% 
  filter(site == "J", ##These are half cages excluded
         trt %in% c("CON","REM","FIX","NOF")) %>% 
  mutate(plot = as.factor(plot))

head(CN)
b <- CN %>% filter(site == "J", year == "2018") %>% 
  select(plot,trt,block,C_mg_g)

carbon <- full_join(a,b,by="plot") %>% na.omit(.) %>%
  filter(plot != "J1E") %>% mutate(spec_resp = Resp_ugC_gsoil_day/C_mg_g)

plot(y = carbon$spec_resp, x = carbon$trt)

mod <- lm(data = carbon, spec_resp ~ trt)
anova(mod)
emmeans(mod, pairwise ~ trt, adjust = "none")
plot(mod)

spec_resp_summ <- carbon %>%
  group_by(trt) %>% 
  summarize(mean_spec_resp = mean(spec_resp), SE = sd(spec_resp)/sqrt(length(spec_resp))) %>%
  as.data.frame()

