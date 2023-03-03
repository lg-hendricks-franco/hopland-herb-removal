#N-mineralization Pull Hopland

library(tidyverse)
library(emmeans)
library(lme4)
library(lmerTest)
library(MuMIn)


##Run Get Full Table by Running File HoplandTableJoin.R

head(hopwide)
head(df.veg_wide)

#Other individiual data sets
head(df.moist_cut)


hopwide_pull <- hopwide %>% filter(site == "J", trt %in% c("CON","REM","FIX","NOF")) %>% 
  unite("date", c("year","month"),sep="_", remove = F) %>%
  mutate(date = as.factor(date))

##Treatment effect
summ <- hopwide_pull %>%
  group_by(month,year,trt) %>% 
  summarize(mean_wat = mean(per_wat), SE = sd(per_wat)/sqrt(length(per_wat))) %>%
  as.data.frame()


ggplot(data=summ, aes(y = mean_wat, x = month, group = trt, color=trt)) +
  #geom_point(position = position_dodge(width=0), size = 2) +
  geom_errorbar(aes(ymin=mean_wat-SE,ymax=mean_wat+SE), size=.4, width=.5,
                position = position_dodge(width=0)) +#, position = position_dodge(width=.3)) +
  geom_line(aes(linetype=trt),
            size=.6, position = position_dodge(width=0)) +
  scale_color_manual(values = c("black","grey50","black","grey50"),name = "") +
  scale_shape_manual(values=c(15,16,15,16),name = "") +
  scale_linetype_manual(values=c("solid","solid","dashed", "dashed"))+
  #ylab(expression("("*mu*"g N/g soil/day)")) +
  #geom_smooth(method = "lm", se = F
  ylim(0,0.3) +
  ylab("Gravimetric Percent Water") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=20), legend.position="bottom", 
        axis.text=element_text(size=12), axis.title=element_text(size=12),
        legend.text=element_text(size=12),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  facet_grid(~year)


mod1 <- lmer(data = hopwide_pull, per_wat ~ trt*date + (1|plot) + (1|block))
anova(mod1)
plot(mod1)
emmeans(mod1, pairwise ~ trt|date, adjust = "none")


