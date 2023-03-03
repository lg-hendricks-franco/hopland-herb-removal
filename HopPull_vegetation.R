#HopPull-vegetation

library(tidyverse)
library(emmeans)
library(lme4)
library(lmerTest)
library(MuMIn)



##Run Get Full Table by Running File HoplandTableJoin.R

head(hopwide)
head(df.veg_wide)
head(df.veg_plot)

hopwide_pull <- hopwide %>% filter(site == "J", trt %in% c("PRE","CON","REM","FIX","NOF")) %>% 
  unite("date", c("year","month"),sep="_", remove = F) %>%
  mutate(date = as.factor(date))
veg_pull <- df.veg_wide %>% filter(site == "J", trt %in% c("PRE","CON","REM","FIX","NOF")) 
vegplot_pull<- df.veg_plot %>% filter(site == "J", trt %in% c("PRE","CON","REM","FIX","NOF"))





veg_long <- vegplot_pull %>% gather(key=group,value=count,tot.m:fracfix.c) %>%
  mutate(group = as.factor(group)) %>% 
  select(-sample_id)

veg_summ <- veg_long %>%
  filter(trt %in% c("CON","FIX","NOF","REM")) %>%
  filter(trt %in% c("CON","FIX"), group == "herb.fix.m") %>%
  #filter(trt %in% c("CON","NOF"), group == "herb.nof.m") %>% 
  #filter(group %in% c("herb.m")) %>% #Export 3 x4
  #filter(group %in% c("herb.fix.m","herb.nof.m")) %>% #Export 4 x4
  group_by(trt,year,group) %>%
  summarise(mn.val = mean(count), se = sd(count)/sqrt(length(count)))


ggplot(veg_summ, aes(y=(mn.val), x=trt,fill=group)) +
  geom_col(position = position_dodge(),color = "black") +
  geom_errorbar(aes(ymin=mn.val-se,ymax=mn.val+se), size=.3, width=.3, position = position_dodge(width=.9)) +
  theme_bw() +
  theme(panel.grid=element_blank(),strip.background = element_blank(), 
        axis.text=element_text(size=10), axis.title=element_text(size=11)) +
  scale_fill_manual(values=c("black")) +
  #ylim(0,300) +
  labs(y=Biomass~(g/m^2), x="", fill = "Functional Group") +
  #labs(y="% Cover", x="", fill = "Functional Group") +
  #theme(legend.position="bottom",legend.text=element_text(size=11)) +
  facet_grid(~year)

#Competitions Experiment

#Look at all herbs together
someherbs <- veg_pull %>% filter(trt %in% c("CON","FIX","NOF"))
mod <- lmer(data = someherbs, (herb.m) ~ trt*year  + (1|block) + (1|plot))
anova(mod)
emmeans(mod, pairwise ~ trt|year, adjust = "none")
plot(mod)

#N-Fixer Response to non-N-Fixer Removal
somefix <- veg_pull %>% filter(trt %in% c("CON","FIX"))
mod <- lmer(data = somefix, sqrt(herb.fix.m) ~ trt*year  + (1|block) + (1|plot))
anova(mod)
emmeans(mod, pairwise ~ trt)
plot(mod)

#non-N-Fixer Response to N-Fixer Removal
somenof <- veg_pull %>% filter(trt %in% c("CON","NOF"))
mod <- lmer(data = somenof, (herb.nof.m) ~ trt*year  + (1|block) + (1|plot))
anova(mod)
emmeans(mod, pairwise ~ trt|year)
plot(mod)

ggplot(data = hopwide_pull, aes(y = ammon_ug, x = herb.m)) +
  geom_point(aes(color = trt)) +
  facet_grid(month~year)


mod <- lmer(data = hopwide_pull, minRATE_ug~ date*trt + (1|block) + (1|plot))
anova(mod)



##Mass by species
head(df.spp_mass)

vegplot_pull<- df.veg_plot 

spp_pull <- df.spp_mass %>%
  filter(site == "J", trt %in% c("FIX"),
         spp == c("Acann"))

mod <- lmer(data = spp_pull, count~ year*trt + (1|block) + (1|plot))
anova(mod)
emmeans(mod, pairwise ~ trt|year)
plot(mod)

spp_summ <- aggregate(data=spp_pull,.~plot+year+spp+trt+block+site+seas+group, FUN=mean) %>%
  select(-sample_id) %>% 
  #filter(group %in%  c("fix.nat.ann","fix.nat.per","fix.inv.ann")) %>% mutate(spp = factor(spp, c("Acann","Lubic","Trmic","Acgla","Trhir"))) %>% filter(spp %in% c("Acann","Trmic","Acgla","Trhir")) %>% 
  #filter(group == "nof.nat.ann", !spp %in% c("Epmin","Femic","Hemic","")) %>% 
  #filter(group == "nof.nat.per", !spp %in% c("Brele","Erlan","Loday","Pehet","Phimb","Pielo","Strig")) %>% 
  #filter(group == "nof.inv.per") %>%
  #filter(group == "nof.inv.ann",!spp %in% c("Ceglo","Cyech","Ercic","Saape")) %>% 
    #filter(group %in% c("fix.shb","nof.shb"), !spp %in% c("Arcan","Bepin","Ceint","Miaur","Pimon","Quber")) %>% 
  group_by(year,trt,group,spp) %>% 
  summarise(mn.val = mean(count), se = sd(count)/sqrt(length(count))) %>% 
  as.data.frame() #%>% 
  #filter(mn.val != 0) #to generate species list

emmeans(mod, pairwise ~ trt|year)
spp_summ %>% select(year:mn.val) 

##Empen - 2017 - 10.6  2018 - 9.6
##Chpom - 2017 - 1.76  2018 - 0.22
#1 CON   2017  herb.nof.m   78.7  24.4
#2 CON   2018  herb.nof.m  220.   39.3
#3 NOF   2017  herb.nof.m   86.5  10.6
#4 NOF   2018  herb.nof.m  169.   37.8

#1 2017 NOF nof.nat.ann Empen 37.05333
#2 2018 NOF nof.nat.ann Empen  0.00000

#1 CON   2017  herb.fix.m   35.9 11.3 
#2 CON   2018  herb.fix.m   47.6  6.99
#3 FIX   2017  herb.fix.m   82.3 12.1 
#4 FIX   2018  herb.fix.m   95.8 18.7 

#year trt       group   spp mn.val
#1 2017 FIX fix.nat.per Acgla   4.62
#2 2018 FIX fix.nat.per Acgla  32.97
#year trt       group   spp mn.val
#year trt       group   spp mn.val
#1 2017 FIX fix.nat.ann Acann  62.37
#2 2018 FIX fix.nat.ann Acann  50.61

write.csv(spp_summ, "test.csv")

ggplot(data=spp_summ, aes(x=year,y=mn.val,fill = trt)) +
  #coord_flip() +
  geom_col(position = position_dodge(),color = "black") +
  geom_errorbar(aes(ymin=mn.val-se,ymax=mn.val+se), size=.3, width=.3, position = position_dodge(width=.9)) +
  theme_bw() +
  theme(panel.grid=element_blank(),strip.background = element_blank(), 
        axis.text=element_text(size=10), axis.title=element_text(size=11),
        legend.position = c(2,2)) +
  #scale_fill_manual(values=c("grey", "white")) +
  scale_fill_grey() +
  #ylim(0,60) +
  labs(y=Biomass~(g/m^2), x="", fill = "Treatment") +
  facet_wrap(spp~., ncol = 4)

####Relative Abundance in control plots##
##Requested by Ecosphere reviewers###

rel_ab17 <- spp_summ %>%
  filter(trt=="CON", year == "2017", !group %in% c("nof.shb","fix.shb")) %>% 
  mutate(rel.ab = mn.val/sum(mn.val)) %>% 
  select(trt:spp,rel.ab) #%>% 
 # arrange(desc(rel.ab))

rel_ab18 <- spp_summ %>%
  filter(trt=="CON", year == "2018", !group %in% c("nof.shb","fix.shb")) %>% 
  mutate(rel.ab = mn.val/sum(mn.val)) %>% 
  select(trt:spp,rel.ab) %>% 
  arrange(desc(rel.ab))

rel_ab <- full_join(rel_ab17,rel_ab18,
by=c("trt","spp","group")) %>% 
  select(trt,group, spp, rel.ab.17 = rel.ab.x, rel.ab.18 = rel.ab.y)

head(rel_ab)

write.csv(rel_ab, "test.csv")

####

#Vegetation with respiration

some17 <- hopwide_pull %>% filter(date == "2017_4")#, trt %in% c("REM","FIX"))
some18 <- hopwide_pull %>% filter(date %in%  c("2018_3"))#, trt %in% c("REM","FIX"))

plot(dat = some, some18$Resp_ugC_gsoil_day ~ some17$herb.m)
mod<-lm(dat = some, some18$Resp_ugC_gsoil_day ~ (some17$herb.m+some18$herb.m))
summary(mod)

#just removal for each block

cut18 <- some18 %>% select(plot:site, per_wat:Resp_ugC_gsoil_day, herb.m, herb.fix.m,herb.nof.m)


rem18 <- some18 %>% select(plot:site, Resp_ugC_gsoil_day) %>% filter(trt == "REM") %>% 
  select(date, year, block, remresp = Resp_ugC_gsoil_day)

testing123 <- cut18 %>% full_join(rem18) %>% 
  mutate(resp_adj = Resp_ugC_gsoil_day - remresp)

plot(testing123$resp_adj ~ some17$herb.m)
mod<-lm(testing123$resp_adj ~ some17$herb.m)
summary(mod)



###########Estimates N in herb pools.######
#N pools in mg/m2

Npools <- vegplot_pull %>%
  select(plot:sample_id,herb.fix.m, herb.nof.m) %>%
  filter(site == "J") %>% 
  mutate(herb.fix.N = herb.fix.m*13.6, herb.nof.N = herb.nof.m*5.6,
         tot.herb.N = herb.fix.N + herb.nof.N)

Npools_long <- Npools %>% select(plot:seas,herb.fix.N:tot.herb.N) %>% 
  gather(key=group,value=count,herb.fix.N:tot.herb.N) %>%
  mutate(group = as.factor(group))

Npools_summ <- Npools_long %>%
  filter(group %in% c("herb.fix.N","herb.nof.N", "tot.herb.N")) %>%
  filter(trt %in% c("CON","NOF","FIX","REM")) %>% #Export 4 x4
  group_by(trt,year,group) %>%
  summarise(mn.val = mean(count), se = sd(count)/sqrt(length(count))) %>% 
  as.data.frame()

#Look at all herbs together
someherbs <- Npools %>% filter(trt %in% c("CON","FIX","NOF"))
mod <- lmer(data = someherbs, (tot.herb.N) ~ trt*year  + (1|block) + (1|plot))
anova(mod, type = 3)
emmeans(mod, pairwise ~ trt|year, adjust = "none")
plot(mod)
