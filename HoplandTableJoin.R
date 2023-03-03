##Hopland Table Join


getwd()
setwd("C:/Users/linds/Dropbox")
library(tidyverse)

##Let's Find All the Important Data Tables
##Condense to plot level where needed
plot_trt<-read.csv("C:/Users/linds/Dropbox/Documents (Selective Sync Conflict)/Dissertation/Dissertation DATA/Herb-removal plots/plot_trt.csv", header=TRUE)


df.CN<-read.csv("C:/Users/linds/Dropbox/Documents (Selective Sync Conflict)/Dissertation/Dissertation DATA/Herb-removal plots/Summarized Data/EA_soil.csv", header=TRUE)
head(df.CN)

df.CN_cut <- df.CN %>% 
  mutate(year = as.factor(year)) %>%
  group_by(year,plot) %>% 
  summarize(perN = mean(per_N), perC = mean(per_C), CNrat = mean(CN_rat)) %>%
  as.data.frame() 
head(df.CN_cut)


df.Nmin<-read.csv("C:/Users/linds/Dropbox/Documents (Selective Sync Conflict)/Dissertation/Dissertation DATA/Herb-removal plots/Plate reader/All_Nmin_test.csv", header=TRUE)
head(df.Nmin)
df.Nmin_cut <- df.Nmin %>% dplyr::na_if("#VALUE!") %>%
  mutate(year = as.factor(year), month = as.factor(month),
         ammon_ug = as.character(ammon_ug), nitrate_ug = as.character(nitrate_ug),
         totminN_ug = as.character(totminN_ug), minRATE_ug = as.character(minRATE_ug),
         nitrRATE_ug = as.character(nitrRATE_ug)) %>%
  mutate(ammon_ug = as.numeric(ammon_ug), nitrate_ug = as.numeric(nitrate_ug),
         totminN_ug = as.numeric(totminN_ug), minRATE_ug = as.numeric(minRATE_ug),
         nitrRATE_ug = as.numeric(nitrRATE_ug)) %>% 
  select(plot,month:year,ammon_ug:nitrRATE_ug)
head(df.Nmin_cut)


df.Cmin<-read.csv("C:/Users/linds/Dropbox/Documents (Selective Sync Conflict)/Dissertation/Dissertation DATA/Herb-removal plots/CO2 Raw Data/RespirationHopland2018_calculations.csv", header=TRUE)
head(df.Cmin)
df.Cmin_cut <- df.Cmin %>% mutate(year = 2018) %>%
  mutate(year = as.factor(year), month = as.factor(month)) %>%
  select(plot,month,year,Resp_ugC_gsoil_day)
head(df.Cmin_cut)


df.moist<-read.csv("C:/Users/linds/Dropbox/Documents (Selective Sync Conflict)/Dissertation/Dissertation DATA/Herb-removal plots/Summarized Data/SoilMoistureSummary.csv", header=TRUE)
head(df.moist)
df.moist_cut <- df.moist %>% rename(plot = sample_id) %>%
  mutate(year = as.factor(year), month = as.factor(month)) %>%
  select(plot,year,month,per_wat)
head(df.moist_cut)

df.veg<-read.csv("C:/Users/linds/Dropbox/Documents (Selective Sync Conflict)/Dissertation/Dissertation DATA/Herb-removal plots/Pinframe_final.csv", header=TRUE)
head(df.veg)
#species summarized to plot level, long-form
#summarized to sub_plot
df.veg_cut <- df.veg %>% replace(is.na(.), 0) %>%
  mutate(sum = rowSums(.[10:29])) %>% 
  mutate(cover = rowSums(.[10:29]!=0)/20) %>% 
  mutate(species =replace(species,species == "Acpar","Acann")) %>% 
  mutate(species =replace(species,species == "Acbra","Acann")) %>%
  select(sample_id:species,sum:cover) %>% group_by(year,plot,species,sample_id) %>%
  summarize(hits = mean(sum), cov=mean(cover))

head(df.veg_cut)
write.csv(df.veg_cut,"vegtest.csv")
#Sums duplicate rows (mainly Acann from 2018). Wide form.
df.veg_hits <- aggregate(data=df.veg_cut,
                      hits ~ sample_id+plot+year+species, FUN=sum) %>% 
  spread(key=species, value=hits) %>% replace(is.na(.), 0)
head(df.veg_hits)

df.veg_cov <- aggregate(data=df.veg_cut,cov ~ sample_id+plot+year+species, FUN=sum) %>% 
  spread(key=species, value=cov) %>% replace(is.na(.), 0)
head(df.veg_cov)

df.grps_mass <- df.veg_hits %>% 
  mutate(tot = rowSums(.[4:76]),
         fix.nat.ann = 3.78*(Acann + Lubic + Trmic),
         fix.nat.per = 3.78*(Acgla),
         fix.inv.ann = 3.78*(Trhir),
         nof.nat.ann = 3.97*(Aghet + Cabre + Cahir + Calas + Crmur + Cuocc + Dapus +
           Empen + Epmin + Femic + Gaapa + Gacal + Gicap + Hemic + Lofil + Magra +
           Namel + Pscal + Racal + Urlin),
         nof.nat.per = 3.97*(Brele + Chpom + Erlan + Hesco + Hycon + Loday + Pehet +
           Phimb + Pielo + Stlep + Strig + Tacal),
         nof.inv.ann = 3.97*(Aicar + Avbar + Brdia + Brhor + Brmad + Ceglo + Cemel + Cyech +
           Erbot + Ercic + Femyu + Gapar + Gaphl + Hygla + Lasal + Laser + Logal +
           Lyarv + Pedub + Pslut + Saape + Soasp + Toarv + Tobar),
         nof.inv.per = 3.97*(Ehcal + Phaqu),
         nof.shb = 4.56*(Adfas + Bepin + Ercal + Quber + Arcan + Miaur),
         fix.shb = 4.56*(Cecun + Ceint + Pimon)) %>% 
  select(sample_id:year,tot:fix.shb) %>% 
  mutate(year = as.factor(year)) %>% 
  mutate(herb.fix = fix.nat.ann + fix.nat.per + fix.inv.ann,
         herb.nof = nof.nat.ann + nof.nat.per + nof.inv.ann + nof.inv.per,
         shb = nof.shb + fix.shb,
         herb.inv = fix.inv.ann + nof.inv.ann + nof.inv.per,
         herb.nat = fix.nat.ann + fix.nat.per + nof.nat.ann + nof.nat.per,
         ann = fix.nat.ann + fix.inv.ann + nof.nat.ann + nof.inv.ann,
         per = fix.nat.per + nof.nat.per + nof.inv.per) %>% 
  mutate(herb = herb.nof + herb.fix) %>% 
  full_join(plot_trt, by ="plot") %>%
  select(sample_id,plot,trt,year,block:seas,everything()) %>%
  mutate(fracfix = herb.fix/herb)

head(df.grps_mass) 

df.grps_cov <- df.veg_cov %>% 
  mutate(tot = 100*rowSums(.[4:76]),
         fix.nat.ann = 100*(Acann + Lubic + Trmic),
         fix.nat.per = 100*(Acgla),
         fix.inv.ann = 100*(Trhir),
         nof.nat.ann = 100*(Aghet + Cabre + Cahir + Calas + Crmur + Cuocc + Dapus +
           Empen + Epmin + Femic + Gaapa + Gacal + Gicap + Hemic + Lofil + Magra +
           Namel + Pscal + Racal + Urlin),
         nof.nat.per = 100*(Brele + Chpom + Erlan + Hesco + Hycon + Loday + Pehet +
           Phimb + Pielo + Stlep + Strig + Tacal),
         nof.inv.ann = 100*(Aicar + Avbar + Brdia + Brhor + Brmad + Ceglo + Cemel + Cyech +
           Erbot + Ercic + Femyu + Gapar + Gaphl + Hygla + Lasal + Laser + Logal +
           Lyarv + Pedub + Pslut + Saape + Soasp + Toarv + Tobar),
         nof.inv.per = 100*(Ehcal + Phaqu),
         nof.shb = 100*(Adfas + Bepin + Ercal + Quber + Arcan + Miaur),
         fix.shb = 100*(Cecun + Ceint + Pimon)) %>% 
  select(sample_id:year,tot:fix.shb) %>% 
  mutate(year = as.factor(year)) %>% 
  mutate(herb.fix = fix.nat.ann + fix.nat.per + fix.inv.ann,
         herb.nof = nof.nat.ann + nof.nat.per + nof.inv.ann + nof.inv.per,
         shb = nof.shb + fix.shb,
         herb.inv = fix.inv.ann + nof.inv.ann + nof.inv.per,
         herb.nat = fix.nat.ann + fix.nat.per + nof.nat.ann + nof.nat.per,
         ann = fix.nat.ann + fix.inv.ann + nof.nat.ann + nof.inv.ann,
         per = fix.nat.per + nof.nat.per + nof.inv.per) %>% 
  mutate(herb = herb.nof + herb.fix) %>% 
  full_join(plot_trt, by ="plot") %>%
  select(sample_id,trt,year,block:seas,everything()) %>%
  mutate(fracfix = herb.fix/herb)

head(df.grps_cov) 

##### Species and groups on sample_id level long####

df.shb_mass <- df.veg_hits %>% 
  select(sample_id:year,Adfas,Bepin,Ercal,Quber,Arcan,Miaur,Cecun,Ceint,Pimon) %>% 
  mutate(year = as.factor(year)) %>% gather(key=spp,value=count,Adfas:Pimon) %>%
  mutate(count = count*4.56)

df.nof_mass <- df.veg_hits %>% 
  select(sample_id:year,Aghet,Cabre,Cahir,Calas,Crmur,Cuocc,Dapus,Empen,Epmin,Femic,Gaapa,Gacal,Gicap,Hemic,Lofil,
         Magra,Namel,Pscal,Racal,Urlin,
         Brele,Chpom,Erlan,Hesco,Hycon,Loday,Pehet,Phimb,Pielo,Stlep,Strig,Tacal,
         Aicar,Avbar,Brdia,Brhor,Brmad,Ceglo,Cemel,Cyech,Erbot,Ercic,Femyu,Gapar,Gaphl,
         Hygla,Lasal,Laser,Logal,Lyarv,Pedub,Pslut,Saape,Soasp,Toarv,Tobar,
         Ehcal,Phaqu) %>% 
  mutate(year = as.factor(year)) %>% gather(key=spp,value=count,Aghet:Phaqu) %>%
  mutate(count = count*3.97)

df.fix_mass <- df.veg_hits %>%
  select(sample_id:year,Acann,Lubic,Trmic,Acgla,Trhir) %>% 
  mutate(year = as.factor(year)) %>% gather(key=spp,value=count,Acann:Trhir) %>%
  mutate(count = count*3.78)

grp1 <- data.frame(group = rep("fix.nat.ann"), spp = c("Acann","Lubic","Trmic"))
grp2 <- data.frame(group = rep("fix.nat.per"), spp = c("Acgla"))
grp3 <- data.frame(group = rep("fix.inv.ann"), spp = c("Trhir"))
grp4 <- data.frame(group = rep("nof.nat.ann"), spp = c("Aghet","Cabre","Cahir","Calas","Crmur","Cuocc","Dapus",
                                                       "Empen","Epmin","Femic","Gaapa","Gacal","Gicap","Hemic",
                                                       "Lofil","Magra","Namel","Pscal","Racal","Urlin"))
grp5 <- data.frame(group = rep("nof.nat.per"), spp = c("Brele","Chpom","Erlan","Hesco","Hycon","Loday",
                                                       "Pehet","Phimb","Pielo","Stlep","Strig","Tacal"))
grp6 <- data.frame(group = rep("nof.inv.ann"), spp = c("Aicar","Avbar","Brdia","Brhor","Brmad","Ceglo","Cemel",
                                                       "Cyech","Erbot","Ercic","Femyu","Gapar","Gaphl","Hygla",
                                                       "Lasal","Laser","Logal","Lyarv","Pedub","Pslut","Saape",
                                                       "Soasp","Toarv","Tobar"))
grp7 <- data.frame(group = rep("nof.inv.per"), spp = c("Ehcal","Phaqu"))
grp8 <- data.frame(group = rep("nof.shb"), spp = c("Adfas","Bepin","Ercal","Quber","Arcan","Miaur"))
grp9 <- data.frame(group = rep("fix.shb"), spp = c("Cecun","Ceint","Pimon"))

spp_grp <- rbind(grp1,grp2,grp3,grp4,grp5,grp6,grp7,grp8,grp9)  

df.spp_mass <- rbind(df.nof_mass,df.fix_mass,df.shb_mass) %>% 
  full_join(plot_trt) %>% full_join(spp_grp) %>% 
  mutate(plot = factor(plot), spp = factor(spp)) #%>% 
  select(sample_id:year,trt:group,spp:count)

spp_mass_wide <-df.spp_mass %>% select(-group) %>% spread(spp,count) %>% 
  select(sample_id:Urlin)
  

####
df.veg_wide <- full_join(df.grps_mass,df.grps_cov,
                     by=c("sample_id","plot","trt","year","block","site","seas"),suffix = c(".m", ".c")) %>% 
  mutate_at(vars(fracfix.m), ~replace(., is.nan(.), 0)) %>%
  mutate_at(vars(fracfix.c), ~replace(., is.nan(.), 0))

head(df.veg_wide)



##summarize to plot

df.veg_plot <- aggregate(data=df.veg_wide,.~plot+trt+year+block+site+seas, FUN=mean)



#######

hopwide <- df.moist_cut %>% full_join(df.Nmin_cut,by=c("plot","month","year")) %>%
  full_join(df.Cmin_cut,by=c("plot","month","year")) %>%
  full_join(df.CN_cut,by=c("plot","year")) %>% 
  full_join(plot_trt,by=c("plot")) %>% 
  select(plot,trt,year,month,block:seas,everything()) %>% 
  full_join(df.veg_plot,by=c("plot","trt","year","block","site","seas"))
  
  head(hopwide)

write.csv(hopwide,"hopwide.csv")

#Correlational DataSets
hop_cut <- hopwide %>% 
  select(plot:CNrat,herb.m,herb.fix.m,herb.nof.m) %>% 
  mutate(N_mg_g = perN*10, C_mg_g = perC*10) %>% 
  rename(resp = Resp_ugC_gsoil_day, ammon = ammon_ug,
         nitrate = nitrate_ug, totminN = totminN_ug,
         minRATE = minRATE_ug, nitrRATE = nitrRATE_ug) %>% 
  select(plot:resp,N_mg_g,C_mg_g,CNrat:herb.nof.m)
head(hop_cut)

