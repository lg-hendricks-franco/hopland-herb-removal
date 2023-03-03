
setwd("C:/Users/linds/Dropbox/Documents (Selective Sync Conflict)/")

library(tidyverse)
library(maps)
library(mapdata)
library(viridis)
library(stringr)
library(ggmap)
library(ggspatial)
library(plotKML)
library(rnaturalearth)
library(naturalearthdata)

register_google(key = "AIzaSyBGbZVe32N8IKUiABy7GUB8Iz_t3Z0tN1I")
google_key()

theme_set(theme_bw())
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world) 


ca_df <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) %>% filter(ID == "california")
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))# %>% filter(ID == "california")
ca_county <- subset(counties, grepl("california", counties$ID))
ca_county$area <- as.numeric(st_area(ca_county))
site <- data.frame(longitude = c(-123.081454), latitude = c(39.021630))
site <- st_as_sf(site, coords = c("longitude", "latitude"), 
                 crs = 4326, agr = "constant")


ggplot(data = ca_df) +
  geom_sf() +
  xlab("") + ylab("") +
  geom_sf(color = "black", fill = "grey") +
  annotation_scale(location = "bl", width_hint = 0.5) +
  geom_sf(data = ca_county, fill = NA , color = gray(.5)) +
  geom_sf(data = site, size = 3, shape = 20, color = "darkred") +
  coord_sf(xlim = c(-124, -122), ylim = c(37, 40)) 


#  geom_rect(xmin = -123.12, xmax = -123.04, ymin = 38.96, ymax = 39.1, 
#            fill = NA, colour = "black", size = 0.5)

ggsave("cacounties.png",path = "C:/Users/linds/Dropbox/Documents (Selective Sync Conflict)/Dissertation/DRAFTS/Ch.3-Hopland Herb Manipulation/Figures/", 
       scale = 1, width = 6, height = 6, units = "in",
       dpi = 300)









##############################
#Code Not Used

ggplot(data = ca_county, mapping = aes(x = long, y = lat, group = group)) +
  #blank() +
  #geom_sf() +
  geom_polygon(data = ca_df, aes(x = long, y = lat), fill = "grey", color = "black") +
  geom_polygon(data = ca_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA) +
  geom_point(aes(x=-123.0815,y=39.02163)) +
  geom_rect(xmin = -123.085, xmax = -123.078, ymin = 39.019, ymax = 39.025, 
            fill = NA, colour = "black", size = 0.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  north(data=ca_county, location = "bottomleft") +
  coord_quickmap(xlim = c(-124, -122),  ylim = c(37, 40))


siteJ_map_sat<-ggmap(get_googlemap(center = c(lon=-123.0815,lat=39.02163),
                                   zoom = 17,
                                   maptype = "satellite", #color = "bw",
                                   my_api = AIzaSyBGbZVe32N8IKUiABy7GUB8Iz_t3Z0tN1I))
siteJ_map_sat

bb <- attr(siteJ_map_sat, "bb")
bb2 <- data.frame(long = unlist(bb[c(2, 4)]), lat = unlist(bb[c(1,3)]))

ggmap(get_map("C:/Users/linds/Dropbox/Documents (Selective Sync Conflict)/Dissertation/DRAFTS/Ch.3-Hopland Herb Manipulation/Figures/Sites"))

siteJ_map_sat + 
  theme_bw() +
  
  
  
  #"terrain", "terrain-background", "terrain-labels", "terrain-lines",
  #"toner", "toner-2010", "toner-2011", "toner-background", "toner-hybrid",
  #"toner-labels", "toner-lines", "toner-lite", "watercolor"
  
  
  #NorthCoast<-get_stamenmap(bbox = c(left = -123.085, bottom = 39.019,
  #                                   right = -123.078, top = 39.025), 
  #              color = "bw", maptype = "terrain-background",zoom =13) 


ggmap(NorthCoast) #+
geom_polygon(data = ca_df, aes(x = long, y = lat, group =group), fill = "none", color = "black") +
  geom_polygon(data = ca_county, aes(x = long, y = lat, group =group), fill = NA, color = "black") +
  geom_polygon(color = "black", fill = NA) +
  geom_rect(xmin = -123.085, xmax = -123.078, ymin = 39.019, ymax = 39.025, 
            fill = NA, colour = "black", size = 0.5) # +




