library(tidyverse)
library(patchwork)
library(ggrepel)
library(geosphere)
library(sf)
library(terra)
library(raster)
library(ggspatial)

####################################################################################################
## Read in shape files, elevation model, site and health facility coordinates
####################################################################################################

#Read in District (ADM2) boundaries from shape file
mg.adm2 <- st_read("/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2023_cyclones/mdg_adm_bngrc_ocha_20181031_shp/mdg_admbnda_adm2_BNGRC_OCHA_20181031.shp")
#Read in Country (ADM0) boundaries for Madagascar
mg.adm0 <- st_read("/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2023_cyclones/mdg_adm_bngrc_ocha_20181031_shp/mdg_admbnda_adm0_BNGRC_OCHA_20181031.shp")

#read in MNJ locality data
df.mnj.points <- readr::read_csv("/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_mapping_site_gps/site_midpoints_20230316.csv")

#read in elev data from: 
#European Space Agency, Sinergise (2021). Copernicus Global Digital Elevation Model. Distributed by OpenTopography. https://doi.org/10.5069/G9028PQB. Accessed: 2024-03-22
#https://portal.opentopography.org/raster?opentopoID=OTSDEM.032021.4326.3
#Using copernicus to get coastal water bodies (elev < 0.5m), then using geom_tile to show on solid base map

#SE Madagascar selection elevation data
r.elev.SE <- raster("/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_mapping_site_gps/digital_elevation_models/SE Madagasccar/output_COP30.tif")
r.elev.SE

#Read in health facility coordinates for Vatovavy and Fitovinany
df.hf.points <- readr::read_csv("/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_mapping_site_gps/health_facilities_20241104.csv")


####################################################################################################
## Processing raster data
####################################################################################################

#convert to data frame and clean
df.elev.SE <- as.data.frame(r.elev.SE, xy = TRUE) %>% 
  #Renaming column for clarity
  rename(elev_m = output_COP30) %>%
  #mutate(elev_m = ifelse(elev_m <= 0, NA, elev_m)) %>%
  #Removing areas outside selected coordinates
  #mutate(elev_m = ifelse(x > 47.5 & x < 48.8, elev_m, NA)) %>% 
  #mutate(elev_m = ifelse(y < -20.7 & y > -21.8, elev_m, NA)) %>%
  #Creating a dichotomous flag for sea level
  #mutate(water_flag = ifelse(elev_m < 0.6, 1, 0)) %>%
  #Creating bins for elevation
  mutate(elev.cat = case_when(
    elev_m <= 0                  ~ NA_character_,
    elev_m >  0   & elev_m < 0.5 ~ "<0.5",
    elev_m >= 0.5 & elev_m < 1.0 ~ "0.5-1.0",
    elev_m >= 1.0 & elev_m < 2.0 ~ "1.0-2.0",
    elev_m >= 2.0 & elev_m < 5.0 ~ "2.0-5.0",
    elev_m >= 5.0 & elev_m < 10  ~ "5.0-10",
    elev_m >= 10  & elev_m < 100 ~ "10-100",
    elev_m >= 100                 ~ "100+")) %>%
  mutate(elev.cat = factor(elev.cat, levels = c("<0.5", "0.5-1.0", "1.0-2.0", "2.0-5.0", "5.0-10", "10-100", "100+")))

####################################################################################################
## MADAGSACAR MAP: Solid fill, highlighting MNJ district
####################################################################################################

p.MADA <- ggplot() + 
  #Base layer: Madagascar, fill color set to match end of cividis color scale
  geom_sf(data = mg.adm0, fill = "#01173A", alpha = 1, color = "white", linewidth = 0) +
  #MNJ District Boundary
  geom_sf(data = mg.adm2 %>% filter(ADM2_EN == "Mananjary"), fill = "black", alpha = 0, color = "white", linewidth = 0.4) +
  #POINTS
  #MNJ District Capital
  geom_point(data = df.mnj.points %>% filter(Code == "MNJ"), 
             aes(x = LON, y = LAT),
             color = "grey70", alpha = 0.7, size = 2.5, shape = 0, stroke = 1) +
  #LABELS
  #MNJ District Capital Label
  geom_label_repel(data = df.mnj.points %>% filter(Code == "MNJ"), 
                   aes(x = LON, y = LAT, label = code.new),
                   color = "black",
                   nudge_x = 50-subset(df.mnj.points, Code == "MNJ")$LON, 
                   hjust = 0.5,
                   segment.size = 0.3,
                   direction = "y",
                   #point.padding = 0.3,
                   segment.curvature = -0.1,
                   segment.ncp = 3,
                   segment.angle = 90) +
  #Coords
  coord_sf() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"))
p.MADA


####################################################################################################
## SE MADAGSACAR MAP (MNJ DISTRICT): Fill by elevation bin
####################################################################################################

#note plotting will throw an error for the elevation values at 0 or below 0
sum(is.na(df.elev.SE$elev.cat))

#Note the plot takes approx. 3 minutes to render
p.SE <- ggplot() + 
  #Base layer: Madagascar
  geom_sf(data = mg.adm0, fill = "#FBEA72", alpha = 1, color = "white", linewidth = 0) +
  #Elevation profile
  geom_raster(data = df.elev.SE %>% filter(!is.na(elev.cat)), aes(x = x, y = y, fill = elev.cat), alpha = 1) +
  #MNJ District Boundary
  geom_sf(data = mg.adm2 %>% filter(ADM2_EN == "Mananjary"), fill = "black", alpha = 0, color = "white", linewidth = 0.4) +
  #POINTS
  #Study Sites
  geom_point(data = df.mnj.points %>% filter(Code != "MNJ"), 
             aes(x = LON, y = LAT),
             color = "black", fill = "grey90", alpha = 0.75, size = 4, shape = 21, stroke = 1) +
  #MNJ District Capital
  geom_point(data = df.mnj.points %>% filter(Code == "MNJ"), 
             aes(x = LON, y = LAT),
             color = "grey70", alpha = 0.9, size = 4, shape = 0, stroke = 1.5) +
  #LABELS
  #Study Site Labels
  geom_label_repel(data = df.mnj.points %>% filter(Code != "MNJ"), 
                   aes(x = LON, y = LAT, label = code.new),
                   color = "black",
                   nudge_x = 48.6 - subset(df.mnj.points, Code != "MNJ")$LON, 
                   hjust = 0.5,
                   segment.size = 0.3,
                   direction = "y",
                   point.padding = 0.3,
                   segment.curvature = -0.1,
                   segment.ncp = 3,
                   segment.angle = 90) +
  #MNJ District Capital Label
  geom_label_repel(data = df.mnj.points %>% filter(Code == "MNJ"), 
                   aes(x = LON, y = LAT, label = code.new),
                   color = "black",
                   nudge_x = 48.5-subset(df.mnj.points, Code == "MNJ")$LON, 
                   hjust = 0.5,
                   segment.size = 0.3,
                   direction = "y",
                   point.padding = 0.2,
                   segment.curvature = -0.1,
                   segment.ncp = 3,
                   segment.angle = 90) +
  #Coords
  coord_sf(xlim = c(47.5, 48.8), ylim = c(-20.7, -21.8)) +
  #Color scale
  scale_fill_viridis_d(option = "cividis", name = "Elevation (m)", direction = -1) +
  theme(legend.position = "right",
    panel.grid = element_blank(),
    axis.title = element_blank(), 
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "white"))
p.SE



####################################################################################################
## Health facilities by distance
####################################################################################################


#Writing a function to calculate distance in km between a locality and a selection of health facilities
#Requires LAT and LON to be in columns named LAT and LON

f.distGeo <- function(df.reference, df.query){
  
  df.ref <- df.reference
  df.que <- df.query
  
  l.dist <- list(NA)
  nl.dist <- list(NA)
  
  for(i in 1:length(df.ref$LAT)){
      t.df.ref <- df.ref[i,]
      
    for(j in 1:length(df.que$LAT)){
      l.dist[[j]] <- tibble(
        dist.km = distGeo(c(t.df.ref$LON, t.df.ref$LAT), c(df.que$LON[j], df.que$LAT[j]))/1000,
        code.new = t.df.ref$code.new,
        code = t.df.ref$Code,
        site = t.df.ref$Site,
        hf.district = df.que$district_name[j],
        hf.name = df.que$fs_name[j],
      )
    }
      nl.dist[[i]] <- l.dist
  }
  
  df.dist <- bind_rows(nl.dist)
  df.dist <- df.dist %>% dplyr::select(code.new:hf.name, dist.km)
  
  return(df.dist)
}


df.hf.dist.all <- f.distGeo(df.reference = df.mnj.points %>% filter(Site != "Mananjary Ville"),
                            df.query = df.hf.points)

df.hf.nearest <- df.hf.dist.all %>% group_by(code.new) %>% mutate(min.dist = min(dist.km)) %>% 
  filter(dist.km == min.dist)

mean(df.hf.nearest$min.dist)



####################################################################################################
## Mapping health facilities
####################################################################################################

p.SE.hf <- ggplot() + 
  #Base layer: Madagascar
  geom_sf(data = mg.adm0, fill = "grey90", alpha = 1, color = "white", linewidth = 0) +
  #MNJ District Boundary
  geom_sf(data = mg.adm2 %>% filter(ADM2_EN == "Mananjary"), fill = "black", alpha = 0, color = "black", linewidth = 0.4) +
  #POINTS
  #Study Sites
  geom_point(data = df.mnj.points %>% filter(Code != "MNJ"), 
             aes(x = LON, y = LAT),
             color = "black", fill = "grey90", alpha = 0.75, size = 4, shape = 21, stroke = 1) +
  #MNJ District Capital
  geom_point(data = df.mnj.points %>% filter(Code == "MNJ"), 
             aes(x = LON, y = LAT),
             color = "grey70", alpha = 0.9, size = 4, shape = 0, stroke = 1.5) +
  #LABELS
  #Study Site Labels
  geom_label_repel(data = df.mnj.points %>% filter(Code != "MNJ"), 
                   aes(x = LON, y = LAT, label = code.new),
                   color = "black",
                   nudge_x = 48.6 - subset(df.mnj.points, Code != "MNJ")$LON, 
                   hjust = 0.5,
                   segment.size = 0.3,
                   direction = "y",
                   point.padding = 0.3,
                   segment.curvature = -0.1,
                   segment.ncp = 3,
                   segment.angle = 90) +
  #MNJ District Capital Label
  geom_label_repel(data = df.mnj.points %>% filter(Code == "MNJ"), 
                   aes(x = LON, y = LAT, label = code.new),
                   color = "black",
                   nudge_x = 48.5-subset(df.mnj.points, Code == "MNJ")$LON, 
                   hjust = 0.5,
                   segment.size = 0.3,
                   direction = "y",
                   point.padding = 0.2,
                   segment.curvature = -0.1,
                   segment.ncp = 3,
                   segment.angle = 90) +
  #Adding nearest health facilities
  geom_point(data = df.hf.points %>% filter(fs_name %in% df.hf.dist.all$hf.name[df.hf.dist.all$dist.km < 15]), 
             aes(x = LON, y = LAT),
             color = "red3", fill = "grey90", alpha = 0.75, size = 1.5, shape = 3, stroke = 1) +
  #Adding all health facilities
  geom_point(data = df.hf.points %>% filter(!(fs_name %in% df.hf.dist.all$hf.name[df.hf.dist.all$dist.km < 15])), 
             aes(x = LON, y = LAT),
             color = "black", fill = "grey90", alpha = 0.75, size = 1.5, shape = 3, stroke = 1) +
  annotation_scale(location = "tl") +
  #Coords
  coord_sf(xlim = c(47.5, 48.8), ylim = c(-20.7, -21.8)) +
  #Theme
  theme(legend.position = "right",
        panel.grid = element_blank(),
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"))
p.SE.hf














