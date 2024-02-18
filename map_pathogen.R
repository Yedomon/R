## Set a working directory

setwd("C:/Users/ange_/Downloads/Alban_work/patogen_work/01.Reception/map")


## Packages

require(pacman)
pacman::p_load(RColorBrewer, ggplot2, patchwork, ggspatial, 
               ggrepel, raster,colorspace, ggpubr, 
               sf,openxlsx, rnaturalearth, rnaturalearthdata,
               scatterpie)


## Data

data_world_eac_summary_table = read.csv("data_map_owner.csv")


# increase max.overlaps for ggprepel

max.overlaps = Inf


  
## Focus on Eastern African countries

### Africa map

africa_map = ne_countries(continent = 'africa', returnclass = "sf")

### Eastern africa region

EACregion = subset(africa_map, africa_map$name == "Burundi" | 
                    africa_map$name == "Dem. Rep. Congo" | 
                    africa_map$name == "Kenya" |
                    africa_map$name == "Rwanda" |
                    africa_map$name== "Uganda" |
                    africa_map$name == "Tanzania")

### EAC data

eac_coord = read.csv("eac_coord.csv")

### plot Africa map by highlithing EAC

p = ggplot() + 
  
  geom_sf(data = africa_map,
          fill="#97cf9580", 
          colour="white", 
          size=10.0) +
  
  geom_sf(data = EACregion, 
          fill = "red", 
          size=10.0, 
          colour="white") +
  
  
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


#Save map

ggsave(p, file = "africa_eac_tech.pdf", limitsize = FALSE, width = 12, height = 10.5, dpi=1500 )


