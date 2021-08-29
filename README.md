
# Mapa de elevación de Chicama en R

```r

#------------------------------------------------------------------------
require(pacman)
pacman::p_load(RColorBrewer, ggspatial, raster,colorspace, ggpubr, sf,openxlsx)
#------------------------------------------------------------------------
Peru               <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
Cuencas_peru       <- st_read ("SHP/Cuencas_peru.shp")  
Rio_libe           <- st_read ("SHP/RIOS_LA_LIBERTAD_geogpsperu_SuyoPomalia_931381206.shp")  
Rio_caja           <- st_read ("SHP/RIOS_CAJAMARCA_geogpsperu_SuyoPomalia_931381206.shp")  
Cuencas_peru       <- st_transform(Cuencas_peru ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Rio_caja           <- st_transform(Rio_caja ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Rio_libe           <- st_transform(Rio_libe  ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Cuenca_Chicama     <- subset(Cuencas_peru , NOMB_UH_N5  == "Chicama")
Cuencas_rios1      <- st_intersection(Rio_caja, Cuenca_Chicama)
Cuencas_rios       <- st_intersection(Rio_libe, Cuenca_Chicama)
Cuencas_peru_c     <- cbind(Cuencas_peru, st_coordinates(st_centroid(Cuencas_peru$geometry)))

dem = raster("raster/ASTGTM_S08W080_dem.tif")
dem2 = raster("raster/ASTGTM_S08W079_dem.tif")
DEM_total<- raster::merge(dem, dem2,dem)

Cuenca_Chicama_alt     <- crop(DEM_total, Cuenca_Chicama)
Cuenca_Chicama_alt     <- Cuenca_Chicama_alt   <- mask(Cuenca_Chicama_alt  , Cuenca_Chicama)
plot(Cuenca_Chicama_alt )

dem.p          <-  rasterToPoints(Cuenca_Chicama_alt )
df             <-  data.frame(dem.p)
colnames(df) = c("lon", "lat", "alt")

aps            = terrain(Cuenca_Chicama_alt , opt = "aspect", unit= "degrees")
dem.pa          <-  rasterToPoints(aps  )
df_a            <-  data.frame(dem.pa)
#------------------------------------------------------------------------
Data     <- read.xlsx("Excel/Embrete.xlsx", sheet="Hoja2") 
Data[1,1] <- "MAPA DE ELEVACION DE \nLA CUENCA, \nChicama"
Data[2,1] <- "Elaboradpor: \nGorky Florez Castillo"
Data[3,1] <- "Escala: Indicadas"
Data[4,1] <- "Sistemas de Coordenadas UTM \nZona 18S \nDatum:WGS84"
colnames(Data ) <- c("Mapa elaborado \nen  RStudio")
Tabla.p <- ggtexttable(Data, rows = NULL,theme =ttheme( base_size =6, "lBlackWhite"))

Data1     <- read.xlsx("Excel/Embrete.xlsx", sheet="Hoja1") 
Tabla.p1 <- ggtexttable(Data1, rows = NULL,theme =ttheme( base_size =6, "mBlue"))
#-----------------------------------------------------------------------

A <-ggplot()+ 
  geom_raster(data = df, aes(lon,lat,fill = alt),alpha=0.75) + 
  geom_raster(data = df_a, aes(x=x, y=y, alpha=aspect),fill="gray20")+
  scale_alpha(guide=FALSE,range = c(0,1.00))   +
  scale_fill_distiller(palette   = "RdYlBu",name="Elevacion \n(m.s.n.m)",
                       labels = c("[1 - 270] ","[270-400]", "[400-700]", "[700-1200]", "[1200-1700]",
                                  "[1700-2200]", "[2200-3500]", "[3500-3700]", "[3700-4100]", "[4100-4286]"),
                       breaks = c(0, 270, 400,700,1200,1700,2200,3500,3700,4100))+
  theme_bw()+coord_equal()+
  guides(fill = guide_legend(title.position = "top",direction = "vertical",
                             title.theme = element_text(angle = 0, size = 9, colour = "black"),
                             barheight = .5, barwidth = .95,
                             title.hjust = 0.5, raster = FALSE,
                             title = 'Elevacion \n(m.s.n.m)'))+
  geom_sf(data=Cuencas_rios, color="blue", size=0.3)+
  geom_sf(data=Cuencas_rios1, color="blue", size=0.3)+
  scale_x_continuous(name=expression(paste("Longitude (",degree,")"))) +
  scale_y_continuous(name=expression(paste("Latitude (",degree,")")))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme(legend.position = c(0.85,0.20),
        panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        legend.background = element_blank(),
        plot.title=element_text(color="#666666", size=12, vjust=1.25,  family="Raleway",  hjust = 0.5),
        legend.box.just = "left",
        legend.text = element_text(size=9,face=2),
        legend.title = element_text(size=9,face=2))+
  guides(fill = guide_legend(nrow = 5, ncol=2))+
  annotation_custom(ggplotGrob(Tabla.p ),
                    xmin = -79.2, xmax = -79,ymin = -8.2,ymax = -8)+
  annotate(geom = "text", x = -78.7, y = -8.18, 
           label = "Quieres desarrollar mapas de este tipo \n inscribete a \nAPRENDE R DESDE CERO PARA SIG ", 
           fontface = "italic", color = "black", size = 3)

B <-ggplot()+
  geom_sf(data= Peru, fill="white")+
  geom_sf(data= Cuenca_Chicama, fill="red")+
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(colour= "black", size= 1))+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering (),
                         height = unit(0.8, "cm"),# tamaño altura
                         width = unit(0.8, "cm"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  annotate(geom = "text", x = -78, y = -17, 
           label = "Mapa de Macrolocalizacion", 
           fontface = "italic", color = "black", size = 3)

C <-ggplot()+
  geom_sf(data= Peru, fill=NA)+
  geom_sf(data= Cuenca_Chicama, fill="gray")+
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(colour= "black", size= 1))+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering (),
                         height = unit(0.8, "cm"),# tamaño altura
                         width = unit(0.8, "cm"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  annotate(geom = "text", x = -78.8, y = -8, 
           label = "Mapa Departamental", 
           fontface = "italic", color = "black", size = 2)+
  annotate(geom = "text", x = -78.8, y = -7.6, 
           label = "Mapa Departamental", 
           fontface = "italic", color = "black", size = 2)+
  coord_sf(xlim = c( -79.3,-78.1 ), ylim = c( -8.5,-6.9),expand = FALSE)



D <-ggplot()+
  geom_raster(data = df, aes(lon,lat,fill = alt), show.legend = F)+
  scale_fill_distiller(palette   = "RdYlBu",name="Elevacion \n(m.s.n.m)",
                       labels = c("[1 - 270] ","[270-400]", "[400-700]", "[700-1200]", "[1200-1700]",
                                  "[1700-2200]", "[2200-3500]", "[3500-3700]", "[3700-4100]", "[4100-4286]"),
                       breaks = c(0, 270, 400,700,1200,1700,2200,3500,3700,4100))+
  theme_bw()+coord_equal()+
  guides(fill = guide_legend(title.position = "top",direction = "vertical",
                             title.theme = element_text(angle = 0, size = 9, colour = "black"),
                             barheight = .5, barwidth = .95,
                             title.hjust = 0.5, raster = FALSE,
                             title = 'Elevacion \n(m.s.n.m)'))+
  scale_x_continuous(name=expression(paste("Longitude (",degree,")"))) +
  scale_y_continuous(name=expression(paste("Latitude (",degree,")")))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme(legend.position = c(0.65,0.15),
        panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        legend.background = element_blank(),
        plot.title=element_text(color="#666666", size=12, vjust=1.25,  family="Raleway",  hjust = 0.5),
        legend.box.just = "left",
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.3,"cm"), #ancho de cuadrados de referencia 
        legend.text = element_text(size=8,face=2),
        legend.title = element_text(size=8,face=2))+
  guides(fill = guide_legend(nrow = 5, ncol=2))


E <-ggplot()+ 
  geom_raster(data = df, aes(lon,lat,fill = alt), show.legend = F) + 
  geom_raster(data = df_a, aes(x=x, y=y, alpha=aspect), show.legend = F)+
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 8, name = "Greys"), 
                       na.value = 'white')+
  scale_x_continuous(name=expression(paste("Longitude (",degree,")"))) +
  scale_y_continuous(name=expression(paste("Latitude (",degree,")")))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme_bw()


Fa <-ggplot() +
  coord_equal(xlim = c(0, 28), ylim = c(0, 20), expand = FALSE) +
  annotation_custom(ggplotGrob(A), xmin = 0, xmax = 20, ymin = 4, ymax = 20)+
  annotation_custom(ggplotGrob(B), xmin = 19.5, xmax = 23.5, ymin = 13.5, ymax = 20) +
  annotation_custom(ggplotGrob(C), xmin = 23.5, xmax = 28, ymin = 13.5, ymax = 20) +
  annotation_custom(ggplotGrob(D), xmin = 19.5, xmax = 28, ymin = 8, ymax = 14) +
  annotation_custom(ggplotGrob(E), xmin = 19.5, xmax = 28, ymin = 2, ymax = 8) +
  annotation_custom(ggplotGrob(Tabla.p1 ),
                    xmin = 4, xmax = 10,ymin = 0,ymax = 4)+
  theme_bw()+
  labs(title="MAPA DE ELEVACIONES CUENCA CHICAMA", 
       subtitle="Elevacion con aspecto con ggplot", 
       caption="Fuente: https://www.geogpsperu.com/2018/08/descargar-imagenes-aster-gdem-aster.html", 
       color=NULL)


ggsave(plot = Fa ,"Mpas/Chicama_elevacion.png", units = "cm", width = 30,height = 22, dpi = 900) 


```

[Code source](https://github.com/GorkyFlorez/Mapa_elevacion_chicama/blob/main/04%20Mapa%20de%20elevacion%20de%20cuenca%20Chicama.R)


Output

![img]()

# Mapa de elevación, precipitacion y temperatura de Calabria en R El modelo es totalmente reproducible solo basta de ejecutar el siguiente script o también anexo el github que lo contiene.


[code source](https://github.com/GorkyFlorez/Bioclima_Calabria)


```r

library(tidyverse)
library(raster)
library(openxlsx)
library(sf)
library(ggspatial)
library(gridExtra)
library(ggrepel)
library(ggplot2)
library(colorspace)
library(cowplot)
Italia      <- getData('GADM', country='ITALY', level=2) %>% st_as_sf()
Calabria    <- subset(Italia, NAME_1  == "Calabria")
ITALY_Alt   <- getData("alt", country='ITALY', mask=TRUE)
Calabria_c  <- cbind(Calabria, st_coordinates(st_centroid(Calabria$geometry)))
Calabria_ch   <- crop(ITALY_Alt, Calabria)
Calabria_ch     <- Calabria_ch  <- mask(Calabria_ch , Calabria)
plot(Calabria_ch )
dem.p          <-  rasterToPoints(Calabria_ch )
df             <-  data.frame(dem.p)
colnames(df) = c("lon", "lat", "alt")
summary(df$alt)
cortes <- c(0,500, 1000,1500,1800,  2101)
A<- ggplot()+
  geom_sf(data= Italia, fill="white")+
  geom_raster(data = df, aes(x=lon, y=lat, fill = alt) )+
  scale_fill_distiller(palette   = "RdYlGn",
                       na.value = 'white',breaks = cortes ,
                       labels = c("[0 - 499] ","[500 - 999]","[1000-1499]", "[1500-1799]", "[1800-1999]", "[2000-2101]"))+
  theme_bw()+
  scale_x_continuous(name=expression(paste("Longitude (",degree,")"))) +
  scale_y_continuous(name=expression(paste("Latitude (",degree,")")))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  guides(fill = guide_legend(title.position = "right",direction = "vertical",
                             title.theme = element_text(angle = 90, size = 9, colour = "black"),
                             barheight = .5, barwidth = .95,
                             title.hjust = 0.5, raster = FALSE,
                             title = 'Elevation \n(m.s.n.m)'))+
  geom_sf(data= Calabria, fill=NA)+
  geom_point(data = Calabria_c, aes(x=X, y=Y),color = "red") +
  geom_text_repel(data = Calabria_c, aes(x=X, y=Y, label = NAME_2),
                       size = 3.5,box.padding = 9, segment.angle = 30, fontface = "bold")+
  theme(legend.position = c(0.85,0.20),
       panel.grid.major = element_line(color = gray(.5),
                                       linetype = "dashed", size = 0.5),
       legend.background = element_blank(),
       legend.text = element_text(size=7,face=2),
       legend.title = element_text(size=7,face=2),
       panel.background = element_rect(fill = "aliceblue"))+
  coord_sf(xlim = c(15.2,17.9), ylim = c(37.8,40.3),expand = FALSE)+
  labs(title = "A)",# añadir titulo
       caption = "Muestra elaborada por @Gorky ")
B <- ggplot()+
  geom_sf(data= Italia, fill="white")+
  geom_sf(data= Calabria, fill="red")+
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(colour= "black", size= 1))
W <-ggdraw() + draw_plot(A) + draw_plot(B, x = 0.77, y = 0.67, width = .25, height = .25)
#------------------------------------------------------------------------
Bio_pre     <-getData("worldclim", var = "prec",res=0.5,lon=15, lat=40) 
Bio_tem     <-getData("worldclim", var = "tmean",res=0.5,lon=15, lat=40)
plot(Bio_tem[[2]] )                                                  
Precipitación_anual   <- crop(Bio_pre[[2]], Calabria)
Precipitación_anual   <- Precipitación_anual  <- mask(Precipitación_anual , Calabria)
plot(Precipitación_anual)
dem.pre          <-  rasterToPoints(Precipitación_anual )
df_pre             <-  data.frame(dem.pre)
colnames(df_pre) = c("lon", "lat", "prec")
summary(df_pre$prec)
Temperatura_anual   <- crop(Bio_tem[[2]] , Calabria)
Temperatura_anual  <- Temperatura_anual  <- mask(Temperatura_anual , Calabria)
plot(Temperatura_anual)
dem.tem          <-  rasterToPoints(Temperatura_anual )
df_tem             <-  data.frame(dem.tem)
colnames(df_tem) = c("lon", "lat", "tem")
summary(df_tem$tem)
C <-ggplot()+
    geom_sf(data= Italia, fill="white")+
    geom_raster(data = df_pre, aes(x=lon, y=lat, fill = prec) )+
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 8, name = "GnBu"), 
                         na.value = 'white', breaks = c(60,80,100,114),
                         labels = c("[51 - 59] ","[60 - 79]","[80-99]", "[100-114]"))+
    guides(fill = guide_legend(title.position = "top",direction = "vertical",
           title.theme = element_text(angle = 0, size = 9, colour = "black"),
           barheight = .5, barwidth = .95,
           title.hjust = 0.5, raster = FALSE,
           title = 'Precipitation\n(mm)'))+
    geom_sf(data= Calabria, fill=NA)+
    geom_point(data = Calabria_c, aes(x=X, y=Y),color = "red") +
    geom_text_repel(data = Calabria_c, aes(x=X, y=Y, label = NAME_2),
    size = 3.5,box.padding = 9, segment.angle = 30, fontface = "bold")+
    theme_bw()+
    scale_x_continuous(name=expression(paste("Longitude (",degree,")"))) +
   scale_y_continuous(name=expression(paste("Latitude (",degree,")")))+
    annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
    ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
    coord_sf(xlim = c(15.2,17.9), ylim = c(37.8,40.3),expand = FALSE)+
    theme(legend.position = c(0.85,0.20),
    panel.grid.major = element_line(color = gray(.5),
          linetype = "dashed", size = 0.5),
     legend.background = element_blank(),
     legend.text = element_text(size=7,face=2),
     legend.title = element_text(size=7,face=2),
     panel.background = element_rect(fill = "aliceblue"))+
     labs(title = "😎",# añadir titulo
          caption = "Muestra elaborada por @Gorky ")
D <-ggplot()+
  geom_sf(data= Italia, fill="white")+
  geom_raster(data = df_tem, aes(x=lon, y=lat, fill = tem) )+
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 8, name = "Spectral"), 
                       na.value = 'white', breaks = c(-30, 0, 30,60,90,129),
                       labels = c("[-10 - -29] ","[-30 - 0]","[0 - 29]", "[30 - 59]",
                                  "[60 - 89]", "[90 - 129]"))+
  guides(fill = guide_legend(title.position = "top",direction = "vertical",
                             title.theme = element_text(angle = 0, size = 9, colour = "black"),
                             barheight = .5, barwidth = .95,
                             title.hjust = 0.5, raster = FALSE,
                             title = 'Temperature \n(°C)'))+
  geom_sf(data= Calabria, fill=NA)+
  geom_point(data = Calabria_c, aes(x=X, y=Y),color = "red") +
  geom_text_repel(data = Calabria_c, aes(x=X, y=Y, label = NAME_2),
                  size = 3.5,box.padding = 9, segment.angle = 30, fontface = "bold")+
  theme_bw()+
  scale_x_continuous(name=expression(paste("Longitude (",degree,")"))) +
  scale_y_continuous(name=expression(paste("Latitude (",degree,")")))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  coord_sf(xlim = c(15.2,17.9), ylim = c(37.8,40.3),expand = FALSE)+
  theme(legend.position = c(0.85,0.20),
        panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        legend.background = element_blank(),
        legend.text = element_text(size=7,face=2),
        legend.title = element_text(size=7,face=2),
        panel.background = element_rect(fill = "aliceblue"))+
  labs(title = "C)",# añadir titulo
       caption = "Muestra elaborada por @Gorky ")
Ww <-ggdraw() + draw_plot(C) + draw_plot(B, x = 0.77, y = 0.67, width = .25, height = .25)
Www <-ggdraw() + draw_plot(D) + draw_plot(B, x = 0.77, y = 0.67, width = .25, height = .25)
ggsave(plot = W ,"Mpas/Italy_elevacion.png", units = "cm", width = 21,height = 29, dpi = 900) 
ggsave(plot = Ww ,"Mpas/Italy_elevacion.png.png", units = "cm", width = 21,height = 29, dpi = 900) 
ggsave(plot = Www ,"Mpas/Italy_Temperatura.png", units = "cm", width = 21,height = 29, dpi = 900)


```
Italy_elevacion

![Italy_elevacion.png]()




Italy_elevacion

![Italy_elevacion.png]()


Italy_Temperatura


![Italy_Temperatura]()






[Incluir subplot en mapa con ggplot](https://analisisydecision.es/incluir-subplot-en-mapa-con-ggplot/)



![img](https://analisisydecision.es/wp-content/uploads/2021/02/mapa_subplot_ggplot3.png)


07 agust 2021

ggally correlation



```r


# Data imporatation

data_f2 = read.csv("F2_Plants_groupingnodes.csv", h = T, sep = ",")

View(data_f2)


# Package

library(GGally)


# Run

ggpairs(data_f2, columns = 2:5)

ggpairs(data_f2, columns = 2:5, ggplot2::aes(colour=Type))





```


output

![img](https://github.com/Yedomon/R/blob/main/ggally_8_78_10_23_landscape.png)




data is [here](https://github.com/Yedomon/R/blob/main/F2_Plants_groupingnodes.csv)









map guy R [github](https://github.com/GorkyFlorez/Mapa_tipos_bosques_Inotawa?fbclid=IwAR2q-FZrQn7sVCom8_Dn-Z27W7VEu0eluwafjcXFCP1HjTnVhJwpA9X72p8)

![img](https://scontent-gmp1-1.xx.fbcdn.net/v/t1.6435-9/221714598_1254804264984251_294660843831595982_n.jpg?_nc_cat=104&ccb=1-4&_nc_sid=8bfeb9&_nc_ohc=1B8E0Hrwj2EAX-W0-Z0&_nc_ht=scontent-gmp1-1.xx&oh=1ddb868a7c5bc0a5930414f6adef10eb&oe=6133383D)

code


```r
# Mapa de Ubicacion de tipo de bosque de Inotawa
#------------------------------------------------------------------------
require(pacman)
pacman::p_load(ggplot2,rgdal,ggspatial, raster, cowplot, egg, rnaturalearth, sf,hddtools,ggsn,ggpubr, yarrr,
               tibble,ggrepel,rnaturalearthdata  )
#-----------------------------------------------------------------------------------
#Cargar los datos Shp
Peru_n          <- getData('GADM', country='Peru', level=0) %>% st_as_sf()       # Extracion del paiz
Sur_America     <- st_read ("Data shp/SurAmerica.shp")  
Tipo_Bosque     <-st_read("Data shp/Tipo_de_Bosque.shp")
Rios            <-st_read("Data shp/Rios.shp")
Agricola        <-st_read("Data shp/Agricola.shp")
Ino             <-st_read("Data shp/Inotawa.shp")
Ino2            <-st_read("Data shp/Inotawa2.shp")
SurAmerica_utm  <- st_transform(Sur_America,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Tipo_Bosque     <- st_transform(Tipo_Bosque,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Rios            <- st_transform(Rios,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Agricola        <- st_transform(Agricola,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Ino             <- st_transform(Ino,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Ino2            <- st_transform(Ino2,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Marco_Tipo_Bosq = st_as_sfc(st_bbox(Tipo_Bosque ))


#-----------------------------------------------------------------------------------
#                        Colores para el panel
col <- c('#ffffff','#eaf1e7','#d4e2cf','#bfd4b8','#abc5a1','#96b78a',
         '#81a974','#6c9b5f','#588d4a','#428034','#28711d','#006400')
library(cartography)
col1 = carto.pal(pal1 = "green.pal", n1 = 15)
map.colors <- c(col,col1)
#-----------------------------------------------------------------------------------

MDD <-ggplot()+
  geom_sf(data= SurAmerica_utm, col = "grey80", fill = "grey80") +
  geom_sf(data= Peru_n, fill= NA, col="black")+
  geom_sf(data = Marco_Tipo_Bosq , fill=NA, color ="red")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect(linetype = "dashed", color = "grey20", fill = NA, size = 0.4))

MDD_bosque <-ggplot() +
  geom_sf(data = Tipo_Bosque,aes(fill = Simbolo),  alpha = 1, linetype = 1 )  +
  scale_fill_manual(values = map.colors)+
  ylab("Latitude") +
  xlab("Longitude") +
  annotate(geom = "text", x = -71, y = -9.5, label = "Tipo de Bosque en \nMadre de Dios", fontface = "italic", color = "black", size = 4)+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-73,-67.6), ylim = c(-13.5,-9),expand = FALSE)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(linetype = "solid", color = "black", fill = NA),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.3,"cm"), #ancho de cuadrados de referencia 
        legend.title =element_text(size=10, face = "bold"), #tamaño de titulo de leyenda
        legend.text =element_text(size=8, face = "bold"),
        legend.position = c(.89, .25),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 16))+
        labs(fill = "Tipo de Bosque")+
        guides(fill = guide_legend(nrow = 28, ncol=1))
  
  
Ino_bosque <-ggplot() +
  geom_sf(data = Tipo_Bosque,aes(fill =CobVeg2013),  alpha = 1, linetype = 1, show.legend = FALSE )+
  scale_fill_manual(values = map.colors)+
  geom_sf(data=Agricola, fill=NA, color="Black")+
  geom_sf(data=Rios, fill=NA, color="blue")+
  geom_sf(data=Ino, fill=NA, color="red")+
  geom_sf(data=Ino2, fill=NA, color="red")+
  annotation_scale() +
  annotation_north_arrow(location="br",which_north="true",style=north_arrow_nautical ())+
  annotate(geom = "text", x = -69.285, y = -12.835, label = "Bosque de terraza baja", fontface = "italic", color = "black", size = 2)+
  annotate(geom = "text", x = -69.282, y = -12.810, label = "Bosque de terraza \nalta con castaña", fontface = "italic", color = "black", size = 2)+
  annotate(geom = "text", x = -69.302, y = -12.815, label = "Bosque de terraza baja", fontface = "italic", color = "black", size = 2)+
  annotate(geom = "text", x = -69.295, y = -12.832, label = "Rios", fontface = "italic", color = "black", size = 2)+
  annotate(geom = "text", x = -69.285, y = -12.820, label = "Inotawa", fontface = "italic", color = "red", size = 3)+
  annotate(geom = "text", x = -69.300, y = -12.813, label = "Inotawa", fontface = "italic", color = "red", size = 3)+
  coord_sf(xlim = c(-69.310,-69.275), ylim = c(-12.845,-12.805),expand = FALSE)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect(linetype = "dashed", color = "grey20", fill = NA, size = 0.4))
# conviértalo en un grob para insertarlo más tarde
MDD.grob        <- ggplotGrob(MDD)
Ino_bosque.grob <- ggplotGrob(Ino_bosque)

long.Dispersion<- c(-69.310, -69.275, -69.275, -69.310)
lat.Dispersion <- c(-12.845,-12.845, -12.805, -12.805)
group <- c(1, 1, 1, 1)
latlong.Dispersion <- data.frame(long.Dispersion, lat.Dispersion, group)

map.bound <- MDD_bosque + 
          geom_polygon(data= latlong.Dispersion , aes(long.Dispersion, lat.Dispersion, group=group), fill = NA, color = "red", 
               linetype = "dashed", size = 1, alpha = 0.8)

map.bound.inset <- map.bound + 
  annotation_custom(grob= MDD.grob, xmin = -73, xmax = -72, ymin =-10, ymax=-9) +
  annotation_custom(grob= Ino_bosque.grob, xmin = -70, xmax = -68, ymin =-11, ymax=-9) 

map.final <- map.bound.inset +
  geom_segment(aes(x=-72.62, xend=-72, y=-9.4, yend=-11),  linetype = "dashed", color = "grey20", size = 0.3) +
  geom_segment(aes(x=-72.6, xend=-70.7, y=-9.4, yend=-10), linetype = "dashed", color = "grey20", size = 0.3) +
  geom_segment(aes(x=-46, xend=-69, y=-13, yend=-30), linetype = "dashed", color = "grey20", size = 0.3) +
  geom_segment(aes(x=-69.3, xend=-69.8, y=-12.8, yend=-11), linetype = "dashed", color = "red", size = 1)+
  geom_segment(aes(x=-69.3, xend=-68.2, y=-12.8, yend=-11), linetype = "dashed", color = "red", size = 1)
#------------------------------------------------------------------------
ggsave(plot = map.final  ,"Mapas exportados/Tipo de bosque en Inotawa.png", units = "cm", 
       width = 29,height = 21, dpi = 900)





```

[Jakub Nowosad](https://nowosad.github.io/)  | [PPT](https://nowosad.github.io/SIGR2021/lecture/lecture.html#64)





![img](https://pbs.twimg.com/media/E5DKj-3X0AMFxnW?format=jpg&name=4096x4096)



code is [here](https://github.com/GuillemSalazar/tidytuesday/blob/main/2021/week_27/code/tidytuesday.R)







[get discrete palette by package and name](https://rdrr.io/cran/paletteer/man/paletteer_d.html)







raster file download [1](https://rspatial.org/raster/sdm/4_sdm_envdata.html) | [2](https://rspatial.org/raster/spatial/8-rastermanip.html)

```r

filename <- system.file("external/rlogo.grd", package="raster")

brick(filename)



nlayers(b)


```











Today 23 june 2021


Inkscape


Make a curve : Use Bezier to draw and Shit + A to make a curve at a specific point

Then fill with a color of your choice by selecting Fill bounded area  

Then harmonize the area and stroke color 

Then move the filled shape to the place you want

Then Click End key to put in in backfront












Dr Tovignan...Please check this answer


**Introductive note**

A geographic information system (GIS) is a system that creates, manages, analyzes, and maps all types of data. GIS connects data to a map, integrating location data (where things are) with all types of descriptive information (what things are like there). Numerous data format are available in GIS. An exhaustive list can be found [here](https://gisgeography.com/gis-formats/). 

**First session**

In the first session, we dealt with the ESRI Shapefile, a widely used GIS data format. The shapefile format is a geospatial vector data format for geographic information system (GIS) software. Using this vector format, we learnt how to manipulate this data, how to add additionnal information and visualize it.

**Second session**

In the second session, we will dive into another GIS file format called Raster format. Raster data is made up of pixels (also referred to as grid cells). They are usually regularly spaced and square but they don’t have to be. Rasters have pixels that are associated with a value (continuous) or class (discrete). The main objective of this second session is to introduce the raster data manipulation in R and how to unravel the information that it contains. 

**Summary**

In summary, we introduced how to handle a vector data in the last session. We will introduce the raster data type in the next one.

**Application note**

Rater data are frequently used to store environmental and climatic data including: rainfall, forest distribution and/or coverage, water, agriculture, land, vegetation, wild animal tracking and management, marine species management, iceberg evolution tracking etc... 


Thank you.







[Make a website easily with wowchemy hugo theme](https://wowchemy.com/templates/)









[Ridgeline Plots in R (3 Examples)](https://statisticsglobe.com/ridgeline-plots-in-r)


![img](https://statisticsglobe.com/wp-content/uploads/2021/06/figure-3-ridgeline-plots-in-r.png)





[Bubble Chart in R-ggplot & Plotly](https://www.r-bloggers.com/2021/06/bubble-chart-in-r-ggplot-plotly/)

![img](https://i2.wp.com/finnstats.com/wp-content/uploads/2021/06/bubble-chart-in-R.png?zoom=1.25&w=450&ssl=1)














xarigan [tuto 1](http://arm.rbind.io/slides/xaringan.html#1)  | [tuto 2](https://annakrystalli.me/talks/xaringan/xaringan.html#1) | [tuto 3](https://alison.rbind.io/project/rladies-xaringan/) | [tuto 4](https://www.katiejolly.io/blog/2021-03-16/designing-slides) | [Code couleur](https://htmlcolorcodes.com/fr/)


import raster file in r


https://www.neonscience.org/resources/learning-hub/tutorials/dc-raster-data-r


https://datacarpentry.org/organization-geospatial/01-intro-raster-data/index.html


https://desktop.arcgis.com/en/arcmap/10.3/manage-data/raster-and-images/what-is-raster-data.htm

http://www.worldclim.com/bioclim


https://worldclim.org/data/v1.4/formats.html


https://worldclim.org/data/bioclim.html

https://emilypiche.github.io/BIO381/raster.html



```r

# downloading the bioclimatic variables from worldclim at a resolution of 30 seconds (.5 minutes)
r <- getData("worldclim", var="bio", res=0.5, lon=-72, lat=44)
# lets also get the elevational data associated with the climate data
alt <- getData("worldclim", var="alt", res=.5, lon=-72, lat=44)

```

















[Introduce yourself online using blogdown and Hugo Apèro](https://youtu.be/RksaNh5Ywbo)





Using Tidyverse for Genomics - Workshop animated by Dr. Janani Ravi [Video](https://youtu.be/m-7knbiSf9A) | [Blog](https://jananiravi.github.io/workshop-tidyverse/genomics/workshop-tidyverse-genomics.html)














[15 Tips to Customize lines in ggplot2 with element_line()](https://cmdlinetips.com/2021/05/tips-to-customize-lines-in-ggplot2-with-element_line/)

![img](https://i1.wp.com/cmdlinetips.com/wp-content/uploads/2021/05/ggplot2_element_line_tips.png?w=660&ssl=1)





















- ####   Water map in African countries

![img](https://github.com/Yedomon/R/blob/main/code.jpg)


[Code](https://github.com/elidom/my_TidyTues/blob/master/week_19/water.Rmd)




- #### Diverging stacked barplot with ggplot2



The [blog](https://luisdva.github.io/rstats/Diverging-bar-plots/) 

Main code


```r

library(dplyr)
library(ggplot2)
library(extrafont)
devtools::install_github('bart6114/artyfarty')
library(artyfarty)

vegSurvey <- vegSurvey %>%  mutate(sppInv= ifelse(veg_Type =="native",spp,spp*-1))

# plot for only the North slope

vegSurvey %>% filter(slope=="North") %>% 
ggplot(aes(x=sampling_point, y=sppInv, fill=veg_Type))+
  geom_bar(stat="identity",position="identity")+
  xlab("sampling point")+ylab("number of species")+
  scale_fill_manual(name="Plant type",values = c("#FFA373","#50486D"))+
  coord_flip()+ggtitle("North slope")+
  geom_hline(yintercept=0)+
  xlab("Sampling Points")+
  ylab("Species number")+
  scale_y_continuous(breaks = pretty(vegSurvey$sppInv),labels = abs(pretty(vegSurvey$sppInv)))+
  theme_scientific()

```



Output




![img](https://luisdva.github.io/assets/images/northslope.png)




A second more details tutorials


The blog [post](https://www.onceupondata.com/2019/01/25/ggplot2-divergent-bars/)



The code:


```r
## calculate breaks values
breaks_values <- pretty(starwars_chars$average_height)


## create plot
starwars_chars %>%
  ggplot(aes(x = homeworld, y = average_height, fill = gender))+
  geom_hline(yintercept = 0)+
  geom_bar(stat = "identity")+
  coord_flip()+
  scale_y_continuous(breaks = breaks_values,
                     labels = abs(breaks_values))+
  theme_minimal()+
  scale_fill_manual(values = c("#bf812d", "#35978f"))


```


Output


![img](https://www.onceupondata.com/post/2019-01-25-ggplot2-divergent-bars_files/figure-html/starwars3-1.png)






- #### Multiple views on how to choose a visualization | [Medium post](https://medium.com/multiple-views-visualization-research-explained/multiple-views-on-how-to-choose-a-visualization-b3ffc99fcddc) | [Indrajeet Patil POST](https://twitter.com/patilindrajeets/status/1380437894859530240) 


![img](https://pbs.twimg.com/media/EyhM8lxW8AEnuyL?format=jpg&name=large)

- #### [Lesson 5: Spatial Data Analysis in R](https://youtu.be/nQVFqkifeSE)
- #### [Plotting a ggtree and ggplots side by side](https://www.r-bloggers.com/2018/10/plotting-a-ggtree-and-ggplots-side-by-side/)


- #### [vhs palette](https://github.com/cj-holmes/vhs)

- #### [Machine learning with R](https://github.com/Yedomon/R/blob/main/lmlcr.pdf)

- #### [Which color scale to use when visualizing data](https://blog.datawrapper.de/which-color-scale-to-use-in-data-vis/)



- #### [Making Extra Great Slides](https://www.garrickadenbuie.com/talk/extra-great-slides-nyhackr/)


- #### [ggplot Wizardry: My Favorite Tricks and Secrets for Beautiful Plots in R ](https://github.com/Z3tt/OutlierConf2021) | [PPT](https://www.cedricscherer.com/slides/OutlierConf2021_ggplot-wizardry.pdf)



![g](https://raw.githubusercontent.com/Z3tt/OutlierConf2021/main/img/2021_outlier.png)

- #### [mfiz](http://mfviz.com/)

- #### [Plagiarism checker at jbnu](https://iei.jbnu.ac.kr) | [LOAD](https://jbnu.copykiller.com/myspace/upload)

- #### Graph color choice example from [2020 | The citation advantage of linking publications to research data](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0230416)


![figure](https://journals.plos.org/plosone/article/figure/image?size=large&id=10.1371/journal.pone.0230416.g002)

# R shiny

- #### [Shiny Apps: Development and Deployment](https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/shiny-apps/)
- #### [Shiny Apps: Development and Deployment Video](https://www.youtube.com/watch?v=QT3WUQu99pM)
- #### [R Recipe: Shiny Tables from Excel Templates](https://ljupcho.com/blog/shiny-excel-table-templates)


Hi Dear Yury Zablotski. Thanks again for this amazing tutorial. I was able to set up and deploy my blog website. However I encountered this issue when writing a blog today.


See:

```

Error in read_xml.character(rss_path) : 
  Input is not proper UTF-8, indicate encoding !
Bytes: 0xE9 0x76 0x72 0x2E [9]
Calls: <Anonymous> ... write_feed_xml -> <Anonymous> -> read_xml.character
Furthermore : Warning messages:
1: In (function (category = "LC_ALL", locale = "")  :
  the OS request to specify the location to "en_US.UTF-8" could not be honored
2: In (function (category = "LC_ALL", locale = "")  :
  the OS request to specify the location to "en_US.UTF-8" could not be honored
3: In (function (category = "LC_ALL", locale = "")  :
  the OS request to specify the location to "en_US.UTF-8" could not be honored
Execution stopped

```


Here is the github link for the site ---> https://github.com/Yedomon/My-website


Here is the deployed website ---> https://yedomon.netlify.app/

Thank you.



# R

```r

sma analysis to get slope and elevation p value

#####

install.packages("smatr")


### load

library(smatr)
b = sma(longev~lma+rain, type="shift", data=leaf.low.soilp) # change type shift to elevation or elev.test=1 or slope.test=1 I do not know
summary(b)



#ggplot2 style
fit1=lm(longev~lma+rain,data=leaf.low.soilp)
summary(fit1)

equation1=function(x){coef(fit1)[2]*x+coef(fit1)[1]}
equation2=function(x){coef(fit1)[2]*x+coef(fit1)[1]+coef(fit1)[3]}

ggplot(leaf.low.soilp,aes(y=longev,x=lma,color=rain))+geom_point()+
  stat_function(fun=equation1,geom="line",color=scales::hue_pal()(2)[1])+
  stat_function(fun=equation2,geom="line",color=scales::hue_pal()(2)[2])



## ggPredict style

ggPredict(fit1,se=TRUE,interactive=FALSE) + 
  theme_minimal()+
  theme(legend.position = "top")+
  labs(x="lma in cm", y = "longev in cm") +
  annotate('text', x = 70, y = 4, label = 'p-value < 0.01)') +
  scale_fill_manual(values = c("#D16103", "#C3D7A4", "#52854C"))+
  scale_color_manual(values = c("#D16103", "#C3D7A4", "#52854C"))

# color selection [here](https://www.datanovia.com/en/fr/blog/couleurs-ggplot-meilleures-astuces-que-vous-allez-adorer/)

```


- #### [Assessing Clustering Tendency](https://www.datanovia.com/en/lessons/assessing-clustering-tendency/)

- #### [Cluster Validation Statistics: Must Know Methods](https://www.datanovia.com/en/lessons/cluster-validation-statistics-must-know-methods/)


- #### [good tuto simple analysis in R](https://biostats.w.uib.no/9-pastecs/)

- #### [setup distill](https://www.shamindras.com/posts/2019-07-31-shrotriya2019distillpt2/)

- #### [smatr 3– an R package for estimation and inference about allometric lines | sma](https://www.rdocumentation.org/packages/smatr/versions/3.4-8/topics/sma)

- #### [An introduction to repeatability estimation with rptR](https://cran.r-project.org/web/packages/rptR/vignettes/rptR.html) | [Installation](https://www.rdocumentation.org/packages/rptR/versions/0.9.22)

- #### ggiraphExtra | Multiple regression model without interaction | ggPredict | [Link](https://cran.r-project.org/web/packages/ggiraphExtra/vignettes/ggPredict.html)

- ##### a great alternative [here](https://rpkgs.datanovia.com/ggpubr/reference/stat_regline_equation.html) with ggpubr


- #### correlation with ggstatplot [r documentation](https://www.rdocumentation.org/packages/ggstatsplot/versions/0.2.0/topics/ggcorrmat) | [PPT](https://indrajeetpatil.github.io/ggstatsplot_slides/slides/ggstatsplot_presentation.html#34)


- #### [ANOVA tutorial and adding an icon on r plot](https://ourcodingclub.github.io/tutorials/anova/)

- #### ggstatsplot | [raison d'etre](https://indrajeetpatil.github.io/ggstatsplot/index.html) |[PPT](https://indrajeetpatil.github.io/ggstatsplot_slides/slides/ggstatsplot_presentation.html#57)



summary of tests
The central tendency measure displayed will depend on the statistics:




| Type           | Measure      | Function used                                                                                                      |
| -------------- | ------------ | ------------------------------------------------------------------------------------------------------------------ |
| Parametric     | mean         | `[parameters::describe_distribution](https://easystats.github.io/parameters/reference/describe_distribution.html)` |
| Non-parametric | median       | `[parameters::describe_distribution](https://easystats.github.io/parameters/reference/describe_distribution.html)` |
| Robust         | trimmed mean | `[parameters::describe_distribution](https://easystats.github.io/parameters/reference/describe_distribution.html)` |
| Bayesian       | MAP estimate | `[parameters::describe_distribution](https://easystats.github.io/parameters/reference/describe_distribution.html)` |





Following (between-subjects) tests are carried out for each type of analyses-



| Type           | No. of groups | Test                                            | Function used                                                              |
| -------------- | ------------- | ----------------------------------------------- | -------------------------------------------------------------------------- |
| Parametric     | \> 2          | Fisher’s or Welch’s one-way ANOVA               | `[stats::oneway.test](https://rdrr.io/r/stats/oneway.test.html)`           |
| Non-parametric | \> 2          | Kruskal–Wallis one-way ANOVA                    | `[stats::kruskal.test](https://rdrr.io/r/stats/kruskal.test.html)`         |
| Robust         | \> 2          | Heteroscedastic one-way ANOVA for trimmed means | `[WRS2::t1way](https://rdrr.io/pkg/WRS2/man/t1way.html)`                   |
| Bayes Factor   | \> 2          | Fisher’s ANOVA                                  | `[BayesFactor::anovaBF](https://rdrr.io/pkg/BayesFactor/man/anovaBF.html)` |
| Parametric     | 2             | Student’s or Welch’s _t_\-test                  | `[stats::t.test](https://rdrr.io/r/stats/t.test.html)`                     |
| Non-parametric | 2             | Mann–Whitney _U_ test                           | `[stats::wilcox.test](https://rdrr.io/r/stats/wilcox.test.html)`           |
| Robust         | 2             | Yuen’s test for trimmed means                   | `[WRS2::yuen](https://rdrr.io/pkg/WRS2/man/yuen.html)`                     |
| Bayesian       | 2             | Student’s _t_\-test                             | `[BayesFactor::ttestBF](https://rdrr.io/pkg/BayesFactor/man/ttestBF.html)` |



[ggbetweenstats R documentation](https://www.rdocumentation.org/packages/ggstatsplot/versions/0.6.6/topics/ggbetweenstats)


```r

ggbetweenstats(
  data,
  x,
  y,
  plot.type = "boxviolin",
  type = "parametric",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  p.adjust.method = "holm",
  effsize.type = "unbiased",
  bf.prior = 0.707,
  bf.message = TRUE,
  results.subtitle = TRUE,
  xlab = NULL,
  ylab = NULL,
  caption = NULL,
  title = NULL,
  subtitle = NULL,
  sample.size.label = TRUE,
  k = 2L,
  var.equal = FALSE,
  conf.level = 0.95,
  nboot = 100L,
  tr = 0.1,
  mean.plotting = TRUE,
  mean.ci = FALSE,
  mean.point.args = list(size = 5, color = "darkred"),
  mean.label.args = list(size = 3),
  notch = FALSE,
  notchwidth = 0.5,
  outlier.tagging = FALSE,
  outlier.label = NULL,
  outlier.coef = 1.5,
  outlier.shape = 19,
  outlier.color = "black",
  outlier.label.args = list(size = 3),
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), alpha
    = 0.4, size = 3, stroke = 0),
  violin.args = list(width = 0.5, alpha = 0.2),
  ggsignif.args = list(textsize = 3, tip_length = 0.01),
  ggtheme = ggplot2::theme_bw(),
  ggstatsplot.layer = TRUE,
  package = "RColorBrewer",
  palette = "Dark2",
  ggplot.component = NULL,
  output = "plot",
  ...
)

```





- #### very nice youtube channel from Dr Yury Zablotski for data exploration, cleaning and so on. Very nice tutorial for [dlookr](https://youtu.be/M7eNYbd4n1Y) | [the blog](https://yuzar-blog.netlify.app/posts/2021-01-30-r-package-reviews-dlookr-diagnose-explore-and-transform-your-data/)



- #### theme blog r [here](https://rfortherestofus.com/2019/08/themes-to-improve-your-ggplot-figures/)

- #### a nice example of theme [ggthemr](https://github.com/Mikata-Project/ggthemr#usage) option  fresh [artyfarty option scientific ou empty](https://datarootsio.github.io/artyfarty/articles/introduction.html) 


- #### tldr: Thomas Vroylandt and I have developed a package called pagedreport to help you make beautiful PDF reports from RMarkdown. Full documentation is available [here](https://pagedreport.rfortherestofus.com/)   


- #### [Announcing pagedreport](https://rfortherestofus.com/2021/01/announcing-pagedreport/)


I found the blog r for the rest of us  [here](https://rfortherestofus.com/blog/) simply wonderfull!!!!  Amazing tutorials



[How to make beatifull table in R](https://rfortherestofus.com/2019/11/how-to-make-beautiful-tables-in-r/)




- #### [Lesson 7. Add Citations and Cross References to an R Markdown Report with Bookdown](https://www.earthdatascience.org/courses/earth-analytics/document-your-science/add-citations-to-rmarkdown-report/)

- #### COLOR SELECTION MAKE EASY [HERE](https://www.w3schools.com/colors/colors_picker.asp) AND [HERE](https://www.color-hex.com/)


- #### facetwap with coloring option of title [here](https://themockup.blog/posts/2020-05-01-tidy-long-models/) and [here](https://www.opensourcefootball.com/posts/2020-09-07-estimating-runpass-tendencies-with-tidymodels-and-nflfastr/)

- #### Setting up a Distill Blog with Netlify [here](https://www.shamindras.com/posts/2019-07-11-shrotriya2019distillpt1/)

- #### Do nice PPT and share it with xarigan and distill package  [here](https://rstudio-education.github.io/teaching-in-production/#prework)


- #### [How to create a blog or a website in R with {Distill} & continuously deploy via Github & Netlify](https://youtu.be/WZt4H-ogH3s)
- #### Creating a Blog with distill [here](https://rstudio.github.io/distill/blog.html) | [Example](https://pkgs.rstudio.com/distill/articles/examples.html)

- #### Building a website using R {distill} [here](https://www.youtube.com/watch?v=Fm3bsYCilEU)

- #### How to create a blog or a website in R with {Distill} package and manually deploy with {Netlify} [here](https://www.youtube.com/watch?v=p9nuRKaF4nM)

- #### Heatmap 


```r

library(pheatmap); library(gplots); library(ggplot2); library(colorspace); library(wesanderson)

tpm = delim("Senna_tora_CHS-L.txt", header=T, row.names='gene')
bk = unique(c(seq(0, 0, length=10), 0, seq(0, 13, length=100)))
colors = colorRampPalette(c("white", "red"))(100)
pheatmap(tpm, 
         color = colors,
         breaks = bk,
         cluster_cols=F,
         cluster_rows=F,
         fontsize_col=9,
         fontsize_row=7,
         cellwidth = 260/ncol(tpm), 
         cellheight = 200/nrow(tpm), 
         border_color = "grey",
         show_rownames=T,
         gaps_col = c(1,2,3,4,5,6),
         labels_row = rownames(tpm),
         main=paste(c(nrow(tpm), " CHS-L genes"),collapse=""))
         
 
  ```
 
 
 - #### Ks plot
 
 
 ```r
 
 library(ggplot2); library(ggrepel); library(gridExtra)
args = commandArgs(trailingOnly=TRUE)

library(limma)
Cats = c()
KaKs = data.frame()
for (i in args){
  Data = data.frame(category = strsplit2(i, '\\.')[1,1], read.table(i, sep = '', header = T, quote = '', fill = T))
  KaKs = rbind(KaKs, Data)
  Label = strsplit2(i, '\\/')
  Cats = c(Cats, strsplit2(Label[1, ncol(Label)], '\\.')[1,1])
}

KaKs$category = factor(KaKs$category, levels = Cats)
NumOrg = length(Cats)


Ks = ggplot(KaKs) + 
  geom_density(aes(Ks, fill = factor(category)), alpha = 0.3, colour = NA) + 
  scale_fill_manual(values = rainbow(NumOrg)) +
  scale_x_continuous(expand = c(0,0), limits = c(0, 4)) + 
  ggtitle('Ks') +  
  theme_bw(base_size = 6) +
  xlab('Ks') +
  theme(legend.position = 'bottom',legend.title = element_blank(),plot.title = element_text(vjust = 0.5, hjust = 0.5, size = 20))

pdf(paste(c('./Plot_Ks.', Cats, '.pdf'), collapse = ''))
grid.arrange(Ks)
dev.off()

```



`

To calculate the synonymous-substitution Ks values, we selected the orthologous gene pairs between species and the paralogous pairs within a species from the orthology analysis. The selected proteins were further subjected to multiple-sequence alignment with MAFFT v7.305b93 and corrected with Gblocks v0.91b94. The corresponding genomic regions of conserved proteins, which were observed from the corrected multiple alignments, were subjected to Ks calculation using ParaAT v2.0 (ref. 98) with the Yang–Nielsen approach implemented in PAML99. The Ks distribution plot (Supplementary Fig. 27) was drawn using in-house Python and R scripts.

`




A new script for ka ks plot | data is [here](https://github.com/Yedomon/R/blob/main/data_th.csv)



```r


ggplot(data_th, aes(x=ks, color=species)) + 
  geom_density(size = 0.75)  +
  scale_x_continuous(name = "Substitutions per synonymous site (Ks)", limits =c(0,4)) + 
  scale_y_continuous(name="Density") + 
  scale_color_brewer(palette ="Paired") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  theme(legend.position = c(0.8, 0.7))


```





[kaks-calculator](https://code.google.com/archive/p/kaks-calculator/downloads)




 - #### [r color inpiration selection](https://htmlcolorcodes.com/)


- #### [R for biologist](http://omgenomics.com/plotting-in-r-for-biologists/)

- color palette   [inspiration](https://www.google.co.kr/imgres?imgurl=https%3A%2F%2Fplastid.readthedocs.io%2Fen%2Flatest%2F_images%2Fstackedbar.png&imgrefurl=https%3A%2F%2Fplastid.readthedocs.io%2Fen%2Flatest%2Fexamples%2Fz_plotting.html&tbnid=qxkqTSGPS4TqXM&vet=12ahUKEwj6p6Od6dntAhVD5ZQKHdO2C5gQMyhUegQIARBx..i&docid=Zi5Bizo5SpBWDM&w=1675&h=1324&q=genome%20plot%20python&hl=fr&ved=2ahUKEwj6p6Od6dntAhVD5ZQKHdO2C5gQMyhUegQIARBx) | [awespme](https://plastid.readthedocs.io/en/latest/examples/z_plotting.html)

- ### Principal Component Analysis | [STHDA Tuto](http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/) | [Visualization](http://www.sthda.com/english/wiki/fviz-pca-quick-principal-component-analysis-data-visualization-r-software-and-data-mining) | [factoextra tuto by Kassambara](https://rpkgs.datanovia.com/factoextra/reference/fviz_pca.html)

- #### [Tidytuesdays Tutorial](https://github.com/jkaupp/tidytuesdays)


- #### [A ggplot2 Tutorial for Beautiful Plotting in R](https://cedricscherer.netlify.app/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/)

- #### [ggplot2 teaching material : Collection of teaching materials for R](https://github.com/Z3tt/ggplot-courses) 
- #### [ggplot2-tutorial](https://github.com/jennybc/ggplot2-tutorial)


- #### [super-and-sub-script-labels.R](https://gist.github.com/benmarwick/5cfeda1d20ff9ab3231a5de8fac36213)
- #### [polyshing a ggplot2 graph](https://ggplot2-book.org/polishing.html)


- #### [Remove 'a' from legend when using aesthetics and geom_text: Reponse 9](https://stackoverflow.com/questions/18337653/remove-a-from-legend-when-using-aesthetics-and-geom-text)


- #### [Find easily color hexa code](https://www.colorhexa.com/) | [website2](https://www.color-hex.com/)
- #### [Showing data values on stacked bar chart in ggplot2](https://intellipaat.com/community/12428/showing-data-values-on-stacked-bar-chart-in-ggplot2)


- #### [flextable](https://davidgohel.github.io/flextable/articles/overview.html)

- #### [y-axis on the right side · Len Kiefer](http://lenkiefer.com/2020/11/20/y-axis-on-the-right-side/)

- ### [acm](http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/75-acm-analyse-des-correspondances-multiples-avec-r-l-essentiel/)

- #### [How to export high quality images in R](https://aebou.rbind.io/posts/2020/11/how-to-export-high-quality-images-in-r/)

- #### [Comment exporter des images de haute qualité avec R](https://aebou.rbind.io/fr/posts/2020/11/comment-exporter-des-images-de-haute-qualit%C3%A9-avec-r/)

- #### [New Version of Patchwork is Here: Inset a plot inside a plot](https://cmdlinetips.com/2020/11/insetting-with-patchwork/) [tuto2](https://www.data-imaginist.com/2020/insetting-a-new-patchwork-version/) | [tuto3](https://www.r-bloggers.com/2020/11/insetting-a-new-patchwork-version/) 

- #### [tips-to-combine-multiple-ggplots-using-patchwork](https://cmdlinetips.com/2020/01/tips-to-combine-multiple-ggplots-using-patchwork/)

- #### [Grouped Stacked bar plot with ggplot](https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html)  | [Dealing with colors in ggplot2](https://www.r-graph-gallery.com/ggplot2-color.html) | [Paletteer package](https://github.com/EmilHvitfeldt/r-color-palettes)

- #### [Supprimer la legende](https://www.datanovia.com/en/fr/blog/gpplot-comment-supprimer-la-legende/)

- #### [Label bar chart ](https://thomasadventure.blog/posts/labels-ggplot2-bar-chart/)
- #### [inspi charles bodet ggplot2](https://www.charlesbordet.com/en/make-beautiful-charts-ggplot2/#)
- #### [BBC](https://bbc.github.io/rcookbook/)  [Exampple](http://www.interhacktives.com/2020/03/09/how-to-create-bbc-style-graphics-with-r-a-bbplot-tutorial/)
- #### [R graph galery ggplot2](https://www.r-graph-gallery.com/ggplot2-package.html)
- #### [Data to viz](https://www.data-to-viz.com/)

- #### [A ggplot2 Tutorial for Beautiful Plotting in R](https://cedricscherer.netlify.app/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/)
- #### [Tidytuesday 2020](https://github.com/jwatzek/tidytuesday)

- #### inspi geom area en haut (plant height par example) et barre pourcentage en bas (pour le nombre de feuille vert et jaune et mort) , vrainspiration [ici](https://twitter.com/watzoever/status/1199508300959752192?ref_src=twsrc%5Etfw%7Ctwcamp%5Etweetembed%7Ctwterm%5E1199508300959752192%7Ctwgr%5Eshare_3%2Ccontainerclick_0&ref_url=https%3A%2F%2Fcedricscherer.netlify.app%2F2019%2F12%2F30%2Fbest-tidytuesday-2019%2F) ... le code dur github est [ici](https://github.com/jwatzek/tidytuesday)

- #### [inspi](https://cedricscherer.netlify.app/2020/01/09/australian-bushfires-comparison-europe-world/)

- #### [Inspi](https://frontpagedata.com/examples/)
- #### [axis label with ggplot2](http://environmentalcomputing.net/plotting-with-ggplot-adding-titles-and-axis-names/#:~:text=To%20alter%20the%20labels%20on,line%20of%20basic%20ggplot%20code.&text=Note%3A%20You%20can%20also%20use,which%20is%20equivalent%20to%20ggtitle%20.)
- ##### [annotation ggplot2](https://viz-ggplot2.rsquaredacademy.com/textann.html)
- #### [Ajouter les pvalues sur un ggplot, manuellement](https://statistique-et-logiciel-r.com/ajouter-pvalues-sur-ggplot/)

- #### [order x axis following a recise axix](https://www.r-graph-gallery.com/267-reorder-a-variable-in-ggplot2.html)

- #### [Modifier les axes color face et autres](http://www.sthda.com/french/wiki/ggplot2-graduation-des-axes-guide-pour-personnaliser-les-etiquettes-des-graduations-logiciel-r-et-visualisation-de-donnees)


- #### [barplot with error bar](https://www.r-graph-gallery.com/4-barplot-with-error-bar.html) or [here](http://www.sthda.com/french/wiki/ggplot2-barres-d-erreur-guide-de-demarrage-rapide-logiciel-r-et-visualisation-de-donnees)
- #### [Chicklet Stacked Bar Chart](https://www.mikelee.co/posts/2020-02-08-recreate-fivethirtyeight-chicklet-stacked-bar-chart-in-ggplot2/)

- #### [Distill for R Markdown: Creating a Website](https://rstudio.github.io/distill/website.html)

- #### [A Maize Color Palette Generator](https://github.com/AndiKur4/MaizePal)

- #### [Exporting editable ggplot graphics to PowerPoint with officer and purrr](https://www.pipinghotdata.com/posts/2020-09-22-exporting-editable-ggplot-graphics-to-powerpoint-with-officer-and-purrr/)
- #### [Learning Statistics with R](https://learningstatisticswithr.com/)
- #### [Becoming An rvest Magician](https://zachbogart.com/post/rvest-wizardry/)
- #### [How to zoom in on a plot in R](https://datavizpyr.com/how-to-zoom-in-on-a-plot-in-r/)

- #### [Top 50 Rggplot2 visualization items](http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html)

- #### [Rboot Camp: A free online R course](https://r-bootcamp.netlify.app/)
- #### [The best books on Computer Science for Data Scientists recommended by Hadley Wickham](https://fivebooks.com/best-books/computer-science-data-science-hadley-wickham/)
- #### [flextable R package](https://davidgohel.github.io/flextable/)
- #### [PLOTTING SIGNIFICANCE ON THE TOP A BAR CHART OPTION1](https://nakedstatistics.wordpress.com/2017/05/11/plotting-significance/)
- #### [PLOTTING SIGNIFICANCE ON THE TOP A BAR CHART OPTION2](http://131.111.177.41/statistics/R/graphs2.html)
- #### [T TEST](http://www.sthda.com/english/wiki/unpaired-two-samples-t-test-in-r)
- #### [R studio data sheet](https://rstudio.cloud/learn/cheat-sheets)
- #### [R Cookbook](http://www.cookbook-r.com/)


- #### [ggupset](https://github.com/const-ae/ggupset)

- #### [How to Change X and Y Axis Values from Real to Integers in ggplot2?](https://datavizpyr.com/change-x-and-y-axis-values-to-integers-ggplot2/)




# R Map

- #### [1](https://www.r-bloggers.com/2019/04/zooming-in-on-maps-with-sf-and-ggplot2/) | [2](https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html) | [3](http://search.r-project.org/library/ggplot2/html/ggsf.html) | [4](https://geanders.github.io/navy_public_health/3-2-basic-mapping.html#basic-mapping) | [5](https://semba-blog.netlify.app/01/26/2020/world-map-and-map-projections/) [7](https://bookdown.org/robinlovelace/geocompr/spatial-class.html)



In cases when a coordinate reference system (CRS) is missing or the wrong CRS is set, the st_set_crs() function can be used:

```
new_vector = st_set_crs(new_vector, 4326) # set CRS
#> Warning: st_crs<- : replacing crs does not reproject data; use st_transform
#> for that
The warning message informs us that the st_set_crs() function does not transform data from one CRS to another.

```

- #### [ggplot2 official guide for map](https://ggplot2-book.org/maps.html)


- #### [How to get terrain data and draw spatial maps in R using ggplot2? | StatswithR | Arnab Hazra](https://youtu.be/tX8RaTp7jL8)

- #### [Choropleth maps with geom_sf() | Professional dataviz with ggplot2 | R](https://youtu.be/nlcZcWJiyX8)

- #### [Raster maps with geom_raster() | Professional dataviz with ggplot2 | R](https://youtu.be/NQZNpyEgVss)

- #### [Create a map of a country with the neighbouring countries and an inset map](https://aebou.rbind.io/posts/2020/11/create-a-map-of-a-country-with-the-neighbouring-countries-and-an-inset-map/)

- #### [Interactive map with R](https://thinkr.fr/cartographie-interactive-avec-r-la-suite/)
- #### [Interactive map with R and Leaflet](https://thinkr.fr/cartographie-interactive-comment-visualiser-mes-donnees-spatiales-de-maniere-dynamique-avec-leaflet/)
- #### [Geocomputation with R](https://geocompr.robinlovelace.net/)
- #### [R as GIS, part 1: vector](https://datascienceplus.com/r-as-gis-part-1-vector/)

# R Markdown

- #### [Remedy provides addins to facilitate writing in markdown with RStudio](https://github.com/ThinkR-open/remedy) 
