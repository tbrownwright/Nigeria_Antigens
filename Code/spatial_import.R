#Using Package Management package to load all library dependencies

if(!require("pacman")) install.packages("pacman")
pacman::p_load(sf, dplyr, tidyr)


library(sf)
library(dplyr)
library(tidyr)

nigeria_polygon_ad1 <- read_sf("Data/ShapeFiles/NGA_adm1.shp")

nigeria_adm2 <- read_sf("Data/ShapeFiles/NGA_adm2.shp")

nigeria_polygon_ad1 <- nigeria_polygon_ad1%>%
  mutate(ADM1NAME = ifelse(NAME_1 == "Federal Capital Territory", "FCT ABUJA", ifelse(NAME_1 == "Nassarawa", "NASARAWA", toupper(NAME_1))))

nigeria <-  sf::read_sf("Data/ShapeFiles/NGGE7AFL.shp")

nigeria_polygon <- read_sf("Data/ShapeFiles/NGA_adm0.shp")

save(nigeria_polygon_ad1, nigeria_adm2, nigeria, nigeria_polygon, file = "Intermediate/shapefiles.RData")
