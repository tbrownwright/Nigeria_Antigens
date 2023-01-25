#Using Package Management package to load all library dependencies

if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, dplyr, tidyr, sf)

load("Intermediate/shapefiles.RData")

tiff_0_f <- raster("Data/WorldPop/nga_f_0_2020.tif")

tiff_0_m <- raster("Data/WorldPop/nga_m_0_2020.tif")

tiff_1_f <- raster("Data/WorldPop/nga_f_1_2020.tif")

tiff_1_m <- raster("Data/WorldPop/nga_m_1_2020.tif")

tiff_5_9_f <- raster("Data/WorldPop/nga_f_5_2020.tif")

tiff_5_9_m <- raster("Data/WorldPop/nga_m_5_2020.tif")

tiff_10_14_f <- raster("Data/WorldPop/nga_f_10_2020.tif")

tiff_10_14_m <- raster("Data/WorldPop/nga_m_10_2020.tif")

tiff_0_f_agg <- raster::aggregate(tiff_0_f, fun = sum, fact = 100, na.rm = TRUE)
tiff_0_m_agg <- raster::aggregate(tiff_0_m, fun = sum, fact = 100, na.rm = TRUE)
tiff_1_f_agg <- raster::aggregate(tiff_1_f, fun = sum, fact = 100, na.rm = TRUE)
tiff_1_m_agg <- raster::aggregate(tiff_1_m, fun = sum, fact = 100, na.rm = TRUE)
tiff_5_9_f_agg <- raster::aggregate(tiff_5_9_f, fun = sum, fact = 100, na.rm = TRUE)
tiff_5_9_m_agg <- raster::aggregate(tiff_5_9_m, fun = sum, fact = 100, na.rm = TRUE)
tiff_10_14_f_agg <- raster::aggregate(tiff_10_14_f, fun = sum, fact = 100, na.rm = TRUE)
tiff_10_14_m_agg <- raster::aggregate(tiff_10_14_m, fun = sum, fact = 100, na.rm = TRUE)

tiff_0_f_df <- as.data.frame(tiff_0_f_agg, xy=TRUE)

tiff_0_f_df_complete <- tiff_0_f_df[complete.cases(tiff_0_f_df),]

tiff_0_m_df <- as.data.frame(tiff_0_m_agg, xy=TRUE)

tiff_0_m_df_complete <- tiff_0_m_df[complete.cases(tiff_0_m_df),]

tiff_1_f_df <- as.data.frame(tiff_1_f_agg, xy=TRUE)

tiff_1_f_df_complete <- tiff_1_f_df[complete.cases(tiff_1_f_df),]

tiff_1_m_df <- as.data.frame(tiff_1_m_agg, xy=TRUE)

tiff_1_m_df_complete <- tiff_1_m_df[complete.cases(tiff_1_m_df),]

tiff_5_9_f_df <- as.data.frame(tiff_5_9_f_agg, xy=TRUE)

tiff_5_9_f_df_complete <- tiff_5_9_f_df[complete.cases(tiff_5_9_f_df),]

tiff_5_9_m_df <- as.data.frame(tiff_5_9_m_agg, xy=TRUE)

tiff_5_9_m_df_complete <- tiff_5_9_m_df[complete.cases(tiff_5_9_m_df),]

tiff_10_14_f_df <- as.data.frame(tiff_10_14_f_agg, xy=TRUE)

tiff_10_14_f_df_complete <- tiff_10_14_f_df[complete.cases(tiff_10_14_f_df),]

tiff_10_14_m_df <- as.data.frame(tiff_10_14_m_agg, xy=TRUE)

tiff_10_14_m_df_complete <- tiff_10_14_m_df[complete.cases(tiff_10_14_m_df),]

tiff_0_4_complete <- tiff_0_f_df_complete

tiff_0_4_complete$value <- tiff_0_f_df_complete$nga_f_0_2020 + tiff_0_m_df_complete$nga_m_0_2020 + tiff_1_f_df_complete$nga_f_1_2020 + tiff_1_m_df_complete$nga_m_1_2020

tiff_5_9_complete <- tiff_5_9_f_df_complete

tiff_5_9_complete$value <- tiff_5_9_f_df_complete$nga_f_5_2020 + tiff_5_9_m_df_complete$nga_m_5_2020

tiff_10_14_complete <- tiff_10_14_f_df_complete

tiff_10_14_complete$value <- tiff_10_14_f_df_complete$nga_f_10_2020 + tiff_10_14_m_df_complete$nga_m_10_2020

tiff_0_4_complete <- tiff_0_4_complete%>%
  mutate(uniqueID = paste(x, y, by = " "))

tiff_5_9_complete <- tiff_5_9_complete%>%
  mutate(uniqueID = paste(x, y, by = " "))

tiff_10_14_complete <- tiff_10_14_complete%>%
  mutate(uniqueID = paste(x, y, by = " "))

tiff_0_14_complete <- tiff_0_4_complete%>%
  mutate(value_total = value +tiff_5_9_complete$value +tiff_10_14_complete$value)%>%
  dplyr::select(x,y,value_total)

tiff_0_14_complete_sp <- st_as_sf(tiff_0_14_complete, coords = c("x", "y"))

st_crs(tiff_0_14_complete_sp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

tiff_0_14_complete_adm1 <- st_intersection(tiff_0_14_complete_sp, nigeria_polygon_ad1)

tiff_0_14_complete_adm1_summarized <- as.data.frame(tiff_0_14_complete_adm1)%>%
  group_by(NAME_1)%>%
  summarise(total_pop = sum(value_total, na.rm = TRUE))

tiff_15_19 <- "Data/WorldPop/nga_f_15_2020.tif"

tiff_15_19_raster <- raster(tiff_15_19)

tiff_15_19_agg <- raster::aggregate(tiff_15_19_raster, fun = sum, fact = 100, na.rm =TRUE)

tiff_15_19_df <- as.data.frame(tiff_15_19_agg, xy = TRUE)

tiff_15_19_complete <- tiff_15_19_df[complete.cases(tiff_15_19_df),]

tiff_20_24 <- "Data/WorldPop/nga_f_20_2020.tif"

tiff_20_24_raster <- raster(tiff_20_24)

tiff_20_24_agg <- raster::aggregate(tiff_20_24_raster, fun = sum, fact = 100, na.rm = TRUE)

tiff_20_24_df <- as.data.frame(tiff_20_24_agg, xy = TRUE)

tiff_20_24_complete <- tiff_20_24_df[complete.cases(tiff_20_24_df),]

tiff_25_29 <- "Data/WorldPop/nga_f_25_2020.tif"

tiff_25_29_raster <- raster(tiff_25_29)

tiff_25_29_agg <- raster::aggregate(tiff_25_29_raster, fun = sum, fact = 100, na.rm = TRUE)

tiff_25_29_df <- as.data.frame(tiff_25_29_agg, xy = TRUE)

tiff_25_29_complete <- tiff_25_29_df[complete.cases(tiff_25_29_df),]

tiff_30_34 <- "Data/WorldPop/nga_f_30_2020.tif"

tiff_30_34_raster <- raster(tiff_30_34)

tiff_30_34_agg <- raster::aggregate(tiff_30_34_raster, fun = sum, fact = 100, na.rm = TRUE)

tiff_30_34_df <- as.data.frame(tiff_30_34_agg, xy = TRUE)

tiff_30_34_complete <- tiff_30_34_df[complete.cases(tiff_30_34_df),]

tiff_35_39 <- "Data/WorldPop/nga_f_35_2020.tif"

tiff_35_39_raster <- raster(tiff_35_39)

tiff_35_39_agg <- raster::aggregate(tiff_35_39_raster, fun = sum, fact = 100, na.rm = TRUE)

tiff_35_39_df <- as.data.frame(tiff_35_39_agg, xy = TRUE)

tiff_35_39_complete <- tiff_35_39_df[complete.cases(tiff_35_39_df),]

tiff_40_44 <- "Data/WorldPop/nga_f_40_2020.tif"

tiff_40_44_raster <- raster(tiff_40_44)

tiff_40_44_agg <- raster::aggregate(tiff_40_44_raster, fun = sum, fact = 100, na.rm = TRUE)

tiff_40_44_df <- as.data.frame(tiff_40_44_agg, xy = TRUE)

tiff_40_44_complete <- tiff_40_44_df[complete.cases(tiff_40_44_df),]

tiff_15_24 <- raster::overlay(tiff_15_19_raster, tiff_20_24_raster, fun = sum)

tiff_15_24_agg <- raster::aggregate(tiff_15_24, fun = sum, fact = 100)

tiff_15_24_df <- as.data.frame(tiff_15_24_agg, xy= TRUE)

tiff_15_24_complete <- tiff_15_24_df[complete.cases(tiff_15_24_df),]

tiff_25_44_complete <- tiff_25_29_complete

tiff_25_44_complete$layer <- tiff_25_29_complete$nga_f_25_2020 + tiff_30_34_complete$nga_f_30_2020 + tiff_35_39_complete$nga_f_35_2020 + tiff_40_44_complete$nga_f_40_2020

tiff_15_44_complete <- tiff_15_19_complete

tiff_15_44_complete_1 <- tiff_15_19_complete

tiff_15_44_complete$layer_1 <- tiff_25_44_complete$layer+tiff_15_24_complete$layer

tiff_15_44_complete_1$layer_1 <- tiff_15_19_complete$nga_f_15_2020 + tiff_20_24_complete$nga_f_20_2020 + tiff_25_29_complete$nga_f_25_2020 + tiff_30_34_complete$nga_f_30_2020 + tiff_35_39_complete$nga_f_35_2020 +tiff_40_44_complete$nga_f_40_2020

tiff_0_1_sum <- tiff_0_f_df_complete
tiff_0_1_sum$age0 <- tiff_0_f_df_complete$nga_f_0_2020 + tiff_0_m_df_complete$nga_m_0_2020

tiff_0_1_sum <- st_as_sf(tiff_0_1_sum, coords = c("x", "y"))
st_crs(tiff_0_1_sum) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

tiff_0_1_sum_adm1 <- st_intersection(tiff_0_1_sum, nigeria_polygon_ad1)

tiff_0_1_sum_adm1 <- tiff_0_1_sum_adm1%>%
  group_by(NAME_1)%>%
  summarise(total_pop = sum(age0))

tiff_1_5_sum <- tiff_1_f_df_complete
tiff_1_5_sum$age1 <- tiff_1_f_df_complete$nga_f_1_2020 + tiff_1_m_df_complete$nga_m_1_2020

tiff_1_5_sum <- st_as_sf(tiff_1_5_sum, coords = c("x", "y"))
st_crs(tiff_1_5_sum) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

tiff_1_5_sum_adm1 <- st_intersection(tiff_1_5_sum, nigeria_polygon_ad1)

tiff_1_5_sum_adm1 <- tiff_1_5_sum_adm1%>%
  group_by(NAME_1)%>%
  summarise(total_pop = sum(age1))

tiff_5_10_sum <- tiff_5_9_f_df_complete
tiff_5_10_sum$age5 <- tiff_5_9_f_df_complete$nga_f_5_2020 + tiff_5_9_m_df_complete$nga_m_5_2020

tiff_5_10_sum <- st_as_sf(tiff_5_10_sum, coords = c("x", "y"))
st_crs(tiff_5_10_sum) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

tiff_5_10_sum_adm1 <- st_intersection(tiff_5_10_sum, nigeria_polygon_ad1)

tiff_5_10_sum_adm1 <- tiff_5_10_sum_adm1%>%
  group_by(NAME_1)%>%
  summarise(total_pop = sum(age5))

tiff_10_15_sum <- tiff_10_14_f_df_complete
tiff_10_15_sum$age5 <- tiff_10_14_f_df_complete$nga_f_10_2020 + tiff_10_14_m_df_complete$nga_m_10_2020

tiff_10_15_sum <- st_as_sf(tiff_10_15_sum, coords = c("x", "y"))
st_crs(tiff_10_15_sum) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

tiff_10_15_sum_adm1 <- st_intersection(tiff_10_15_sum, nigeria_polygon_ad1)

tiff_10_15_sum_adm1 <- tiff_10_15_sum_adm1%>%
  group_by(NAME_1)%>%
  summarize(total_pop = sum(age5))

tiff_15_19_sum <- st_as_sf(tiff_15_19_complete, coords = c("x", "y"))
st_crs(tiff_15_19_sum) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

tiff_15_19_adm1 <- st_intersection(tiff_15_19_sum, nigeria_polygon_ad1)

tiff_15_19_adm1 <- tiff_15_19_adm1%>%
  group_by(NAME_1)%>%
  summarize(total_pop = sum(nga_f_15_2020))

tiff_20_24_sum <- st_as_sf(tiff_20_24_complete, coords = c("x", "y"))
st_crs(tiff_20_24_sum) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

tiff_20_24_adm1 <- st_intersection(tiff_20_24_sum, nigeria_polygon_ad1)

tiff_20_24_adm1 <- tiff_20_24_adm1%>%
  group_by(NAME_1)%>%
  summarise(total_pop = sum(nga_f_20_2020))

tiff_25_29_sum <- st_as_sf(tiff_25_29_complete, coords = c("x", "y"))
st_crs(tiff_25_29_sum) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

tiff_25_29_adm1 <- st_intersection(tiff_25_29_sum, nigeria_polygon_ad1)

tiff_25_29_adm1 <- tiff_25_29_adm1%>%
  group_by(NAME_1)%>%
  summarize(total_pop = sum(nga_f_25_2020))

tiff_30_34_sum <- st_as_sf(tiff_30_34_complete, coords = c("x", "y"))
st_crs(tiff_30_34_sum) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

tiff_30_34_adm1 <- st_intersection(tiff_30_34_sum, nigeria_polygon_ad1)

tiff_30_34_adm1 <- tiff_30_34_adm1%>%
  group_by(NAME_1)%>%
  summarize(total_pop = sum(nga_f_30_2020))

tiff_35_39_sum <- st_as_sf(tiff_35_39_complete, coords = c("x", "y"))
st_crs(tiff_35_39_sum) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

tiff_35_39_adm1 <- st_intersection(tiff_35_39_sum, nigeria_polygon_ad1)

tiff_35_39_adm1 <- tiff_35_39_adm1%>%
  group_by(NAME_1)%>%
  summarize(total_pop = sum(nga_f_35_2020))

tiff_40_44_sum <- st_as_sf(tiff_40_44_complete, coords = c("x", "y"))
st_crs(tiff_40_44_sum) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

tiff_40_44_adm1 <- st_intersection(tiff_40_44_sum, nigeria_polygon_ad1)

tiff_40_44_adm1 <- tiff_40_44_adm1%>%
  group_by(NAME_1)%>%
  summarize(total_pop = sum(nga_f_40_2020))

tiff_15_19_complete <- tiff_15_19_complete%>%
  mutate(uniqueID = paste(x, y, by = " "))

tiff_20_24_complete <- tiff_20_24_complete%>%
  mutate(uniqueID = paste(x, y, by = " "))

tiff_25_29_complete <- tiff_25_29_complete%>%
  mutate(uniqueID = paste(x, y, by = " "))

tiff_30_34_complete <- tiff_30_34_complete%>%
  mutate(uniqueID = paste(x, y, by = " "))

tiff_35_39_complete <- tiff_35_39_complete%>%
  mutate(uniqueID = paste(x, y, by = " "))

tiff_40_44_complete <- tiff_40_44_complete%>%
  mutate(uniqueID = paste(x, y, by = " "))

save.image(file = "Intermediate/WorldPop.RData")
