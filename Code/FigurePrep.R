if(!require("pacman")) install.packages("pacman")
pacman::p_load(mgcv, tidyverse, dplyr, raster, sf, ggplot2, ggrepel)

load("Intermediate/WorldPop.RData")
load("Intermediate/shapefiles.RData")
load("Intermediate/figures_prep.RData")

name_counter <- 0

for(k in 1:length(antigens)){
  
  for(j in 1:length(type_predict)){
    
    for(i in 1:length(ages_total)){
      
      name_counter <- name_counter+1
      
      overall_sero_total <- cbind(overall_sero_total, get(paste0(antigens[k], "_", type_predict[j],"_", ages_total[i])))
      colnames(overall_sero_total)[name_counter + 10] <- paste0(antigens[k], "_", type_predict[j],"_", ages_total[i])
      
    }
    
  }
  
}

overall_sero_total <- overall_sero_total%>%
  dplyr::select(-fit, -right_fit, -fit_link, -se_link, -fit_response, -UC, -LC)

overall_sero_total_sf <- st_as_sf(overall_sero_total, coords = c("X", "Y"), crs = "+proj=longlat +datum=WGS84 +no_defs")

overall_sero_total_sf_adm1 <- st_intersection(overall_sero_total_sf, nigeria_polygon_ad1)%>%
  as.data.frame()%>%
  dplyr::select(-geometry)

overall_sero_total_sf_adm2 <- st_intersection(overall_sero_total_sf, nigeria_adm2)%>%
  as.data.frame()%>%
  dplyr::select(-geometry)

overall_sero_total_sf_adm1 <- overall_sero_total_sf_adm1%>%
  dplyr::select(-Age, -GID_0, -GID_1, -NAME_0, -VARNAME_1, -NL_NAME_1, -TYPE_1, -ENGTYPE_1, -CC_1, -HASC_1, -ADM1NAME)

overall_sero_total_sf_adm1_gather <- overall_sero_total_sf_adm1%>%
  gather(key = "Key", value = "Value", -NAME_1)%>%
  mutate(Age = sub(".*_","", Key))%>%
  mutate(Antigen = substr(Key, 1,3))

type_fit <- c()

total_key <- overall_sero_total_sf_adm1_gather$Key

key_all <- str_split(total_key, "_")

for(i in 1:length(total_key)){
  
  type_fit[i] <- key_all[[i]][2]
  
  print(i)
  
}

overall_sero_total_sf_adm2 <- overall_sero_total_sf_adm2%>%
  dplyr::select(-Age, -GID_0, -GID_1, -GID_2, -NAME_0, -VARNAME_2, -NL_NAME_2, -TYPE_2, -ENGTYPE_2, -CC_2, -HASC_2)

overall_sero_total_sf_adm2_gather <- overall_sero_total_sf_adm2%>%
  gather(key = "Key", value = "Value", -NAME_2)%>%
  mutate(Age = sub(".*_","", Key))%>%
  mutate(Antigen = substr(Key, 1,3))

overall_sero_total_sf_adm1_gather_type <- cbind(overall_sero_total_sf_adm1_gather, type_fit)

overall_sero_total_sf_adm1_gather_type <- overall_sero_total_sf_adm1_gather_type%>%
  group_by(Age, NAME_1, Antigen, type_fit)%>%
  summarize(mean_sero = mean(Value))
