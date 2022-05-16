library(tidyr)
library(dplyr)
library(ggplot2)
library(haven)
library(ggjoy)
library(ggridges)
library(viridis)
library(sf)
library(mgcv)
library(spdep)
library(spaMM)
library(sjPlot)
library(stringr)
library(readr)
library(forcats)
library(rgdal)
library(tidyverse)
library(automap)
library(gstat)
library(gridExtra)
library(grid)
library(ggpubr)

#Child Data Import

Child_geo <- read_sas("C:/Users/tzb5519/Desktop/PostDoc/Nigeria/CDCAnalysis/CDC/Data/nms4_gid_01dec2020.sas7bdat", 
                      NULL)%>%
  mutate(uniqueGeo = paste(LATNUM, LONGNUM, sep = " "))%>%
  mutate(age_group = ifelse(Age < 1, 1,ifelse(Age <2, 2, ifelse(Age < 3, 3, ifelse(Age <6, 4, ifelse(Age <10, 5, 6))))))

child_geo_sum <- Child_geo%>%
  group_by(Age, uniqueGeo)%>%
  summarise(meanTet = mean(tet, na.rm = TRUE), 
            TetSero = sum(tet_bin),
            meanRub = mean(ruv, na.rm = TRUE),
            RubSero = sum(ruv_bin),
            meanMea = mean(mev),
            MeaSero = sum(mev_bin),
            meanDip = mean(dip),
            DipSero = sum(dip_bin),
            ageCount = n())%>%
  mutate(TetSeroPercentage = TetSero/ageCount)%>%
  mutate(RubSeroPercentage = RubSero/ageCount)%>%
  mutate(MeaSeroPercentage = MeaSero/ageCount)%>%
  mutate(DipSeroPercentage = DipSero/ageCount)

#Mother data Import

mother_geo <- read_sas("C:/Users/tzb5519/Desktop/PostDoc/Nigeria/CDCAnalysis/CDC/Data/nms4_gid_wra_01dec2020.sas7bdat", NULL)%>%
  mutate(uniqueGeo = paste(LATNUM, LONGNUM, sep = " "))%>%
  mutate(age_group = ifelse(Age < 20, 1, ifelse(Age >19 & Age < 25, 2, ifelse(Age >24 & Age < 30, 3, ifelse(Age > 29 & Age <35, 4, ifelse(Age > 34 & Age < 40, 5, ifelse(Age> 39 & Age < 45, 6, 7)))))))

mother_geo_sum <- mother_geo%>%
  group_by(Age, uniqueGeo)%>%
  summarise(meanTet = mean(tet, na.rm = TRUE), 
            TetSero = sum(tet_bin),
            meanRub = mean(ruv, na.rm = TRUE),
            RubSero = sum(ruv_bin),
            meanMea = mean(mev),
            MeaSero = sum(mev_bin),
            meanDip = mean(dip),
            DipSero = sum(dip_bin),
            ageCount = n())%>%
  mutate(TetSeroPercentage = TetSero/ageCount)%>%
  mutate(RubSeroPercentage = RubSero/ageCount)%>%
  mutate(MeaSeroPercentage = MeaSero/ageCount)%>%
  mutate(DipSeroPercentage = DipSero/ageCount)%>%
  mutate(age_group = ifelse(Age < 20, 1, ifelse(Age >19 & Age < 25, 2, ifelse(Age >24 & Age < 30, 3, ifelse(Age > 29 & Age <35, 4, ifelse(Age > 34 & Age < 40, 5, ifelse(Age> 39 & Age < 45, 6, 7)))))))%>%
  mutate(age_under25 = ifelse(Age <25 , 0, 1))

mother_geo_sum_1 <- mother_geo_sum%>%
  select(-age_group)

total_age_df <- rbind(child_geo_sum, mother_geo_sum_1)

LATLONG <- stringr::str_split(total_age_df$uniqueGeo, boundary("word"))

LATNUM <- c()
LONGNUM <- c()

for(i in 1:length(LATLONG)){
  
  LATNUM[i] <-LATLONG[[i]][1]
  LONGNUM[i] <- LATLONG[[i]][2]
  
}

total_age_df$LATNUM <- as.numeric(LATNUM)
total_age_df$LONGNUM <- as.numeric(LONGNUM)

total_age_df <- total_age_df%>%
  filter(!is.na(LONGNUM))%>%
  mutate(X= LONGNUM)%>%
  mutate(Y=LATNUM)%>%
  select(-LONGNUM, -LATNUM)

total_age_df_sf <- st_as_sf(total_age_df, coords = c("X", "Y"), crs = "+proj=longlat +datum=WGS84 +no_defs")

save.image(file = "Intermediate/cdc_import_data.RData")

