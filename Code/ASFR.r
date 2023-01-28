#Using Package Management package to load all library dependencies

if(!require("pacman")) install.packages("pacman")
pacman::p_load(haven, sf, tidyr, dplyr, gstat, ggplot2, kriging, sjPlot, raster, stars, foreign, survey, wpp2019, automap, mgcv)

load("Intermediate/shapefiles.RData")
load("Intermediate/WorldPop.RData")

individual_SAS <- read_sas("Data/Individual_Recode/NGIR7AFL.SAS7BDAT")

individual_SAS$id <- seq.int(nrow(individual_SAS))

## note - keeping v005, v021, and v022 because needed to account for sampling

## Get births
individual_SAS_sm1 <- individual_SAS %>% 
  dplyr::select(id, V001, V002, V003, V005, V021, V022, V008,  V011, paste("B3_0", 1:9, sep = ""), paste("B3_", 10:20, sep = ""))

individual_SAS_sm1 <- as.data.frame(individual_SAS_sm1)%>%
  mutate(uniqueID = paste0("C", V001, "H", V002, "R", V003))

births <- reshape::melt(individual_SAS_sm1, id = c("id", "V001", "V002", "V003", "uniqueID", "V008", "V011", "V005", "V021", "V022")) %>% #getting line for each b3_ (birth) index
  rename("B3" = "value") %>% #rename "value" back to b3 or child dob CMC
  filter(!is.na(B3)) %>% #every row is now a birth
  mutate(child_age = as.numeric(V008 - B3)) %>% #v008 survey CMC; b3 child dob CMC
  mutate(agegroup = as.integer( (B3 - V011) /60)) %>% #mothers agegroup; v011 mom dob CMC
  filter(agegroup > 2) %>% #limit dataset to mothers 15 years and older (i.e., age group at least 3)
  filter(child_age <= 36) %>% #limit dataset to only births in last 36 months
  dplyr::select(id, uniqueID, agegroup, V001, V002, V005, V003, V021, V022)%>%
  mutate(birth =1, exposure=0)

individual_SAS_sm2 <- individual_SAS %>% 
  dplyr::select(id, V001, V002, V003, V008, V011, V005, V021, V022)

individual_SAS_sm2 <- as.data.frame(individual_SAS_sm2)%>%
  mutate(uniqueID = paste0("C", V001, "H", V002, "R", V003))

exposure.tmp <- individual_SAS_sm2 %>%
  mutate(survey_age = V008-V011-1,
         agegroup = as.integer(survey_age / 60), #mothers age group
         exposure = survey_age - (agegroup*60)+1, #exposure (number of months) 
         upper_exposure = ifelse(exposure >= 36, 36, exposure), #upper age exposure
         lower_exposure = ifelse(exposure < 36 & agegroup >=4, 36-exposure, 0)) %>% #lower age exposure
  filter(agegroup > 2)%>% #limit to ages 15-49
  dplyr::select(-V008, -V011, -survey_age) 

upper.exposure <- exposure.tmp %>% 
  dplyr::select(-lower_exposure, -exposure) %>% rename(exposure = upper_exposure) 

lower.exposure <- exposure.tmp %>%
  filter(lower_exposure !=0) %>% #limit dataset to only rows where there is some exposure in lower age group
  dplyr::select(-upper_exposure, -exposure) %>% rename(exposure = lower_exposure) %>%
  mutate(agegroup = agegroup-1)

exposure <- rbind(upper.exposure, lower.exposure) %>% 
  mutate(exposure = exposure / 12) %>% #put exposure in years from months
  mutate(birth=0) 

exposure <- exposure%>%
  dplyr::select(id, uniqueID, agegroup, V001, V002, V005, V003, V021, V022, birth, exposure)

combine <- rbind(births, exposure) 

final_df <- combine %>% 
  group_by(id, uniqueID, agegroup) %>% 
  summarise(birth_tot = sum(birth), exp_tot = sum(exposure)) %>% 
  filter(birth_tot !=0 | exp_tot !=0) %>% 
  mutate(mother_agegroup = factor(agegroup, levels = 3:9, labels=c("15-19","20-24","25-29","30-34","35-39","40-44","45-49")))%>%
  mutate(cvalue = sub("H.*", "", uniqueID))%>%
  mutate(DHSCLUST = sub(".*C", "", cvalue))%>%
  mutate(DHSCLUST = as.numeric(DHSCLUST))

final_mat_spatial <- left_join(final_df, nigeria, by = "DHSCLUST")

final_mat_spatial_SF <- st_as_sf(final_mat_spatial, coords = c("LONGNUM", "LATNUM"),
                                 crs = "+proj=longlat +datum=WGS84 +no_defs")

mat_spatial <- final_mat_spatial_SF%>%
  group_by(agegroup, DHSCLUST, mother_agegroup)%>%
  summarize(birth_total = sum(birth_tot),
            exposure_total = sum(exp_tot))%>%
  mutate(asfr = birth_total / exposure_total)

mat_spatial_ID <- st_intersection(mat_spatial, nigeria_polygon)

st_crs(mat_spatial_ID) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

mat_spatial_ID_st <- st_transform(mat_spatial_ID, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

st_crs(mat_spatial_ID_st) <- "WGS 84"

mat_spatial_ID_sp <- as(mat_spatial_ID_st, 'Spatial')

df2_mat <- data.frame(st_coordinates(mat_spatial_ID))

df2_mat$birth_total <- mat_spatial_ID$birth_total

df2_mat$exposure_total <- mat_spatial_ID$exposure_total

df2_mat$asfr <- mat_spatial_ID$asfr

df2_mat$agegroup <- mat_spatial_ID$agegroup

coordinates(df2_mat) <- ~X+Y

proj4string(df2_mat) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

df3_mat <- cbind(df2_mat, coordinates(df2_mat))

proj4string(df3_mat) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

gam_mat <- gam(data = df3_mat, asfr ~ s(X, Y) + as.factor(agegroup) + as.factor(agegroup) * (X+ Y), family = binomial(logit))

grd_mat_3 <- tiff_10_14_complete[,1:2]
grd_mat_3$agegroup <- 3
names(grd_mat_3) <- c("X", "Y", "agegroup")

grd_mat_4 <- tiff_10_14_complete[,1:2]
grd_mat_4$agegroup <- 4
names(grd_mat_4) <- c("X", "Y", "agegroup")

grd_mat_5 <- tiff_10_14_complete[,1:2]
grd_mat_5$agegroup <- 5
names(grd_mat_5) <- c("X", "Y", "agegroup")

grd_mat_6 <- tiff_10_14_complete[,1:2]
grd_mat_6$agegroup <- 6
names(grd_mat_6) <- c("X", "Y", "agegroup")

grd_mat_7 <- tiff_10_14_complete[,1:2]
grd_mat_7$agegroup <- 7
names(grd_mat_7) <- c("X", "Y", "agegroup")

grd_mat_8 <- tiff_10_14_complete[,1:2]
grd_mat_8$agegroup <- 8
names(grd_mat_8) <- c("X", "Y", "agegroup")

XY <- tiff_10_14_complete[,1:2]

df_mat_3_pred <- predict.gam(gam_mat, grd_mat_3, se.fit = TRUE, type = "response")
df_mat_4_pred <- predict.gam(gam_mat, grd_mat_4, se.fit = TRUE, type = "response")
df_mat_5_pred <- predict.gam(gam_mat, grd_mat_5, se.fit = TRUE, type = "response")
df_mat_6_pred <- predict.gam(gam_mat, grd_mat_6, se.fit = TRUE, type = "response")
df_mat_7_pred <- predict.gam(gam_mat, grd_mat_7, se.fit = TRUE, type = "response")
df_mat_8_pred <- predict.gam(gam_mat, grd_mat_8, se.fit = TRUE, type = "response")

df_mat_3_total <- XY
df_mat_3_total$asfr <- df_mat_3_pred$fit

df_mat_4_total <- XY
df_mat_4_total$asfr <- df_mat_4_pred$fit

df_mat_5_total <- XY
df_mat_5_total$asfr <- df_mat_5_pred$fit

df_mat_6_total <- XY
df_mat_6_total$asfr <- df_mat_6_pred$fit

df_mat_7_total <- XY
df_mat_7_total$asfr <- df_mat_7_pred$fit

df_mat_8_total <- XY
df_mat_8_total$asfr <- df_mat_8_pred$fit

save(df_mat_3_total, df_mat_4_total, df_mat_5_total, df_mat_6_total, df_mat_7_total, df_mat_8_total, XY, file = "Intermediate/asfr.RData")
