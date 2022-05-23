#Using Package Management package to load all library dependencies

if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyr, dplyr, ggplot2, haven, ggjoy, viridis, sf, mgcv, spdep, spaMM, sjPlot, stringr, readr, forcats, rgdal, tidyverse, automap, gstat, gridExtra, grid, ggpubr)

load("Intermediate/shapefiles.RData")
load("Intermediate/WorldPop.RData")

#Read in Birth DHS Data
BirthSAS <- read_sas("Data/CDC_Data/NGBR7AFL.SAS7BDAT", 
                     NULL)


#Read in Household data 

HouseholdSAS <- read_sas("Data/CDC_Data/NGHR7AFL.SAS7BDAT",
                         NULL)
#Case ID NUmber
caseID <- BirthSAS$CASEID

#Birth Column Number
BIDX <- BirthSAS$BIDX

#Cluster

clusterID <- BirthSAS$V001

#Household Number

householdID <- BirthSAS$V002

#Respondent Line Number

respondentID <- BirthSAS$V003

#Sample Weight

sampleWeight <- BirthSAS$V005

#Mother Age

motherAge <- BirthSAS$V012

#Residence - Urban/Rural

urban_rural <- BirthSAS$V102

#Number of Births

number_births <- BirthSAS$V201

#Reproduction Data

#Sex of Child

ChildS <- BirthSAS$B4

#Age of Children: Months

Age_m <- BirthSAS$B19

#Birth Order

MIDX <- BirthSAS$MIDX

#Tetanus Toxicoid

toxicoid_injection <- BirthSAS$M1

toxicoid_injection_pre_pregnancy <- BirthSAS$M1A

toxicoid_injection_months <- BirthSAS$M1B

toxicoid_injection_year <- BirthSAS$M1C

toxicoid_injection_years_ago <- BirthSAS$M1D

toxicoid_injection_last <- BirthSAS$M1E

#Healthcare Attendent

healthcare_doctor <- BirthSAS$M2A

healthcare_traditional <- BirthSAS$M2G

healthcare_noone <- BirthSAS$M2N

delivery_doctor <- BirthSAS$M3A

delivery_nurse <- BirthSAS$M3B

delivery_aux_midwife <- BirthSAS$M3C

deliver_community_health_worker <- BirthSAS$M3D

delivery_traditional <- BirthSAS$M3G

delivery_noone <- BirthSAS$M3N

#Place of delivery

delivery_place <- BirthSAS$M15

#Care 2 days after birth

cord_care <- BirthSAS$M78A

#Economic Status

wealth_index <- HouseholdSAS$HV270A

#Vaccination History

DTP1 <- BirthSAS$H3

DTP2 <- BirthSAS$H5

DTP3 <- BirthSAS$H7

Measles <- BirthSAS$H9

#Cord Care

applied_cord <- BirthSAS$S434H

chlorhexidine <- BirthSAS$S434IA

antiseptic <- BirthSAS$S434IB

oliveoil <- BirthSAS$S434IC

ash <- BirthSAS$S434ID

animaldung <- BirthSAS$S434IE

turmeric <- BirthSAS$S434IF

toothpaste <- BirthSAS$S434IG

applied_other <- BirthSAS$S434IX

nigeria.full <- as.data.frame(cbind(caseID, BIDX, MIDX, clusterID, householdID, respondentID, sampleWeight,
                                    motherAge, number_births, ChildS, Age_m, urban_rural, toxicoid_injection, toxicoid_injection_last,
                                    toxicoid_injection_months, toxicoid_injection_pre_pregnancy, toxicoid_injection_year, toxicoid_injection_years_ago,
                                    healthcare_doctor, healthcare_traditional, healthcare_noone,
                                    delivery_doctor, delivery_nurse, delivery_aux_midwife, deliver_community_health_worker, delivery_traditional, delivery_noone, delivery_place,
                                    cord_care, DTP1, DTP2, DTP3, Measles, applied_cord, chlorhexidine, antiseptic, oliveoil, ash, animaldung, turmeric, toothpaste, applied_other))%>%
  mutate(uniqueID = paste0("C", clusterID, "H", householdID, "R", respondentID))


#Household Dataset, Cluster

household_cluster <- HouseholdSAS$HV001

#Household Dataset, Household Number

household_number <- HouseholdSAS$HV002

#household dataset, respondent line number

household_linenumber <- HouseholdSAS$HV003

#Household Wealth Index for Urban Rural

household_WI <- HouseholdSAS$HV270A

#Household Wealth Index Raw

household_WI_raw <- HouseholdSAS$HV270

nigeria_latest_birth <- nigeria.full%>%
  mutate(motherAge=as.numeric(as.character(motherAge)))%>%
  mutate(number_births = as.numeric(as.character(number_births)))

nigeria.household.merge <- as.data.frame(cbind(household_cluster, household_number, household_linenumber, household_WI, household_WI_raw))%>%
  mutate(uniqueID = paste0("C", household_cluster, "H", household_number, "R", household_linenumber))

#Join with Household Data

nigeria_latest_birth_WI <- nigeria_latest_birth%>%
  left_join(nigeria.household.merge, by = "uniqueID")

nigeria_latest_birth_WI <- nigeria_latest_birth_WI%>%
  mutate(DHSCLUST = as.numeric(as.character(clusterID)))

spatial_nigeria <- left_join(nigeria_latest_birth_WI, nigeria, by = "DHSCLUST")

spatial_nigeria <- spatial_nigeria%>%
  mutate(DTP1_1 = ifelse(DTP1== "1" | DTP1=="2" |DTP1 == "3", 1, 0))%>%
  mutate(DTP2_1 = ifelse(DTP2 == "1" | DTP2 == "2" | DTP2 == "3", 1, 0))%>%
  mutate(DTP3_1 = ifelse(DTP3 == "1" |DTP3 == "2" | DTP3 =="3" , 1,0))%>%
  mutate(Measles_1 = ifelse(Measles == "1" | Measles == "2" | Measles == "3", 1, 0))%>%
  mutate(new_weight = as.numeric(as.character(sampleWeight))/1000000)%>%
  dplyr::filter(!is.na(applied_cord))%>%
  mutate(SBA = ifelse(delivery_doctor == 1 | delivery_nurse == 1 | delivery_aux_midwife == 1, 1, ifelse(is.na(delivery_doctor) & is.na(delivery_nurse) & is.na(delivery_aux_midwife), NA, 0)))%>%
  mutate(urban_rural = as.numeric(as.character(urban_rural)))%>%
  mutate(urban_rural = ifelse(is.na(urban_rural), NA, ifelse(urban_rural == 1, 0, 1)))%>%
  dplyr::mutate(applied_cord = as.numeric(as.character(applied_cord)))%>%
  mutate(applied_cord = ifelse(applied_cord == 1, 1, 0))%>%
  mutate(chlorhexi = ifelse(chlorhexidine == 1 | antiseptic == 1, 1, 0))%>%
  mutate(clean_cord = ifelse(applied_cord == 0, 1, 
                             ifelse(oliveoil == "1" | ash == "1" | animaldung == "1" | turmeric == "1" | toothpaste == "1" | applied_other == "1", 0, 
                                    ifelse(chlorhexi == 1, 1, 0))))%>%
  mutate(unclean_cord = ifelse(clean_cord == 1, 0, 1))%>%
  mutate(non_SBA = ifelse(SBA == 1, 0, 1))%>%
  mutate(toxicoid_injection = as.numeric(as.character(toxicoid_injection)))%>%
  mutate(toxicoid_injection_pre_pregnancy = as.numeric(as.character(toxicoid_injection_pre_pregnancy)))%>%
  mutate(neonatal_tetanus = ifelse(toxicoid_injection == 8, 0, ifelse(toxicoid_injection > 1, 1, ifelse(is.na(toxicoid_injection), NA, ifelse(toxicoid_injection_pre_pregnancy > 2 & toxicoid_injection_pre_pregnancy != 8, 1,0)))))%>%
  mutate(delivery_place = as.numeric(as.character(delivery_place)))%>%
  mutate(birth_facility = ifelse(delivery_place > 12 & delivery_place <96, 1, ifelse(is.na(delivery_place), NA, 0)))%>%
  mutate(risk_births_indicator_2 = ifelse(household_WI_raw < 3 & urban_rural == 1, 1, 0))%>%
  mutate(risk_births_indicator_3 = ifelse(household_WI_raw < 4 & urban_rural == 1, 1, 0))%>%
  mutate(risk_births_indicator_1 = ifelse(household_WI_raw < 2 & urban_rural == 1, 1, 0))%>%
  mutate(risk_births_indicator_SBA = ifelse(non_SBA == 1 & unclean_cord == 1, 1, 0))%>%
  mutate(risk_births_indicator_OR = ifelse(non_SBA == 1 | unclean_cord == 1, 1, 0))%>%
  mutate(age_under25 = ifelse(motherAge < 25, 0, 1))

spatial_nigeria_Sf <- st_as_sf(spatial_nigeria, coords = c("LONGNUM", "LATNUM"),
                               crs = "+proj=longlat +datum=WGS84 +no_defs")

save(spatial_nigeria_Sf, file = "Intermediate/spatial_DHS.RData")

risky_births <- spatial_nigeria_Sf%>%
  group_by(clusterID, age_under25)%>%
  summarize(total_clean_cord = sum(clean_cord, na.rm = TRUE),
            clean_cord_births = sum(!is.na(clean_cord)),
            total_SBA = sum(SBA, na.rm =TRUE),
            total_neonatal_tetanus = sum(neonatal_tetanus, na.rm = TRUE),
            total_birth_facility = sum(birth_facility, na.rm = TRUE),
            total_urban_rural = sum(urban_rural, na.rm = TRUE),
            mean_WI = mean(household_WI_raw, na.rm = TRUE),
            total_non_clean_cord = sum(unclean_cord, na.rm = TRUE),
            total_non_SBA = sum(non_SBA, na.rm = TRUE),
            total_risky_1 = sum(risk_births_indicator_1, na.rm = TRUE),
            total_risky_2 = sum(risk_births_indicator_2, na.rm = TRUE),
            total_risky_3 = sum(risk_births_indicator_3, na.rm = TRUE),
            total_risky_SBA = sum(risk_births_indicator_SBA, na.rm = TRUE),
            total_risky_OR = sum(risk_births_indicator_OR, na.rm = TRUE),
            totalbirths = n())%>%
  mutate(clean_birth_prop = total_clean_cord / totalbirths)%>%
  mutate(clean_birth_prop_1 = total_clean_cord/clean_cord_births)%>%
  mutate(non_clean_cord_prop = total_non_clean_cord / totalbirths)%>%
  mutate(non_SBA_prop =total_non_SBA / totalbirths)%>%
  mutate(SBA_pro = total_SBA / totalbirths)%>%
  mutate(neonatal_tetanus_prop = total_neonatal_tetanus / totalbirths)%>%
  mutate(birth_facility_prop = total_birth_facility / totalbirths)%>%
  mutate(urban_rural_prop = total_urban_rural / totalbirths)%>%
  mutate(risky_1_prop = total_risky_1 / totalbirths)%>%
  mutate(risky_2_prop = total_risky_2 / totalbirths)%>%
  mutate(risky_3_prop = total_risky_3 / totalbirths)%>%
  mutate(risky_SBA_prop = total_risky_SBA / totalbirths)%>%
  mutate(risky_OR_prop = total_risky_OR / totalbirths)

non_risky_births_ID <- st_intersection(risky_births, nigeria_polygon)

st_crs(non_risky_births_ID) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

non_risky_births_ID_1 <- st_transform(non_risky_births_ID, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

st_crs(non_risky_births_ID_1) <- "WGS 84"

non_risky_births_ID_sp <- as(non_risky_births_ID_1, 'Spatial')

df2_risk <- data.frame(st_coordinates(non_risky_births_ID))

df2_risk$total_clean_cord <- non_risky_births_ID$total_clean_cord

df2_risk$total_neonatal_tetanus <- non_risky_births_ID$total_neonatal_tetanus

df2_risk$totalbirths <- non_risky_births_ID$totalbirths

df2_risk$clean_birth_prop <- non_risky_births_ID$clean_birth_prop

df2_risk$total_SBA <- non_risky_births_ID$total_SBA

df2_risk$SBA_prop <- non_risky_births_ID$SBA_pro

df2_risk$total_birth_facility <- non_risky_births_ID$total_birth_facility

df2_risk$total_urban_rural <- non_risky_births_ID$total_urban_rural

df2_risk$neonatal_tetanus_prop <- non_risky_births_ID$neonatal_tetanus_prop

df2_risk$birth_facility_prop <- non_risky_births_ID$birth_facility_prop

df2_risk$urban_rural_prop <- non_risky_births_ID$urban_rural_prop

df2_risk$total_non_SBA <- non_risky_births_ID$total_non_SBA

df2_risk$total_non_clean_cord <- non_risky_births_ID$total_non_clean_cord

df2_risk$non_clean_cord_prop <- non_risky_births_ID$non_clean_cord_prop

df2_risk$non_SBA_prop <- non_risky_births_ID$non_SBA_prop

df2_risk$mean_WI <- non_risky_births_ID$mean_WI

df2_risk$total_risky_1 <- non_risky_births_ID$total_risky_1

df2_risk$total_risky_2 <- non_risky_births_ID$total_risky_2

df2_risk$total_risky_3 <- non_risky_births_ID$total_risky_3

df2_risk$risky_1_prop <- non_risky_births_ID$risky_1_prop

df2_risk$risky_2_prop <- non_risky_births_ID$risky_2_prop

df2_risk$risky_3_prop <- non_risky_births_ID$risky_3_prop

df2_risk$risky_SBA_prop <- non_risky_births_ID$risky_SBA_prop

df2_risk$total_risky_SBA <- non_risky_births_ID$total_risky_SBA

df2_risk$risky_OR_prop <- non_risky_births_ID$risky_OR_prop

df2_risk$total_risky_OR <- non_risky_births_ID$total_risky_OR

df2_risk$age_under25 <- non_risky_births_ID$age_under25

coordinates(df2_risk) <- ~X+Y

proj4string(df2_risk) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

df3_risk <- cbind(df2_risk, coordinates(df2_risk))

proj4string(df3_risk) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

df3_risk_sf <- st_as_sf(df3_risk)

st_crs(df3_risk_sf) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

gam_risky_under25 <- gam(data = df3_risk, risky_OR_prop ~ s(X, Y) + as.factor(age_under25) + as.factor(age_under25)*X + as.factor(age_under25)*Y, family = binomial(logit))

XY <- tiff_10_14_complete[,1:2]

grd_u_25 <- XY
grd_u_25$age_under25 <- 0
names(grd_u_25) <- c("X", "Y", "age_under25")

grd_o_25 <- XY
grd_o_25$age_under25 <- 1
names(grd_o_25) <- c("X", "Y", "age_under25")

r_under25_pred <- predict.gam(gam_risky_under25, grd_u_25, se.fit = TRUE, type = "response")
r_over25_pred <- predict.gam(gam_risky_under25, grd_o_25, se.fit = TRUE, type = "response")

risky_under25 <- XY
risky_under25$risk <- r_under25_pred$fit

risky_over25 <- XY
risky_over25$risk <- r_over25_pred$fit

save(risky_under25, risky_over25, file = "Intermediate/risky_births.RData")
