#Using Package Management package to load all library dependencies

if(!require("pacman")) install.packages("pacman")
pacman::p_load(mgcv, tidyverse, dplyr, raster, sf, ggplot2, ggrepel)

load("Intermediate/cdc_import_data.RData")
load("Intermediate/WorldPop.RData")
load("Intermediate/shapefiles.RData")
load("Intermediate/asfr.RData")
load("Intermediate/risky_births.RData")

#GAM models for each antigen, using logit link functions

gam_total_tet <- gam(TetSeroPercentage ~ s(X,Y) + Age + X * Age +Y*Age, family = binomial(link = "logit"), data = total_age_df)
gam_total_rub <- gam(RubSeroPercentage ~ s(X,Y) + Age + X * Age +Y*Age, family = binomial(link = "logit"), data = total_age_df)
gam_total_mea <- gam(MeaSeroPercentage ~ s(X,Y) + Age + X * Age +Y*Age, family = binomial(link = "logit"), data = total_age_df)
gam_total_dip <- gam(DipSeroPercentage ~s(X, Y) + Age + X * Age + Y * Age, family = binomial(link = "logit"), data = total_age_df)

#Specifying the inverse link functions in order to generate appropriate 95% CI later on

fam_rub_ttl <- family(gam_total_rub)
fam_tet_ttl <- family(gam_total_tet)
fam_mea_ttl <- family(gam_total_mea)
fam_dip_ttl <- family(gam_total_dip)
ilink_rub_ttl <- fam_rub_ttl$linkinv
ilink_tet_ttl <- fam_tet_ttl$linkinv
ilink_mea_ttl <- fam_mea_ttl$linkinv
ilink_dip_ttl <- fam_dip_ttl$linkinv

ages_total <- rep(0:44,1)

#Ensure imported data have unique ID to join with other datasets later on
risky_under25 <- risky_under25%>%
  mutate(uniqueID = paste(x, y, by = " "))%>%
  dplyr::select(risk, uniqueID)

risky_over25 <- risky_over25%>%
  mutate(uniqueID = paste(x, y, by = " "))%>%
  dplyr::select(risk, uniqueID)

df_mat_3_total <- df_mat_3_total%>%
  mutate(uniqueID = paste(x, y, by = " "))%>%
  dplyr::select(asfr, uniqueID)

colnames(df_mat_3_total) <- c("asfr", "uniqueID")

df_mat_4_total  <- df_mat_4_total%>%
  mutate(uniqueID = paste(x, y, by = " "))%>%
  dplyr::select(asfr, uniqueID)

colnames(df_mat_4_total) <- c("asfr", "uniqueID")

df_mat_5_total  <- df_mat_5_total%>%
  mutate(uniqueID = paste(x, y, by = " "))%>%
  dplyr::select(asfr, uniqueID)

colnames(df_mat_5_total ) <- c("asfr", "uniqueID")

df_mat_6_total <- df_mat_6_total%>%
  mutate(uniqueID = paste(x, y, by = " "))%>%
  dplyr::select(asfr, uniqueID)

colnames(df_mat_6_total) <- c("asfr", "uniqueID")

df_mat_7_total <- df_mat_7_total%>%
  mutate(uniqueID = paste(x, y, by = " "))%>%
  dplyr::select(asfr, uniqueID)

colnames(df_mat_7_total) <- c("asfr", "uniqueID")

df_mat_8_total <- df_mat_8_total%>%
  mutate(uniqueID = paste(x, y, by = " "))%>%
  dplyr::select(asfr, uniqueID)

colnames(df_mat_8_total) <- c("asfr", "uniqueID")

#Generate grid for predicted outcomes

grd_total <- c()

for(i in 1:length(ages_total)){
  
  grd_total[i] <- paste("grd_sp",ages_total[i],"age", sep="")
  
}

for(i in 1:length(ages_total)){
  
  temp_XY <- XY
  
  temp_XY$Age <- ages_total[i]
  
  colnames(temp_XY) <- c("X", "Y", "Age")
  
  assign(paste0("grd_sp",ages_total[i],"age"), temp_XY)
  
}

#Specifying loop parameters
models_total <- c("gam_total_tet", "gam_total_rub", "gam_total_mea", "gam_total_dip")

antigens <- c("tet", "rub", "mea", "dip")

family_total <- c("fam_tet_ttl", "fam_rub_ttl", "fam_mea_ttl", "fam_dip_ttl")
ilink_total <- c("ilink_tet_ttl", "ilink_rub_ttl", "ilink_mea_ttl", "ilink_dip_ttl")

#Using the GAM models, predicting the seroprevalence of each antigen for each age

for(i in 1:length(antigens)){
  
  for(j in 1:length(unique(grd_total))){
    
    temp <- add_column(get(grd_total[j]), fit=predict.gam(get(models_total[i]), newdata = get(grd_total[j]), se.fit = TRUE, type = "response")$fit, right_fit = predict.gam(get(models_total[i]), newdata = get(grd_total[j]), type = "response", se.fit = TRUE)$se.fit)
    temp_1 <- bind_cols(temp, setNames(as_tibble(predict.gam(get(models_total[i]), get(grd_total[j]), se.fit = TRUE)[1:2]), c('fit_link', 'se_link')))  
    temp_1 <- mutate(temp_1,
                     fit_response = get(ilink_total[i])(fit_link),
                     UC = get(ilink_total[i])(fit_link + (1.96*se_link)),
                     LC = get(ilink_total[i])(fit_link -(1.96*se_link)))
    
    assign(paste("pred.total_", antigens[i], "_", ages_total[j], sep = ""), temp_1)
  }
  
  
}

overall_sero_total <- pred.total_mea_0

type_predict <- c("fit", "UC", "LC")

#Generate individual object for each predicted value 
for(k in 1:length(type_predict)){
  
  if(k == 1){
    #Fitted value
    for(j in 1:length(antigens)){
      
      for (i in 1:length(ages_total)){
        
        temp_overall <- get(paste("pred.total_", antigens[j], "_", ages_total[i],sep =""))$fit
        assign(paste0(antigens[j], "_", type_predict[k], "_", ages_total[i]), temp_overall)
        
      }  
      
    }}
  
  else if(k ==2){
    #UC bound
    
    for(j in 1:length(antigens)){
      
      for (i in 1:length(ages_total)){
        
        temp_overall <- get(paste("pred.total_", antigens[j], "_", ages_total[i],sep =""))$UC
        assign(paste0(antigens[j], "_", type_predict[k], "_", ages_total[i]), temp_overall)
        
      }  
      
    }
    
  }
  
  else{
    #LC Bound
    
    for(j in 1:length(antigens)){
      
      for (i in 1:length(ages_total)){
        
        temp_overall <- get(paste("pred.total_", antigens[j], "_", ages_total[i],sep =""))$LC
        assign(paste0(antigens[j], "_", type_predict[k], "_", ages_total[i]), temp_overall)
        
      }  
      
    }
    
  }
}

age_ranges <- c(15,20, 20, 25, 25, 30, 30, 35, 35, 40, 40, 45)

XY <- pred.total_mea_0[,1:2]

antigens_names <- c("Tetanus", "Rubella", "Measles", "Diphtheria")

#Calculating the burden and pop risk for each antigen, by joining with the appropriate dataframes
#Pop risk calculation for each antigen is different, hence the if statement for each antigen index i
#Each age group generates an unique object per antigen

for(i in 1:length(antigens)){
  
  for(j in 1:6){
    
    length_range <- rep(age_ranges[j*2-1]: (age_ranges[j*2]-1)) 
    
    overall_temp_df <- c()
    
    overall_temp_UC_df <- c()
    
    overall_temp_LC_df <- c()
    
    for(k in 1:length(length_range)){
      
      temp_df <- get(paste0(antigens[i],"_fit_",length_range[k]))
      
      UC_df <- get(paste0(antigens[i],"_UC_", length_range[k]))
      
      LC_df <- get(paste0(antigens[i],"_LC_", length_range[k]))
      
      overall_temp_df <- cbind(overall_temp_df, temp_df)
      
      overall_temp_UC_df <- cbind(overall_temp_UC_df, UC_df)
      
      overall_temp_LC_df <- cbind(overall_temp_LC_df, LC_df)
      
      if(k == 5){
        
        if (i == 1){
          #Tetanus
          
          temp_rowmeans <- rowMeans(overall_temp_df)
          
          UC_rowmeans <- rowMeans(overall_temp_UC_df)
          
          LC_rowmeans <- rowMeans(overall_temp_LC_df)
          
          temp_df_XY <- cbind(XY, temp_rowmeans, UC_rowmeans, LC_rowmeans)
          
          temp_df_XY <- temp_df_XY%>%
            mutate(uniqueID = paste(X, Y, by = " "))
          
          if(j == 1){
            
            temp_df_XY_all <- inner_join(temp_df_XY, risky_under25, by = "uniqueID")%>%
              inner_join(df_mat_3_total, by = "uniqueID")%>%
              inner_join(tiff_15_19_complete, by = "uniqueID")
            
            names(temp_df_XY_all) <- c("X", "Y", "sero", "UC", "LC", "uniqueID", "risk", "asfr", "x","y",  "pop")
            
            temp_df_XY_all <- temp_df_XY_all%>%
              mutate(total_risk = (1-sero) * risk * asfr)%>%
              mutate(risk_UC = (1-UC)*risk *asfr)%>%
              mutate(risk_LC = (1-LC)*risk*asfr)%>%
              mutate(burden = (1-sero) * risk * asfr * pop)%>%
              mutate(burden_UC = (1-UC) * risk * asfr * pop)%>%
              mutate(burden_LC = (1-LC) * risk * asfr * pop)
            
            assign(paste0(antigens[i], "_", age_ranges[j*2-1], "_", age_ranges[j*2],"_burden"), temp_df_XY_all)
            
          }else if(j == 2){
            
            temp_df_XY_all <- inner_join(temp_df_XY, risky_under25, by = "uniqueID")%>%
              inner_join(df_mat_4_total, by = "uniqueID")%>%
              inner_join(tiff_20_24_complete, by = "uniqueID")
            
            names(temp_df_XY_all) <- c("X", "Y", "sero", "UC", "LC", "uniqueID", "risk", "asfr", "x","y",  "pop")
            
            temp_df_XY_all <- temp_df_XY_all%>%
              mutate(total_risk = (1-sero) * risk * asfr)%>%
              mutate(risk_UC = (1-UC)*risk *asfr)%>%
              mutate(risk_LC = (1-LC)*risk*asfr)%>%
              mutate(burden = (1-sero) * risk * asfr * pop)%>%
              mutate(burden_UC = (1-UC) * risk * asfr * pop)%>%
              mutate(burden_LC = (1-LC) * risk * asfr * pop)
            
            assign(paste0(antigens[i], "_", age_ranges[j*2-1], "_", age_ranges[j*2],"_burden"), temp_df_XY_all)
            
          } else if(j == 3){
            
            temp_df_XY_all <- inner_join(temp_df_XY, risky_over25, by = "uniqueID")%>%
              inner_join(df_mat_5_total, by = "uniqueID")%>%
              inner_join(tiff_25_29_complete, by = "uniqueID")
            
            names(temp_df_XY_all) <- c("X", "Y", "sero", "UC", "LC", "uniqueID", "risk", "asfr","x","y", "pop")
            
            temp_df_XY_all <- temp_df_XY_all%>%
              mutate(total_risk = (1-sero) * risk * asfr)%>%
              mutate(risk_UC = (1-UC)*risk *asfr)%>%
              mutate(risk_LC = (1-LC)*risk*asfr)%>%
              mutate(burden = (1-sero) * risk * asfr * pop)%>%
              mutate(burden_UC = (1-UC) * risk * asfr * pop)%>%
              mutate(burden_LC = (1-LC) * risk * asfr * pop)
            
            assign(paste0(antigens[i], "_", age_ranges[j*2-1], "_", age_ranges[j*2],"_burden"), temp_df_XY_all)
          } else if (j == 4){
            
            temp_df_XY_all <- inner_join(temp_df_XY, risky_over25, by = "uniqueID")%>%
              inner_join(df_mat_6_total, by = "uniqueID")%>%
              inner_join(tiff_30_34_complete, by = "uniqueID")
            
            names(temp_df_XY_all) <- c("X", "Y", "sero", "UC", "LC", "uniqueID", "risk", "asfr","x","y", "pop")
            
            temp_df_XY_all <- temp_df_XY_all%>%
              mutate(total_risk = (1-sero) * risk * asfr)%>%
              mutate(risk_UC = (1-UC)*risk *asfr)%>%
              mutate(risk_LC = (1-LC)*risk*asfr)%>%
              mutate(burden = (1-sero) * risk * asfr * pop)%>%
              mutate(burden_UC = (1-UC) * risk * asfr * pop)%>%
              mutate(burden_LC = (1-LC) * risk * asfr * pop)
            
            assign(paste0(antigens[i], "_", age_ranges[j*2-1], "_", age_ranges[j*2],"_burden"), temp_df_XY_all)
          } else if( j == 5){
            
            temp_df_XY_all <- inner_join(temp_df_XY, risky_over25, by = "uniqueID")%>%
              inner_join(df_mat_7_total, by = "uniqueID")%>%
              inner_join(tiff_35_39_complete, by = "uniqueID")
            
            names(temp_df_XY_all) <- c("X", "Y", "sero", "UC", "LC", "uniqueID", "risk", "asfr","x","y", "pop")
            
            temp_df_XY_all <- temp_df_XY_all%>%
              mutate(total_risk = (1-sero) * risk * asfr)%>%
              mutate(risk_UC = (1-UC)*risk *asfr)%>%
              mutate(risk_LC = (1-LC)*risk*asfr)%>%
              mutate(burden = (1-sero) * risk * asfr * pop)%>%
              mutate(burden_UC = (1-UC) * risk * asfr * pop)%>%
              mutate(burden_LC = (1-LC) * risk * asfr * pop)
            
            assign(paste0(antigens[i], "_", age_ranges[j*2-1], "_", age_ranges[j*2],"_burden"), temp_df_XY_all)
            
          } else{
            
            temp_df_XY_all <- inner_join(temp_df_XY, risky_over25, by = "uniqueID")%>%
              inner_join(df_mat_8_total, by = "uniqueID")%>%
              inner_join(tiff_40_44_complete, by = "uniqueID")
            
            names(temp_df_XY_all) <- c("X", "Y", "sero", "UC", "LC", "uniqueID", "risk", "asfr", "x","y", "pop")
            
            temp_df_XY_all <- temp_df_XY_all%>%
              mutate(total_risk = (1-sero) * risk * asfr)%>%
              mutate(risk_UC = (1-UC)*risk *asfr)%>%
              mutate(risk_LC = (1-LC)*risk*asfr)%>%
              mutate(burden = (1-sero) * risk * asfr * pop)%>%
              mutate(burden_UC = (1-UC) * risk * asfr * pop)%>%
              mutate(burden_LC = (1-LC) * risk * asfr * pop)
            
            assign(paste0(antigens[i], "_", age_ranges[j*2-1], "_", age_ranges[j*2],"_burden"), temp_df_XY_all)
            
          }
          
        } else if(i == 2){
          
          #Rubella
          
          temp_rowmeans <- rowMeans(overall_temp_df)
          
          UC_rowmeans <- rowMeans(overall_temp_UC_df)
          
          LC_rowmeans <- rowMeans(overall_temp_LC_df)
          
          temp_df_XY <- cbind(XY, temp_rowmeans, UC_rowmeans, LC_rowmeans)
          
          temp_df_XY <- temp_df_XY%>%
            mutate(uniqueID = paste(X, Y, by = " "))
          
          if(j == 1){
            
            temp_df_XY_all <- inner_join(temp_df_XY, df_mat_3_total, by = "uniqueID")%>%
              inner_join(tiff_15_19_complete, by = "uniqueID")
            
            names(temp_df_XY_all) <- c("X", "Y", "sero", "UC", "LC", "uniqueID", "asfr", "x", "y", "pop")
            
            temp_df_XY_all <- temp_df_XY_all%>%
              mutate(total_risk = (1-sero) * asfr)%>%
              mutate(risk_UC = (1-UC)*asfr)%>%
              mutate(risk_LC = (1-LC)*asfr)%>%
              mutate(burden = (1-sero)  * asfr * pop)%>%
              mutate(burden_UC = (1-UC)  * asfr * pop)%>%
              mutate(burden_LC = (1-LC) * asfr * pop)
            
            assign(paste0(antigens[i], "_", age_ranges[j*2-1], "_", age_ranges[j*2],"_burden"), temp_df_XY_all)
            
          }else if(j == 2){
            
            temp_df_XY_all <- inner_join(temp_df_XY, df_mat_4_total, by = "uniqueID")%>%
              inner_join(tiff_20_24_complete, by = "uniqueID")
            
            names(temp_df_XY_all) <- c("X", "Y", "sero", "UC", "LC", "uniqueID", "asfr", "x", "y", "pop")
            
            temp_df_XY_all <- temp_df_XY_all%>%
              mutate(total_risk = (1-sero) * asfr)%>%
              mutate(risk_UC = (1-UC)*asfr)%>%
              mutate(risk_LC = (1-LC)*asfr)%>%
              mutate(burden = (1-sero)  * asfr * pop)%>%
              mutate(burden_UC = (1-UC)  * asfr * pop)%>%
              mutate(burden_LC = (1-LC) * asfr * pop)
            
            assign(paste0(antigens[i], "_", age_ranges[j*2-1], "_", age_ranges[j*2],"_burden"), temp_df_XY_all)
            
          } else if(j == 3){
            
            temp_df_XY_all <- inner_join(temp_df_XY, df_mat_5_total, by = "uniqueID")%>%
              inner_join(tiff_25_29_complete, by = "uniqueID")
            
            names(temp_df_XY_all) <- c("X", "Y", "sero", "UC", "LC", "uniqueID", "asfr", "x", "y", "pop")
            
            temp_df_XY_all <- temp_df_XY_all%>%
              mutate(total_risk = (1-sero) * asfr)%>%
              mutate(risk_UC = (1-UC)*asfr)%>%
              mutate(risk_LC = (1-LC)*asfr)%>%
              mutate(burden = (1-sero)  * asfr * pop)%>%
              mutate(burden_UC = (1-UC)  * asfr * pop)%>%
              mutate(burden_LC = (1-LC) * asfr * pop)
            
            assign(paste0(antigens[i], "_", age_ranges[j*2-1], "_", age_ranges[j*2],"_burden"), temp_df_XY_all)
          } else if (j == 4){
            
            temp_df_XY_all <- inner_join(temp_df_XY, df_mat_6_total, by = "uniqueID")%>%
              inner_join(tiff_30_34_complete, by = "uniqueID")
            
            names(temp_df_XY_all) <- c("X", "Y", "sero", "UC", "LC", "uniqueID", "asfr", "x", "y", "pop")
            
            temp_df_XY_all <- temp_df_XY_all%>%
              mutate(total_risk = (1-sero) * asfr)%>%
              mutate(risk_UC = (1-UC)*asfr)%>%
              mutate(risk_LC = (1-LC)*asfr)%>%
              mutate(burden = (1-sero)  * asfr * pop)%>%
              mutate(burden_UC = (1-UC)  * asfr * pop)%>%
              mutate(burden_LC = (1-LC) * asfr * pop)
            
            assign(paste0(antigens[i], "_", age_ranges[j*2-1], "_", age_ranges[j*2],"_burden"), temp_df_XY_all)
          } else if( j == 5){
            
            temp_df_XY_all <- inner_join(temp_df_XY, df_mat_7_total, by = "uniqueID")%>%
              inner_join(tiff_35_39_complete, by = "uniqueID")
            
            names(temp_df_XY_all) <- c("X", "Y", "sero", "UC", "LC", "uniqueID", "asfr", "x", "y", "pop")
            
            temp_df_XY_all <- temp_df_XY_all%>%
              mutate(total_risk = (1-sero) * asfr)%>%
              mutate(risk_UC = (1-UC)*asfr)%>%
              mutate(risk_LC = (1-LC)*asfr)%>%
              mutate(burden = (1-sero)  * asfr * pop)%>%
              mutate(burden_UC = (1-UC)  * asfr * pop)%>%
              mutate(burden_LC = (1-LC) * asfr * pop)
            
            assign(paste0(antigens[i], "_", age_ranges[j*2-1], "_", age_ranges[j*2],"_burden"), temp_df_XY_all)
            
          } else{
            
            temp_df_XY_all <- inner_join(temp_df_XY, df_mat_8_total, by = "uniqueID")%>%
              inner_join(tiff_40_44_complete, by = "uniqueID")
            
            names(temp_df_XY_all) <- c("X", "Y", "sero", "UC", "LC", "uniqueID", "asfr", "x", "y", "pop")
            
            temp_df_XY_all <- temp_df_XY_all%>%
              mutate(total_risk = (1-sero) * asfr)%>%
              mutate(risk_UC = (1-UC)*asfr)%>%
              mutate(risk_LC = (1-LC)*asfr)%>%
              mutate(burden = (1-sero)  * asfr * pop)%>%
              mutate(burden_UC = (1-UC)  * asfr * pop)%>%
              mutate(burden_LC = (1-LC) * asfr * pop)
            
            assign(paste0(antigens[i], "_", age_ranges[j*2-1], "_", age_ranges[j*2],"_burden"), temp_df_XY_all)
            
          }
          
          
        } else{
          
          #Measles and Diphtheria
          
          temp_rowmeans <- rowMeans(overall_temp_df)
          
          UC_rowmeans <- rowMeans(overall_temp_UC_df)
          
          LC_rowmeans <- rowMeans(overall_temp_LC_df)
          
          temp_df_XY <- cbind(XY, temp_rowmeans, UC_rowmeans, LC_rowmeans)
          
          temp_df_XY <- temp_df_XY%>%
            mutate(uniqueID = paste(X, Y, by = " "))
          
          if(j == 1){
            
            temp_df_XY_all <- inner_join(temp_df_XY, tiff_15_19_complete, by = "uniqueID")
            
            names(temp_df_XY_all) <- c("X", "Y", "sero", "UC", "LC", "uniqueID", "x", "y", "pop")
            
            temp_df_XY_all <- temp_df_XY_all%>%
              mutate(burden = (1-sero) * pop)%>%
              mutate(burden_UC = (1-UC) * pop)%>%
              mutate(burden_LC = (1-LC) * pop)
            
            assign(paste0(antigens[i], "_", age_ranges[j*2-1], "_", age_ranges[j*2],"_burden"), temp_df_XY_all)
          }else if(j == 2){
            
            temp_df_XY_all <- inner_join(temp_df_XY, tiff_20_24_complete, by = "uniqueID")
            
            names(temp_df_XY_all) <- c("X", "Y", "sero", "UC", "LC", "uniqueID", "x", "y", "pop")
            
            temp_df_XY_all <- temp_df_XY_all%>%
              mutate(burden = (1-sero) * pop)%>%
              mutate(burden_UC = (1-UC) * pop)%>%
              mutate(burden_LC = (1-LC) * pop)
            
            assign(paste0(antigens[i], "_", age_ranges[j*2-1], "_", age_ranges[j*2],"_burden"), temp_df_XY_all)
            
          } else if(j == 3){
            
            temp_df_XY_all <- inner_join(temp_df_XY, tiff_25_29_complete, by = "uniqueID")
            
            names(temp_df_XY_all) <- c("X", "Y", "sero", "UC", "LC", "uniqueID", "x", "y", "pop")
            
            temp_df_XY_all <- temp_df_XY_all%>%
              mutate(burden = (1-sero) * pop)%>%
              mutate(burden_UC = (1-UC) * pop)%>%
              mutate(burden_LC = (1-LC) * pop)
            
            assign(paste0(antigens[i], "_", age_ranges[j*2-1], "_", age_ranges[j*2],"_burden"), temp_df_XY_all)
          } else if (j == 4){
            
            temp_df_XY_all <- inner_join(temp_df_XY, tiff_30_34_complete, by = "uniqueID")
            
            names(temp_df_XY_all) <- c("X", "Y", "sero", "UC", "LC", "uniqueID", "x", "y", "pop")
            
            temp_df_XY_all <- temp_df_XY_all%>%
              mutate(burden = (1-sero) * pop)%>%
              mutate(burden_UC = (1-UC) * pop)%>%
              mutate(burden_LC = (1-LC) * pop)
            
            assign(paste0(antigens[i], "_", age_ranges[j*2-1], "_", age_ranges[j*2],"_burden"), temp_df_XY_all)
          } else if( j == 5){
            
            temp_df_XY_all <- inner_join(temp_df_XY, tiff_35_39_complete, by = "uniqueID")
            
            names(temp_df_XY_all) <- c("X", "Y", "sero", "UC", "LC", "uniqueID", "x", "y", "pop")
            
            temp_df_XY_all <- temp_df_XY_all%>%
              mutate(burden = (1-sero) * pop)%>%
              mutate(burden_UC = (1-UC) * pop)%>%
              mutate(burden_LC = (1-LC) * pop)
            
            assign(paste0(antigens[i], "_", age_ranges[j*2-1], "_", age_ranges[j*2],"_burden"), temp_df_XY_all)
            
          } else{
            
            temp_df_XY_all <- inner_join(temp_df_XY, tiff_40_44_complete, by = "uniqueID")
            
            names(temp_df_XY_all) <- c("X", "Y", "sero", "UC", "LC", "uniqueID", "x", "y", "pop")
            
            temp_df_XY_all <- temp_df_XY_all%>%
              mutate(burden = (1-sero) * pop)%>%
              mutate(burden_UC = (1-UC) * pop)%>%
              mutate(burden_LC = (1-LC) * pop)
            
            assign(paste0(antigens[i], "_", age_ranges[j*2-1], "_", age_ranges[j*2],"_burden"), temp_df_XY_all)
            
          }
          
        }
        
      }
      
    }
    
    
  }
  
}

age_groups <- c("15_20", "20_25", "25_30", "30_35", "35_40", "40_45")

#Combine age group results to generate one df for each antigen
for(i in 1:length(antigens)){
  
  final_data_df <- as.data.frame(matrix(ncol = 16))
  names(final_data_df) <- c("risky_birth", "asfr", "sero", "sero_UC", "sero_LC", "risk", "risk_UC", "risk_LC", "pop_risk", "pop_risk_UC", "pop_risk_LC", "population", "age_group", "antigen", "X", "Y")
  
  if(i == 1){
    
    total_antigens <- as.data.frame(matrix(ncol = 16))
    names(total_antigens) <- c("risky_birth", "asfr", "sero", "sero_UC", "sero_LC", "risk", "risk_UC", "risk_LC", "pop_risk", "pop_risk_UC", "pop_risk_LC", "population", "age_group", "antigen", "X", "Y")
    
    #Tetanus
    
    for(j in 1:length(age_groups)){
      temp_sero <-  get(paste0(antigens[i],"_",age_groups[j],"_burden"))$sero
      temp_sero_UC <- get(paste0(antigens[i],"_",age_groups[j],"_burden"))$UC
      temp_sero_LC <- get(paste0(antigens[i],"_",age_groups[j],"_burden"))$LC
      temp_risk <- get(paste0(antigens[i],"_",age_groups[j],"_burden"))$risk
      temp_asfr <- get(paste0(antigens[i],"_",age_groups[j],"_burden"))$asfr
      temp_total_risk <- get(paste0(antigens[i],"_",age_groups[j],"_burden"))$total_risk
      temp_risk_UC <- get(paste0(antigens[i],"_",age_groups[j],"_burden"))$risk_UC
      temp_risk_LC <- get(paste0(antigens[i],"_",age_groups[j],"_burden"))$risk_LC
      temp_total <- get(paste0(antigens[i],"_",age_groups[j],"_burden"))$burden
      temp_burden_UC <- get(paste0(antigens[i],"_",age_groups[j],"_burden"))$burden_UC
      temp_burden_LC <- get(paste0(antigens[i],"_",age_groups[j],"_burden"))$burden_LC
      temp_pop <- get(paste0(antigens[i],"_",age_groups[j],"_burden"))$pop
      
      total_antigens_temp <- as.data.frame(cbind(temp_risk, temp_asfr, temp_sero, temp_sero_UC, temp_sero_LC,temp_total_risk, temp_risk_UC,
                                                 temp_risk_LC,temp_total, temp_burden_UC, temp_burden_LC, temp_pop))
      
      total_antigens_temp$age_group <- age_groups[j]
      
      total_antigens_temp$antigen <- antigens[i]
      
      total_antigens_temp$X <- tet_15_20_burden$X
      
      total_antigens_temp$Y <- tet_15_20_burden$Y
      
      names(total_antigens_temp) <- c("risky_birth", "asfr", "sero", "sero_UC", "sero_LC", "risk", "risk_UC", "risk_LC", "pop_risk", "pop_risk_UC", "pop_risk_LC", "population", "age_group", "antigen", "X", "Y")
      
      total_antigens <- rbind(total_antigens, total_antigens_temp)
      
      
    }
    
    total_antigens <- total_antigens%>%
      filter(!is.na(X))
    
    assign(paste0(antigens[i], "_total_df"), total_antigens)
    
  }
  
  else if(i == 2){
    
    #Rubella
    
    total_antigens <- as.data.frame(matrix(ncol = 16))
    names(total_antigens) <- c("risky_birth", "asfr", "sero", "sero_UC", "sero_LC", "risk", "risk_UC", "risk_LC", "pop_risk", "pop_risk_UC", "pop_risk_LC", "population", "age_group", "antigen", "X", "Y")
    
    
    for(j in 1:length(age_groups)){
      temp_sero <-  get(paste0(antigens[i],"_",age_groups[j],"_burden"))$sero
      temp_sero_UC <- get(paste0(antigens[i],"_",age_groups[j],"_burden"))$UC
      temp_sero_LC <- get(paste0(antigens[i],"_",age_groups[j],"_burden"))$LC
      temp_total_risk <- get(paste0(antigens[i],"_",age_groups[j],"_burden"))$total_risk
      temp_risk_UC <- get(paste0(antigens[i],"_",age_groups[j],"_burden"))$risk_UC
      temp_risk_LC <- get(paste0(antigens[i],"_",age_groups[j],"_burden"))$risk_LC
      temp_total <- get(paste0(antigens[i],"_",age_groups[j],"_burden"))$burden
      temp_burden_UC <- get(paste0(antigens[i],"_",age_groups[j],"_burden"))$burden_UC
      temp_burden_LC <- get(paste0(antigens[i],"_",age_groups[j],"_burden"))$burden_LC
      temp_pop <- get(paste0(antigens[i],"_",age_groups[j],"_burden"))$pop
      temp_risk <- NA
      temp_asfr <- get(paste0(antigens[i],"_",age_groups[j],"_burden"))$asfr
      
      
      total_antigens_temp <- as.data.frame(cbind(temp_risk, temp_asfr, temp_sero, temp_sero_UC, temp_sero_LC,temp_total_risk, temp_risk_UC,
                                                 temp_risk_LC,temp_total, temp_burden_UC, temp_burden_LC, temp_pop))
      
      total_antigens_temp$age_group <- age_groups[j]
      
      total_antigens_temp$antigen <- antigens[i]
      
      total_antigens_temp$X <- tet_15_20_burden$X
      
      total_antigens_temp$Y <- tet_15_20_burden$Y
      
      names(total_antigens_temp) <- c("risky_birth", "asfr", "sero", "sero_UC", "sero_LC", "risk", "risk_UC", "risk_LC", "pop_risk", "pop_risk_UC", "pop_risk_LC", "population", "age_group", "antigen", "X", "Y")
      
      total_antigens <- rbind(total_antigens, total_antigens_temp)
      
      
    }
    
    total_antigens <- total_antigens%>%
      filter(!is.na(X))
    
    assign(paste0(antigens[i], "_total_df"), total_antigens)
    
  } else {
    
    total_antigens <- as.data.frame(matrix(ncol = 16))
    names(total_antigens) <- c("risky_birth", "asfr", "sero", "sero_UC", "sero_LC", "risk", "risk_UC", "risk_LC", "pop_risk", "pop_risk_UC", "pop_risk_LC", "population", "age_group", "antigen", "X", "Y")
    
    
    for(j in 1:length(age_groups)){
      temp_sero <-  get(paste0(antigens[i],"_",age_groups[j],"_burden"))$sero
      temp_sero_UC <- get(paste0(antigens[i],"_",age_groups[j],"_burden"))$UC
      temp_sero_LC <- get(paste0(antigens[i],"_",age_groups[j],"_burden"))$LC
      temp_total <- get(paste0(antigens[i],"_",age_groups[j],"_burden"))$burden
      temp_burden_UC <- get(paste0(antigens[i],"_",age_groups[j],"_burden"))$burden_UC
      temp_burden_LC <- get(paste0(antigens[i],"_",age_groups[j],"_burden"))$burden_LC
      temp_pop <- get(paste0(antigens[i],"_",age_groups[j],"_burden"))$pop
      temp_total_risk <- NA
      temp_risk_UC <- NA
      temp_risk_LC <- NA
      temp_risk <- NA
      temp_asfr <- NA
      temp_age_group <- age_groups[j]
      
      total_antigens_temp <- as.data.frame(cbind(temp_risk, temp_asfr, temp_sero, temp_sero_UC, temp_sero_LC,temp_total_risk, temp_risk_UC,
                                                 temp_risk_LC,temp_total, temp_burden_UC, temp_burden_LC, temp_pop))
      
      total_antigens_temp$age_group <- age_groups[j]
      
      total_antigens_temp$antigen <- antigens[i]
      
      total_antigens_temp$X <- tet_15_20_burden$X
      
      total_antigens_temp$Y <- tet_15_20_burden$Y
      
      names(total_antigens_temp) <- c("risky_birth", "asfr", "sero", "sero_UC", "sero_LC", "risk", "risk_UC", "risk_LC", "pop_risk", "pop_risk_UC", "pop_risk_LC", "population", "age_group", "antigen", "X", "Y")
      
      total_antigens <- rbind(total_antigens, total_antigens_temp)
      
    }
    
    total_antigens <- total_antigens%>%
      filter(!is.na(X))
    
    assign(paste0(antigens[i], "_total_df"), total_antigens)
    
  }
  
  
  
}

#Spatial interpolation for State

for(i in 1:length(antigens)){
  
  temp_sum <- get(paste0(antigens[i], "_total_df"))
  
  temp_sum_sf <- st_as_sf(temp_sum, coords = c("X", "Y"))
  
  st_crs(temp_sum_sf) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  temp_sum_adm1 <- st_intersection(temp_sum_sf, nigeria_polygon_ad1)
  
  temp_sum_adm1_summ <- temp_sum_adm1%>%
    group_by(NAME_1, age_group, antigen)%>%
    summarise(weighted.sero = weighted.mean(sero, population, na.rm = TRUE),
              weighted.sero.UC = weighted.mean(sero_UC, population, na.rm = TRUE),
              weighted.sero.LC = weighted.mean(sero_LC, population, na.rm = TRUE),
              weighted.risk = weighted.mean(risk, population, na.rm = TRUE),
              weighted.risk.UC = weighted.mean(risk_UC, population, na.rm = TRUE),
              weighted.risk.LC = weighted.mean(risk_LC, population, na.rm=TRUE),
              pop_risk_adm1 = sum(pop_risk),
              pop_risk_UC_adm1 = sum(pop_risk_UC),
              pop_risk_LC_adm1 = sum(pop_risk_LC),
              total_pop = sum(population))%>%
    as.data.frame()%>%
    dplyr::select(-geometry)
  
  assign(paste0(antigens[i], "_adm_1"), temp_sum_adm1_summ)
  
  
}

#Spatial interpolation for LGA
#Note, due to internal memory limits, spatial interpolation for LGA must be done by age group antigen objects instead of antigen objects
#This could be better optimized if ran on dedicated server

for(i in 1:length(antigens)){
  
  temp_antigens <- as.data.frame(matrix(ncol = 13))
  
  colnames(temp_antigens) <- c("NAME_2", "age_group", "antigen", "weighted.sero", "weighted.sero.UC", "weighted.sero.LC", "weighted.risk",
                               "weighted.risk.UC", "weighted.risk.LC", "pop_risk_adm2", "pop_risk_UC_adm2", "pop_risk_LC_adm2", "total_pop")
  
  for(j in 1:length(age_groups)){
    
    temp_sum <- get(paste0(antigens[i], "_total_df"))
    
    temp_sum_age <- temp_sum%>%
      filter(age_group == age_groups[j])
    
    temp_sum_age_sf <- st_as_sf(temp_sum_age, coords = c("X", "Y"))
    
    st_crs(temp_sum_age_sf) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    temp_sum_age_adm2 <- st_intersection(temp_sum_age_sf, nigeria_adm2)
    
    temp_sum_age_adm2_sum <- temp_sum_age_adm2%>%
      group_by(NAME_2, age_group, antigen)%>%
      summarise(weighted.sero = weighted.mean(sero, population, na.rm = TRUE),
                weighted.sero.UC = weighted.mean(sero_UC, population, na.rm = TRUE),
                weighted.sero.LC = weighted.mean(sero_LC, population, na.rm = TRUE),
                weighted.risk = weighted.mean(risk, population, na.rm = TRUE),
                weighted.risk.UC = weighted.mean(risk_UC, population, na.rm = TRUE),
                weighted.risk.LC = weighted.mean(risk_LC, population, na.rm=TRUE),
                pop_risk_adm2 = sum(pop_risk),
                pop_risk_UC_adm2 = sum(pop_risk_UC),
                pop_risk_LC_adm2 = sum(pop_risk_LC),
                total_pop = sum(population))%>%
      as.data.frame()%>%
      dplyr::select(-geometry)
    
    temp_antigens <- rbind(temp_antigens, temp_sum_age_adm2_sum)
    
  }
  
  assign(paste0(antigens[i], "_adm_2"), temp_antigens)
  
}

final_adm1 <- as.data.frame(matrix(ncol = 13))

colnames(final_adm1) <- c("NAME_1", "age_group", "antigen", "weighted.sero", "weighted.sero.UC", "weighted.sero.LC", "weighted.risk",
                             "weighted.risk.UC", "weighted.risk.LC", "pop_risk_adm1", "pop_risk_UC_adm1", "pop_risk_LC_adm1", "total_pop")


final_adm2 <- as.data.frame(matrix(ncol = 13))

colnames(final_adm2) <- c("NAME_2", "age_group", "antigen", "weighted.sero", "weighted.sero.UC", "weighted.sero.LC", "weighted.risk",
                          "weighted.risk.UC", "weighted.risk.LC", "pop_risk_adm2", "pop_risk_UC_adm2", "pop_risk_LC_adm2", "total_pop")


for (i in 1:length(antigens)){
  
  temp_adm1_anti <- get(paste0(antigens[i], "_adm_1"))
  
  temp_adm2_anti <- get(paste0(antigens[i], "_adm_2"))
  
  final_adm1 <- rbind(final_adm1, temp_adm1_anti)%>%
    filter(!is.na(NAME_1))
  
  final_adm2 <- rbind(final_adm2, temp_adm2_anti)%>%
    filter(!is.na(NAME_2))
}

adm1_code <- nigeria_polygon_ad1%>%
  as.data.frame()%>%
  dplyr::select(GID_1, NAME_1)

adm2_code <- nigeria_adm2%>%
  as.data.frame()%>%
  dplyr::select(GID_2, NAME_2)

final_adm1 <- left_join(adm1_code, final_adm1, by = "NAME_1")
final_adm2 <- left_join(adm2_code, final_adm2, by = "NAME_2")

write.csv(final_adm1, "Output/PopRisk_State.csv")
write.csv(final_adm2, "Output/PopRisk_LGA.csv")



#Repeat for Children, which are only appropriate for Measles and Diptheria

antigens_children <- c("mea", "dip")

ages_children <- c("0_5", "5_10", "10_15")

length_range_children <- c(0, 5, 5, 10, 10, 15)

for (i in 1:length(antigens_children)){
  
  for(j in 1:length(ages_children)){
    
    length_range <- rep(length_range_children[j*2-1]: (length_range_children[j*2]-1)) 
    
    overall_temp_df <- c()
    
    overall_temp_UC_df <- c()
    
    overall_temp_LC_df <- c()
    
    for(k in 1:length(length_range)){
      
      temp_df <- get(paste0(antigens_children[i],"_fit_",length_range[k]))
      
      UC_df <- get(paste0(antigens_children[i],"_UC_", length_range[k]))
      
      LC_df <- get(paste0(antigens_children[i],"_LC_", length_range[k]))
      
      overall_temp_df <- cbind(overall_temp_df, temp_df)
      
      overall_temp_UC_df <- cbind(overall_temp_UC_df, UC_df)
      
      overall_temp_LC_df <- cbind(overall_temp_LC_df, LC_df)
      
      if(k == 5){

          #Measles and Diphtheria
          
          temp_rowmeans <- rowMeans(overall_temp_df)
          
          UC_rowmeans <- rowMeans(overall_temp_UC_df)
          
          LC_rowmeans <- rowMeans(overall_temp_LC_df)
          
          temp_df_XY <- cbind(XY, temp_rowmeans, UC_rowmeans, LC_rowmeans)
          
          temp_df_XY <- temp_df_XY%>%
            mutate(uniqueID = paste(X, Y, by = " "))
          
          if(j == 1){
            
            temp_df_XY_all <- inner_join(temp_df_XY, tiff_0_4_complete, by = "uniqueID")
            
            names(temp_df_XY_all) <- c("X", "Y", "sero", "UC", "LC", "uniqueID", "x", "y", "oldvalue", "pop")
            
            temp_df_XY_all <- temp_df_XY_all%>%
              mutate(burden = (1-sero) * pop)%>%
              mutate(burden_UC = (1-UC) * pop)%>%
              mutate(burden_LC = (1-LC) * pop)
            
            assign(paste0(antigens_children[i], "_", length_range_children[j*2-1], "_", length_range_children[j*2],"_burden"), temp_df_XY_all)
          }else if(j == 2){
            
            temp_df_XY_all <- inner_join(temp_df_XY, tiff_5_9_complete, by = "uniqueID")
            
            names(temp_df_XY_all) <- c("X", "Y", "sero", "UC", "LC", "uniqueID", "x", "y", "oldvalue", "pop")
            
            temp_df_XY_all <- temp_df_XY_all%>%
              mutate(burden = (1-sero) * pop)%>%
              mutate(burden_UC = (1-UC) * pop)%>%
              mutate(burden_LC = (1-LC) * pop)
            
            assign(paste0(antigens_children[i], "_", length_range_children[j*2-1], "_", length_range_children[j*2],"_burden"), temp_df_XY_all)
            
          } else {
            
            temp_df_XY_all <- inner_join(temp_df_XY, tiff_10_14_complete, by = "uniqueID")
            
            names(temp_df_XY_all) <- c("X", "Y", "sero", "UC", "LC", "uniqueID", "x", "y", "oldvalue", "pop")
            
            temp_df_XY_all <- temp_df_XY_all%>%
              mutate(burden = (1-sero) * pop)%>%
              mutate(burden_UC = (1-UC) * pop)%>%
              mutate(burden_LC = (1-LC) * pop)
            
            assign(paste0(antigens_children[i], "_", length_range_children[j*2-1], "_", length_range_children[j*2],"_burden"), temp_df_XY_all)
          }
          
    
  }
  
    }
    }
}

for(i in 1:length(antigens_children)){
  
  final_data_df <- as.data.frame(matrix(ncol = 11))
  names(final_data_df) <- c("sero", "sero_UC", "sero_LC", "pop_risk", "pop_risk_UC", "pop_risk_LC", "population", "age_group", "antigen", "X", "Y")
  
  total_antigens <- as.data.frame(matrix(ncol = 11))
    names(total_antigens) <- c("sero", "sero_UC", "sero_LC", "pop_risk", "pop_risk_UC", "pop_risk_LC", "population", "age_group", "antigen", "X", "Y")

    for(j in 1:length(ages_children)){
      temp_sero <-  get(paste0(antigens_children[i],"_",ages_children [j],"_burden"))$sero
      temp_sero_UC <- get(paste0(antigens_children[i],"_",ages_children [j],"_burden"))$UC
      temp_sero_LC <- get(paste0(antigens_children[i],"_",ages_children [j],"_burden"))$LC
      temp_total <- get(paste0(antigens_children[i],"_",ages_children[j],"_burden"))$burden
      temp_burden_UC <- get(paste0(antigens_children[i],"_",ages_children[j],"_burden"))$burden_UC
      temp_burden_LC <- get(paste0(antigens_children[i],"_",ages_children[j],"_burden"))$burden_LC
      temp_pop <- get(paste0(antigens_children[i],"_",ages_children[j],"_burden"))$pop
      
      total_antigens_temp <- as.data.frame(cbind(temp_sero, temp_sero_UC, temp_sero_LC,temp_total, temp_burden_UC, 
                                                 temp_burden_LC, temp_pop))
      
      total_antigens_temp$age_group <- ages_children[j]
      
      total_antigens_temp$antigen <- antigens_children[i]
      
      total_antigens_temp$X <- mea_0_5_burden$X
      
      total_antigens_temp$Y <- mea_0_5_burden$Y
      
      names(total_antigens_temp) <- c("sero", "sero_UC", "sero_LC", "pop_risk", "pop_risk_UC", 
                                      "pop_risk_LC", "population", "age_group", "antigen", "X", "Y")
      
      total_antigens <- rbind(total_antigens, total_antigens_temp)
      
      
    }
    
    total_antigens <- total_antigens%>%
      filter(!is.na(X))
    
    assign(paste0(antigens_children[i], "_children_total_df"), total_antigens)
  
  
  
}



for(i in 1:length(antigens_children)){
  
  temp_sum <- get(paste0(antigens_children[i], "_children_total_df"))
  
  temp_sum_sf <- st_as_sf(temp_sum, coords = c("X", "Y"))
  
  st_crs(temp_sum_sf) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  temp_sum_adm1 <- st_intersection(temp_sum_sf, nigeria_polygon_ad1)
  
  temp_sum_adm1_summ <- temp_sum_adm1%>%
    group_by(NAME_1, age_group, antigen)%>%
    summarise(weighted.sero = weighted.mean(sero, population, na.rm = TRUE),
              weighted.sero.UC = weighted.mean(sero_UC, population, na.rm = TRUE),
              weighted.sero.LC = weighted.mean(sero_LC, population, na.rm = TRUE),
              pop_risk_adm1 = sum(pop_risk),
              pop_risk_UC_adm1 = sum(pop_risk_UC),
              pop_risk_LC_adm1 = sum(pop_risk_LC),
              total_pop = sum(population))%>%
    as.data.frame()%>%
    dplyr::select(-geometry)
  
  assign(paste0(antigens_children[i], "_children_adm_1"), temp_sum_adm1_summ)
  
  
}


for(i in 1:length(antigens_children)){
  
  temp_antigens <- as.data.frame(matrix(ncol = 10))
  
  colnames(temp_antigens) <- c("NAME_2", "age_group", "antigen", "weighted.sero", "weighted.sero.UC", "weighted.sero.LC", "pop_risk_adm2", "pop_risk_UC_adm2", "pop_risk_LC_adm2", "total_pop")
  
  for(j in 1:length(ages_children)){
    
    temp_sum <- get(paste0(antigens_children[i], "_total_df"))
    
    temp_sum_age <- temp_sum%>%
      filter(age_group == ages_children[j])
    
    temp_sum_age_sf <- st_as_sf(temp_sum_age, coords = c("X", "Y"))
    
    st_crs(temp_sum_age_sf) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    temp_sum_age_adm2 <- st_intersection(temp_sum_age_sf, nigeria_adm2)
    
    temp_sum_age_adm2_sum <- temp_sum_age_adm2%>%
      group_by(NAME_2, age_group, antigen)%>%
      summarise(weighted.sero = weighted.mean(sero, population, na.rm = TRUE),
                weighted.sero.UC = weighted.mean(sero_UC, population, na.rm = TRUE),
                weighted.sero.LC = weighted.mean(sero_LC, population, na.rm = TRUE),
                pop_risk_adm2 = sum(pop_risk),
                pop_risk_UC_adm2 = sum(pop_risk_UC),
                pop_risk_LC_adm2 = sum(pop_risk_LC),
                total_pop = sum(population))%>%
      as.data.frame()%>%
      dplyr::select(-geometry)
    
    temp_antigens <- rbind(temp_antigens, temp_sum_age_adm2_sum)
    
  }
  
  assign(paste0(antigens_children[i], "_children_adm_2"), temp_antigens)
  
}

final_children_adm1 <- as.data.frame(matrix(ncol = 10))

colnames(final_children_adm1) <- c("NAME_1", "age_group", "antigen", "weighted.sero", "weighted.sero.UC", "weighted.sero.LC", "pop_risk_adm1", "pop_risk_UC_adm1", "pop_risk_LC_adm1", "total_pop")


final_children_adm2 <- as.data.frame(matrix(ncol = 10))

colnames(final_children_adm2) <- c("NAME_2", "age_group", "antigen", "weighted.sero", "weighted.sero.UC", "weighted.sero.LC", "pop_risk_adm2", "pop_risk_UC_adm2", "pop_risk_LC_adm2", "total_pop")


for (i in 1:length(antigens_children)){
  
  temp_adm1_anti <- get(paste0(antigens_children[i], "_children_adm_1"))
  
  temp_adm2_anti <- get(paste0(antigens_children[i], "_children_adm_2"))
  
  final_children_adm1 <- rbind(final_children_adm1, temp_adm1_anti)%>%
    filter(!is.na(NAME_1))
  
  final_children_adm2 <- rbind(final_children_adm2, temp_adm2_anti)%>%
    filter(!is.na(NAME_2))
}

final_children_adm1 <- left_join(adm1_code, final_children_adm1, by = "NAME_1")
final_children_adm2 <- left_join(adm2_code, final_children_adm2, by = "NAME_2")

write.csv(final_children_adm1, "Output/Children_State.csv")
write.csv(final_children_adm2, "Output/Children_LGA.csv")
