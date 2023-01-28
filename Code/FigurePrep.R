if(!require("pacman")) install.packages("pacman")
pacman::p_load(mgcv, tidyverse, dplyr, raster, sf, ggplot2, ggrepel, ggpubr)

load("Intermediate/figures_prep.RData")
load(file = "Intermediate/spatial_DHS.RData")
load(file = "Intermediate/foi_data.RData")

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

tiff_15_19_complete <- st_as_sf(tiff_15_19_complete, coords = c("x", "y"))
st_crs(tiff_15_19_complete) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

tiff_15_19_adm1 <- st_intersection(tiff_15_19_complete, nigeria_polygon_ad1)

tiff_15_19_adm1 <- tiff_15_19_adm1%>%
  group_by(NAME_1)%>%
  summarize(total_pop = sum(nga_f_15_2020))

tiff_20_24_complete <- st_as_sf(tiff_20_24_complete, coords = c("x", "y"))
st_crs(tiff_20_24_complete) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

tiff_20_24_adm1 <- st_intersection(tiff_20_24_complete, nigeria_polygon_ad1)

tiff_20_24_adm1 <- tiff_20_24_adm1%>%
  group_by(NAME_1)%>%
  summarise(total_pop = sum(nga_f_20_2020))

tiff_25_29_complete <- st_as_sf(tiff_25_29_complete, coords = c("x", "y"))
st_crs(tiff_25_29_complete) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

tiff_25_29_adm1 <- st_intersection(tiff_25_29_complete, nigeria_polygon_ad1)

tiff_25_29_adm1 <- tiff_25_29_adm1%>%
  group_by(NAME_1)%>%
  summarize(total_pop = sum(nga_f_25_2020))

tiff_30_34_complete <- st_as_sf(tiff_30_34_complete, coords = c("x", "y"))
st_crs(tiff_30_34_complete) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

tiff_30_34_adm1 <- st_intersection(tiff_30_34_complete, nigeria_polygon_ad1)

tiff_30_34_adm1 <- tiff_30_34_adm1%>%
  group_by(NAME_1)%>%
  summarize(total_pop = sum(nga_f_30_2020))

tiff_35_39_complete <- st_as_sf(tiff_35_39_complete, coords = c("x", "y"))
st_crs(tiff_35_39_complete) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

tiff_35_39_adm1 <- st_intersection(tiff_35_39_complete, nigeria_polygon_ad1)

tiff_35_39_adm1 <- tiff_35_39_adm1%>%
  group_by(NAME_1)%>%
  summarize(total_pop = sum(nga_f_35_2020))

tiff_40_44_complete <- st_as_sf(tiff_40_44_complete, coords = c("x", "y"))
st_crs(tiff_40_44_complete) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

tiff_40_44_adm1 <- st_intersection(tiff_40_44_complete, nigeria_polygon_ad1)

tiff_40_44_adm1 <- tiff_40_44_adm1%>%
  group_by(NAME_1)%>%
  summarize(total_pop = sum(nga_f_40_2020))

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


overall_sero_sf_adm1_gather_type_spread_spread <- overall_sero_total_sf_adm1_gather_type%>%
  spread(type_fit, mean_sero)%>%
  mutate(Age = as.numeric(Age))

overall_sero_total_sf_gather_type <- overall_sero_total_sf_adm1_gather_type%>%
  group_by(Age, Antigen, type_fit)%>%
  summarize(mean_sero = mean(mean_sero))

overall_sero_sf_gather_type_spread_spread <- overall_sero_total_sf_gather_type%>%
  spread(type_fit, mean_sero)%>%
  mutate(Age = as.numeric(Age))

age_ranges <- c(0, 1, 1, 2, 2, 3, 3, 6, 6, 10, 10, 15, 15,20, 20, 25, 25, 30, 30, 35, 35, 40, 40, 45)

XY <- pred.total_mea_0[,1:2]

antigens_names <- c("Tetanus", "Rubella", "Measles", "Diphtheria")

for(i in 1:length(antigens)){
  
  for(j in 1:12){
    
    length_range <- rep(age_ranges[j*2-1]: (age_ranges[j*2]-1)) 
    
    overall_temp_df <- c()
    
    for(k in 1:length(length_range)){
      
      temp_df <- get(paste0(antigens[i],"_fit_",length_range[k]))
      
      overall_temp_df <- cbind(overall_temp_df, temp_df)
      
      if (i == 1){
        
        temp_rowmeans <- rowMeans(overall_temp_df)
        
        temp_df_XY <- cbind(XY, temp_rowmeans)
        
        temp_plot <- ggplot()+
          geom_tile(data = temp_df_XY, aes(x = X, y = Y, fill = temp_rowmeans), alpha = 0.9)+
          geom_polygon(data = nigeria_polygon_sp, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.25)+
          geom_sf(data = nigeria_polygon_ad1, fill = NA)+
          scale_fill_gradientn(name =paste0(antigens_names[i]," Seroprevalence %, \n", age_ranges[j*2-1], "-", age_ranges[j*2], " Age Old"),
                               colours = terrain.colors(5),
                               limits = c(0.15, 1),
                               na.value = "white")+
          coord_sf() + 
          #ggtitle(paste0(age_ranges[j*2-1], "-", age_ranges[j*2] -1, " years"))+
          theme(axis.line=element_blank(),
                axis.text.x=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks=element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                panel.background=element_blank(),
                panel.border=element_blank(),
                panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),
                plot.background=element_blank(),
                plot.title = element_text(hjust = 0.5),
                legend.title = element_text(size = 14),
                legend.text = element_text(size = 10))
        
        assign(paste0(antigens[i],"_",age_ranges[j*2-1], "_", age_ranges[j*2]), temp_plot)
        
        ggsave(paste0("Figures/FourAntigens/Maps/",antigens[i], "_", age_ranges[j*2-1], "_", age_ranges[j*2],".pdf"), device = "pdf", width = 8, height = 6, unit = "in")
        
      } else if(i == 2){
        
        if(j > 6){
          
          temp_rowmeans <- rowMeans(overall_temp_df)
          
          temp_df_XY <- cbind(XY, temp_rowmeans)
          
          temp_plot <- ggplot()+
            geom_tile(data = temp_df_XY, aes(x = X, y = Y, fill = temp_rowmeans), alpha = 0.9)+
            geom_polygon(data = nigeria_polygon_sp, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.25)+
            geom_sf(data = nigeria_polygon_ad1, fill = NA)+
            scale_fill_gradientn(name =paste0(antigens_names[i]," Seroprevalence %, \n", age_ranges[j*2-1], "-", age_ranges[j*2], "Age Old"),
                                 colours = terrain.colors(5),
                                 limits = c(0.15, 1),
                                 na.value = "white")+
            coord_sf() + 
            theme(axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(),
                  plot.background=element_blank(),
                  legend.title = element_text(size = 14),
                  legend.text = element_text(size = 10))
          
          assign(paste0(antigens[i],"_",age_ranges[j*2-1], "_", age_ranges[j*2]), temp_plot)
          
          ggsave(paste0("Figures/FourAntigens/Maps/",antigens[i], "_", age_ranges[j*2-1], "_", age_ranges[j*2],".pdf"), device = "pdf", width = 8, height = 6, unit = "in")
          
          
        } else{
          
          temp_rowmeans <- rowMeans(overall_temp_df)
          
          temp_df_XY <- cbind(XY, temp_rowmeans)
          
          temp_plot <- ggplot()+
            geom_tile(data = temp_df_XY, aes(x = X, y = Y, fill = temp_rowmeans), alpha = 0.9)+
            geom_polygon(data = nigeria_polygon_sp, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.25)+
            geom_sf(data = nigeria_polygon_ad1, fill = NA)+
            scale_fill_gradientn(name =paste0(antigens_names[i]," Seroprevalence %, \n", age_ranges[j*2-1], "-", age_ranges[j*2], "Age Old"),
                                 colours = terrain.colors(5),
                                 limits = c(0.15, 1),
                                 na.value = "white")+
            coord_sf() + 
            theme(axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(),
                  plot.background=element_blank(),
                  legend.title = element_text(size = 14),
                  legend.text = element_text(size = 10))
          
          assign(paste0(antigens[i],"_",age_ranges[j*2-1], "_", age_ranges[j*2]), temp_plot)
          
          ggsave(paste0("Figures/FourAntigens/Maps/",antigens[i], "_", age_ranges[j*2-1], "_", age_ranges[j*2],".pdf"), device = "pdf", width = 8, height = 6, unit = "in")
          
          
        }        
        
        
      } else if(i ==3){
        
        temp_rowmeans <- rowMeans(overall_temp_df)
        
        temp_df_XY <- cbind(XY, temp_rowmeans)
        
        temp_plot <- ggplot()+
          geom_tile(data = temp_df_XY, aes(x = X, y = Y, fill = temp_rowmeans), alpha = 0.9)+
          geom_polygon(data = nigeria_polygon_sp, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.25)+
          geom_sf(data = nigeria_polygon_ad1, fill = NA)+
          scale_fill_gradientn(name =paste0(antigens_names[i]," Seroprevalence %, \n", age_ranges[j*2-1], "-", age_ranges[j*2], " Age Old"),
                               colours = terrain.colors(5),
                               limits = c(0.15, 1),
                               na.value = "white")+
          coord_sf() + 
          theme(axis.line=element_blank(),
                axis.text.x=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks=element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                panel.background=element_blank(),
                panel.border=element_blank(),
                panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),
                plot.background=element_blank(),
                legend.title = element_text(size = 14),
                legend.text = element_text(size = 10))
        
        assign(paste0(antigens[i],"_",age_ranges[j*2-1], "_", age_ranges[j*2]), temp_plot)
        
        ggsave(paste0("Figures/FourAntigens/Maps/",antigens[i], "_", age_ranges[j*2-1], "_", age_ranges[j*2],".pdf"), device = "pdf", width = 8, height = 6, unit = "in")
        
        
      } else{
        
        {
          
          temp_rowmeans <- rowMeans(overall_temp_df)
          
          temp_df_XY <- cbind(XY, temp_rowmeans)
          
          temp_plot <- ggplot()+
            geom_tile(data = temp_df_XY, aes(x = X, y = Y, fill = temp_rowmeans), alpha = 0.9)+
            geom_polygon(data = nigeria_polygon_sp, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.25)+
            geom_sf(data = nigeria_polygon_ad1, fill = NA)+
            scale_fill_gradientn(name =paste0(antigens_names[i]," Seroprevalence %, \n", age_ranges[j*2-1], "-", age_ranges[j*2], " Age Old"),
                                 colours = terrain.colors(5),
                                 limits = c(0.15, 1),
                                 na.value = "white")+
            coord_sf() + 
            theme(axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(),
                  plot.background=element_blank(),
                  legend.title = element_text(size = 14),
                  legend.text = element_text(size = 10))
          
          assign(paste0(antigens[i],"_",age_ranges[j*2-1], "_", age_ranges[j*2]), temp_plot)
          
          ggsave(paste0("Figures/FourAntigens/Maps/",antigens[i], "_", age_ranges[j*2-1], "_", age_ranges[j*2],".pdf"), device = "pdf", width = 8, height = 6, unit = "in")
          
          
        }
        
      }
      
    }
    
    
  }
  
}

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend_plot <- g_legend(tet_15_20)

text.tet <- "Tetanus"
text.rub <- "Rubella"
text.mea <- "Measles"
text.dip <- "Diphtheria"

text.t <- text_grob(text.tet, just = c("center", "center"), color = "black", size = 16, face = "bold")

text.r <- text_grob(text.rub, just = c("center", "center"), color = "black", size = 16, face = "bold")

text.m <- text_grob(text.mea, just = c("center", "center"), color = "black", size = 16, face = "bold")

text.d <- text_grob(text.dip, just = c("center", "center"), color = "black", size = 16, face = "bold")

text.1519 <- text_grob("15-19", just = c("right", "center"), color = "black", size = 16, face = "bold")
text.2024 <- text_grob("20-24", just = c("right", "center"), color = "black", size = 16, face = "bold")
text.2529 <- text_grob("25-29", just = c("right", "center"), color = "black", size = 16, face = "bold")
text.3034 <- text_grob("30-34", just = c("right", "center"), color = "black", size = 16, face = "bold")
text.3539 <- text_grob("35-39", just = c("right", "center"), color = "black", size = 16, face = "bold")
text.4044 <- text_grob("40-44", just = c("right", "center"), color = "black", size = 16, face = "bold")

text.1 <- text_grob("0-1", just = c("right", "center"), color = "black", size = 16, face = "bold")
text.2 <- text_grob("1-2", just = c("right", "center"), color = "black", size = 16, face = "bold")
text.3 <- text_grob("2-3", just = c("right", "center"), color = "black", size = 16, face = "bold")
text.35 <- text_grob("3-6", just = c("right", "center"), color = "black", size = 16, face = "bold")
text.69 <- text_grob("6-10", just = c("right", "center"), color = "black", size = 16, face = "bold")
text.1014 <- text_grob("10-14", just = c("right", "center"), color = "black", size = 16, face = "bold")

text.under <- text_grob("Children", just = c("right", "center"), color = "black", size = 16, face = "bold")
text.over <- text_grob("Adult", just = c("right", "center"), color = "black", size = 16, face = "bold")

summary_df <- as.data.frame(nigeria_polygon_ad1$NAME_1)
colnames(summary_df) <- c("NAME_1")

spatial_dhs_adm1 <- st_intersection(spatial_nigeria_Sf, nigeria_polygon_ad1)

spatial_dhs_adm1_filtered <- as.data.frame(spatial_dhs_adm1)%>%
  dplyr::select(-geometry)%>%
  filter(!is.na(DTP1))%>%
  group_by(NAME_1)%>%
  summarize(total_n = n(),
            DTP_1_sum = sum(DTP1_1, na.rm = TRUE),
            DTP_2_sum = sum(DTP2_1, na.rm = TRUE),
            DTP_3_sum = sum(DTP3_1, na.rm = TRUE),
            Measles_sum = sum(Measles_1, na.rm =TRUE))%>%
  mutate(DTP1_coverage = DTP_1_sum/total_n)%>%
  mutate(DTP2_coverage = DTP_2_sum/total_n)%>%
  mutate(DTP3_coverage = DTP_3_sum/total_n)%>%
  mutate(Measles_coverage = Measles_sum/total_n)

summary_df$DTP1_coverage <- spatial_dhs_adm1_filtered$DTP1_coverage
summary_df$DTP2_coverage <- spatial_dhs_adm1_filtered$DTP2_coverage
summary_df$DTP3_coverage <- spatial_dhs_adm1_filtered$DTP3_coverage
summary_df$Measles_coverage <- spatial_dhs_adm1_filtered$Measles_coverage


coverage_prep <- overall_sero_total_sf_adm1_gather_type

mea_0year_sero <- coverage_prep%>%
  filter(Age == 0)

mea_9year_sero <- coverage_prep%>%
  filter(Age == 9)

mea_4year_sero <- coverage_prep%>%
  filter(Age == 4)

rub_0year_sero <- coverage_prep%>%
  filter(Age == 0)

rub_9year_sero <- coverage_prep%>%
  filter(Age == 9)

rub_4year_sero <- coverage_prep%>%
  filter(Age == 4)


dip_0year_sero <- coverage_prep%>%
  filter(Age == 0 & Antigen == "dip" & type_fit =="fit")

dip_9year_sero <- coverage_prep%>%
  filter(Age == 9 & Antigen == "dip" & type_fit =="fit")

tet_0year_sero <- coverage_prep%>%
  filter(Age == 0 & Antigen == "tet" & type_fit=="fit")

summary_df$mea_0year <- mea_0year_sero$mean_sero
summary_df$mea_9year <- mea_9year_sero$mean_sero
summary_df$mea_4year <- mea_4year_sero$mean_sero
summary_df$rub_0year <- rub_0year_sero$mean_sero
summary_df$rub_9year <- rub_9year_sero$mean_sero
summary_df$rub_4year <- rub_4year_sero$mean_sero
summary_df$dip_0year <- dip_0year_sero$mean_sero
summary_df$dip_9year <- dip_9year_sero$mean_sero
summary_df$tet_0year <- tet_0year_sero$mean_sero
summary_df$rub_foi <- vv$f_rubella
summary_df$dip_foi <- vv$f_diphtheria
save.image(file = "Intermediate/Figure_Packed_Prep.RData")
