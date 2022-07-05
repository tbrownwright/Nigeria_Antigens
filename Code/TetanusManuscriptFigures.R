if(!require("pacman")) install.packages("pacman")
pacman::p_load(mgcv, tidyverse, dplyr, raster, sf, ggplot2, ggrepel, gridExtra, grid)

load("Intermediate/Figure_Packed_Prep.RData")
load("Intermediate/cdc_import_data.RData")
load("Intermediate/WorldPop.RData")
load("Intermediate/shapefiles.RData")
load("Intermediate/asfr.RData")
load("Intermediate/risky_births.RData")

mother_geo_sum_1 <- mother_geo_sum%>%
  dplyr::select(-age_group)

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
  dplyr::select(-LONGNUM, -LATNUM)

total_age_df_sf <- st_as_sf(total_age_df, coords = c("X", "Y"), crs = "+proj=longlat +datum=WGS84 +no_defs")

tet_sero_raw <- total_age_df%>%
  group_by(Age)%>%
  summarize(TetSero = sum(TetSero),
            total_n = sum(ageCount))%>%
  mutate(tet_Sero_pct = TetSero/total_n)

tet <- overall_sero_total_sf_adm1_gather_type%>%
  filter(type_fit == "fit" & Antigen == "tet")

tet_1 <- tet%>%
  mutate(Age=as.numeric(Age))%>%
  dplyr::filter(type_fit=="fit")


tet_1$total_pop <- 0

for(i in 1:nrow(tet_1)){
  
  if(tet_1[i,1] == 0){
    
    adm_name <- as.character(tet_1[i, 2])
    temp_row <- filter(tiff_0_1_sum_adm1, NAME_1 == adm_name)
    tet_1[i,6] <- temp_row$total_pop
    
  } else if(tet_1[i,1] > 0 & tet_1[i,1] < 5){
    
    adm_name <- as.character(tet_1[i, 2])
    temp_row <- filter(tiff_1_5_sum_adm1, NAME_1 == adm_name)
    tet_1[i,6] <- temp_row$total_pop/4
    
  } else if(tet_1[i, 1] > 4 & tet_1[i,1] < 10){
    
    adm_name <- as.character(tet_1[i, 2])
    temp_row <- filter(tiff_5_10_sum_adm1, NAME_1 == adm_name)
    tet_1[i,6] <- temp_row$total_pop/5
    
  } else if(tet_1[i,1] > 9 & tet_1[i,1] < 15){
    
    adm_name <- as.character(tet_1[i, 2])
    temp_row <- filter(tiff_10_15_sum_adm1, NAME_1 == adm_name)
    tet_1[i,6] <- temp_row$total_pop/5
    
  } else if(tet_1[i,1]> 14 & tet_1[i,1] < 20){
    
    adm_name <- as.character(tet_1[i, 2])
    temp_row <- filter(tiff_15_19_adm1, NAME_1 == adm_name)
    tet_1[i,6] <- temp_row$total_pop/5
    
  } else if(tet_1[i, 1]>19 & tet_1[i,1] < 25){
    
    adm_name <- as.character(tet_1[i, 2])
    temp_row <- filter(tiff_20_24_adm1, NAME_1 == adm_name)
    tet_1[i,6] <- temp_row$total_pop/5
    
  }else if(tet_1[i, 1]>24 & tet_1[i,1] < 30){
    
    adm_name <- as.character(tet_1[i, 2])
    temp_row <- filter(tiff_25_29_adm1, NAME_1 == adm_name)
    tet_1[i,6] <- temp_row$total_pop/5
    
  } else if(tet_1[i, 1]>29 & tet_1[i,1] < 35){
    
    adm_name <- as.character(tet_1[i, 2])
    temp_row <- filter(tiff_30_34_adm1, NAME_1 == adm_name)
    tet_1[i,6] <- temp_row$total_pop/5
    
  } else if(tet_1[i, 1]>34 & tet_1[i,1] < 40){
    
    adm_name <- as.character(tet_1[i, 2])
    temp_row <- filter(tiff_35_39_adm1, NAME_1 == adm_name)
    tet_1[i,6] <- temp_row$total_pop/5
    
  } else{
    
    adm_name <- as.character(tet_1[i, 2])
    temp_row <- filter(tiff_40_44_adm1, NAME_1 == adm_name)
    tet_1[i,6] <- temp_row$total_pop/5
    
  }
  
  
}


tet_1 <- tet_1%>%
  group_by(Age)%>%
  summarize(mean_sero = weighted.mean(mean_sero, total_pop))


tet_both <- left_join(tet_sero_raw, tet_1, by = "Age")

#Figure 1

line_colors <- c("Raw" = "red", "Modeled" = "blue")

ggplot(data = subset(tet_both, Age >14), aes(x = Age, y = tet_Sero_pct))+
  geom_point(aes(y = tet_Sero_pct))+
  geom_line(aes(x = Age, y = tet_Sero_pct, color = "Raw"))+
  geom_point(aes(y = mean_sero))+
  geom_line(aes(y = mean_sero, color = "Modeled"))+
  labs(color = "Legend")+
  scale_color_manual(values = line_colors)+
  theme_bw()+
  xlab("Age")+
  ylab("Tetanus Seroprevalence")+
  scale_x_continuous(expand = c(0,0), limits = c(15, NA))+
  scale_y_continuous(limits = c(0.1, 1))+
  ggtitle("Tetanus Seroprevalence 15-44 Year Olds")+
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 16),
        text = element_text(size = 10))

ggsave("Output/NeonatalTetanus/Figure1.png")

#Figure 2

grid.arrange(text.1519, text.2024, text.2529,
             tet_15_20+theme(legend.position = "none"), tet_20_25+theme(legend.position = "none"), tet_25_30+theme(legend.position = "none"),
             text.3034, text.3539, text.4044,
             tet_30_35+theme(legend.position = "none"), tet_35_40+theme(legend.position = "none"), tet_40_45+theme(legend.position = "none"),
             legend_plot,
             layout_matrix = rbind(c(1, 2, 3, NA),
                                   c(4, 5, 6, NA),
                                   c(7, 8, 9, NA),
                                   c(10, 11, 12, 13)),
             top = textGrob("Estimated Proportion of Population with Antibodies to Tetanus", gp = gpar(fontsize = 18)))

#Figure 3

over25_risk_plot <- ggplot()+
  geom_tile(data = risky_over25, aes(x = x, y = y, fill = risk), alpha = 0.9)+
  geom_polygon(data = nigeria_polygon_sp, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.25)+
  scale_fill_gradientn(name = "BaR",
                       colours = terrain.colors(5),
                       limits = c(0.02, 1),
                       na.value = "white")+
  coord_fixed() + 
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
        plot.background=element_blank())

under25_risk_plot <- ggplot()+
  geom_tile(data = risky_under25, aes(x = x, y = y, fill = risk), alpha = 0.9)+
  geom_polygon(data = nigeria_polygon_sp, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.25)+
  scale_fill_gradientn(name = "BaR",
                       colours = terrain.colors(5),
                       limits = c(0.02, 1),
                       na.value = "white")+
  coord_fixed() + 
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
        plot.background=element_blank())

text.under <- text_grob("Under 25", just = c("right", "center"), color = "black", size = 16, face = "bold")
text.over <- text_grob("Over 25", just = c("right", "center"), color = "black", size = 16, face = "bold")

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend_plot_BaR <- g_legend(under25_risk_plot)

grid.arrange(text.under, text.over,
             under25_risk_plot+theme(legend.position = "none"), over25_risk_plot+theme(legend.position = "none"),legend_plot_BaR,
             layout_matrix = rbind(c(1,1,1,2,2,2,NA),
                                   c(3,3, 3,4,4,4, NA),
                                   c(3,3, 3, 4,4, 4, 5),
                                   c(3,3,3,4,4,4,5),
                                   c(3,3,3,4,4,4,5)),
             top = textGrob("Births at risk of neonatal tetanus exposure (BaR)", gp = gpar(fontsize = 18)))

#Figure 4

tetadm_1 <- read.csv("Output/PopRisk_State.csv")%>%
  filter(antigen == "tet")%>%
  group_by(NAME_1)%>%
  summarise(pop_risk_adm1 = sum(pop_risk_adm1),
            pop_risk_UC_adm1 = sum(pop_risk_UC_adm1),
            pop_risk_LC_adm1 = sum(pop_risk_LC_adm1),
            total_pop = sum(total_pop))%>%
  mutate(NAME_1 = fct_reorder(NAME_1, pop_risk_adm1))

tet_burden <- ggplot(tetadm_1, aes(x = NAME_1, y = pop_risk_adm1))+
  geom_point()+
  geom_errorbar(aes(ymin = pop_risk_LC_adm1, ymax = pop_risk_UC_adm1), width = 0.5)+
  scale_y_continuous(breaks = c(0, 30000, 60000, 90000, 120000, 150000, 180000),
                     limits = c(0, 215000))+
  xlab("State")+
  ylab("Tetanus Burden: 15-44 Year Old")+
  coord_flip()+
  theme_bw()+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

tetadm_2 <- read.csv("Output/PopRisk_LGA.csv")%>%
  filter(antigen == "tet")%>%
  group_by(NAME_2)%>%
  summarise(pop_risk_adm2 = sum(pop_risk_adm2),
            pop_risk_UC_adm2 = sum(pop_risk_UC_adm2),
            pop_risk_LC_adm2 = sum(pop_risk_LC_adm2),
            total_pop = sum(total_pop))%>%
  mutate(NAME_2 = fct_reorder(NAME_2, pop_risk_adm2))


tetadm_2_1 <- StatMeasures::decile(tetadm_2$pop_risk_adm2)

tetadm_2$ranked <- tetadm_2_1

tetadm_2$shown <- ifelse(is.na(tetadm_2$ranked), 0, ifelse(tetadm_2$ranked == "10", 1, 0))

tetadm_2_poly <- left_join(nigeria_adm2, as.data.frame(tetadm_2), by = "NAME_2")

tet_top10 <- ggplot()+
  geom_sf(data = tetadm_2_poly, aes(fill = as.factor(shown)), size = 0.3)+
  scale_fill_manual(name = "",
                    values = c("0" = "grey100",
                               "1" = "red"),
                    breaks= c("1"),
                    labels = c("Top 10% Tetanus Burden"),
                    na.value = "white")+
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
        legend.text = element_text(size = 8))

tet_burden_map <- tet_burden + annotation_custom(ggplotGrob(tet_top10), ymin = 80000, ymax = 215000, xmin = 1, xmax = 30)
