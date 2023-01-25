if(!require("pacman")) install.packages("pacman")
pacman::p_load(mgcv, tidyverse, dplyr, raster, sf, ggplot2, ggrepel, ggpubr, gridExtra, grid)

load(file = "Intermediate/Figure_Packed_Prep.RData")

#Not in Paper, but might be relevant for Appendix, these are DHS and CDC cluster locations
ggplot()+
  geom_polygon(data = nigeria_polygon_sp, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.25)+
  geom_sf(data = spatial_nigeria_Sf)+
  coord_sf(xlim = c(2, 15),
           ylim = c(4, 14))+ 
  ggtitle("DHS Clusters")+
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
        legend.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 24))

ggplot()+
  geom_polygon(data = nigeria_polygon_sp, aes(x=long, y=lat, z=group), colour="gray40", fill=NA, cex=0.25)+
  geom_sf(data = total_age_df_sf)+ 
  ggtitle("CDC Clusters")+
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
        legend.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 24))



#Figure 1

overall_sero_sf_gather_type_spread_spread_1 <- overall_sero_sf_gather_type_spread_spread%>%
  filter(Antigen != "tet")

tet_dot <- overall_sero_sf_gather_type_spread_spread%>%
  filter(Antigen =="tet" & Age == 0)

ggplot(data = overall_sero_sf_gather_type_spread_spread_1, aes(x = Age, y = fit, fill = Antigen))+
  geom_ribbon(aes(ymin = LC, ymax = UC), alpha =0.6)+
  geom_line(aes(color = Antigen))+
  geom_rect(data = tet_dot, aes(xmin = 0, xmax = 1, ymin = LC, ymax = UC), fill = "Black", alpha = 0.7)+
  annotate(geom = "text", x = 4, y = 0.67, label="Tetanus at Birth", color = "black", size = 4.5)+
  theme_bw()+
  xlab("Age")+
  ylab("Seroprevalence")+
  scale_fill_manual(values = c("grey60", "red", "dodgerblue4"))+
  scale_color_manual(name = "Antigen",
                      labels = c("Diphtheria", "Measles", "Rubella"), values = c("grey60", "red", "dodgerblue4"))+
  scale_x_continuous(expand = c(0,0), limits = c(0, NA))+
  guides(size = "none", fill = "none")+
  scale_y_continuous(limits = c(0.1, 1))+
  ggtitle("Seroprevalence All Ages")+
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 16),
        text = element_text(size = 15))

ggsave("Figures/FourAntigens/Figure1A.pdf", device = "pdf", width = 10, units = "in", height = 8)
ggsave("Figures/FourAntigens/Figure1A.eps", device = "eps", width = 10, units = "in", height = 8)

overall_sero_sf_adm1_gather_type_spread_spread_1 <- overall_sero_sf_adm1_gather_type_spread_spread%>%
  dplyr::filter(Antigen == "dip" | Antigen == "mea" | Antigen == "rub")

tet_spread <- overall_sero_sf_adm1_gather_type_spread_spread%>%
  filter(Antigen== "tet")%>%
  filter(Age == 0)%>%
  mutate(size = UC - LC)

ggplot(data = overall_sero_sf_adm1_gather_type_spread_spread_1, aes(x = Age, y = fit, color = Antigen))+
  facet_wrap(~NAME_1, ncol =6)+
  geom_ribbon(aes(ymin = LC, ymax = UC), fill = "lightgrey")+
  geom_line()+
  geom_rect(data = tet_spread, aes(xmin = 0, xmax = 1, ymin = LC, ymax = UC), fill = "White", alpha = 0.7)+
  theme_bw()+
  xlab("Age")+
  ylab("Seroprevalence")+
  scale_color_manual(name = "Antigen",
                     labels = c("Diphtheria", "Measles", "Rubella", "Tetanus"), values = c("red", "green", "blue", "black"))+
  scale_x_continuous(expand = c(0,0), limits = c(0, NA))+
  scale_y_continuous(limits = c(0.1, 1))+
  ggtitle("Seroprevalence All Ages")+
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 14),
        text = element_text(size = 14))

ggsave("Figures/FourAntigens/Figure1B.png", device = "png", width = 16, units = "in", height = 12)
ggsave("Figures/FourAntigens/Figure1B.eps", device = "eps", width = 16, units = "in", height = 12)

#Figure 2
#Maps are generated in FigurePrep.R file and saved in Figures/FourAntigens/Maps


#Figure 3 


ggplot(data = summary_df, aes(x =DTP1_coverage , y = tet_0year, label = NAME_1))+
  ggrepel::geom_text_repel(max.overlaps = Inf, size =5)+
  geom_point(alpha = 0.4, size = 2)+
  theme_bw()+
  xlab("DTP 1 Vaccine Coverage Pct")+
  ylab("Tetanus Seroprevalence")+
  scale_y_continuous(limits = c(0.1, 0.95))+
  scale_x_continuous(limits=c(0, 1))+
  #ggtitle("DTP1 Vaccine Coverage vs. Tetanus Seroprevalence at Birth")+
  theme(axis.title = element_text(size = 18),
        text = element_text(size = 16))

ggsave("Figures/FourAntigens/Figure3A.pdf", device = "pdf", width = 10, units = "in", height = 8)
ggsave("Figures/FourAntigens/Figure3A.eps", device = "eps", width = 10, units = "in", height = 8)


ggplot(data = summary_df, aes(x =dip_0year , y = dip_9year, label = NAME_1))+
  ggrepel::geom_text_repel(max.overlaps = Inf, size = 5)+
  geom_point(alpha = 0.4, size = 2)+
  theme_bw()+
  ylab("Diphtheria Seroprevalence at 9")+
  xlab("Diphtheria Seroprevalence at 0")+
  scale_y_continuous(limits = c(0.7, 0.95))+
  scale_x_continuous(limits=c(0.5, 1))+
  #ggtitle("Diphtheria Seroprevalence at 0 vs 9")+
  theme(axis.title = element_text(size = 18),
        text = element_text(size = 16))

ggsave("Figures/FourAntigens/Figure3B.pdf", device = "pdf", width = 10, units = "in", height = 8)
ggsave("Figures/FourAntigens/Figure3B.eps", device = "eps", width = 10, units = "in", height = 8)

ggplot(data = summary_df, aes(x = DTP1_coverage, y = dip_0year, label = NAME_1))+
  ggrepel::geom_text_repel(max.overlaps = Inf, size = 5)+
  geom_point(alpha = 0.4, size = 2)+
  theme_bw()+
  ylab("Diphtheria Seroprevalence")+
  xlab("DTP1 Vaccine Coverage Pct")+
  scale_y_continuous(limits = c(0.5, 1))+
  scale_x_continuous(limits=c(0, 0.9))+
  #ggtitle("Diphtheria Seroprevalence at 0-1 Year Old vs. DTP1 Vaccine Coverage")+
  theme(axis.title = element_text(size = 18),
        text = element_text(size = 16))

ggsave("Figures/FourAntigens/Figure3C.pdf", device = "pdf", width = 10, units = "in", height = 8)
ggsave("Figures/FourAntigens/Figure3C.eps", device = "eps", width = 10, units = "in", height = 8)

ggplot(data = summary_df, aes(x = tet_0year, y = dip_0year, label = NAME_1))+
  ggrepel::geom_text_repel(max.overlaps = Inf, size =5)+
  geom_point(alpha = 0.4, size = 2)+
  theme_bw()+
  ylab("Diphtheria Seroprevalence")+
  xlab("Tetanus Seroprevalence")+
  scale_y_continuous(limits = c(0.5, 1))+
  scale_x_continuous(limits=c(0.25, 0.95))+
  #ggtitle("Diphtheria Seroprevalence vs. Tetanus at 0-1 Year Old")+
  theme(axis.title = element_text(size = 18),
        text = element_text(size = 16))

ggsave("Figures/FourAntigens/Figure3D.pdf", device = "pdf", width = 10, units = "in", height = 8)
ggsave("Figures/FourAntigens/Figure3D.eps", device = "eps", width = 10, units = "in", height = 8)


#Figure 4

DTP3Mea <- read.csv("Data/CDC_Data/DTP3MeaslesVaccine.csv")

names(DTP3Mea) <- c("NAME_1", "DTP3", "Mea")

DTP3Mea[5,1] <- "Nassarawa"

ggplot(data = summary_df, aes(x = Measles_coverage, y = mea_9year, label = NAME_1))+
  ggrepel::geom_text_repel(max.overlaps = Inf, size =5)+
  geom_point(alpha = 0.4, size = 2)+
  theme_bw()+
  ylab("Measles Seroprevalence")+
  xlab("Measles Vaccine Coverage Pct")+
  scale_y_continuous(limits = c(0.5, 1))+
  scale_x_continuous(limits=c(0, 0.75))+
  #ggtitle("Measles Seroprevalence at 9-10 Year Old vs. Measles Vaccine Coverage")+
  theme(axis.title = element_text(size = 18),
        text = element_text(size = 16))

ggsave("Figures/FourAntigens/Figure4A.pdf", device = "pdf", width = 10, units = "in", height = 8)
ggsave("Figures/FourAntigens/Figure4A.eps", device = "eps", width = 10, units = "in", height = 8)

ggplot(data =DTP3Mea, aes(x = DTP3, y = Mea, label = NAME_1))+
  geom_abline(intercept = 0, size = 1)+
  ggrepel::geom_text_repel(max.overlaps = Inf, size = 5)+
  geom_point(alpha = 0.4, size = 2)+
  theme_bw()+
  xlab("DTP3 Vaccine Coverage Pct")+
  ylab("Measles Vaccine Coverage Pct")+
  scale_y_continuous(limits = c(5, 95))+
  scale_x_continuous(limits=c(5, 95))+
  #ggtitle("DTP3 vs. Measles Vaccine Coverage")+
  theme(axis.title = element_text(size = 18),
        text = element_text(size = 16))

ggsave("Figures/FourAntigens/Figure4B.pdf", device = "pdf", width = 10, units = "in", height = 8)
ggsave("Figures/FourAntigens/Figure4B.eps", device = "eps", width = 10, units = "in", height = 8)


ggplot(data = summary_df, aes(x = rub_0year, y = mea_0year, label = NAME_1))+
  ggrepel::geom_text_repel(max.overlaps = Inf, size =5)+
  geom_point(alpha = 0.4, size = 2)+
  theme_bw()+
  ylab("Measles Seroprevalence")+
  xlab("Rubella Seroprevalence")+
  scale_y_continuous(limits = c(0.5, 0.85))+
  scale_x_continuous(limits=c(0.1, 0.5))+
  #ggtitle("Measles Seroprevalence vs. Rubella at 0-1 Year Old")+
  theme(axis.title = element_text(size = 18),
        text = element_text(size = 16))

ggsave("Figures/FourAntigens/Figure4C.pdf", device = "pdf", width = 10, units = "in", height = 8)
ggsave("Figures/FourAntigens/Figure4C.eps", device = "eps", width = 10, units = "in", height = 8)


# margin <- unit(1, "inch")
# margin_h <- unit(4, "inch")
# 
# text.title <- textGrob("Estimated Proportion of Population with Antibodies To", gp=gpar(fontsize = 18))
# 
# grid.arrange(text.d, text.m, text.r,
#              text.1, text.2,
#              dip_0_1+ theme(legend.position = "none", plot.margin = unit(c(5.5,0,5.5,0), "pt")), dip_1_2 + theme(legend.position = "none", plot.margin = unit(c(5.5,0,5.5,0), "pt")), dip_2_3+ theme(legend.position = "none", plot.margin = unit(c(5.5,3,5.5,3), "pt")), 
#              dip_3_6+ theme(legend.position = "none", plot.margin = unit(c(5.5,0,5.5,0), "pt")), dip_6_10+ theme(legend.position = "none", plot.margin = unit(c(5.5,0,5.5,0), "pt")), dip_10_15+ theme(legend.position = "none", plot.margin = unit(c(5.5,3,5.5,3), "pt")),
#              text.3,
#              mea_0_1+ theme(legend.position = "none", plot.margin = unit(c(5.5,0,5.5,0), "pt")), mea_1_2 + theme(legend.position = "none", plot.margin = unit(c(5.5,0,5.5,0), "pt")), mea_2_3+ theme(legend.position = "none", plot.margin = unit(c(5.5,3,5.5,3), "pt")), 
#              mea_3_6+ theme(legend.position = "none", plot.margin = unit(c(5.5,0,5.5,0), "pt")), mea_6_10+ theme(legend.position = "none", plot.margin = unit(c(5.5,0,5.5,0), "pt")), mea_10_15+ theme(legend.position = "none", plot.margin = unit(c(5.5,3,5.5,3), "pt")),
#              text.35,
#              rub_0_1+ theme(legend.position = "none", plot.margin = unit(c(5.5,0,5.5,0), "pt")), rub_1_2 + theme(legend.position = "none", plot.margin = unit(c(5.5,0,5.5,0), "pt")), rub_2_3+ theme(legend.position = "none", plot.margin = unit(c(5.5,3,5.5,3), "pt")), 
#              rub_3_6+ theme(legend.position = "none", plot.margin = unit(c(5.5,0,5.5,0), "pt")), rub_6_10+ theme(legend.position = "none", plot.margin = unit(c(5.5,0,5.5,0), "pt")), rub_10_15+ theme(legend.position = "none", plot.margin = unit(c(5.5,3,5.5,3), "pt")),
#              text.69,
#              text.1014,
#              legend_plot,
#              text.title,
#              layout_matrix = rbind(c(NA,29, 29, 29, NA),
#                                    c(NA, 1, 2, 3, NA),
#                                    c(4, 6, 13, 20, NA),
#                                    c(5, 7, 14, 21, NA),
#                                    c(12, 8, 15, 22, 28),
#                                    c(19, 9, 16, 23, NA),
#                                    c(26, 10, 17, 24, NA),
#                                    c(27, 11, 18, 25, NA)),
#              #top = textGrob("Estimated Proportion of Population with Antibodies To", gp=gpar(fontsize = 18)),
#              widths = unit.c(grobWidth(text.1) + margin, grobWidth(text.r) + margin, grobWidth(text.m) + margin, grobWidth(text.m) + margin, grobWidth(legend_plot) + margin, unit(1,"null")))
# 
# ggsave(file = "/Output/FourAntigens/Figure3.png", child_sero_maps)
# 
# tet_0_1+theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 18))+ggtitle("Tetanus 0-1 Estimated Proportion of Population with Antibodies")
# 
# #Figure 4
# 
# grid.arrange(text.t, text.r, text.m, text.d,
#              text.1519,
#              tet_15_20+ theme(legend.position = "none"), tet_20_25 + theme(legend.position = "none"), tet_25_30+ theme(legend.position = "none"), 
#              tet_30_35+ theme(legend.position = "none"), tet_35_40+ theme(legend.position = "none"), tet_40_45+ theme(legend.position = "none"),
#              text.2024,
#              rub_15_20+ theme(legend.position = "none"), rub_20_25 + theme(legend.position = "none"), rub_25_30+ theme(legend.position = "none"), 
#              rub_30_35+ theme(legend.position = "none"), rub_35_40+ theme(legend.position = "none"), rub_40_45+ theme(legend.position = "none"),
#              text.2529,
#              mea_15_20+ theme(legend.position = "none"), mea_20_25 + theme(legend.position = "none"), mea_25_30+ theme(legend.position = "none"), 
#              mea_30_35+ theme(legend.position = "none"), mea_35_40+ theme(legend.position = "none"), mea_40_45+ theme(legend.position = "none"),
#              text.3034,
#              dip_15_20+ theme(legend.position = "none"), dip_20_25 + theme(legend.position = "none"), dip_25_30+ theme(legend.position = "none"), 
#              dip_30_35+ theme(legend.position = "none"), dip_35_40+ theme(legend.position = "none"), dip_40_45+ theme(legend.position = "none"),
#              text.3539,
#              text.4044,
#              legend_plot,
#              layout_matrix = rbind(c(NA, 1, 2, 3, 4, NA),
#                                    c(5,  6, 13, 20, 27, NA),
#                                    c(12, 7, 14, 21, 28, NA),
#                                    c(19, 8, 15, 22, 29, 35),
#                                    c(26, 9, 16, 23, 30, NA),
#                                    c(33, 10, 17, 24, 31, NA),
#                                    c(34, 11, 18, 25, 32, NA)),
#              top = textGrob("Estimated Proportion of Population with Antibodies To", gp=gpar(fontsize = 18)))
# 
# ggsave("Output/FourAntigens/Figure4.png", adult_sero_maps)

#Figure 5

summary_df_poly <- left_join(nigeria_polygon_ad1, summary_df, by = "NAME_1")

ggplot()+
  geom_sf(data = summary_df_poly, aes(fill = rub_foi))+
  coord_sf() + 
  scale_fill_gradientn(name = "Rubella FOI",
                       colours = terrain.colors(5),
                       limits = c(0.1, 0.3),
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
        plot.background=element_blank())

ggsave("Figures/FourAntigens/Figure5A.pdf", device = "pdf", width = 8, units = "in", height = 6)
ggsave("Figures/FourAntigens/Figure5A.eps", device = "eps", width = 8, units = "in", height = 6)

ggplot()+
  geom_sf(data = summary_df_poly, aes(fill = dip_foi))+
  coord_sf() + 
  scale_fill_gradientn(name = "Diphtheria FOI",
                       colours = terrain.colors(5),
                       limits = c(0.1, 0.3),
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
        plot.background=element_blank())

ggsave("Figures/FourAntigens/Figure5B.pdf", device = "pdf", width = 8, units = "in", height = 6)
ggsave("Figures/FourAntigens/Figure5B.eps", device = "eps", width = 8, units = "in", height = 6)

#Figure 6

ggplot(data = summary_df, aes(x =dip_foi , y = DTP1_coverage, label = NAME_1))+
  ggrepel::geom_text_repel(max.overlaps = Inf, size = 5)+
  geom_point(alpha = 0.4, size = 2)+
  theme_bw()+
  ylab("DTP 1 Vaccine Coverage Pct")+
  xlab("Diphtheria FOI")+
  scale_y_continuous(limits = c(0.1, 0.9))+
  scale_x_continuous(limits=c(0, 0.5))+
  #ggtitle("DTP1 Vaccine Coverage vs. Diphtheria FOI")+
  theme(axis.title = element_text(size = 18),
        text = element_text(size = 16))

ggsave("Figures/FourAntigens/Figure6A.pdf", device = "pdf", width = 10, units = "in", height = 8)
ggsave("Figures/FourAntigens/Figure6A.eps", device = "eps", width = 10, units = "in", height = 8)

ggplot(data = vv, aes(x = f_diphtheria, y = f_rubella, label = state))+
  ggrepel::geom_text_repel(max.overlaps = Inf, size =5)+
  geom_point(alpha = 0.4, size = 2)+
  theme_bw()+
  ylab("Rubella FOI")+
  xlab("Diphtheria FOI")+
  scale_y_continuous(limits = c(0.1, 0.3))+
  scale_x_continuous(limits=c(0.1, 0.3))+
  #ggtitle("Diphtheria vs. Rubella FOI")+
  theme(axis.title = element_text(size = 18),
        text = element_text(size = 16))

ggsave("Figures/FourAntigens/Figure6B.pdf", device = "pdf", width = 10, units = "in", height = 8)
ggsave("Figures/FourAntigens/Figure6B.eps", device = "eps", width = 10, units = "in", height = 8)

#Correlation Calculations

cor(summary_df$dip_0year, summary_df$tet_0year)

cor(summary_df$dip_foi, summary_df$rub_foi)

cor(summary_df$mea_9year, summary_df$Measles_coverage)

