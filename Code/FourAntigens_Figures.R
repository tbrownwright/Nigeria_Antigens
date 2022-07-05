if(!require("pacman")) install.packages("pacman")
pacman::p_load(mgcv, tidyverse, dplyr, raster, sf, ggplot2, ggrepel, ggpubr, gridExtra, grid)


#Figure 1

ggplot(data = overall_sero_sf_gather_type_spread_spread, aes(x = Age, y = fit, fill = Antigen))+
  geom_ribbon(aes(ymin = LC, ymax = UC), alpha =0.6)+
  geom_line()+
  geom_point()+
  theme_bw()+
  xlab("Age")+
  ylab("Seroprevalence")+
  scale_fill_discrete(name = "Antigen",
                      labels = c("Diphtheria", "Measles", "Rubella", "Tetanus"))+
  scale_x_continuous(expand = c(0,0), limits = c(0, NA))+
  scale_y_continuous(limits = c(0.1, 1))+
  ggtitle("Seroprevalence All Ages")+
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 12),
        text = element_text(size = 10))

ggsave("Figures/FourAntigens/Figure1.png")

#Figure 2

ggplot(data = overall_sero_sf_adm1_gather_type_spread_spread, aes(x = Age, y = fit, color = Antigen))+
  facet_wrap(~NAME_1, nrow =7)+
  geom_ribbon(aes(ymin = LC, ymax = UC), fill = "lightgrey")+
  geom_line()+
  geom_point()+
  theme_bw()+
  xlab("Age")+
  ylab("Seroprevalence")+
  scale_color_discrete(name = "Antigen",
                       labels = c("Diphtheria", "Measles", "Rubella", "Tetanus"))+
  scale_x_continuous(expand = c(0,0), limits = c(0, NA))+
  scale_y_continuous(limits = c(0.1, 1))+
  ggtitle("Seroprevalence All Ages")+
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 12),
        text = element_text(size = 10))

ggsave("Figures/FourAntigens/Figure2.png")

#Figure 3

grid.arrange(text.t, text.r, text.m, text.d,
             text.1,
             tet_0_1+ theme(legend.position = "none"), tet_1_2 + theme(legend.position = "none"), tet_2_3+ theme(legend.position = "none"), 
             tet_3_6+ theme(legend.position = "none"), tet_6_10+ theme(legend.position = "none"), tet_10_15+ theme(legend.position = "none"),
             text.2,
             rub_0_1+ theme(legend.position = "none"), rub_1_2 + theme(legend.position = "none"), rub_2_3+ theme(legend.position = "none"), 
             rub_3_6+ theme(legend.position = "none"), rub_6_10+ theme(legend.position = "none"), rub_10_15+ theme(legend.position = "none"),
             text.3,
             mea_0_1+ theme(legend.position = "none"), mea_1_2 + theme(legend.position = "none"), mea_2_3+ theme(legend.position = "none"), 
             mea_3_6+ theme(legend.position = "none"), mea_6_10+ theme(legend.position = "none"), mea_10_15+ theme(legend.position = "none"),
             text.35,
             dip_0_1+ theme(legend.position = "none"), dip_1_2 + theme(legend.position = "none"), dip_2_3+ theme(legend.position = "none"), 
             dip_3_6+ theme(legend.position = "none"), dip_6_10+ theme(legend.position = "none"), dip_10_15+ theme(legend.position = "none"),
             text.69,
             text.1014,
             legend_plot,
             layout_matrix = rbind(c(NA, 1, 2, 3, 4, NA),
                                   c(5,  6, 13, 20, 27, NA),
                                   c(12, 7, 14, 21, 28, NA),
                                   c(19, 8, 15, 22, 29, 35),
                                   c(26, 9, 16, 23, 30, NA),
                                   c(33, 10, 17, 24, 31, NA),
                                   c(34, 11, 18, 25, 32, NA)),
             top = textGrob("Estimated Proportion of Population with Antibodies To", gp=gpar(fontsize = 18)))

ggsave(file = "/Output/FourAntigens/Figure3.png", child_sero_maps)

#Figure 4

grid.arrange(text.t, text.r, text.m, text.d,
             text.1519,
             tet_15_20+ theme(legend.position = "none"), tet_20_25 + theme(legend.position = "none"), tet_25_30+ theme(legend.position = "none"), 
             tet_30_35+ theme(legend.position = "none"), tet_35_40+ theme(legend.position = "none"), tet_40_45+ theme(legend.position = "none"),
             text.2024,
             rub_15_20+ theme(legend.position = "none"), rub_20_25 + theme(legend.position = "none"), rub_25_30+ theme(legend.position = "none"), 
             rub_30_35+ theme(legend.position = "none"), rub_35_40+ theme(legend.position = "none"), rub_40_45+ theme(legend.position = "none"),
             text.2529,
             mea_15_20+ theme(legend.position = "none"), mea_20_25 + theme(legend.position = "none"), mea_25_30+ theme(legend.position = "none"), 
             mea_30_35+ theme(legend.position = "none"), mea_35_40+ theme(legend.position = "none"), mea_40_45+ theme(legend.position = "none"),
             text.3034,
             dip_15_20+ theme(legend.position = "none"), dip_20_25 + theme(legend.position = "none"), dip_25_30+ theme(legend.position = "none"), 
             dip_30_35+ theme(legend.position = "none"), dip_35_40+ theme(legend.position = "none"), dip_40_45+ theme(legend.position = "none"),
             text.3539,
             text.4044,
             legend_plot,
             layout_matrix = rbind(c(NA, 1, 2, 3, 4, NA),
                                   c(5,  6, 13, 20, 27, NA),
                                   c(12, 7, 14, 21, 28, NA),
                                   c(19, 8, 15, 22, 29, 35),
                                   c(26, 9, 16, 23, 30, NA),
                                   c(33, 10, 17, 24, 31, NA),
                                   c(34, 11, 18, 25, 32, NA)),
             top = textGrob("Estimated Proportion of Population with Antibodies To", gp=gpar(fontsize = 18)))

ggsave("Output/FourAntigens/Figure4.png", adult_sero_maps)
