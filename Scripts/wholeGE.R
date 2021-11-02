#PLOTS REGARDING WHOLE COMMUNITY GRAZING EXPERIMENTS CARRIED OUT DURING 
#OCTOBER 2019 & FEBRUARY 2020

#PACKAGES REQUIRED
library(ggplot2) #nice plots
library(RColorBrewer) #define number of colors you want from brewer palette
library(cowplot) #nice plots
library(gtable) #align plots
library(gridExtra) #nice plots
library(tidyverse) #manipulate datasets
library(dplyr) #rename factors


library (TimeSeries) #for timeseries
library(lubridate) #for timeseries
library(scales) #labels and axis
library(zoo) #for timeseries

#SESTON COMMUNITY COMPOSITION
#input file
PhytoComdf <- read.csv("Data/wholeGE_Phyto.csv", stringsAsFactors = T)
str(PhytoComdf)

PhytoComdf$Treatment <- ordered(PhytoComdf$Treatment, levels=c("Cnt_t0", "Cnt_t24", "TotG_t24","MicroG_t24"))
PhytoComdf$PhytoTaxa <- ordered(PhytoComdf$PhytoTaxa , 
                                   levels = c("Ciliates", 
                                              "Rhodomonas minuta", "Cryptomonas", "Chroomonas",
                                              "Chromulina","Dinobryon sociale", "Gymnodinium",
                                              "Chrysochromulina parva","Pyramimonadales",
                                              "Monoraphidium minutum", "Monoraphidium contortum", "Chlamydomonas", "Cosmarium", "Kirchneriella obesa", "Raphidocelis", "Closterium acutum", "Closterium", "Oocystis", "Coenococcus planctonicus", "Radiococcus polycoccus",
                                              "Pennate_cylinder","Pennate_cylinderLong","Pennate_cylinderSmall", "Pennate_cones","Pennate_conesFat","Tabellaria fenestrate","Tabellaria flocculosa","Asterionella formosa", "Aulacoseira alpigena","Aulacoseira distans", "Aulacoseira subarctica","Cyclotella"))
PhytoAutumn <- filter(PhytoComdf, Season =="Autumn")
PhytoWinter <- filter(PhytoComdf, Season =="Winter")

PhytoCom_Autumn <- ggplot(PhytoAutumn, aes(x = Treatment, y = Mean_cells.mL, fill = PhytoTaxa )) +
  geom_bar(stat = "identity", color = "black", alpha = 1, width = 0.5)+
  geom_errorbar(aes(x=Treatment, ymin=Total_cells.mL, ymax=Total_cells.mL+Total_cells.mL_SD), 
                alpha=1, width = 0, inherit.aes = FALSE)+
  scale_fill_manual(name = "Phytoplankton species",
                    values = c("Ciliates" = "pink", 
                               "Rhodomonas minuta"="chocolate1", 
                               "Cryptomonas"="chocolate4",
                               "Chroomonas" = "chocolate3",
                               "Chromulina" = "gold1",
                               "Dinobryon sociale"="gold3",
                               "Gymnodinium" = "purple",
                               "Chrysochromulina parva"= "chartreuse4",
                               "Pyramimonadales"= "chartreuse3",
                               "Monoraphidium minutum"="greenyellow", 
                               "Monoraphidium contortum"="green1", 
                               "Chlamydomonas"="olivedrab1", 
                               "Cosmarium"="palegreen1", 
                               "Kirchneriella obesa"="olivedrab2", 
                               "Raphidocelis"="palegreen2", 
                               "Closterium acutum"="green3", 
                               "Closterium"="olivedrab3", 
                               "Oocystis"="palegreen3", 
                               "Coenococcus planctonicus"="green4", 
                               "Radiococcus polycoccus"="olivedrab4",
                               "Pennate_cylinder"="grey100", 
                               "Pennate_cylinderLong" = "grey92",
                               "Pennate_cylinderSmall" = "grey86",
                               "Pennate_cones"="grey80",
                               "Pennate_conesFat" = "grey70",
                               "Tabellaria fenestrate"="grey60",
                               "Tabellaria flocculosa"="grey40",
                               "Asterionella formosa"="grey30", 
                               "Aulacoseira alpigena"="grey20", 
                               "Aulacoseira distans" = "grey12",
                               "Aulacoseira subarctica"="grey8",
                               "Cyclotella"="grey1"))+
  ylab(~paste("Seston ", "(cells", "·mL"^-1, ")"))+
  labs(subtitle="Autumn")+
  theme(text = element_text(size = 10),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")
PhytoCom_Autumn

PhytoCom_Winter <- ggplot(PhytoWinter, aes(x = Treatment, y = Mean_cells.mL, fill = PhytoTaxa )) +
  geom_bar(stat = "identity", color = "black", alpha = 1, width = 0.5)+
  geom_errorbar(aes(x=Treatment, ymin=Total_cells.mL, ymax=Total_cells.mL+Total_cells.mL_SD), 
                alpha=1, width = 0, inherit.aes = FALSE)+
  scale_fill_manual(name = "Phytoplankton species",
                    values = c("Ciliates" = "pink", 
                               "Rhodomonas minuta"="chocolate1", 
                               "Cryptomonas"="chocolate4",
                               "Chroomonas" = "chocolate3",
                               "Chromulina" = "gold1",
                               "Dinobryon sociale"="gold3",
                               "Gymnodinium" = "purple",
                               "Chrysochromulina parva"= "chartreuse4",
                               "Pyramimonadales"= "chartreuse3",
                               "Monoraphidium minutum"="greenyellow", 
                               "Monoraphidium contortum"="green1", 
                               "Chlamydomonas"="olivedrab1", 
                               "Cosmarium"="palegreen1", 
                               "Kirchneriella obesa"="olivedrab2", 
                               "Raphidocelis"="palegreen2", 
                               "Closterium acutum"="green3", 
                               "Closterium"="olivedrab3", 
                               "Oocystis"="palegreen3", 
                               "Coenococcus planctonicus"="green4", 
                               "Radiococcus polycoccus"="olivedrab4",
                               "Pennate_cylinder"="grey100", 
                               "Pennate_cylinderLong" = "grey92",
                               "Pennate_cylinderSmall" = "grey86",
                               "Pennate_cones"="grey80",
                               "Pennate_conesFat" = "grey70",
                               "Tabellaria fenestrate"="grey60",
                               "Tabellaria flocculosa"="grey40",
                               "Asterionella formosa"="grey30", 
                               "Aulacoseira alpigena"="grey20", 
                               "Aulacoseira distans" = "grey12",
                               "Aulacoseira subarctica"="grey8",
                               "Cyclotella"="grey1"))+
  ylab(~paste("Seston ", "(cells", "·mL"^-1, ")"))+
  labs(subtitle="Winter")+
  theme(text = element_text(size = 10),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")
PhytoCom_Winter

#final plot
wholeGE_Phyto <- plot_grid(PhytoCom_Autumn, PhytoCom_Winter,
                         align="hv", axis="tblr", ncol = 2, nrow=1,
                         labels =c("a","b"))

wholeGE_Phyto

#get and add common legend manually
legend <- get_legend(PhytoCom_Autumn + theme(legend.position = "right", legend.key.height = unit(0.2, 'cm'),legend.key.width = unit(0.5, 'cm'))+
                       guides(fill = guide_legend( ncol=1)))
wholeGE_Zoo_legend <- plot_grid(wholeGE_Phyto, legend, rel_widths = c(3, 1))

wholeGE_Zoo_legend  

#save figure as image 
ggsave("Figures/wholeGE_Phyto_legend.jpeg", width = 20, height = 12, units = "cm")



#ZOOPLANKTON COMMUNITY COMPOSITION
#input file
ZooComdf <- read.csv("Data/wholeGE_Zoo.csv", stringsAsFactors = T)
str(ZooComdf)

ZooComdf$ZooTaxa  <- ordered(ZooComdf$ZooTaxa, 
                         levels=c("Diaphanosoma", 
                                  "Daphnia", 
                                  "Ceriodaphnia", 
                                  "Bosmina", 
                                  "Nauplii", 
                                  "Cyclopoid", 
                                  "Calanoid",
                                  "Conochiloides",
                                  "Filinia",
                                  "Ascomorpha",
                                  "Asplanchna", 
                                  "Synchaeta", 
                                  "Ploesoma", 
                                  "Polyarthra", 
                                  "Trichocerca",
                                  "Kellicottia",
                                  "Keratella"))

TotG <- filter(ZooComdf, Treatment =="TotGrazers")
MicroG <- filter(ZooComdf, Treatment =="MicroGrazers")



wholeGE_TotG_abu  <- ggplot(TotG, aes(x = Season, y = Mean_ind.L, fill = ZooTaxa)) +
  geom_bar(stat = "identity", color = "black", alpha = 1, width = 0.5)+
  geom_errorbar(aes(x=Season , ymin=Total.G_ind.L, ymax=Total.G_ind.L+Total.G_ind.L_SD ), 
                alpha=1, width = 0, inherit.aes = FALSE)+
  scale_fill_manual(name = "Zooplankton",
                    values = c("Diaphanosoma"="turquoise1",
                               "Daphnia"="blue",
                               "Ceriodaphnia"="red",
                               "Bosmina"="grey40",
                               "Nauplii"="yellow",
                               "Cyclopoid" = "green4",
                               "Calanoid" = "green3",
                               "Conochiloides"= "gray100",
                               "Filinia"= "lightsalmon",
                               "Ascomorpha"= "rosybrown2",
                               "Asplanchna"="lightpink3", 
                               "Synchaeta"="hotpink1", 
                               "Ploesoma"="hotpink3", 
                               "Polyarthra"= "violetred", 
                               "Trichocerca"="violetred1",
                               "Kellicottia"= "violetred3",
                               "Keratella"="violetred4"))+
  ylab(~paste("Abundance ", "(ind.", "·L"^-1, ")"))+
  labs(subtitle="Total Grazers")+
  theme(text = element_text(size = 10),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.x=element_blank(), 
        legend.position = "none")
wholeGE_TotG_abu

wholeGE_TotG_bio  <- ggplot(TotG, aes(x = Season, y = Mean_ugDW.L, fill = ZooTaxa)) +
  geom_bar(stat = "identity", color = "black", alpha = 1, width = 0.5)+
  geom_errorbar(aes(x=Season , ymin=Total.G_ugDW.L, ymax=Total.G_ugDW.L+Total.G_ugDW.L_SD), 
                alpha=1, width = 0, inherit.aes = FALSE)+
  scale_fill_manual(name = "Zooplankton",
                    values = c("Diaphanosoma"="turquoise1",
                               "Daphnia"="blue",
                               "Ceriodaphnia"="red",
                               "Bosmina"="grey40",
                               "Nauplii"="yellow",
                               "Cyclopoid" = "green4",
                               "Calanoid" = "green3",
                               "Conochiloides"= "gray100",
                               "Filinia"= "lightsalmon",
                               "Ascomorpha"= "rosybrown2",
                               "Asplanchna"="lightpink3", 
                               "Synchaeta"="hotpink1", 
                               "Ploesoma"="hotpink3", 
                               "Polyarthra"= "violetred", 
                               "Trichocerca"="violetred1",
                               "Kellicottia"= "violetred3",
                               "Keratella"="violetred4"))+
  ylab(~paste("Biomass ", "(ugDW.", "·L"^-1, ")"))+
  labs(subtitle="Total Grazers")+
  theme(text = element_text(size = 10),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.x=element_blank(),
        legend.position = "none")
wholeGE_TotG_bio


wholeGE_MicroG_abu  <- ggplot(MicroG, aes(x = Season, y = Mean_ind.L, fill = ZooTaxa)) +
  geom_bar(stat = "identity", color = "black", alpha = 1, width = 0.5)+
  geom_errorbar(aes(x=Season , ymin=Total.G_ind.L, ymax=Total.G_ind.L+Total.G_ind.L_SD ), 
                alpha=1, width = 0, inherit.aes = FALSE)+
  scale_fill_manual(name = "Zooplankton",
                    values = c("Diaphanosoma"="turquoise1",
                               "Daphnia"="blue",
                               "Ceriodaphnia"="red",
                               "Bosmina"="grey40",
                               "Nauplii"="yellow",
                               "Cyclopoid" = "green4",
                               "Calanoid" = "green3",
                               "Conochiloides"= "gray100",
                               "Filinia"= "lightsalmon",
                               "Ascomorpha"= "rosybrown2",
                               "Asplanchna"="lightpink3", 
                               "Synchaeta"="hotpink1", 
                               "Ploesoma"="hotpink3", 
                               "Polyarthra"= "violetred", 
                               "Trichocerca"="violetred1",
                               "Kellicottia"= "violetred3",
                               "Keratella"="violetred4"))+
  ylab(~paste("Abundance ", "(ind.", "·L"^-1, ")"))+
  labs(subtitle="Micro Grazers")+
  theme(text = element_text(size = 10),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.x=element_blank(),
        legend.position = "none")
wholeGE_MicroG_abu

wholeGE_MicroG_bio  <- ggplot(MicroG, aes(x = Season, y = Mean_ugDW.L, fill = ZooTaxa)) +
  geom_bar(stat = "identity", color = "black", alpha = 1, width = 0.5)+
  geom_errorbar(aes(x=Season , ymin=Total.G_ugDW.L, ymax=Total.G_ugDW.L+Total.G_ugDW.L_SD), 
                alpha=1, width = 0, inherit.aes = FALSE)+
  scale_fill_manual(name = "Zooplankton",
                    values = c("Diaphanosoma"="turquoise1",
                               "Daphnia"="blue",
                               "Ceriodaphnia"="red",
                               "Bosmina"="grey40",
                               "Nauplii"="yellow",
                               "Cyclopoid" = "green4",
                               "Calanoid" = "green3",
                               "Conochiloides"= "gray100",
                               "Filinia"= "lightsalmon",
                               "Ascomorpha"= "rosybrown2",
                               "Asplanchna"="lightpink3", 
                               "Synchaeta"="hotpink1", 
                               "Ploesoma"="hotpink3", 
                               "Polyarthra"= "violetred", 
                               "Trichocerca"="violetred1",
                               "Kellicottia"= "violetred3",
                               "Keratella"="violetred4"))+
  ylab(~paste("Biomass ", "(ugDW.", "·L"^-1, ")"))+
  labs(subtitle="Micro Grazers")+
  theme(text = element_text(size = 10),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.x=element_blank(),
        legend.position = "none")
wholeGE_MicroG_bio


#final plot
wholeGE_Zoo <- plot_grid(wholeGE_TotG_abu, wholeGE_MicroG_abu,
                         wholeGE_TotG_bio, wholeGE_MicroG_bio,
                                  align="hv", axis="tblr", ncol = 2, nrow=2,
                                  labels =c("a", "c", "b","d"))

wholeGE_Zoo

#get and add common legend manually
legend <- get_legend(wholeGE_TotG_abu + theme(legend.position = "right", legend.key.height = unit(0.5, 'cm'),legend.key.width = unit(0.5, 'cm'))+
                       guides(fill = guide_legend( ncol=1)))
wholeGE_Zoo_legend <- plot_grid(wholeGE_Zoo, legend, rel_widths = c(4, 1))

wholeGE_Zoo_legend  

#save figure as image 
ggsave("Figures/wholeGE_Zoo_legend.jpeg", width = 20, height = 12, units = "cm")


#Effect sizes as lnR
library(ggpattern)
#input file
wholeGE_metricsdf <- read.csv("Data/wholeGE_metrics.csv", stringsAsFactors = T)
str(wholeGE_metricsdf)

wholeGE_metricsdf$Treatment <- ordered(wholeGE_metricsdf$Treatment, levels=c("TotalGrazers", "MicroGrazers"))
wholeGE_metricsdf$Prey <- ordered(wholeGE_metricsdf$Prey, 
                                levels = c("Ciliates", 
                                           "Rhodomonas minuta", "Cryptomonas","Gymnodinium","Chrysochromulina parva",
                                           "Monoraphidium minutum", "Chlamydomonas", "Kirchneriella obesa", "Closterium acutum", "Radiococcus polycoccus",
                                           "Pennate_cylinder","Pennate_cones","Tabellaria fenestrate","Asterionella formosa", "Aulacoseira alpigena","Cyclotella"))

wholeGE_metricsAutumn <- filter(wholeGE_metricsdf, Season =="Autumn")
wholeGE_metricsWinter <- filter(wholeGE_metricsdf, Season =="Winter")

wholeGE_lnR_autumn <- ggplot(wholeGE_metricsAutumn, aes(x = Prey, y = lnR, fill=Prey, pattern=Treatment)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6)+
  #colours slightly changed from whole phyto community
  scale_fill_manual(name = "Prey",
                    values = c("Ciliates" = "pink", 
                               "Rhodomonas minuta"="chocolate1", 
                               "Cryptomonas"="chocolate4",
                               "Gymnodinium" = "purple",
                               "Chrysochromulina parva"= "chartreuse4",
                               "Monoraphidium minutum"="greenyellow", 
                               "Chlamydomonas"="palegreen", 
                               "Kirchneriella obesa"="olivedrab3", 
                               "Closterium acutum"="green3", 
                               "Radiococcus polycoccus"="olivedrab4",
                               "Pennate_cylinder"="grey100", 
                               "Pennate_cones"="grey80",
                               "Tabellaria fenestrate"="grey60",
                               "Asterionella formosa"="grey30", 
                               "Aulacoseira alpigena"="grey20", 
                               "Cyclotella"="grey1"))+
  scale_pattern_manual(values = c(TotalGrazers = "none", MicroGrazers = "stripe")) +
  geom_errorbar(aes(ymin=lnR-var_lnR, ymax=lnR+var_lnR), position = position_dodge(0.8), 
                alpha=1, width = 0)+
  scale_y_reverse()+
  labs(x = "Prey", y = "Effect size (lnR±var)", pattern = "Treatment") +
  labs(subtitle="Autumn")+
  #guides(pattern = guide_legend(override.aes = list(fill = "white")),
         #fill = guide_legend(override.aes = list(pattern = "none")))+
  theme(text = element_text(size = 10),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.x=element_blank(), axis.text.x=element_blank(),
        legend.position = "none", legend.key.height = unit(0.2, 'cm'))
wholeGE_lnR_autumn

wholeGE_lnR_winter <- ggplot(wholeGE_metricsWinter, aes(x = Prey, y = lnR, fill=Prey, pattern=Treatment)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6)+
  #colours slightly changed from whole phyto community
  scale_fill_manual(name = "Prey",
                    values = c("Ciliates" = "pink", 
                               "Rhodomonas minuta"="chocolate1", 
                               "Cryptomonas"="chocolate4",
                               "Gymnodinium" = "purple",
                               "Chrysochromulina parva"= "chartreuse4",
                               "Monoraphidium minutum"="greenyellow", 
                               "Chlamydomonas"="palegreen", 
                               "Kirchneriella obesa"="olivedrab3", 
                               "Closterium acutum"="green3", 
                               "Radiococcus polycoccus"="olivedrab4",
                               "Pennate_cylinder"="grey100", 
                               "Pennate_cones"="grey80",
                               "Tabellaria fenestrate"="grey60",
                               "Asterionella formosa"="grey30", 
                               "Aulacoseira alpigena"="grey20", 
                               "Cyclotella"="grey1"))+
  scale_pattern_manual(values = c(TotalGrazers = "none", MicroGrazers = "stripe")) +
  geom_errorbar(aes(ymin=lnR-var_lnR, ymax=lnR+var_lnR), position = position_dodge(0.8), 
                alpha=1, width = 0)+
  scale_y_reverse()+
  labs(x = "Prey", y = "Effect size (lnR±var)", pattern = "Treatment") +
  labs(subtitle="Winter")+
  #guides(pattern = guide_legend(override.aes = list(fill = "white")),
         #fill = guide_legend(override.aes = list(pattern = "none")))+
  theme(text = element_text(size = 10),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.x=element_blank(), axis.text.x=element_blank(),
        legend.position = "none", legend.key.height = unit(0.2, 'cm'))
wholeGE_lnR_winter

#final plot
wholeGE_lnR <- plot_grid(wholeGE_lnR_autumn, wholeGE_lnR_winter,
                         align="hv", axis="tblr", ncol = 1, nrow=2,
                         labels =c("a", "b"))

#get and add common legend manually
legend <- get_legend(wholeGE_lnR_winter + theme(legend.position = "right", legend.key.height = unit(0.2, 'cm'),legend.key.width = unit(0.5, 'cm'))+
                       guides(pattern = guide_legend(override.aes = list(fill = "white")),
                              fill = guide_legend(override.aes = list(pattern = "none"), ncol=1)))
wholeGE_lnR_legend <- plot_grid(wholeGE_lnR, legend, rel_widths = c(2.5, 1))


#save figure as image 
ggsave("Figures/wholeGE_lnR_legend.jpeg", width = 15, height = 10, units = "cm")
