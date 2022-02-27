#PLOTS REGARDING SPECIFIC GRAZING EXPERIMENTS CARRIED OUT DURING SEPTEMBER 2020

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


#PHYTOPLANKTON COMMUNITY COMPOSITION
#input file
PhytoComdf <- read.csv("Data/specificGE_Phyto.csv", stringsAsFactors = T)
str(PhytoComdf)

PhytoComdf$Treatment <- ordered(PhytoComdf$Treatment, levels=c("Cnt_t0", "Cnt_t24", "Cala_t24","Cycl_t24"))
PhytoComdf$PhytoSpecies <- ordered(PhytoComdf$PhytoSpecies, 
                                  levels = c("Ciliates", 
                                            "Dolichospermum flos-aquae", "Oscillatoria",
                                            "Rhodomonas minuta", "Cryptomonas",
                                            "Dinobryon sociale",
                                            "Chrysochromulina parva",
                                            "Pyramimonadales",
                                            "Monoraphidium minutum", "Monoraphidium contortum", "Chlamydomonas", "Cosmarium", "Desmodesmus","Kirchneriella obesa", "Raphidocelis", "Closterium acutum", "Oocystis parva", "Oocystis", "Coenococcus planctonicus", "Radiococcus polycoccus",
                                            "Pennate_cylinder", "Pennate_cones","Tabellaria fenestrate","Tabellaria flocculosa","Asterionella formosa", "Aulacoseira alpigena", "Aulacoseira subarctica","Cyclotella"))

PhytoCom_A <- ggplot(PhytoComdf, aes(x = Treatment, y = Mean_cellmL, fill = PhytoSpecies)) +
  geom_bar(stat = "identity", color = "black", alpha = 1, width = 0.5)+
  geom_errorbar(aes(x=Treatment, ymin=Total_cellmL, ymax=Total_cellmL+Total.cellmL_SD), 
                alpha=1, width = 0, inherit.aes = FALSE)+
    scale_fill_manual(name = "Phytoplankton species",
                      values = c("Ciliates" = "pink", 
                               "Dolichospermum flos-aquae" ="cyan4", 
                               "Oscillatoria" ="cyan",
                               "Rhodomonas minuta"="chocolate1", 
                               "Cryptomonas"="chocolate4",
                               "Dinobryon sociale"="gold",
                               "Chrysochromulina parva"= "chartreuse4",
                               "Pyramimonadales"= "chartreuse3",
                               "Monoraphidium minutum"="greenyellow", 
                               "Monoraphidium contortum"="green1", 
                               "Chlamydomonas"="olivedrab1", 
                               "Cosmarium"="palegreen1", 
                               "Desmodesmus"="green2",
                               "Kirchneriella obesa"="olivedrab2", 
                               "Raphidocelis"="palegreen2", 
                               "Closterium acutum"="green3", 
                               "Oocystis parva"="olivedrab3", 
                               "Oocystis"="palegreen3", 
                               "Coenococcus planctonicus"="green4", 
                               "Radiococcus polycoccus"="olivedrab4",
                               "Pennate_cylinder"="grey100", 
                               "Pennate_cones"="grey80",
                               "Tabellaria fenestrate"="grey60",
                               "Tabellaria flocculosa"="grey40",
                               "Asterionella formosa"="grey30", 
                               "Aulacoseira alpigena"="grey20", 
                               "Aulacoseira subarctica"="grey10",
                               "Cyclotella"="grey1"))+
    ylab(~paste("Seston ", "(cells", "·mL"^-1, ")"))+
  theme(text = element_text(size = 10),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")
PhytoCom_A


PhytoCom_B <- ggplot(PhytoComdf, aes(x = Treatment, y = Mean_ngCmL, fill = PhytoSpecies)) +
  geom_bar(stat = "identity", color = "black", alpha = 1, width = 0.5)+
  geom_errorbar(aes(x=Treatment, ymin=Total_ngmL, ymax=Total_ngmL+Total.ngCmL_SD), 
                alpha=1, width = 0, inherit.aes = FALSE)+
  scale_fill_manual(name = "Phytoplankton species",
                    values = c("Ciliates" = "pink", 
                               "Dolichospermum flos-aquae" ="cyan4", 
                               "Oscillatoria" ="cyan",
                               "Rhodomonas minuta"="chocolate1", 
                               "Cryptomonas"="chocolate4",
                               "Dinobryon sociale"="gold",
                               "Chrysochromulina parva"= "chartreuse4",
                               "Pyramimonadales"= "chartreuse3",
                               "Monoraphidium minutum"="greenyellow", 
                               "Monoraphidium contortum"="green1", 
                               "Chlamydomonas"="olivedrab1", 
                               "Cosmarium"="palegreen1", 
                               "Desmodesmus"="green2",
                               "Kirchneriella obesa"="olivedrab2", 
                               "Raphidocelis"="palegreen2", 
                               "Closterium acutum"="green3", 
                               "Oocystis parva"="olivedrab3", 
                               "Oocystis"="palegreen3", 
                               "Coenococcus planctonicus"="green4", 
                               "Radiococcus polycoccus"="olivedrab4",
                               "Pennate_cylinder"="grey100", 
                               "Pennate_cones"="grey80",
                               "Tabellaria fenestrate"="grey60",
                               "Tabellaria flocculosa"="grey40",
                               "Asterionella formosa"="grey30", 
                               "Aulacoseira alpigena"="grey20", 
                               "Aulacoseira subarctica"="grey10",
                               "Cyclotella"="grey1"))+
  ylab(~paste("Seston ", "(ngC", "·mL"^-1, ")"))+
  theme(text = element_text(size = 10),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")
PhytoCom_B


#final plot
specificGE_phytoPlot <- plot_grid(PhytoCom_A,PhytoCom_B,
                          align="hv", axis="tblr", ncol = 2, nrow=1,
                           labels =c("a","b"), legend)

specificGE_phytoPlot

#get and add common legend manually
legend <- get_legend(PhytoCom_B + theme(legend.position = "right", legend.key.height = unit(0.2, 'cm'),legend.key.width = unit(0.5, 'cm'))+
                       guides(fill=guide_legend(ncol=1)))
 
specificGE_phytoPlot_legend <- plot_grid(specificGE_phytoPlot, legend, rel_widths = c(3, 1))

specificGE_phytoPlot_legend 

#save figure as image 
ggsave("Figures/specificGE_phytoPlot_legend.jpeg", width = 20, height = 12, units = "cm")

#OSCILLATORIA CHANGES IN FILAMENT SIZES
#input file
OsciSizedf <- read.csv("Data/specificGE_OscillatoriaSize.csv", stringsAsFactors = T)
str(OsciSizedf)
OsciSizedf$Treatment <- ordered(OsciSizedf$Treatment, levels=c("Cnt_t0", "Cnt_t24", "Cala_t24","Cycl_t24"))


OsciSize <- ggplot(OsciSizedf, aes(x = Treatment, y = Size.um, fill = Treatment)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Cnt_t0" ="aquamarine",
                               "Cnt_t24" = "lightskyblue",
                               "Cala_t24" = "green3",
                               "Cycl_t24" = "green4"))+
  geom_point(pch = 21, position = position_jitterdodge(jitter.width = 0, dodge.width = 0.75))+
  ylab("Filament size (um)")+
  theme(text = element_text(size = 10),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "top")
OsciSize 

#save figure as image 
ggsave("Figures/OsciSize .jpeg", width = 12, height = 8, units = "cm")

#CLEARANCE RATES, INGESTION RATES AND EFFEC SIZES
#remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)
#input file
speGE_metricsdf <- read.csv("Data/specificGE_metrics.csv", stringsAsFactors = T)
str(speGE_metricsdf)

speGE_metricsdf$Treatment <- ordered(speGE_metricsdf$Treatment, levels=c("Cala", "Cycl"))
speGE_metricsdf$Prey <- ordered(speGE_metricsdf$Prey, 
                                   levels = c("Ciliates","Oscillatoria",
                                              "Rhodomonas minuta", "Cryptomonas",
                                             "Monoraphidium minutum", "Monoraphidium contortum", "Kirchneriella obesa", "Closterium acutum", 
                                             "Oocystis", "Tabellaria fenestrate","Asterionella formosa", "Aulacoseira alpigena", "Cyclotella"))

 
CR_N <- ggplot(speGE_metricsdf, aes(x = Prey, y = CR_mL.Nh, fill=Prey, pattern=Treatment)) +
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
                               "Oscillatoria" ="cyan",
                               "Rhodomonas minuta"="chocolate1", 
                               "Cryptomonas"="chocolate4",
                               "Monoraphidium minutum"="greenyellow", 
                               "Monoraphidium contortum"="green1", 
                               "Kirchneriella obesa"="olivedrab3", 
                              "Closterium acutum"="green3", 
                               "Oocystis"="palegreen3", 
                              "Tabellaria fenestrate"="grey60",
                               "Asterionella formosa"="grey50", 
                               "Aulacoseira alpigena"="grey30", 
                               "Cyclotella"="grey20"))+
  scale_pattern_manual(values = c(Cala = "none", Cycl = "stripe")) +
  geom_errorbar(aes(ymin=CR_mL.Nh, ymax=CR_mL.Nh+CR.SD_N), position = position_dodge(0.8), 
                alpha=1, width = 0)+
  ylab(~paste("CR ", "(ml", "·number-grazers"^-1, "·h"^-1, ")"))+
  labs(x = "Prey", pattern = "Treatment") +
  ylim(0,4.5)+
  #guides(pattern = guide_legend(override.aes = list(fill = "white")),
         #fill = guide_legend(override.aes = list(pattern = "none")))+
  theme(text = element_text(size = 10),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.x=element_blank(), axis.text.x=element_blank(),
        legend.position = "none", legend.key.height = unit(0.2, 'cm'))
CR_N
#save figure as image 
ggsave("Figures/CR_N.jpeg", width = 15, height = 10, units = "cm")

CR_M <- ggplot(speGE_metricsdf, aes(x = Prey, y = CR_mL.Mh, fill=Prey, pattern=Treatment)) +
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
                               "Oscillatoria" ="cyan",
                               "Rhodomonas minuta"="chocolate1", 
                               "Cryptomonas"="chocolate4",
                               "Monoraphidium minutum"="greenyellow", 
                               "Monoraphidium contortum"="green1", 
                               "Kirchneriella obesa"="olivedrab3", 
                               "Closterium acutum"="green3", 
                               "Oocystis"="palegreen3", 
                               "Tabellaria fenestrate"="grey60",
                               "Asterionella formosa"="grey50", 
                               "Aulacoseira alpigena"="grey30", 
                               "Cyclotella"="grey20"))+
  scale_pattern_manual(values = c(Cala = "none", Cycl = "stripe")) +
  geom_errorbar(aes(ymin=CR_mL.Mh, ymax=CR_mL.Mh+CR.SD_M), position = position_dodge(0.8), 
                alpha=1, width = 0)+
  ylab(~paste("CR ", "(ml", "·mgDW-grazers"^-1, "·h"^-1, ")"))+
  labs(x = "Prey", pattern = "Treatment") +
  ylim(0,500)+
  #guides(pattern = guide_legend(override.aes = list(fill = "white")),
         #fill = guide_legend(override.aes = list(pattern = "none")))+
  theme(text = element_text(size = 10),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.x=element_blank(), axis.text.x=element_blank(),
        legend.position = "none", legend.key.height = unit(0.2, 'cm'))
CR_M
#save figure as image 
ggsave("Figures/CR_M.jpeg", width = 15, height = 10, units = "cm")

names(speGE_metricsdf)
IR_N_cells <- ggplot(speGE_metricsdf, aes(x = Prey, y = IR_cell.Nh, fill=Prey, pattern=Treatment)) +
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
                               "Oscillatoria" ="cyan",
                               "Rhodomonas minuta"="chocolate1", 
                               "Cryptomonas"="chocolate4",
                               "Monoraphidium minutum"="greenyellow", 
                               "Monoraphidium contortum"="green1", 
                               "Kirchneriella obesa"="olivedrab3", 
                               "Closterium acutum"="green3", 
                               "Oocystis"="palegreen3", 
                               "Tabellaria fenestrate"="grey60",
                               "Asterionella formosa"="grey50", 
                               "Aulacoseira alpigena"="grey30", 
                               "Cyclotella"="grey20"))+
  scale_pattern_manual(values = c(Cala = "none", Cycl = "stripe")) +
  geom_errorbar(aes(ymin=IR_cell.Nh, ymax=IR_cell.Nh+IR.SD_cell.Nh), position = position_dodge(0.8), 
                alpha=1, width = 0)+
  ylab(~paste("IR ", "(cells", "·number-grazers"^-1, "·h"^-1, ")"))+
  labs(x = "Prey", pattern = "Treatment") +
  #ylim(0,4.5)+
  #guides(pattern = guide_legend(override.aes = list(fill = "white")),
  #fill = guide_legend(override.aes = list(pattern = "none")))+
  theme(text = element_text(size = 10),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.x=element_blank(), axis.text.x=element_blank(),
        legend.position = "none", legend.key.height = unit(0.2, 'cm'))
IR_N_cells
#save figure as image 
ggsave("Figures/IR_N_cells.jpeg", width = 15, height = 10, units = "cm")


IR_M_cells <- ggplot(speGE_metricsdf, aes(x = Prey, y = IR_cell.Mh, fill=Prey, pattern=Treatment)) +
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
                               "Oscillatoria" ="cyan",
                               "Rhodomonas minuta"="chocolate1", 
                               "Cryptomonas"="chocolate4",
                               "Monoraphidium minutum"="greenyellow", 
                               "Monoraphidium contortum"="green1", 
                               "Kirchneriella obesa"="olivedrab3", 
                               "Closterium acutum"="green3", 
                               "Oocystis"="palegreen3", 
                               "Tabellaria fenestrate"="grey60",
                               "Asterionella formosa"="grey50", 
                               "Aulacoseira alpigena"="grey30", 
                               "Cyclotella"="grey20"))+
  scale_pattern_manual(values = c(Cala = "none", Cycl = "stripe")) +
  geom_errorbar(aes(ymin=IR_cell.Mh, ymax=IR_cell.Mh+IR.SD_cell.Mh), position = position_dodge(0.8), 
                alpha=1, width = 0)+
  ylab(~paste("IR ", "(cells", "·mgDW-grazers"^-1, "·h"^-1, ")"))+
  labs(x = "Prey", pattern = "Treatment") +
  #ylim(0,500)+
  #guides(pattern = guide_legend(override.aes = list(fill = "white")),
  #fill = guide_legend(override.aes = list(pattern = "none")))+
  theme(text = element_text(size = 10),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.x=element_blank(), axis.text.x=element_blank(),
        legend.position = "none", legend.key.height = unit(0.2, 'cm'))
IR_M_cells
#save figure as image 
ggsave("Figures/IR_M_cells.jpeg", width = 15, height = 10, units = "cm")


IR_N_ngC <- ggplot(speGE_metricsdf, aes(x = Prey, y = IR_ngC.Nh, fill=Prey, pattern=Treatment)) +
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
                               "Oscillatoria" ="cyan",
                               "Rhodomonas minuta"="chocolate1", 
                               "Cryptomonas"="chocolate4",
                               "Monoraphidium minutum"="greenyellow", 
                               "Monoraphidium contortum"="green1", 
                               "Kirchneriella obesa"="olivedrab3", 
                               "Closterium acutum"="green3", 
                               "Oocystis"="palegreen3", 
                               "Tabellaria fenestrate"="grey60",
                               "Asterionella formosa"="grey50", 
                               "Aulacoseira alpigena"="grey30", 
                               "Cyclotella"="grey20"))+
  scale_pattern_manual(values = c(Cala = "none", Cycl = "stripe")) +
  geom_errorbar(aes(ymin=IR_ngC.Nh, ymax=IR_ngC.Nh+IR.SD_ngC.Nh), position = position_dodge(0.8), 
                alpha=1, width = 0)+
  ylab(~paste("IR ", "(ngC", "·number-grazers"^-1, "·h"^-1, ")"))+
  labs(x = "Prey", pattern = "Treatment") +
  #ylim(0,500)+
  #guides(pattern = guide_legend(override.aes = list(fill = "white")),
         #fill = guide_legend(override.aes = list(pattern = "none")))+
  theme(text = element_text(size = 10),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.x=element_blank(), axis.text.x=element_blank(),
        legend.position = "none", legend.key.height = unit(0.2, 'cm'))
IR_N_ngC
#save figure as image 
ggsave("Figures/IR_N_ngC.jpeg", width = 15, height = 10, units = "cm")

IR_M_ngC <- ggplot(speGE_metricsdf, aes(x = Prey, y = IR_ngC.Mh, fill=Prey, pattern=Treatment)) +
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
                               "Oscillatoria" ="cyan",
                               "Rhodomonas minuta"="chocolate1", 
                               "Cryptomonas"="chocolate4",
                               "Monoraphidium minutum"="greenyellow", 
                               "Monoraphidium contortum"="green1", 
                               "Kirchneriella obesa"="olivedrab3", 
                               "Closterium acutum"="green3", 
                               "Oocystis"="palegreen3", 
                               "Tabellaria fenestrate"="grey60",
                               "Asterionella formosa"="grey50", 
                               "Aulacoseira alpigena"="grey30", 
                               "Cyclotella"="grey20"))+
  scale_pattern_manual(values = c(Cala = "none", Cycl = "stripe")) +
  geom_errorbar(aes(ymin=IR_ngC.Mh, ymax=IR_ngC.Mh+IR.SD_ngC.Mh), position = position_dodge(0.8), 
                alpha=1, width = 0)+
  ylab(~paste("IR ", "(ngC", "·mgDW-grazers"^-1, "·h"^-1, ")"))+
  labs(x = "Prey", pattern = "Treatment") +
  #ylim(0,500)+
  #guides(pattern = guide_legend(override.aes = list(fill = "white")),
         #fill = guide_legend(override.aes = list(pattern = "none")))+
  theme(text = element_text(size = 10),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.x=element_blank(), axis.text.x=element_blank(),
        #axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none", legend.key.height = unit(0.2, 'cm'))
IR_M_ngC
#save figure as image 
ggsave("Figures/IR_M_ngC.jpeg", width = 15, height = 10, units = "cm")



#final plot
all <- plot_grid(CR_N, IR_N_cells, IR_N_ngC,
                 CR_M, IR_M_cells, IR_M_ngC,
                      align="hv", axis="tblr", ncol = 3, nrow=2,
                      labels =c("a", "c", "e",
                                "b",  "d", "f")) 

all

#get and add common legend manually
legend <- get_legend(CR_M + theme(legend.position = "right", legend.key.height = unit(0.2, 'cm'),legend.key.width = unit(0.5, 'cm'))+
                       guides(pattern = guide_legend(override.aes = list(fill = "white")),
                              fill = guide_legend(override.aes = list(pattern = "none"), ncol=1)))
CR_IR_legend <- plot_grid(all, legend, rel_widths = c(5.5, 1))

CR_IR_legend 

#save figure as image 
ggsave("Figures/CR_IR_legend.jpeg", width = 26, height = 15, units = "cm")


#Effect sizes as lnR
specificGE_lnR <- ggplot(speGE_metricsdf, aes(x = Prey, y = lnR, fill=Prey, pattern=Treatment)) +
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
                               "Oscillatoria" ="cyan",
                               "Rhodomonas minuta"="chocolate1", 
                               "Cryptomonas"="chocolate4",
                               "Monoraphidium minutum"="greenyellow", 
                               "Monoraphidium contortum"="green1", 
                               "Kirchneriella obesa"="olivedrab3", 
                               "Closterium acutum"="green3", 
                               "Oocystis"="palegreen3", 
                               "Tabellaria fenestrate"="grey60",
                               "Asterionella formosa"="grey50", 
                               "Aulacoseira alpigena"="grey30", 
                               "Cyclotella"="grey20"))+
  scale_pattern_manual(values = c(Cala = "none", Cycl = "stripe")) +
  geom_errorbar(aes(ymin=lnR-var_lnR, ymax=lnR+var_lnR), position = position_dodge(0.8), 
                alpha=1, width = 0)+
  scale_y_reverse()+
  labs(x = "Prey", y = "Effect size (lnR±var)", pattern = "Treatment") +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
  fill = guide_legend(override.aes = list(pattern = "none")))+
  theme(text = element_text(size = 10),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.x=element_blank(), axis.text.x=element_blank(),
        legend.position = "right", legend.key.height = unit(0.2, 'cm'))
specificGE_lnR
#save figure as image 
ggsave("Figures/specificGE_lnR.jpeg", width = 15, height = 10, units = "cm")
