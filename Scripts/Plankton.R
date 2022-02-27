#PHYTOPLANKTON & ZOOPLANKTON PLOTS
#RELATIONSHIPS BETWEEN PLANKTON AND NUTRIENT RATIOS
#FECUNDITY PLOTS

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

#PHYTOPLANKTON
#input file
phytodf <- read.csv("Data/Phyto.csv", stringsAsFactors = T)
phytodf$Date <- as.Date(phytodf$Date,"%d/%m/%Y")
str(phytodf)
names(phytodf)
summary(phytodf)

#phyto abundance
phyto_A <- phytodf %>% 
  select(Date, Ciliates.cellmL,  Cyano.cellmL,Crypto.cellmL,Chryso.cellmL,
         Dino.cellmL,Diatoms.cellmL,Chloro.cellmL,Eugle.cellmL,Prasi.cellmL) %>%
  pivot_longer(cols = -("Date"), names_to = "Variable", values_to = "Value")
phyto_A$Variable <- as.factor(phyto_A$Variable)
#rename and reorder phyto abundances names
phyto_A$Variable <- recode_factor(phyto_A$Variable,
                                  Ciliates.cellmL = "Ciliates", 
                                  Cyano.cellmL = "Cyanobacteria",
                                  Crypto.cellmL = "Cryptophyta",
                                  Chryso.cellmL = "Chrysophyta",
                                  Dino.cellmL  = "Dinophyta",
                                  Diatoms.cellmL = "Bacillariophyta",
                                  Chloro.cellmL = "Chlorophyta",
                                  Eugle.cellmL  = "Euglenophyta",
                                  Prasi.cellmL = "Prasinophyta")
phyto_A$Variable  <- ordered(phyto_A$Variable, 
                                levels=c("Ciliates", 
                                         "Cyanobacteria", 
                                         "Chlorophyta",
                                         "Chrysophyta",
                                         "Euglenophyta",
                                         "Dinophyta",
                                         "Prasinophyta",
                                         "Cryptophyta",
                                         "Bacillariophyta"))

phyt_A <- ggplot(phyto_A, aes(x = Date, y = Value, fill = Variable))+ 
  geom_bar(stat = "identity", color = "black", alpha = 1, width = 5)+
  scale_fill_manual(name = "Phytoplankton",
                    values = c("Ciliates"="pink",
                               "Cyanobacteria"="cyan",
                               "Chlorophyta"="greenyellow",
                               "Chrysophyta"="gold3",
                               "Euglenophyta"="orangered",
                               "Dinophyta" = "purple",
                               "Prasinophyta" = "chartreuse3",
                               "Cryptophyta"="chocolate1",
                               "Bacillariophyta" = "grey60"))+
  ylab(~paste("Phytoplankton ", "(cell", "·mL"^-1, ")"))+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank(),
        legend.position = "right", legend.key.height = unit(0.2, 'cm'))+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
phyt_A

#phyto biovolume
phyto_B <- phytodf %>% 
  select(Date, Ciliates.mm3m3,  Cyano.mm3m3,Crypto.mm3m3,Chryso.mm3m3,
         Dino.mm3m3,Diatoms.mm3m3,Chloro.mm3m3,Eugle.mm3m3,Prasi.mm3m3) %>%
  pivot_longer(cols = -("Date"), names_to = "Variable", values_to = "Value")
phyto_B$Variable <- as.factor(phyto_B$Variable)
#rename and reorder phyto abundances names
phyto_B$Variable <- recode_factor(phyto_B$Variable,
                                  Ciliates.mm3m3 = "Ciliates", 
                                  Cyano.mm3m3 = "Cyanobacteria",
                                  Crypto.mm3m3 = "Cryptophyta",
                                  Chryso.mm3m3 = "Chrysophyta",
                                  Dino.mm3m3  = "Dinophyta",
                                  Diatoms.mm3m3 = "Bacillariophyta",
                                  Chloro.mm3m3 = "Chlorophyta",
                                  Eugle.mm3m3  = "Euglenophyta",
                                  Prasi.mm3m3 = "Prasinophyta")
phyto_B$Variable  <- ordered(phyto_B$Variable, 
                             levels=c("Ciliates", 
                                      "Cyanobacteria", 
                                      "Chlorophyta",
                                      "Chrysophyta",
                                      "Euglenophyta",
                                      "Dinophyta",
                                      "Prasinophyta",
                                      "Cryptophyta",
                                      "Bacillariophyta"))

phyt_B <- ggplot(phyto_B, aes(x = Date, y = Value, fill = Variable))+ 
  geom_bar(stat = "identity", color = "black", alpha = 1, width = 5)+
  scale_fill_manual(name = "Phytoplankton",
                    values = c("Ciliates"="pink",
                               "Cyanobacteria"="cyan",
                               "Chlorophyta"="greenyellow",
                               "Chrysophyta"="gold3",
                               "Euglenophyta"="orangered",
                               "Dinophyta" = "purple",
                               "Prasinophyta" = "chartreuse3",
                               "Cryptophyta"="chocolate1",
                               "Bacillariophyta" = "grey60"))+
  ylab(~paste("Phytoplankton ", "(mm"^3 , "·m"^-3, ")"))+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank(),
        legend.position = "right", legend.key.height = unit(0.2, 'cm'))+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
phyt_B


#ZOOPLANKTON
#input file
zoodf <- read.csv("Data/Zoo.csv", stringsAsFactors = T)
zoodf$Date <- as.Date(zoodf$Date,"%d/%m/%Y")
str(zoodf)
names(zoodf)
summary(zoodf)

#rotifers abundance
rotifers_A <- zoodf %>% 
  select(Date, Keratella.indL, Kellicottia.indL,Conochilus.s.indL,Conochilus.c.indL,
         Synchaeta.indL,Asplanchna.indL,Trichocerca.indL,Ploesoma.indL,Polyarthra.indL,
         Filinia.indL, Conochiloides.indL) %>%
  pivot_longer(cols = -("Date"), names_to = "Variable", values_to = "Value")
rotifers_A$Variable <- as.factor(rotifers_A$Variable)
#rename and reorder rotifers abundances names
rotifers_A$Variable <- recode_factor(rotifers_A$Variable,
                                    Keratella.indL = "Keratella", 
                                    Kellicottia.indL = "Kellicottia",
                                    Conochilus.s.indL = "Conochilus single",
                                    Conochilus.c.indL = "Conochilus colony",
                                    Synchaeta.indL = "Synchaeta",
                                    Asplanchna.indL = "Asplanchna",
                                    Trichocerca.indL = "Trichocerca",
                                    Ploesoma.indL  = "Ploesoma",
                                    Polyarthra.indL = "Polyarthra",
                                    Filinia.indL = "Filinia",
                                    Conochiloides.indL = "Conochiloides")
rotifers_A$Variable  <- ordered(rotifers_A$Variable, 
                                levels=c("Conochiloides", 
                                         "Conochilus colony", 
                                         "Conochilus single", 
                                         "Filinia", 
                                         "Asplanchna", 
                                         "Synchaeta", 
                                         "Ploesoma", 
                                         "Polyarthra", 
                                         "Trichocerca",
                                         "Kellicottia",
                                         "Keratella"))
nb.cols <- 11
mycolors <- colorRampPalette(brewer.pal(8, "PuRd"))(nb.cols)
rot_A <- ggplot(rotifers_A, aes(x = Date, y = Value, fill = Variable))+ 
  geom_bar(stat = "identity", color = "black", alpha = 1, width = 5)+
  scale_fill_manual(name = "Rotifers",
                    values = mycolors)+
  ylab(~paste("Rotifers ", "(ind", "·L"^-1, ")"))+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank(),
        legend.position = "right", legend.key.height = unit(0.2, 'cm'))+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
rot_A

#rotifers biomass
rotifers_B <- zoodf %>% 
  select(Date, Keratella.ugDWL, Kellicottia.ugDWL,Conochilus.s.ugDWL,Conochilus.c.ugDWL,
         Synchaeta.ugDWL,Asplanchna.ugDWL,Trichocerca.ugDWL,Ploesoma.ugDWL,Polyarthra.ugDWL,
         Filinia.ugDWL, Conochiloides.ugDWL) %>%
  pivot_longer(cols = -("Date"), names_to = "Variable", values_to = "Value")
rotifers_B$Variable <- as.factor(rotifers_B$Variable)
#rename and reorder rotifers biomass names
rotifers_B$Variable <- recode_factor(rotifers_B$Variable,
                                     Keratella.ugDWL = "Keratella", 
                                     Kellicottia.ugDWL = "Kellicottia",
                                     Conochilus.s.ugDWL = "Conochilus single",
                                     Conochilus.c.ugDWL = "Conochilus colony",
                                     Synchaeta.ugDWL = "Synchaeta",
                                     Asplanchna.ugDWL = "Asplanchna",
                                     Trichocerca.ugDWL = "Trichocerca",
                                     Ploesoma.ugDWL  = "Ploesoma",
                                     Polyarthra.ugDWL = "Polyarthra",
                                     Filinia.ugDWL = "Filinia",
                                     Conochiloides.ugDWL = "Conochiloides")
rotifers_B$Variable  <- ordered(rotifers_B$Variable, 
                                levels=c("Conochiloides", 
                                         "Conochilus colony", 
                                         "Conochilus single", 
                                         "Filinia", 
                                         "Asplanchna", 
                                         "Synchaeta", 
                                         "Ploesoma", 
                                         "Polyarthra", 
                                         "Trichocerca",
                                         "Kellicottia",
                                         "Keratella"))
nb.cols <- 11
mycolors <- colorRampPalette(brewer.pal(8, "PuRd"))(nb.cols)
rot_B <- ggplot(rotifers_B, aes(x = Date, y = Value, fill = Variable))+ 
  geom_bar(stat = "identity", color = "black", alpha = 1, width = 5)+
  scale_fill_manual(name = "Rotifers",
                    values = mycolors)+
  ylab(~paste("Rotifers ", "(ugDW", "·L"^-1, ")"))+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank(),
        legend.position = "right", legend.key.height = unit(0.2, 'cm'))+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
rot_B

#crustaceans abundance
crustaceans_A <- zoodf %>% 
  select(Date, Calanoid.indL, Cyclopoid.indL,Nauplii.indL,Ceriodaphnia.indL,
         Bosmina.indL,Daphnia.indL, Diaphanosoma.indL) %>%
  pivot_longer(cols = -("Date"), names_to = "Variable", values_to = "Value")
crustaceans_A$Variable <- as.factor(crustaceans_A$Variable)
#rename and reorder crustaceans abundances names
crustaceans_A$Variable <- recode_factor(crustaceans_A$Variable,
                                        Calanoid.indL = "Calanoid", 
                                        Cyclopoid.indL = "Cyclopoid",
                                        Nauplii.indL = "Nauplii",
                                        Ceriodaphnia.indL = "Ceriodaphnia",
                                        Bosmina.indL = "Bosmina",
                                        Daphnia.indL = "Daphnia",
                                        Diaphanosoma.indL = "Diaphanosoma")
crustaceans_A$Variable  <- ordered(crustaceans_A$Variable, 
                                levels=c("Diaphanosoma", 
                                         "Daphnia", 
                                         "Ceriodaphnia", 
                                         "Bosmina", 
                                         "Nauplii", 
                                         "Cyclopoid", 
                                         "Calanoid"))
crust_A <- ggplot(crustaceans_A, aes(x = Date, y = Value, fill = Variable))+ 
  geom_bar(stat = "identity", color = "black", alpha = 1, width = 5)+
  scale_fill_manual(name = "Crustaceans",
                    values = c("Diaphanosoma"="turquoise1",
                               "Daphnia"="blue",
                               "Ceriodaphnia"="red",
                               "Bosmina"="grey40",
                               "Nauplii"="yellow",
                              "Cyclopoid" = "green4",
                               "Calanoid" = "green3"))+
  ylab(~paste("Crustaceans ", "(ind", "·L"^-1, ")"))+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank(),
        legend.position = "right",legend.key.height = unit(0.2, 'cm'))+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
crust_A

#crustaceans biomass
crustaceans_B <- zoodf %>% 
  select(Date, Calanoid.ugDWL, Cyclopoid.ugDWL,Nauplii.ugDWL,Ceriodaphnia.ugDWL,
         Bosmina.ugDWL,Daphnia.ugDWL, Diaphanosoma.ugDWL) %>%
  pivot_longer(cols = -("Date"), names_to = "Variable", values_to = "Value")
crustaceans_B$Variable <- as.factor(crustaceans_B$Variable)
#rename and reorder crustaceans abundances names
crustaceans_B$Variable <- recode_factor(crustaceans_B$Variable,
                                        Calanoid.ugDWL = "Calanoid", 
                                        Cyclopoid.ugDWL = "Cyclopoid",
                                        Nauplii.ugDWL = "Nauplii",
                                        Ceriodaphnia.ugDWL = "Ceriodaphnia",
                                        Bosmina.ugDWL = "Bosmina",
                                        Daphnia.ugDWL = "Daphnia",
                                        Diaphanosoma.ugDWL = "Diaphanosoma")
crustaceans_B$Variable  <- ordered(crustaceans_B$Variable, 
                                   levels=c("Diaphanosoma", 
                                            "Daphnia", 
                                            "Ceriodaphnia", 
                                            "Bosmina", 
                                            "Nauplii", 
                                            "Cyclopoid", 
                                            "Calanoid"))
crust_B <- ggplot(crustaceans_B, aes(x = Date, y = Value, fill = Variable))+ 
  geom_bar(stat = "identity", color = "black", alpha = 1, width = 5)+
  scale_fill_manual(name = "Crustaceans",
                    values = c("Diaphanosoma"="turquoise1",
                               "Daphnia"="blue",
                               "Ceriodaphnia"="red",
                               "Bosmina"="grey40",
                               "Nauplii"="yellow",
                               "Cyclopoid" = "green4",
                               "Calanoid" = "green3"))+
  ylab(~paste("Crustaceans ", "(ugDW", "·L"^-1, ")"))+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank(),
        legend.position = "right", legend.key.height = unit(0.2, 'cm'))+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
crust_B

#chaoborus abundance
chaoborus_A <- zoodf %>% 
  select(Date, Chaoborus.indL)

chao_A <- ggplot(chaoborus_A, aes(x = Date, y = Chaoborus.indL))+ 
  geom_bar(stat = "identity", color = "black", fill= "grey40", alpha = 1, width = 5)+
  ylab(~paste("Chaoborus ", "(ind", "·L"^-1, ")"))+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank(),
        legend.position = "right")+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
chao_A



#TOTALS


#FINAL FIGURE ABUNDANCE
planktonAPlot <- plot_grid(phyt_A,
                         rot_A,
                         crust_A,
                         chao_A,
                         align="hv", axis="tblr", ncol = 1, nrow=4,
                         labels =c("a","b", "c","d"))
title <- ggdraw() + 
  draw_label("Lough Feeagh plankton abundances (2018-2019)",
             fontface = 'bold',
             x = 0,
             hjust = 0
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))

PlanktonAbundances <- plot_grid(title, planktonAPlot, ncol=1, rel_heights = c(0.1, 1))

#save figure as image 
ggsave("Figures/PlanktonAbundances.jpeg", width = 18, height = 25, units = "cm")

#FINAL FIGURE BIOMASS
planktonBPlot <- plot_grid(phyt_B,
                           rot_B,
                           crust_B,
                           align="hv", axis="tblr", ncol = 1, nrow=3,
                           labels =c("a","b", "c"))
title <- ggdraw() + 
  draw_label("Lough Feeagh plankton biomasses (2018-2019)",
             fontface = 'bold',
             x = 0,
             hjust = 0
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))

PlanktonBiomasses <- plot_grid(title, planktonBPlot, ncol=1, rel_heights = c(0.1, 1))

#save figure as image 
ggsave("Figures/PlanktonBiomasses.jpeg", width = 18, height = 25, units = "cm")


#FINAL FIGURE TOTALS
#input file
plankdf <- read.csv("Data/TotalPlankton.csv", stringsAsFactors = T)
plankdf$Date <- as.Date(plankdf$Date,"%d/%m/%Y")
str(plankdf)
names(plankdf)
summary(plankdf)

#plankton biomass in C
planktonC_B <- plankdf %>% 
  pivot_longer(cols = -c("Date","CampaignID"), names_to = "Variable", values_to = "Value")
planktonC_B$Variable <- as.factor(planktonC_B$Variable)

#rename and reorder plankton biomass names
planktonC_B$Variable <- recode_factor(planktonC_B$Variable,
                                      Phyto.ugCL = "Phytoplankton", 
                                      Rot.ugCL = "Rotifers",
                                      Crust.ugCL = "Crustaceans")
planktonC_B$Variable  <- ordered(planktonC_B$Variable, 
                             levels=c("Crustaceans", 
                                      "Rotifers", 
                                      "Phytoplankton"))

plank_biomC <- ggplot(planktonC_B, aes(x = Date, y = Value, fill = Variable))+ 
  geom_bar(position="fill", stat = "identity", color = "black", alpha = 1, width = 5)+
  scale_fill_manual(name = "Plankton",
                    values = c("Crustaceans"="gray10",
                               "Rotifers"="deeppink",
                               "Phytoplankton"="gray70"))+
  ylab("Pelagic plankton relative C biomass")+
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank(),
        legend.position = "right", legend.key.height = unit(0.2, 'cm'))+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
plank_biomC

#save figure as image 
ggsave("Figures/plank_biomC.jpeg", width = 15, height = 8, units = "cm")


#RELATIONSHIPS BETWEEN PLANKTON AND NUTRIENT RATIOS
#input file
ESplankdf <- read.csv("Data/ES_plankton.csv", stringsAsFactors = T)
ESplankdf$Date <- as.Date(ESplankdf$Date,"%d/%m/%Y")
str(ESplankdf)
names(ESplankdf)
summary(ESplankdf)

#Diatoms relative biovolume vs Si:TP ratio
diat_SiTP <- ESplankdf %>% 
  select(Date, Diatoms.RelBiov,Si.TP.molar) %>%
  pivot_longer(cols = -("Date"), names_to = "Variable", values_to = "Value")
diat_SiTP$Variable <- as.factor(diat_SiTP$Variable)
diat_SiTP$Variable <- recode_factor(diat_SiTP$Variable,
                                    Diatoms.RelBiov = "Diatoms", 
                                    Si.TP.molar = "Si:TP ratio")


diatoms_ratio <- ggplot(diat_SiTP, aes(x = Date, y = Value, group=Variable))+ 
  geom_line()+
  geom_point(aes(fill=Variable), shape = 21, color="black", size=2)+
  scale_fill_manual(name = "Variables",
                    values=c("Diatoms" = "gray60", 
                             "Si:TP ratio" = "violetred")) +
  ylab("Values")+
  xlab("Year (2018-2019)")+
  scale_x_date(breaks=date_breaks("2 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10),
        legend.position = "right")+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
diatoms_ratio

diatoms_scatt <- ggplot(ESplankdf, aes(x = Si.TP.molar, y = Diatoms.RelBiov))+ 
  geom_point(colour = "black", fill = "black", size=2, shape=21)+ 
  ylab("Diatoms relative biovolume (%)")+
  ylim(0,100)+
  xlab("Si:TP ratio (molar)")+
  xlim(0,150)+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10))+
  geom_smooth(method=lm, se=TRUE, colour="black", fill="black", alpha = 0.3)
  
diatoms_scatt

diatomsPlot <- plot_grid(diatoms_ratio,diatoms_scatt,
                        align="hv", axis="tblr", ncol = 2,
                        labels =c("a","b"))
diatomsPlot

#save figure as image 
ggsave("Figures/diatomsPlot.jpeg", width = 20, height = 10, units = "cm")

#zooplankton community ratio (higer N require taxa vs higher P require taxa) vs seston N:P ratio
sestonNP <- ggplot(ESplankdf, aes(x = Date, y = sestonN.P))+ 
  geom_line()+
  geom_point( shape=21, color="black", fill="grey60", size=3)+
  ylab("Seston N:P")+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank())+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
sestonNP

zoo_ratio <- ggplot(ESplankdf, aes(x = Date, y = ln_ZooNZooPtaxaDW))+ 
  geom_line()+
  geom_point( shape=21, color="black", fill="grey20", size=3)+
  ylab("ln(ZooN:ZooP)")+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank())+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
zoo_ratio 

zoo_sestonNP <- ggplot(ESplankdf, aes(x = sestonN.P, y = ln_ZooNZooPtaxaDW))+ 
  geom_point(colour = "black", fill = "black", size=3, shape=21)+ 
  ylab("ln(ZooN:ZooP)")+
  xlab("Seston N:P")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10))+
  geom_smooth(method=lm, se=TRUE, colour="black", fill="black", alpha = 0.3)
zoo_sestonNP 


zoo_seston <- plot_grid(sestonNP,
                        zoo_ratio,
                        zoo_sestonNP,
                      align="hv", axis="tblr", ncol = 1, nrow=3,
                      labels =c("a","b","c"))

title <- ggdraw() + 
  draw_label("Lough Feeagh 2018-2019 period",
             fontface = 'bold',
             x = 0,
             hjust = 0
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))

zoo_sestonPlot <- plot_grid(title, zoo_seston, ncol=1, rel_heights = c(0.1, 1))


#save figure as image 
ggsave("Figures/zoo_sestonPlot.jpeg", width = 10, height = 20, units = "cm")


#linear regression

model <- lm(ln_ZooNZooPtaxaDW~sestonN.P, data=ESplankdf)
summary(model)

#residuals
layout(matrix(c(1,1,2,3),2,2,byrow=T))
#LAKEw x Residuals Plot
plot(model$resid~ESplankdf$sestonN.P[order(ESplankdf$sestonN.P)],
     main="sestonN.P x Residuals\nfor Simple Regression",
     xlab="sestonN.P", ylab="Residuals")
abline(h=0,lty=2)
#Histogram of Residuals
hist(model$resid, main="Histogram of Residuals",
     ylab="Residuals")
#Q-Q Plot
qqnorm(model$resid)
qqline(model$resid)

#test residuals are normally distributed
install.packages("fBasics")
library(fBasics)
jarqueberaTest(model$resid) #Test residuals for normality
#Null Hypothesis: Skewness and Kurtosis are equal to zero = residuals are normally distributed
#Residuals X-squared: 0.9226 p Value: 0.6305  
#p-value > 0.05 --> accept the null hypothesis

#test residuals are independent
library(lmtest) #dwtest
dwtest(model) #Test for independence of residuals
#Null Hypothesis: there is no correlation among the residuals
#Results: DW = 0.96039, p-value = 0.001374
#p-value < 0.05 --> reject the null hypothesis

#GLMM (to be able to add an autocorrelation structure)
install.packages("nlme")
library(nlme)
library(mgcv)

model1 <- lme(ln_ZooNZooPtaxaDW~1,random=~1|f,data=ESplankdf,correlation=corAR1())








#ZOOPLANKTON FECUNDITY
Cala_fecu <- ggplot(ESplankdf, aes(x = Date, y = Cala.eggsFem))+ 
  geom_bar(stat = "identity", color = "black", fill="green3", alpha = 1, width = 5)+
 ylab(~paste("Calanoid ", "(eggs", "·mature female"^-1, ")"))+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b"), limits= as.Date(c("2018-01-01","2019-03-01"))) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank())+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
Cala_fecu 

Cyc_fecu <- ggplot(ESplankdf, aes(x = Date, y = Cycl.eggsFem))+ 
  geom_bar(stat = "identity", color = "black", fill="green4", alpha = 1, width = 5)+
  ylab(~paste("Cyclopoid ", "(eggs", "·mature female"^-1, ")"))+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b"), limits= as.Date(c("2018-01-01","2019-03-01"))) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank())+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
Cyc_fecu

Diap_fecu <- ggplot(ESplankdf, aes(x = Date, y = Diaph.eggsFem))+ 
  geom_bar(stat = "identity", color = "black", fill="turquoise1", alpha = 1, width = 5)+
  ylab(~paste("Diaphanosoma ", "(eggs", "·mature female"^-1, ")"))+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b"), limits= as.Date(c("2018-01-01","2019-03-01"))) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank())+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
Diap_fecu

Dap_fecu <- ggplot(ESplankdf, aes(x = Date, y = Dap.eggsFem))+ 
  geom_bar(stat = "identity", color = "black", fill="blue", alpha = 1, width = 5)+
  ylab(~paste("Daphnia ", "(eggs", "·mature female"^-1, ")"))+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b"), limits= as.Date(c("2018-01-01","2019-03-01"))) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank())+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
Dap_fecu

Cer_fecu <- ggplot(ESplankdf, aes(x = Date, y = Cerio.eggsFem))+ 
  geom_bar(stat = "identity", color = "black", fill="red", alpha = 1, width = 5)+
  ylab(~paste("Ceriodaphnia ", "(eggs", "·mature female"^-1, ")"))+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b"), limits= as.Date(c("2018-01-01","2019-03-01"))) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank())+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
Cer_fecu

Bos_fecu <- ggplot(ESplankdf, aes(x = Date, y = Bosm.eggsFem))+ 
  geom_bar(stat = "identity", color = "black", fill="grey40", alpha = 1, width = 5)+
  ylab(~paste("Bosmina ", "(eggs", "·mature female"^-1, ")"))+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b"), limits= as.Date(c("2018-01-01","2019-03-01"))) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank())+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
Bos_fecu

#Final fecundity plot
Fecundity <- plot_grid(Cala_fecu,Dap_fecu,
                       Cyc_fecu, Cer_fecu,
                       Diap_fecu, Bos_fecu,
                       align="hv", axis="tblr", ncol = 2, nrow=3,
                        labels =c("a", "d", "b","e", "c","f"))

title <- ggdraw() + 
  draw_label("Crustaceans' fecundity (2018-2019)",
             fontface = 'bold',
             x = 0,
             hjust = 0
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))

FecundityPlot <- plot_grid(title, Fecundity, ncol=1, rel_heights = c(0.1, 1))


#save figure as image 
ggsave("Figures/FecundityPlot.jpeg", width = 20, height = 20, units = "cm")
