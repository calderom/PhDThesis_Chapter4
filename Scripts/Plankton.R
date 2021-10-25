#PHYTOPLANKTON & ZOOPLANKTON PLOTS

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
                               "Nauplii"="yellow",
                               "Bosmina"="grey40",
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
                               "Nauplii"="yellow",
                               "Bosmina"="grey40",
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
