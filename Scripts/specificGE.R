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
ggsave("Figures/OsciSize .jpeg", width = 10, height = 8, units = "cm")
