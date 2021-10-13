#SIA and CNP content data

#PACKAGES REQUIRED
library(ggplot2) #nice plots
library(RColorBrewer) #define number of colors you want from brewer palette
library(cowplot) #nice plots
library(gtable) #align plots
library(gridExtra) #nice plots
library(tidyverse) #manipulate datasets

#BIPLOTS SIA
#input file
SIAdf <- read.csv("Data/SIA_biplot.csv", stringsAsFactors = T)
str(SIAdf)
names(SIAdf)

#plot
Fig_BI_SIA <- ggplot(SIAdf, aes(x=C13mean, y=N15mean)) + 
  geom_point(aes(shape = Sample_type , fill = Sample_name), size=5) + # add the point markers
  scale_shape_manual(name = "Sample_type",
                     values =c("Lake" = 21,
                               "River" = 22,
                               "Terrestrial" = 23,
                               "Zooplankton" = 24))+
  scale_fill_manual(name = "Sample_name",
                    values=c("Calanoid" = "green3", 
                             "Cyclopoid" = "green4",
                             "Diaphanosoma"="turquoise1", 
                             "Daphnia"="blue",
                             "Seston"="azure3",
                             "Sediment"="orange4",
                             "Periphyton"="yellow3",
                             "Leaves"="orange",
                             "Soil"="gray20")) +
  guides(fill = guide_legend(override.aes = list(shape = 21)))+
  geom_errorbar(data = SIAdf, 
               mapping = aes(x = C13mean,
                             ymin = N15mean - N15sd, 
                             ymax = N15mean + N15sd), 
                width = 0, inherit.aes = FALSE) +
 geom_errorbarh(data = SIAdf, 
                 mapping = aes(y = N15mean,
                               xmin = C13mean - C13sd,
                               xmax = C13mean + C13sd),
                 height = 0, inherit.aes = FALSE)+
  
  xlim(-32, -15)+
  xlab("C-13 (towards autochthony)")+
  ylim(-5,15)+
  ylab("N-15 (towards higher trophic levels)")+
geom_text(data=SIAdf,aes(x=C13mean,y=N15mean,label=Sample_ID),size=3,vjust=2, alpha = 0.8, check_overlap = F) +
 theme(text = element_text(size = 10))+
  geom_hline(yintercept = 5, lty = 2) +
  geom_hline(yintercept = 9.5, lty = 2) +
  geom_vline(xintercept = -25, lty = 2) +
  geom_vline(xintercept = -28, lty = 2) +
theme(panel.background = element_rect(fill = "white", colour = "black"),
      legend.position = "right")

#view plot
Fig_BI_SIA
#save figure as image
ggsave("Figures/Fig_BI_SIA.jpeg", width = 20, height = 15, units = "cm")

