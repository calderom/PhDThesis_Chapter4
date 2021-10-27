#CNP data

#PACKAGES REQUIRED
library(ggplot2) #nice plots
library(RColorBrewer) #define number of colors you want from brewer palette
library(cowplot) #nice plots
library(gtable) #align plots
library(gridExtra) #nice plots
library(tidyverse) #manipulate datasets


library (TimeSeries) #for timeseries
library(lubridate) #for timeseries
library(scales) #labels and axis
library(zoo) #for timeseries


#..................................................................................#


#BIPLOTS CNP content %w/w 
#input file
CNPdf <- read.csv("Data/CNP_biplot.csv", stringsAsFactors = T)
str(CNPdf)
names(CNPdf)

#plots
Fig_CN_A <- ggplot(CNPdf, aes(x=Cper_m, y=Nper_m)) + 
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
  geom_errorbar(data = CNPdf, 
                mapping = aes(x = Cper_m,
                              ymin = Nper_m - Nper_sd, 
                              ymax = Nper_m + Nper_sd), 
                width = 0, inherit.aes = FALSE) +
  geom_errorbarh(data = CNPdf, 
                 mapping = aes(y = Nper_m,
                               xmin = Cper_m - Cper_sd,
                               xmax = Cper_m + Cper_sd),
                 height = 0, inherit.aes = FALSE)+
  
  xlim(0, 55)+
  xlab("C (%)")+
  ylim(0,10)+
  ylab("N (%)")+
  geom_text(data=CNPdf,aes(x=Cper_m,y=Nper_m,label=Sample_ID),size=3,vjust=2, alpha = 0.8, check_overlap = F) +
  theme(text = element_text(size = 10))+
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.position = "none")

#view plots
Fig_CN_A


Fig_CN_B <- ggplot(CNPdf, aes(x=Cper_m, y=Pper_m)) + 
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
  geom_errorbar(data = CNPdf, 
                mapping = aes(x = Cper_m,
                              ymin = Pper_m - Pper_sd, 
                              ymax = Pper_m + Pper_sd), 
                width = 0, inherit.aes = FALSE) +
  geom_errorbarh(data = CNPdf, 
                 mapping = aes(y = Pper_m,
                               xmin = Cper_m - Cper_sd,
                               xmax = Cper_m + Cper_sd),
                 height = 0, inherit.aes = FALSE)+
  
  xlim(0, 55)+
  xlab("C (%)")+
  ylim(0,1)+
  ylab("P (%)")+
  geom_text(data=CNPdf,aes(x=Cper_m,y=Pper_m,label=Sample_ID),size=3,vjust=2, alpha = 0.8, check_overlap = F) +
  theme(text = element_text(size = 10))+
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.position = "right", legend.key.size = unit(0.2, 'cm'))

#view plots
Fig_CN_B



Fig_CN_C <- ggplot(CNPdf, aes(x=Nper_m, y=Pper_m)) + 
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
  geom_errorbar(data = CNPdf, 
                mapping = aes(x = Nper_m,
                              ymin = Pper_m - Pper_sd, 
                              ymax = Pper_m + Pper_sd), 
                width = 0, inherit.aes = FALSE) +
  geom_errorbarh(data = CNPdf, 
                 mapping = aes(y = Pper_m,
                               xmin = Nper_m - Nper_sd,
                               xmax = Nper_m + Nper_sd),
                 height = 0, inherit.aes = FALSE)+
  
  xlim(0, 10)+
  xlab("N (%)")+
  ylim(0,1)+
  ylab("P (%)")+
  geom_text(data=CNPdf,aes(x=Nper_m,y=Pper_m,label=Sample_ID),size=3,vjust=2, alpha = 0.8, check_overlap = F) +
  theme(text = element_text(size = 10))+
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.position = "none")

#view plots
Fig_CN_C



#final plot 
Fig_CNP.biplots <- plot_grid(Fig_CN_A,Fig_CN_B, Fig_CN_C, 
                            align="hv", axis="tblr",
                            ncol = 1, nrow = 3,  
                            labels =c("a","b","c"))
#view plot
Fig_CNP.biplots

#save figure as image --> only 1 legend!
ggsave("Figures/Fig_CNP.biplots.jpeg", width = 15, height = 26, units = "cm")

#..................................................................................#


#ONLY ZOOPLANKTON P to add Bosmina and Ceriodaphnia from winter samples
CNPdf_zoo <-CNPdf[CNPdf$Sample_type %in% "Zooplankton",]

CNPdf_zoo$Sample_ID  <- ordered(CNPdf_zoo$Sample_ID , levels=c("cala_a", "cyc_a", "dap_a", "diap_a",
                                                               "cala_w", "cyc_w", "dap_w", "cerio_w", "bos_w"))

ZooP <- 
  ggplot(CNPdf_zoo, aes(x = Sample_ID, y =  Pper_m, fill = Sample_name))+ 
  geom_bar(stat = "identity", color = "black",  alpha = 1)+ 
  geom_errorbar(aes(x=Sample_ID, ymin=Pper_m-Pper_sd, ymax=Pper_m+Pper_sd), 
                alpha=0.9, width = 0, inherit.aes = FALSE)+
  scale_fill_manual(name = "Sample_name",
                    values=c("Calanoid" = "green3", 
                             "Cyclopoid" = "green4",
                             "Diaphanosoma"="turquoise1", 
                             "Daphnia"="blue",
                             "Ceriodaphnia"="red",
                             "Bosmina"="gray40"))+
  scale_x_discrete(labels=c("cala_a", "cyc_a", "dap_a", "diap_a",
                            "cala_w", "cyc_w", "dap_w", "cerio_w", "bos_w")) +
  ylab("P (%)") +
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "right")+
  geom_vline(xintercept = 4.5, lty = 1)

#view plot
ZooP

#save figure 
ggsave("Figures/ZooP.jpeg", width = 15, height = 10, units = "cm")

#..................................................................................#

#BIPLOTS CNP ratios (molar)
#input file
CNPdf <- read.csv("Data/CNP_biplot.csv", stringsAsFactors = T)
str(CNPdf)
names(CNPdf)

#plots
Fig_CPvsNP <- ggplot(CNPdf, aes(x=lnCP, y=lnNP)) + 
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
  xlab("ln(C:P) molar")+
  ylab("ln(N:P) molar")+
  geom_text(data=CNPdf,aes(x=lnCP,y=lnNP,label=Sample_ID),size=3,vjust=2, alpha = 0.8, check_overlap = F) +
  theme(text = element_text(size = 10))+
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.position = "none", legend.key.size = unit(0.2, 'cm'))

#view plots
Fig_CPvsNP

#save figure 
ggsave("Figures/Fig_CPvsNP.jpeg", width = 20, height = 15, units = "cm")

Fig_CNvsNP <- ggplot(CNPdf, aes(x=lnCN, y=lnNP)) + 
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
  xlab("ln(C:N) molar")+
  ylab("ln(N:P) molar")+
  geom_text(data=CNPdf,aes(x=lnCN,y=lnNP,label=Sample_ID),size=3,vjust=2, alpha = 0.8, check_overlap = F) +
  theme(text = element_text(size = 10))+
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.position = "right", legend.key.size = unit(0.2, 'cm'))

#view plots
Fig_CNvsNP

#save figure 
ggsave("Figures/Fig_CNvsNP.jpeg", width = 20, height = 15, units = "cm")


#final plot 
Fig_ratios.biplots <- plot_grid(Fig_CPvsNP,Fig_CNvsNP, 
                             align="hv", axis="tblr",
                             ncol = 2, nrow = 1,  
                             labels =c("a","b"))
#view plot
Fig_ratios.biplots

#save figure as image --> only 1 legend!
ggsave("Figures/Fig_ratios.biplots.jpeg", width = 28, height = 10, units = "cm")

#..................................................................................#

#2018 fortnightly seston and zooplankton CNP content, ratios and imbalances
#input file
stoich <- read.csv("Data/ES_seston&zoo.csv", stringsAsFactors = T)
str(stoich)
stoich$Date <- as.Date(stoich$Date,"%d/%m/%Y")
names(stoich)

#pivot long stoich dataframe to plot
C_content <- stoich %>% 
  select(Date, sestonC, zooC) %>%
  pivot_longer(cols = -("Date"), names_to = "Variable", values_to = "Value")

N_content <- stoich %>% 
  select(Date, sestonN, zooN) %>%
  pivot_longer(cols = -("Date"), names_to = "Variable", values_to = "Value")

P_content <- stoich %>% 
  select(Date, sestonP, zooP) %>%
  pivot_longer(cols = -("Date"), names_to = "Variable", values_to = "Value")

CN_ratio <- stoich %>% 
  select(Date, sestonC.N, zooC.N) %>%
  pivot_longer(cols = -("Date"), names_to = "Variable", values_to = "Value")

CP_ratio <- stoich %>% 
  select(Date, sestonC.P, zooC.P) %>%
  pivot_longer(cols = -("Date"), names_to = "Variable", values_to = "Value")

NP_ratio <- stoich %>% 
  select(Date, sestonN.P, zooN.P) %>%
  pivot_longer(cols = -("Date"), names_to = "Variable", values_to = "Value")


#NUTRIENT CONTENT
a <- ggplot(C_content, aes(x = Date, y = Value, group=Variable))+ 
  geom_line()+
  geom_point(aes(fill=Variable), shape = 21, color="black", size=2)+
  scale_fill_manual(name = "Fractions",
                    values=c("sestonC" = "orange", 
                             "zooC" = "orange4")) +
  ylab("C content (uM)")+
  scale_x_date(breaks=date_breaks("2 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank(),
        legend.position = "right")+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
a
b <- ggplot(N_content, aes(x = Date, y = Value, group=Variable))+ 
  geom_line()+
  geom_point(aes(fill=Variable), shape = 21, color="black", size=2)+
  scale_fill_manual(name = "Fractions",
                    values=c("sestonN" = "green", 
                             "zooN" = "green4")) +
  ylab("N content (uM)")+
  scale_x_date(breaks=date_breaks("2 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank(),
        legend.position = "right")+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
b
c <- ggplot(P_content, aes(x = Date, y = Value, group=Variable))+ 
  geom_line()+
  geom_point(aes(fill=Variable), shape = 21, color="black", size=2)+
  scale_fill_manual(name = "Fractions",
                    values=c("sestonP" = "deeppink", 
                             "zooP" = "deeppink4")) +
  ylab("P content (uM)")+
  scale_x_date(breaks=date_breaks("2 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank(),
        legend.position = "right")+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
c
d <- ggplot(stoich, aes(x = sestonC, y = zooC))+ 
  geom_point(colour = "black", fill = "orange2", size=3, shape=21)+ 
  ylab("Zooplankton C (uM)")+
  ylim(0,20)+
  xlab("Seston C (uM)")+
  xlim(0,60)+
  geom_text(data=stoich,aes(x=sestonC,y=zooC,label=Date),size=2,vjust=2, alpha = 0.8, check_overlap = T) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10))
d
e <- ggplot(stoich, aes(x = sestonN, y = zooN))+ 
  geom_point(colour = "black", fill = "green2", size=3, shape=21)+ 
  ylab("Zooplankton N (uM)")+
  ylim(0,2)+
  xlab("Seston N (uM)")+
  xlim(0,12)+
  geom_text(data=stoich,aes(x=sestonN,y=zooN,label=Date),size=2,vjust=2, alpha = 0.8, check_overlap = T) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10))
e
f <- ggplot(stoich, aes(x = sestonP, y = zooP))+ 
  geom_point(colour = "black", fill = "deeppink2", size=3, shape=21)+ 
  ylab("Zooplankton P (uM)")+
  ylim(0,0.06)+
  xlab("Seston P (uM)")+
  xlim(0,0.25)+
  geom_text(data=stoich,aes(x=sestonP,y=zooP,label=Date),size=2,vjust=2, alpha = 0.8, check_overlap = T) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10))
f

contentPlot <- plot_grid(a,b,c,
                         d,e,f,
                         align="hv", axis="tblr", ncol = 3,
                         labels =c("a","b", "c","d","e", "f"))
title <- ggdraw() + 
  draw_label("Nutrient content (2018-2019)",
             fontface = 'bold',
             x = 0,
             hjust = 0
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))

NutrientContent <- plot_grid(title, contentPlot, ncol=1, rel_heights = c(0.1, 1))

#save figure as image 
ggsave("Figures/NutrientContent.jpeg", width = 28, height = 15, units = "cm")


#NUTRIENT RATIOS
a <- ggplot(CN_ratio, aes(x = Date, y = Value, group=Variable))+ 
  geom_line()+
  geom_point(aes(fill=Variable), shape = 21, color="black", size=2)+
  scale_fill_manual(name = "Fractions",
                    values=c("sestonC.N" = "grey60", 
                             "zooC.N" = "grey20")) +
  ylab("C:N ratio (molar)")+
  scale_x_date(breaks=date_breaks("2 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank(),
        legend.position = "right")+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
a
b <- ggplot(CP_ratio, aes(x = Date, y = Value, group=Variable))+ 
  geom_line()+
  geom_point(aes(fill=Variable), shape = 21, color="black", size=2)+
  scale_fill_manual(name = "Fractions",
                    values=c("sestonC.P" = "grey60", 
                             "zooC.P" = "grey20")) +
  ylab("C:P ratio (molar)")+
  scale_x_date(breaks=date_breaks("2 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank(),
        legend.position = "right")+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
b
c <- ggplot(NP_ratio, aes(x = Date, y = Value, group=Variable))+ 
  geom_line()+
  geom_point(aes(fill=Variable), shape = 21, color="black", size=2)+
  scale_fill_manual(name = "Fractions",
                    values=c("sestonN.P" = "grey60", 
                             "zooN.P" = "grey20")) +
  ylab("N:P ratio (molar)")+
  scale_x_date(breaks=date_breaks("2 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank(),
        legend.position = "right")+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
c
d <- ggplot(stoich, aes(x = sestonC.N, y = zooC.N))+ 
  geom_point(colour = "black", fill = "grey40", size=3, shape=21)+ 
  ylab("Zooplankton C:N (molar)")+
  ylim(0,20)+
  xlab("Seston C:N (molar)")+
  xlim(-5,80)+
  geom_text(data=stoich,aes(x=sestonC.N,y=zooC.N,label=Date),size=2,vjust=2, alpha = 0.8, check_overlap = T) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10))+
  geom_abline(intercept = 0, slope = 1, size = 1, linetype=2)
d
e <- ggplot(stoich, aes(x = sestonC.P, y = zooC.P))+ 
  geom_point(colour = "black", fill = "grey40", size=3, shape=21)+ 
  ylab("Zooplankton C:P (molar)")+
  ylim(0,400)+
  xlab("Seston C:P (molar)")+
  xlim(0,550)+
  geom_text(data=stoich,aes(x=sestonC.P,y=zooC.P,label=Date),size=2,vjust=2, alpha = 0.8, check_overlap = T) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10))+
  geom_abline(intercept = 0, slope = 1, size = 1, linetype=2)
e
f <- ggplot(stoich, aes(x = sestonN.P, y = zooN.P))+ 
  geom_point(colour = "black", fill = "grey40", size=3, shape=21)+ 
  ylab("Zooplankton N:P (molar)")+
  ylim(0,60)+
  xlab("Seston N:P (molar)")+
  xlim(0,150)+
  geom_text(data=stoich,aes(x=sestonN.P,y=zooN.P,label=Date),size=2,vjust=2, alpha = 0.8, check_overlap = T) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10))+
  geom_abline(intercept = 0, slope = 1, size = 1, linetype=2)
f


ratiosPlot <- plot_grid(a,b,c,
                        d,e,f,
                        align="hv", axis="tblr", ncol = 3,
                        labels =c("a","b", "c","d","e", "f"))
title <- ggdraw() + 
  draw_label("Nutrient ratios (2018-2019)",
             fontface = 'bold',
             x = 0,
             hjust = 0
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))

NutrientRatios <- plot_grid(title, ratiosPlot, ncol=1, rel_heights = c(0.1, 1))

#save figure as image 
ggsave("Figures/NutrientRatios.jpeg", width = 28, height = 15, units = "cm")


#NUTRIENT IMBALANCES#
a <- ggplot(stoich, aes(x = Date, y = C.Nimb))+ 
  geom_bar(stat = "identity", colour = "black", fill = "grey40", width=6)+ 
  ylab("C:N imbalance")+
  ylim(-25,75)+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank())+
  geom_hline(yintercept=0, linetype=2, size=1, colour="red")
b <- ggplot(stoich, aes(x = Date, y = C.Pimb))+ 
  geom_bar(stat = "identity", colour = "black", fill = "grey40", width=6)+ 
  ylab("C:P imbalance")+
  ylim(-80,350)+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank())+
  geom_hline(yintercept=0, linetype=2, size=1, colour="red")
c <- ggplot(stoich, aes(x = Date, y = N.Pimb))+ 
  geom_bar(stat = "identity", colour = "black", fill = "grey40", width=6)+ 
  ylab("N:P imbalance")+
  ylim(-50,110)+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank())+
  geom_hline(yintercept=0, linetype=2, size=1, colour="red")

imbPlot <- plot_grid(a,b,c,
                     align="hv", axis="tblr", ncol = 3)
title <- ggdraw() + 
  draw_label("Nutrient imbalances (2018-2019)",
             fontface = 'bold',
             x = 0,
             hjust = 0
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))

NutrientImbalances <- plot_grid(title, imbPlot, ncol=1, rel_heights = c(0.1, 1))


#save figure as image 
ggsave("Figures/NutrientImbalances.jpeg", width = 28, height = 10, units = "cm")


