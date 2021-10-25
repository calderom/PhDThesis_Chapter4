#LAKE WATER CHEMISTRY

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


#Temporal plots for 2018 fortnightly water chemistry parameters
#input file
LakeWC <- read.csv("Data/Lake_WaterChemistry.csv", stringsAsFactors = T)
LakeWC$Date <- as.Date(LakeWC$Date,"%d/%m/%Y")
str(LakeWC)
names(LakeWC)

NitrogenWC <- read.csv("Data/Lake_Nitrogen.csv", stringsAsFactors = T)
NitrogenWC$Date <- as.Date(NitrogenWC$Date,"%d/%m/%Y")
str(NitrogenWC)
names(NitrogenWC)

PhosphorusWC <- read.csv("Data/Lake_Phosphorus.csv", stringsAsFactors = T)
PhosphorusWC$Date <- as.Date(PhosphorusWC$Date,"%d/%m/%Y")
str(PhosphorusWC)
names(PhosphorusWC)

a <- ggplot(LakeWC, aes(x = Date, y = DOC.mgL))+ 
  geom_line()+
  geom_point(shape=21, color="black", fill="orange", size=2)+
  ylab(~paste("DOC ", "(mgC", "·L"^-1, ")"))+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank())+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
a

b <- ggplot(LakeWC, aes(x = Date, y = alkalinity.ueqL))+ 
  geom_line()+
  geom_point(shape=21, color="black", fill="red", size=2)+
  ylab(~paste("Alkalinity ", "(ueq", "·L"^-1, ")"))+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank())+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
b

c <- ggplot(LakeWC, aes(x = Date, y = colour.mgPtL))+ 
  geom_line()+
  geom_point(shape=21, color="black", fill="yellow", size=2)+
  ylab(~paste("Colour ", "(mgPt", "·L"^-1, ")"))+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank())+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
c

d <- ggplot(LakeWC, aes(x = Date, y = SUVA254.AULmmgC))+ 
  geom_line()+
  geom_point(shape=21, color="black", fill="gold2", size=2)+
  ylab(~paste("SUVA254 ", "(AU L", "·m mgC"^-1, ")"))+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank())+
  geom_hline(yintercept = 3.5, lty = 2)+ #reference to have an idea of auto-C (below) vs allo-C (above)
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
d


e <- ggplot(LakeWC, aes(x = Date, y = chla.ugL))+ 
  geom_line()+
  geom_point(shape=21, color="black", fill="greenyellow", size=2)+
  ylab(~paste("Chlorophyll-a ", "(ugChl-a", "·L"^-1, ")"))+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank())+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
e

f <- ggplot(LakeWC, aes(x = Date, y = TSS.mgL))+ 
  geom_line()+
  geom_point(shape=21, color="black", fill="chocolate", size=2)+
  ylab(~paste("TSS ", "(mg", "·L"^-1, ")"))+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank())+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
f

g <- ggplot(NitrogenWC, aes(x = Date, y = Value, group=Variable))+ 
  geom_line()+
  geom_point(aes(fill=Variable), shape = 21, color="black", size=2)+
  scale_fill_manual(name = "Fractions",
                    values=c("NH4" = "yellowgreen", 
                             "NO3" = "green",
                             "TN"="green4")) +
  ylab(~paste("Nitrogen Fractions ", "(mgN", "·L"^-1, ")"))+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank(),
        legend.position = "right")+
  geom_hline(aes(yintercept = 0.02), linetype = "dashed", color = "yellowgreen", size = 1)+  #LOD references NH4 < 0.02
  #geom_hline(aes(yintercept = 0.01), linetype = "dotted", color="deeppink") #LOD reference N03 < 0.01
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")             
g #ammonium always <LOD!

h <- ggplot(PhosphorusWC, aes(x = Date, y = Value, group=Variable))+ 
  geom_line()+
  geom_point(aes(fill=Variable), shape = 21, color="black", size=2)+
  scale_fill_manual(name = "Fractions",
                    values=c("SRP" = "deeppink", 
                             "TP" = "deeppink4")) +
  ylab(~paste("Phosphorus Fractions ", "(ugP", "·L"^-1, ")"))+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank(),
        legend.position = "right")+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
h

i <- ggplot(LakeWC, aes(x = Date, y = dissSi.mgL))+ 
  geom_line()+
  geom_point( shape=21, color="black", fill="purple", size=2)+
  ylab(~paste("Dissolved Silicon ", "(mgSi", "·L"^-1, ")"))+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank())+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
i

j <- ggplot(LakeWC, aes(x = Date, y = Si.TP.molar))+ 
  geom_line()+
  geom_point( shape=21, color="black", fill="violetred", size=2)+
  ylab("Si:TP (molar)")+
  scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 10), axis.title.x = element_blank())+
  geom_vline(xintercept = as.Date("2018-06-14"), linetype="dotdash", colour = "red")
j

FeeaghWC <- plot_grid(a,b,
                      c,d,
                      e,f,
                      g,h,
                      i,j,
                      align="hv", axis="tblr", ncol = 2, nrow=5,
                      labels =c("a","b","c","d","e","f", "g", "h", "i", "j"))

title <- ggdraw() + 
  draw_label("Lough Feeagh Water Chemistry (2018-2019)",
             fontface = 'bold',
             x = 0,
             hjust = 0
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))

FeeaghWCplots <- plot_grid(title, FeeaghWC, ncol=1, rel_heights = c(0.1, 1))

#save figure as image 
ggsave("Figures/FeeaghWCplots.jpeg", width = 25, height = 28, units = "cm")
