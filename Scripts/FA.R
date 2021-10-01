#FATTY ACIDS DATA

#PACKAGES REQUIRED
library(ggplot2) #nice plots
library(cowplot) #nice plots
library(gtable) #align plots
library(gridExtra) #nice plots
library(ggrepel) #to plot NMDS
library(grid) #to plot NMDS
library(tidyverse) #manipulate datasets
library(MASS) #for NMDS statistics
library(vegan) #for NMDS statistics
library(pairwiseAdonis) #PERMANVA test
library(RColorBrewer) #define number of colors you want from brewer palette

#FUNCTION TO ALIGN PLOTS:
AlignPlots <- function(...) {
  LegendWidth <- function(x) x$grobs[[8]]$grobs[[1]]$widths[[4]]
  
  plots.grobs <- lapply(list(...), ggplotGrob)
  
  max.widths <- do.call(unit.pmax, lapply(plots.grobs, "[[", "widths"))
  plots.grobs.eq.widths <- lapply(plots.grobs, function(x) {
    x$widths <- max.widths
    x
  })
  
  legends.widths <- lapply(plots.grobs, LegendWidth)
  max.legends.width <- do.call(max, legends.widths)
  plots.grobs.eq.widths.aligned <- lapply(plots.grobs.eq.widths, function(x) {
    if (is.gtable(x$grobs[[8]])) {
      x$grobs[[8]] <- gtable_add_cols(x$grobs[[8]],
                                      unit(abs(diff(c(LegendWidth(x),
                                                      max.legends.width))),
                                           "mm"))
    }
    x
  })
  
  plots.grobs.eq.widths.aligned
}

#NMDS: MAINLY TO VISUALISE ALL INDIVIDUAL FAME! 
#LOG10(X+2) TRANSFORMED DATA TO "REMOVE" ZEROS & NORMALISE DATA
#AVERAGES OF SAMPLE REPLICATES
#input file
bio <- read.csv("Data/FA_nmds.csv", head = TRUE, check.names = FALSE, row.names = 1)
str(bio)
#ready for NMDS
bio1 <- subset(bio, select=-c(FAME))
#always at the beginning before any random numbers generating function to get a reproducible random result
#for example, when metaMDS using permutation tests!
set.seed(50) 
#NMDS with Bray Curtis distance
fm <- metaMDS(bio1, autotransform = FALSE)
#stress value
fm #0.03 
#PERMANOVA: test significant differences among samples
library(pairwiseAdonis)
pairwise.adonis(bio[,1:5], bio$FAME) #no significant differences between FAME groups

#NMDS REVERSE
#4TH ROOD TRANSFORMED, NO ZEROS WITH TOTALS 
#INCLUDE RATIOS
#input file
bio_r <- read.csv("Data/FA_nmds_reverse.csv", head = TRUE, check.names = FALSE, row.names = 1)
str(bio_r)
#ready for NMDS
bio1_r <- subset(bio_r, select=-c(Group))
#always at the beginning before any random numbers generating function to get a reproducible random result
#for example, when metaMDS using permutation tests!
set.seed(50) 
#NMDS with Bray Curtis distance
fm <- metaMDS(bio1_r, autotransform = FALSE)
#stress value
fm #Warning message: In metaMDS(bio1_r, autotransform = FALSE): stress is (nearly) zero: you may have insufficient data 
#PERMANOVA: test significant differences among samples
pairwise.adonis(bio_r[,1:9], bio$Group) #NOT ENOUGH DATA!
#NOT POSSIBLE TO DO IT AS NORMALLY DONE BECAUSE NOT ENOUGH SAMPLES!


#TRANSFORM NMDS RESULTS TO PLOT
#data.scores
data.scores <- as.data.frame(scores(fm))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$FA <- rownames(data.scores)  # create a column of FA names, from the rownames of data.scores
data.scores$FAME <- bio$FAME  #  add the group variable created earlier
head(data.scores)  #look at the data
#species.scores
species.scores <- as.data.frame(scores(fm, "species"))  #Using the scores function from vegan to extract the sample="species" scores and convert to a data.frame
species.scores$samples <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data
#order legend
data.scores$FAME <- ordered(data.scores$FAME, levels=c("terrFA", 
                                                       "SAFA", 
                                                       "MUFA",
                                                       "BAFA",
                                                       "n-3 PUFA",
                                                       "n-6 PUFA"))
#PLOT
Fig_FA.NMDS <- ggplot(data=data.scores,aes(x=NMDS1,y=NMDS2)) + 
  geom_point(aes(shape = FAME, fill = FAME), size=4) + # add the point markers
  scale_shape_manual(name = "FAME",
                     values =c("terrFA" = 21,
                               "SAFA" = 21,
                               "MUFA" = 22,
                               "BAFA" = 23,
                               "n-6 PUFA" = 24,
                               "n-3 PUFA" = 25))+
  scale_fill_manual(name = "FAME",
                    values=c("terrFA" = "orangered", 
                             "SAFA" = "darkorange",
                             "MUFA"="yellow", 
                             "BAFA"="deepskyblue",
                             "n-6 PUFA"="chartreuse3",
                             "n-3 PUFA"="chartreuse" )) + 
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  xlim(-0.5, 0.8)+
  ylim(-0.3,0.3)+
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=samples),fontface="bold", size=3) +  # add the species labels
  geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=FA),size=2,vjust=2, alpha = 1, check_overlap = T) +  # add the date labels
  coord_equal() + #important for the dimension of the NMDS
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        legend.position="right")

#view plot
Fig_FA.NMDS
#save figure as image
ggsave("Figures/Fig_FA.NMDS.jpeg", width = 20, height = 10, units = "cm")


#BARPLOTS FOR RELATIVE/ABSOLUTE VALUES
#input file
FAMEdf <- read.csv("Data/FA_quantities.csv", stringsAsFactors = T)
str(FAMEdf)

#terrFA
FAMEdf_terrFA <-FAMEdf[FAMEdf$FAME %in% "terrFA",]
FAMEdf_terrFA$FA_ID  <- ordered(FAMEdf_terrFA$FA_ID, levels=c("C20:0", 
                                                       "C22:0", 
                                                       "C23:0",
                                                       "C24:0"))
FAMEdf_terrFA$Samples  <- ordered(FAMEdf_terrFA$Samples, levels=c("LAKEw", 
                                                              "RIVERw", 
                                                              "DAPw",
                                                              "CALAw",
                                                              "CALAa"))

# Define the number of colors you want
nb.cols <- 4
mycolors <- colorRampPalette(brewer.pal(8, "YlOrRd"))(nb.cols)

A_terrFA <- 
  ggplot(FAMEdf_terrFA, aes(x = Samples, y =  value, fill = FA_ID))+ 
  geom_bar(stat = "identity", color = "black",  alpha = 1)+ 
  scale_fill_manual(name = "terrFA",
                    values = mycolors)+
  scale_x_discrete(labels=c("LAKEw", "RIVERw", "DAPw", "CALAw", "CALAa")) +
  ylab(~paste("Fatty Acid ", "(ugFAME", "·mgDW"^-1, ")")) +
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "right")
A_terrFA

#SAFA
FAMEdf_SAFA <-FAMEdf[FAMEdf$FAME %in% "SAFA",]
FAMEdf_SAFA$FA_ID  <- ordered(FAMEdf_SAFA$FA_ID, levels=c("C17:0", 
                                                              "C18:0", 
                                                              "C21:0"))
FAMEdf_SAFA$Samples  <- ordered(FAMEdf_SAFA$Samples, levels=c("LAKEw", 
                                                                  "RIVERw", 
                                                                  "DAPw",
                                                                  "CALAw",
                                                                  "CALAa"))

# Define the number of colors you want
nb.cols <- 3
mycolors <- colorRampPalette(brewer.pal(8, "Oranges"))(nb.cols)

B_SAFA <- 
  ggplot(FAMEdf_SAFA, aes(x = Samples, y =  value, fill = FA_ID))+ 
  geom_bar(stat = "identity", color = "black",  alpha = 1)+ 
  scale_fill_manual(name = "SAFA",
                    values = mycolors)+
  scale_x_discrete(labels=c("LAKEw", "RIVERw", "DAPw", "CALAw", "CALAa")) +
    ylab(~paste("Fatty Acid ", "(ugFAME", "·mgDW"^-1, ")")) +
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "right")
B_SAFA

#MUFA
FAMEdf_MUFA <-FAMEdf[FAMEdf$FAME %in% "MUFA",]
FAMEdf_MUFA$FA_ID  <- ordered(FAMEdf_MUFA$FA_ID, levels=c("C18:1n-12", 
                                                          "C24:1n-9", 
                                                          "C22:1n-9",
                                                          "C20:1n-9",
                                                          "C16:1n-9",
                                                          "C18:1n-7",
                                                          "C17:1n-7",
                                                          "C16:1n-7",
                                                          "C18:1n-6",
                                                          "C15:1n-5",
                                                          "C14:1n-5"))
FAMEdf_MUFA$Samples  <- ordered(FAMEdf_MUFA$Samples, levels=c("LAKEw", 
                                                              "RIVERw", 
                                                              "DAPw",
                                                              "CALAw",
                                                              "CALAa"))

# Define the number of colors you want
nb.cols <- 11
mycolors <- colorRampPalette(brewer.pal(8, "YlOrBr"))(nb.cols)

C_MUFA <- 
  ggplot(FAMEdf_MUFA, aes(x = Samples, y =  value, fill = FA_ID))+ 
  geom_bar(stat = "identity", color = "black",  alpha = 1)+ 
  scale_fill_manual(name = "MUFA",
                    values = mycolors)+
  scale_x_discrete(labels=c("LAKEw", "RIVERw", "DAPw", "CALAw", "CALAa")) +
  ylab(~paste("Fatty Acid ", "(ugFAME", "·mgDW"^-1, ")")) +
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "right")
C_MUFA


#BAFA
FAMEdf_BAFA <-FAMEdf[FAMEdf$FAME %in% "BAFA",]
FAMEdf_BAFA$FA_ID  <- ordered(FAMEdf_BAFA$FA_ID, levels=c("C14:0", 
                                                          "C15:0", 
                                                          "C16:0",
                                                          "C18:1n-9cis",
                                                          "C18:1n-9trans",
                                                          "iso-15:0",
                                                          "anteiso-15:0",
                                                          "iso-16:0",
                                                          "iso-17:0",
                                                          "9,10 16",
                                                          "9,10 18"))
FAMEdf_BAFA$Samples  <- ordered(FAMEdf_BAFA$Samples, levels=c("LAKEw", 
                                                              "RIVERw", 
                                                              "DAPw",
                                                              "CALAw",
                                                              "CALAa"))

# Define the number of colors you want
nb.cols <- 11
mycolors <- colorRampPalette(brewer.pal(8, "YlGnBu"))(nb.cols)

D_BAFA <- 
  ggplot(FAMEdf_BAFA, aes(x = Samples, y =  value, fill = FA_ID))+ 
  geom_bar(stat = "identity", color = "black",  alpha = 1)+ 
  scale_fill_manual(name = "BAFA",
                    values = mycolors)+
  scale_x_discrete(labels=c("LAKEw", "RIVERw", "DAPw", "CALAw", "CALAa")) +
  ylab(~paste("Fatty Acid ", "(ugFAME", "·mgDW"^-1, ")")) +
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "right")
D_BAFA

#n-6 PUFA
FAMEdf_n6PUFA <-FAMEdf[FAMEdf$FAME %in% "n-6 PUFA",]
FAMEdf_n6PUFA$FA_ID  <- ordered(FAMEdf_n6PUFA$FA_ID, levels=c("C18:2n-6trans", 
                                                          "C18:2n-6cis", 
                                                          "C18:3n-6",
                                                          "C20:2n-6",
                                                          "C20:3n-6",
                                                          "C20:4n-6",
                                                          "C22:2n-6",
                                                          "C22:4n-6"))
FAMEdf_n6PUFA$Samples  <- ordered(FAMEdf_n6PUFA$Samples, levels=c("LAKEw", 
                                                              "RIVERw", 
                                                              "DAPw",
                                                              "CALAw",
                                                              "CALAa"))

# Define the number of colors you want
nb.cols <- 8
mycolors <- colorRampPalette(brewer.pal(8, "Greens"))(nb.cols)

E_n6PUFA <- 
  ggplot(FAMEdf_n6PUFA, aes(x = Samples, y =  value, fill = FA_ID))+ 
  geom_bar(stat = "identity", color = "black",  alpha = 1)+ 
  scale_fill_manual(name = "n-6 PUFA",
                    values = mycolors)+
  scale_x_discrete(labels=c("LAKEw", "RIVERw", "DAPw", "CALAw", "CALAa")) +
  ylab(~paste("Fatty Acid ", "(ugFAME", "·mgDW"^-1, ")")) +
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "right")
E_n6PUFA

#n-3 PUFA
FAMEdf_n3PUFA <-FAMEdf[FAMEdf$FAME %in% "n-3 PUFA",]
FAMEdf_n3PUFA$FA_ID  <- ordered(FAMEdf_n3PUFA$FA_ID, levels=c("C18:3n-3", 
                                                              "C18:4n-3", 
                                                              "C20:3n-3",
                                                              "C20:4n-3",
                                                              "C20:5n-3",
                                                              "C22:3n-3",
                                                              "C22:5n-3",
                                                              "C22:6n-3"))
FAMEdf_n3PUFA$Samples  <- ordered(FAMEdf_n3PUFA$Samples, levels=c("LAKEw", 
                                                                  "RIVERw", 
                                                                  "DAPw",
                                                                  "CALAw",
                                                                  "CALAa"))

# Define the number of colors you want
nb.cols <- 8
mycolors <- colorRampPalette(brewer.pal(8, "YlGn"))(nb.cols)

F_n3PUFA <- 
  ggplot(FAMEdf_n3PUFA, aes(x = Samples, y =  value, fill = FA_ID))+ 
  geom_bar(stat = "identity", color = "black",  alpha = 1)+ 
  scale_fill_manual(name = "n-3 PUFA",
                    values = mycolors)+
  scale_x_discrete(labels=c("LAKEw", "RIVERw", "DAPw", "CALAw", "CALAa")) +
  ylab(~paste("Fatty Acid ", "(ugFAME", "·mgDW"^-1, ")")) +
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "right")
F_n3PUFA

#Omega ratio
FAMEdf_omega <-FAMEdf[FAMEdf$FAME %in% "Omega ratio",]
FAMEdf_omega$Samples  <- ordered(FAMEdf_omega$Samples, levels=c("LAKEw", 
                                                                  "RIVERw", 
                                                                  "DAPw",
                                                                  "CALAw",
                                                                  "CALAa"))

G_omega <- 
  ggplot(FAMEdf_omega, aes(x = Samples, y =  value))+ 
  geom_bar(stat = "identity", color = "black",  fill = "greenyellow",  alpha = 1)+ 
  scale_x_discrete(labels=c("LAKEw", "RIVERw", "DAPw", "CALAw", "CALAa")) +
  ylab("Omega ratio (n-3:n-6)") +
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  geom_hline(yintercept=1, linetype="dotted", size=1, colour="black")
G_omega


#Essential ratio
FAMEdf_essential <-FAMEdf[FAMEdf$FAME %in% "Essential ratio",]
FAMEdf_essential$Samples  <- ordered(FAMEdf_omega$Samples, levels=c("LAKEw", 
                                                                "RIVERw", 
                                                                "DAPw",
                                                                "CALAw",
                                                                "CALAa"))

H_essential <- 
  ggplot(FAMEdf_essential, aes(x = Samples, y =  value))+ 
  geom_bar(stat = "identity", color = "black",  fill = "green",  alpha = 1)+ 
  scale_x_discrete(labels=c("LAKEw", "RIVERw", "DAPw", "CALAw", "CALAa")) +
  ylab("Essential ratio (DHA:EPA)") +
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "none")+
  geom_hline(yintercept=1, linetype="dotted", size=1, colour="black")
H_essential



#final plot 
plotsaligned <- AlignPlots(A_terrFA,E_n6PUFA,
                           B_SAFA, F_n3PUFA,
                           C_MUFA, G_omega,
                           D_BAFA, H_essential)

Fig_FA.quantities <- do.call("plot_grid", c(plotsaligned, 
                                            ncol = 2, 
                                            nrow = 4, 
                                            labels =c("a","e","b","f","c","g", "d", "h")))

#View plot
Fig_FA.quantities
#save figure as image
ggsave("Figures/Fig_FA.quantities.jpeg", width = 30, height = 35, units = "cm")


#RETENTION PLOT - HEATMAP
#input file
RETdf <- read.csv("Data/FA_retention.csv", stringsAsFactors = T)
str(RETdf)

RETdf$FAME  <- ordered(RETdf$FAME , levels=c("terrFA",
                                             "SAFA",
                                             "MUFA",
                                             "BAFA",
                                             "n-6 PUFA",
                                             "LIN",
                                             "n-3 PUFA",
                                             "ALA",
                                             "EPA",
                                             "DHA"))

nb.cols <- 5
mycolors <- colorRampPalette(brewer.pal(8, "Reds"))(nb.cols)

Fig_FA.retention <- ggplot(RETdf, aes(FAME, Group)) +
  geom_tile(aes(fill = Retention), colour = "black") + 
  geom_text(aes(label = round(Retention, 1)), size=2) +
  scale_fill_gradientn(colours = mycolors)+
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  theme(panel.background = element_rect(fill = "white", colour = "black"))

Fig_FA.retention

#save figure as image
ggsave("Figures/Fig_FA.retention.jpeg", width = 15, height = 5, units = "cm")


#DIFFERENCES PLOT - HEATMAP
#input file
DIFdf <- read.csv("Data/FA_differences.csv", stringsAsFactors = T)
str(DIFdf)

DIFdf$FAME  <- ordered(DIFdf$FAME , levels=c("terrFA",
                                             "SAFA",
                                             "MUFA",
                                             "BAFA",
                                             "n-6 PUFA",
                                             "LIN",
                                             "n-3 PUFA",
                                             "ALA",
                                             "EPA",
                                             "DHA"))

nb.cols <- 11
mycolors <- colorRampPalette(brewer.pal(8, "PiYG"))(nb.cols)

Fig_FA.differences <- ggplot(DIFdf, aes(FAME, Group)) +
  geom_tile(aes(fill = Differences), colour = "black") + 
  geom_text(aes(label = round(Differences, 1)), size=2) +
  scale_fill_gradientn(colours = mycolors)+
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  theme(panel.background = element_rect(fill = "white", colour = "black"))

Fig_FA.differences

#save figure as image
ggsave("Figures/Fig_FA.differences.jpeg", width = 15, height = 5, units = "cm")


#final plot 
plotsaligned <- AlignPlots(Fig_FA.retention,Fig_FA.differences)

Fig_FA.heatmaps <- do.call("plot_grid", c(plotsaligned, 
                                            ncol = 1, 
                                            nrow = 2, 
                                            labels =c("a","b")))
Fig_FA.heatmaps

#save figure as image
ggsave("Figures/Fig_FA.heatmaps.jpeg", width = 15, height = 10, units = "cm")
