#FATTY ACIDS DATA

#PACKAGES REQUIRED
library(ggplot2) #nice plots
library(cowplot) #nice plots
library(gridExtra) #nice plots
library(ggrepel) #to plot NMDS
library(grid) #to plot NMDS
library(tidyverse) #manipulate datasets
library(MASS) #for NMDS statistics
library(vegan) #for NMDS statistics
library(pairwiseAdonis) #PERMANVA test

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

A_terrFA <- 
  ggplot(FAMEdf_terrFA, aes(x = Samples, y =  value, fill = FA_ID))+ 
  geom_bar(stat = "identity", color = "black",  alpha = 1)+ 
  scale_fill_manual(name = "terrFA",
    values= c("chocolate1", "orangered2", "orangered3", "orangered4"))+
  scale_x_discrete(labels=c("LAKEw", "RIVERw", "DAPw", "CALAw", "CALAa")) +
  
  ylab(~paste("Fatty Acid ", "(ugFAME", "·mgDW"^-1, ")")) +
  theme(text = element_text(size = 10),
        axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.position = "right")
A_terrFA



