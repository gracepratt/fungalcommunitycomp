########################################################################
## packages
########################################################################


library(lme4)
library(car)
library(lmerTest)
library(vegan)
library(reshape2)
library(ggplot2)
# library(ggsignif)
# library(ggpmisc)
library(ggthemes)
#library(gridExtra)
library(cowplot)
library(egg)
library(emmeans)
# library(multcomp)
# library(effects)
library(MuMIn)
#library(optimx)
# library(afex)
# library("numDeriv")
# library("RCurl")
# library(devtools)
# library(fundiv)
library(stringr)
library(plyr)
library(dplyr)
# library(ggbiplot)
# library(visreg)
library(plotrix)
library(tidyr)
library(tidyverse)
#library(indicspecies)
library(gdm)
library(geosphere)
library(microbiome)
library(ggfortify)
library(lme4)
library(lmerTest)
library(betapart)
library(ape)
library(TITAN2)
library(ecodist)
library(adespatial)


########################################################################
## set figure path
########################################################################

fig.path <- "Outputs/Figures/FINAL"

########################################################################
## load data
########################################################################


setwd('/Volumes/GoogleDrive/My Drive/Research/Research projects/Spatial project/Analysis/R workflow/fungalcommunitycomp')


# 2017-2018 dataset
data <- read.csv("Raw Data/20172018-Complete.csv") # nrow=378, ncol=328

# # 2017 dataset
# data2017 <- read.csv("Raw Data/2017-Complete.csv") # nrow=120, ncol=69
# 
# # 2018 dataset
# data2018 <- read.csv("Raw Data/2018-Complete.csv") # nrow=258, ncol=69

# load plant traits table
# plantID <- read.csv("Raw Data/PlantID.csv") # nrow=258, ncol=69

# OTU tables
otu <- read.csv("Raw Data/fungiOTU.csv") # nrow=29, ncol=6

#amf rarerified table
amf_otu <- read.csv("Raw Data/glomOTU_rMin.csv") # nrow=378, ncol=244
old_amf <- read.csv("Raw Data/Old AMF Tables/otu.rr.csv") # nrow=378, ncol=244
# amf_otu_100 <- read.csv("Raw Data/glomOTU_r100.csv")
# amf_otu_nr <- read.csv("Raw Data/glomOTU_nr.csv")

# taxanomy list 
# tax <- read.csv('Raw Data/fungGuildOTU.guilds.csv') # nrow=3427, ncol=389
tax <- read.csv('Raw Data/guilds.csv') # nrow=3427, ncol=389

glomNum <- tax %>% filter(Phylum == "Glomeromycota") %>% pull(OTU)

length(glomNum)

# crop diversity

cropDiv <- read.csv('Raw Data/cropdiversity.csv') # nrow=3427, ncol=389

########################################################################
## load functions
########################################################################

source("R Scripts/functions.R")



#SET SEED
set.seed(5)


