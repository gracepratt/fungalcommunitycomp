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
library(gridExtra)
library(cowplot)
library(emmeans)
# library(multcomp)
# library(effects)
library(MuMIn)
library(optimx)
# library(afex)
# library("numDeriv")
# library("RCurl")
library(devtools)
#library(fundiv)
library(stringr)
library(plyr)
library(dplyr)
# library(ggbiplot)
# library(visreg)
library(plotrix)
library(tidyr)
library(tidyverse)
library(indicspecies)
library(gdm)
library(geosphere)


########################################################################
## set figure path
########################################################################

fig.path <- "Outputs/Figures"

########################################################################
## load data
########################################################################

# 2017 dataset
data2017 <- read.csv("Raw Data/2017-Complete.csv")

# 2018 dataset
data2018 <- read.csv("Raw Data/2018-Complete.csv")

# load plant traits table
plantID <- read.csv("Raw Data/PlantID.csv")

# OTU tables
otu <- read.csv("Raw Data/fungiOTU.csv")

#amf rarerified table
amf_otu <- read.csv("Raw Data/otu.rr.csv")

# taxanomy list 
tax <- read.csv('Raw Data/fungGuildOTU.guilds.csv')

#plant id
plantID <- read.csv("Raw Data/PlantID.csv")


