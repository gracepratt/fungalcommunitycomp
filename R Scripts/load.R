########################################################################
## load packages and data for 2018 and 2017 field research study
########################################################################




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

# taxanomy list 
tax <- read.csv('Raw Data/taxonomy.csv')











