## *****************************************************************************
## clean #######################################################################
## *****************************************************************************

## *****************************************************************************
## 1. clean and bind 2017 and 2018 soils dataset ###############################
## *****************************************************************************

# Enter 2017-2018 data as prop (since the relational database combines both now )
# Only include non-OTU columns 
prop <- data %>% dplyr::select(-contains("OTU"))

# rename columns
colnames(prop)[4] <- c("Transect")
colnames(prop)[8] <- c("Block")

prop$farmCode <- paste(prop$Year, prop$FarmKey, sep="_")
prop$FTBL <- paste(prop$FarmType, prop$Block, sep="_")


# change row.names to Key
row.names(prop) <- prop$Key # nrow=378, ncol=86


#remove Extra samples
extraKey <- prop[ grepl('Ex', prop$Code),]$Key
prop <- prop[!prop$Key %in% extraKey,] #nrow=372, ncol=86


# add cropdiversity

prop <- prop %>%
  left_join(cropDiv, by=c("FarmKey","Year")) %>%
  mutate(cropDiversity = ifelse(FarmKey == "GA", 0, cropDiversity))
  # mutate(TOC = ifelse(FarmKey == "BE", OM/1.72, TOC),
  #        PolyYears = ifelse(FarmKey == "FO" & Year == "2018" , PolyYears + 3, PolyYears),
  #        PolyYears = ifelse(FarmKey == "FO" & Year == "2017" , PolyYears + 2, PolyYears),
  #        PolyYears = ifelse(FarmKey == "KY" & Year == "2018" , PolyYears + 1, PolyYears))



## *****************************************************************************
## 2. adjust lat long for each point ###########################################
## *****************************************************************************

# extract unique lat long by farm

farms <- prop[,c("FarmKey","Lat","Long")]

farms <- farms[! duplicated(farms[,1:3]),]


# F1_1
farms$F1_1.LAT <- farms$Lat + 0 
farms$F1_1.LONG <- farms$Long + 0

# F1_2
farms$F1_2.LAT <- farms$Lat + 0 
farms$F1_2.LONG <- farms$Long + (1*0.0001)

# F1_3
farms$F1_3.LAT <- farms$Lat + 0 
farms$F1_3.LONG <- farms$Long + (2*0.0001)

# F2_1
farms$F2_1.LAT <- farms$Lat + (1*0.0001)
farms$F2_1.LONG <- farms$Long + (1*0.0001)

# F2_2
farms$F2_2.LAT <- farms$Lat + (1*0.0001)
farms$F2_2.LONG <- farms$Long + (2*0.0001)

# F2_3
farms$F2_3.LAT <- farms$Lat + (1*0.0001)
farms$F2_3.LONG <- farms$Long + (3*0.0001)

# N1_1
farms$N1_1.LAT <- farms$Lat + (2*0.0001)
farms$N1_1.LONG <- farms$Long + (1*0.0001)

# N1_2
farms$N1_2.LAT <- farms$Lat + (3*0.0001)
farms$N1_2.LONG <- farms$Long + (1*0.0001)

# N1_3
farms$N1_3.LAT <- farms$Lat + (4*0.0001)
farms$N1_3.LONG <- farms$Long + (1*0.0001)

# N2_1
farms$N2_1.LAT <- farms$Lat + (3*0.0001)
farms$N2_1.LONG <- farms$Long + (2*0.0001)

# N2_2
farms$N2_2.LAT <- farms$Lat + (4*0.0001)
farms$N2_2.LONG <- farms$Long + (2*0.0001)

# N2_3
farms$N2_3.LAT <- farms$Lat + (5*0.0001)
farms$N2_3.LONG <- farms$Long + (2*0.0001)

# melt

latlong <- melt(farms, id.vars = c("FarmKey", "Lat", "Long"))

latlong$coord <- substring(latlong$variable, 6)
latlong$Transect <- substring(latlong$variable, 1,2)
latlong$Point <- substring(latlong$variable, 4,4)

latlong  <- dcast(latlong, FarmKey + Lat + Long  + Transect + Point  ~ coord)

prop$Lat_point <- latlong$LAT[match( interaction(prop$FarmKey, prop$Transect, prop$Point), interaction(latlong$FarmKey, latlong$Transect, latlong$Point))]
prop$Long_point <- latlong$LONG[match( interaction(prop$FarmKey, prop$Transect, prop$Point), interaction(latlong$FarmKey, latlong$Transect, latlong$Point))] 
# nrow=372, ncol=88

## *****************************************************************************
## 3. adding additional variables (N:P ratio & mono vs poly) ###################
## *****************************************************************************

# This was used as check to see if the trends were to the inherent high turnover between sites. 
# FarmBi_codes = c(14,1,14, 2, 14, 3, 4, 5, 14, 14, 6, 14, 14, 7, 8, 9, 10, 11, 14, 14, 14, 14, 14, 12, 13)

# FarmBi_codes = c(0,1,0, 2, 0, 3, 4, 5, 0, 0, 6, 0, 0, 7, 8, 9, 10, 11, 0, 0, 0, 0, 0, 12, 13)


# farmBi <- data.frame(FarmBi = FarmBi_codes,
                     # FarmKey = levels(factor(prop$FarmKey)))

prop <- prop %>% mutate( FarmBi = dplyr::recode(FarmType, "Monoculture" = 0,
                                          "Polyculture" = 1),
                        CropBi = dplyr::recode(FocalCrop, "Eggplant" = 1,
                                        "Squash" = 0) ) #%>%
  # left_join(farmBi, by="FarmKey")

# prop <- prop %>% mutate(FarmBi = )

# create N:P ratio column
prop$NP_ratio <- ((prop$N)/prop$P)*100 #nrow=372, ncol=90

#remove N:P ratio outlier
prop <- prop %>% filter(NP_ratio < 600, TOC <0.7) #nrow=371, ncol=90

 
## *****************************************************************************
## 3. rarefy dataset with all fungi ############################################
## *****************************************************************************

#add key to OTUs
otu$Key <- row.names(otu)

#remove the mock community OTUs
wo_mock <- otu %>% dplyr::select(-contains("mock")) 

#rarefy to minimum number of sequences observed
species_only <- wo_mock %>% dplyr::select(contains("OTU"))

minReads <- min(rowSums(species_only))

species.rr_df <- data.frame(rrarefy(species_only, sample=minReads))

#taking away OTUs with no reads
species.rr_df <- species.rr_df[,colSums(species.rr_df!= 0)>0]

species.rr_df$Key <- otu$Key
species.rr_df <- species.rr_df[!species.rr_df$Key %in% extraKey,]

#taking away OTUs with no reads
species.rr_df <- species.rr_df[,colSums(species.rr_df!= 0)>0]

# ncol(species.rr_df)

## *****************************************************************************
## 4. Add OTU tables ###########################################################
## *****************************************************************************

# add rarefied OTU table to complete dataset
# all_fungi <- prop %>%
#   join(species.rr_df) #nrow=371, ncol=3387


# add non-rarefied OTU table to complete dataset
all_fungi <- prop %>%
  join(otu) #nrow=371, ncol=3387


#count OTU 
# ncol(all_fungi %>% dplyr::select(contains("OTU")))

## *****************************************************************************
## 6. Environmental factors ####################################################
## *****************************************************************************

# choose the variables you want

envi_factors <- c("pH", "P", "TOC", "N","cropDiversity")
edaphic_variables <- c("pH","P","TOC","N")

## *****************************************************************************
## 7. full data frame ######################################################
## *****************************************************************************

all <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family="NA", block= c("F","N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture","Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "landscape")$df

row.names(all) <- all$Key

# ncol(all %>% dplyr::select(contains("OTU")))

# ncol(guild_filter(all_fungi, guild= "Arbuscular Mycorrhizal", family="NA") %>% dplyr::select(contains("OTU")))
  

all_sap <- simpleSelection(df=all_fungi, guild= "Saprotroph", family="NA", block= c("F","N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture","Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "both")$df

all_fun <- simpleSelection(df=all_fungi, guild= "all fungi", family="NA", block= c("F","N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture","Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "both")$df

## *****************************************************************************
## 8. ALL FARMS #############################################################
## *****************************************************************************

# within and across rows
all_wa <- simpleSelection(df=all_fungi %>% mutate(cropDiversity = FarmBi), guild= "Arbuscular Mycorrhizal",  family="NA", block= c("F","N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture","Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "both")
all_wa$plotList

# within
all_w <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal",  family="NA", block= c("F") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture","Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "both")

# across
all_a <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal",  family="NA", block= c("N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture","Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "both")

## *****************************************************************************
## 9. MONOCULTURE ############################################################
## *****************************************************************************

# within and across rows
mono_wa <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal",  family="NA", block= c("F","N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "both")

# within
mono_w <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal",  family="NA", block= c("F") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "both")

# across
mono_a <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal",  family="NA", block= c("N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "both")

## *****************************************************************************
## 10. POLYCULTURE ############################################################
## *****************************************************************************

# within and across rows
poly_wa <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal",  family="NA", block= c("F","N") ,focalcrop= c("Eggplant"), farmtype=c("Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "both")

# within
poly_w <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal",  family="NA", block= c("F") ,focalcrop= c("Eggplant"), farmtype=c("Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "both")

# across
poly_a <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal",  family="NA", block= c("N") ,focalcrop= c("Eggplant"), farmtype=c("Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "both")

## *****************************************************************************
## 11. full predictors table ###################################################
## *****************************************************************************

# ALL FARMS
all_predictorsDF <- rbind(
  all_wa$predictors %>% mutate(dataset= "all farms", location = "whole farm"),
  all_w$predictors %>% mutate(dataset= "all farms", location = "within rows"),
  all_a$predictors %>% mutate(dataset= "all farms", location = "across rows")) %>%
  mutate(location = factor(location, levels = c("whole farm", "within rows", "across rows")))


# MONOCULTURE
mono_predictorsDF <- rbind(
  mono_wa$predictors %>% mutate(dataset= "monoculture farms", location = "whole farm"),
  mono_w$predictors %>% mutate(dataset= "monoculture farms", location = "within rows"),
  mono_a$predictors %>% mutate(dataset= "monoculture farms", location = "across rows")) %>%
  mutate(location = factor(location, levels = c("whole farm", "within rows", "across rows")))


# POLYCULTURE
poly_predictorsDF <- rbind(
  poly_wa$predictors %>% mutate(dataset= "polyculture farms", location = "whole farm"),
  poly_w$predictors %>% mutate(dataset= "polyculture farms", location = "within rows"),
  poly_a$predictors %>% mutate(dataset= "polyculture farms", location = "across rows")) %>%
  mutate(location = factor(location, levels = c("whole farm", "within rows", "across rows")))


# Complete
predictorsDF <- rbind(all_predictorsDF, mono_predictorsDF, poly_predictorsDF) %>%
  mutate(location = factor(location, levels = c("whole farm", "within rows", "across rows")),
         dataset = factor(dataset, levels=c("all farms","monoculture farms", "polyculture farms")))

## *****************************************************************************
## 12. distance matrix #########################################################
## *****************************************************************************
