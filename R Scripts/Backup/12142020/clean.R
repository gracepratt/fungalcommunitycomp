## *****************************************************************************
## clean #######################################################################
## *****************************************************************************

## *****************************************************************************
## 1. clean and bind 2017 and 2018 soils dataset ###############################
## *****************************************************************************

# Enter 2017-2018 data as prop (since the relational database combines both now )
# Only include non-OTU columns 
prop <- data[1:84] # nrow=378, ncol=84

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
  mutate(TOC = ifelse(FarmKey == "BE", OM/1.72, TOC),
         PolyYears = ifelse(FarmKey == "FO" & Year == "2018" , PolyYears + 3, PolyYears),
         PolyYears = ifelse(FarmKey == "FO" & Year == "2017" , PolyYears + 2, PolyYears),
         PolyYears = ifelse(FarmKey == "KY" & Year == "2018" , PolyYears + 1, PolyYears))



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


## *****************************************************************************
## 4. Add OTU tables ###########################################################
## *****************************************************************************


# add rarefied OTU table to complete dataset
all_fungi <- prop %>% 
  join(species.rr_df) #nrow=371, ncol=3387

# rows to switch
KY_rows <- all_fungi[all_fungi$farmCode %in% c("2018_KY"), c(93:ncol(all_fungi))]
VD_rows <- all_fungi[all_fungi$farmCode %in% c("2018_VD"), c(93:ncol(all_fungi))]
# switch
all_fungi[all_fungi$farmCode %in% c("2018_KY"), c(93:ncol(all_fungi))] <- VD_rows
all_fungi[all_fungi$farmCode %in% c("2018_VD"), c(93:ncol(all_fungi))] <- KY_rows

#count OTU 
ncol(all_fungi %>% dplyr::select(contains("OTU")))


# AMF

#add AMF table for AMF dataset
amf_otu$Key <- row.names(amf_otu) # nrow=378, ncol=245
  
amf <- prop %>%
  join(amf_otu) # nrow=371, ncol=334

# rows to switch
KY_rows <- amf[amf$farmCode %in% c("2018_KY"), c(93:ncol(amf))]
VD_rows <- amf[amf$farmCode %in% c("2018_VD"), c(93:ncol(amf))]
# switch
amf[amf$farmCode %in% c("2018_KY"), c(93:ncol(amf))] <- VD_rows
amf[amf$farmCode %in% c("2018_VD"), c(93:ncol(amf))] <- KY_rows


#taking out samples with no AMF
amf$rowsum <- rowSums(amf %>% dplyr::select(contains("OTU"))) # nrow=371, ncol=335
amf <- amf %>% filter(rowsum > 0) # nrow=321, ncol=335


## *****************************************************************************
## 6. Environmental factors ####################################################
## *****************************************************************************

# choose the variables you want

envi_factors <- c("pH", "P", "TOC", "N", "NP_ratio", "FarmBi","cropDiversity")
edaphic_variables <- c("pH","P","TOC","N","NP_ratio")

# crop richness not working well w/n maybe because there's only 1 value for local? but that would be the same for crop diversity... hm hm hm hm 

## *****************************************************************************
## 7. full data frame ######################################################
## *****************************************************************************

test <- all_fungi %>% filter(farmCode != "2018_FO")

all <- as.data.frame(simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family="NA", block= c("F","N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture","Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "landscape")$df)

all_sap <- as.data.frame(simpleSelection(df=all_fungi, guild= "Saprotroph", family="NA", block= c("F","N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture","Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "landscape")$df)

all_fun <- as.data.frame(simpleSelection(df=all_fungi, guild= "all fungi", family="NA", block= c("F","N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture","Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "landscape")$df)

all_summary <- all %>%
  group_by(PolyYears) %>%
  summarise_at(vars(observed), list(mean=mean, SE=std.error), na.rm=TRUE)


themeBorder <- theme_bw() + theme(legend.title=element_blank(), 
                                  panel.background = element_blank(),
                                  panel.grid.major = element_blank(), 
                                  panel.grid.minor = element_blank(),
                                  axis.line =element_blank(),
                                  panel.border = element_rect(colour = "black", fill=NA, size=0.75))

test <- ggplot(all, aes(x=factor(PolyYears), y=observed, group=PolyYears)) +
  geom_boxplot() +
  ylab("observed AMF richness") +
  xlab("years under polyculure management") + 
  themeBorder 


ggsave(filename = "mgmtLegacy.pdf", 
       plot = test,
       width = 3, 
       height = 2.5,
       useDingbats=FALSE)


ggsave(filename = "mgmtLegacy.png", 
       plot = test,
       width = 3, 
       height = 2.5,)


ggplot(all_summary, aes(x=PolyYears, y=mean)) +
  geom_line() +
  geom_point()

## *****************************************************************************
## 8.  no scale #############################################################
## *****************************************************************************

# Monoculture + Polyculture
noscale <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal",  family="NA", block= c("F","N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture","Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "both")
noscale_f <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal",  family="NA", block= c("F") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture","Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "both")
noscale_n <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal",  family="NA", block= c("N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture","Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "both")



all_fungi_test <- all_fungi
all_fungi_test$P <- log(all_fungi$P+1)

# Monoculture

noscale_mono <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA", block= c("F","N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "both")

noscale_mono_f <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA", block= c("F") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "both")

noscale_mono_f_df <- as.data.frame(noscale_mono_f$df)

noscale_mono_n <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA", block= c("N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "both")

noscale_mono_n_df <- as.data.frame(noscale_mono_f$df)


# Polyculture
noscale_poly <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA", block= c("F","N") ,focalcrop= c("Eggplant"), farmtype=c("Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "both")

noscale_poly_f <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA", block= c("F") ,focalcrop= c("Eggplant"), farmtype=c("Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "both")

noscale_poly_n <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA", block= c("N") ,focalcrop= c("Eggplant"), farmtype=c("Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "both")



noscale$plotList
noscale_f$plotList
noscale_n$plotList
noscale_mono$plotList
noscale_mono_f$plotList
noscale_mono_n$plotList
noscale_poly$plotList
noscale_poly_f$plotList
noscale_poly_n$plotList

## *****************************************************************************
## 9. all blocks ############################################################
## *****************************************************************************

# Monoculture + Polyculture
landscape <- simpleSelection(df=all, guild= "Arbuscular Mycorrhizal", family = "NA", block= c("F","N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture","Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "landscape")

local <- simpleSelection(df=all, guild= "Arbuscular Mycorrhizal", family = "NA", block= c("F","N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture","Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "local")


# Monoculture 
landscape_mono <- simpleSelection(df=all, guild= "Arbuscular Mycorrhizal", family = "NA", block= c("F","N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "landscape")

local_mono <- simpleSelection(df=all, guild= "Arbuscular Mycorrhizal", family = "NA", block= c("F","N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "local")

# Polyculture 
landscape_poly <- simpleSelection(df=all, guild= "Arbuscular Mycorrhizal", family = "NA", block= c("F","N") ,focalcrop= c("Eggplant"), farmtype=c("Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "landscape")

local_poly <- simpleSelection(df=all, guild= "Arbuscular Mycorrhizal", family = "NA", block= c("F","N") ,focalcrop= c("Eggplant"), farmtype=c("Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "local")


## *****************************************************************************
## 10. landscape ############################################################
## *****************************************************************************

# Monoculture + Polyculture

# Focal
landscape_f <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA", block= c("F") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture","Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "landscape")


# Non-focal
landscape_n <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA", block= c("N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture","Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "landscape")


# Monoculture

# Focal
landscape_mono_f <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA", block= c("F") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "landscape")


# Non-focal
landscape_mono_n <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA", block= c("N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "landscape")



# Polyculture

# Focal
landscape_poly_f <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA", block= c("F") ,focalcrop= c("Eggplant"), farmtype=c("Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "landscape")


# Non-focal
landscape_poly_n <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA", block= c("N") ,focalcrop= c("Eggplant"), farmtype=c("Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "landscape")




## *****************************************************************************
## 11. local ############################################################
## *****************************************************************************

# Monoculture + Polyculture

# Focal
local_f <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA", block= c("F") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture","Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "local")


# Non-focal
local_n <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA", block= c("N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture","Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "local")


# Monoculture

# Focal
local_mono_f <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA", block= c("F") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "local")


# Non-focal
local_mono_n <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA", block= c("N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "local")



# Polyculture

# Focal
local_poly_f <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA", block= c("F") ,focalcrop= c("Eggplant"), farmtype=c("Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "local")


# Non-focal
local_poly_n <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA", block= c("N") ,focalcrop= c("Eggplant"), farmtype=c("Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "local")




## *****************************************************************************
## 12. full predictors table ###################################################
## *****************************************************************************

predictorsTable <- rbind(
landscape_f$predictors %>% mutate(scaleLevel = "landscape_F"),
landscape_n$predictors %>% mutate(scaleLevel = "landscape_N"),
local_f$predictors %>% mutate(scaleLevel = "local_F"),
local_n$predictors %>% mutate(scaleLevel = "local_N") ) %>%
  mutate(scaleLevel = as.factor(scaleLevel)) %>%
  dplyr::select(Key, scaleLevel, Factor, Y, X, X_unscaled)


predictorsTable_mono <- rbind(
  landscape_mono_f$predictors %>% mutate(scaleLevel = "landscape_F"),
  landscape_mono_n$predictors %>% mutate(scaleLevel = "landscape_N"),
  local_mono_f$predictors %>% mutate(scaleLevel = "local_F"),
  local_mono_n$predictors %>% mutate(scaleLevel = "local_N") ) %>%
  mutate(scaleLevel = as.factor(scaleLevel)) %>%
  dplyr::select(Key, scaleLevel, Factor, Y, X, X_unscaled)


predictorsTable_poly <- rbind(
  landscape_poly_f$predictors %>% mutate(scaleLevel = "landscape_F"),
  landscape_poly_n$predictors %>% mutate(scaleLevel = "landscape_N"),
  local_poly_f$predictors %>% mutate(scaleLevel = "local_F"),
  local_poly_n$predictors %>% mutate(scaleLevel = "local_N") ) %>%
  mutate(scaleLevel = as.factor(scaleLevel)) %>%
  dplyr::select(Key, scaleLevel, Factor, Y, X, X_unscaled)


## *****************************************************************************
## 13. relative abundance ######################################################
## *****************************************************************************

all.pa <- decostand(all %>% dplyr::select(contains("OTU")), "pa")  %>%
  mutate(Key = all$Key)
all.pa <-  all.pa %>% mutate(totalFreq = c(rowSums(all.pa %>% dplyr::select(contains("OTU"))))) %>%
  pivot_longer(cols=contains("OTU"), names_to = "OTU", values_to = "freq") %>%
  filter(freq != 0) %>%
  mutate(relFreq = freq/totalFreq) #%>%
  # left_join(tax , by=c("OTU"))

# total reads = 3258
rel <- all %>%
  mutate(totalReads = rowSums(all %>% dplyr::select(contains("OTU")))) %>%
  pivot_longer(cols=contains("OTU"), names_to = "OTU", values_to = "reads") %>%
  filter(reads != 0) %>%
  mutate(relAbun = reads/totalReads) %>%
  left_join(tax , by=c("OTU")) %>%
  left_join(all.pa, by = c("Key","OTU"))


relAbun_ag <- all %>%
  mutate(totalReads = rowSums(all %>% dplyr::select(contains("OTU")))) %>%
  pivot_longer(cols=contains("OTU"), names_to = "OTU", values_to = "reads") %>%
  left_join(tax %>% dplyr::select(OTU, Family, Taxon), by=c("OTU")) %>%
  group_by(Key, Taxon) %>%
  summarise_at(vars(reads, totalReads), list(sum=sum, mean=mean), na.rm=TRUE) %>%
  dplyr::select(Key, Family, reads_sum, totalReads_mean) %>%
  rename("reads"= reads_sum ,"totalReads"= totalReads_mean) %>%
  filter(reads != 0) %>%
  mutate(relAbun = reads/totalReads) %>%
  left_join(all, by="Key") %>%
  ungroup()


rel_ag <- rel %>%
  group_by(Key, Taxon) %>%
  summarise_at(vars(reads, totalReads, freq, totalFreq), list(sum=sum, mean=mean)) %>%
  dplyr::select(Key, Taxon, reads_sum, totalReads_mean,  freq_sum, totalFreq_mean) %>%
  # rename("reads"= reads_sum ,"totalReads"= totalReads_mean) %>%
  # filter(reads != 0) %>%
  # mutate(relAbun = reads/totalReads) %>%
  left_join(all, by="Key") %>%
  ungroup()

# [1] "Acaulospora"      "Acaulosporaceae"  "Archaeospora"     "Cetraspora"      
# [5] "Claroideoglomus"  "Diversispora"     "Diversisporaceae" "Diversisporales" 
# [9] "Funneliformis"    "Glomeraceae"      "Paraglomeraceae"  "Rhizophagus"  

ggplot(rel_ag %>% filter(Taxon %in% c("Rhizophagus")), aes( y=freq_sum/totalFreq_mean, x= TOC)) +
  geom_point() +
  stat_smooth(method="lm")

ggplot(rel_ag %>% filter(Taxon %in% c("Rhizophagus")), aes( y=reads_sum/totalReads_mean, x= TOC)) +
  geom_point() +
  stat_smooth(method="lm")

relAbun_P <-  ggplot(rel_ag %>% filter(Taxon %in% c("Rhizophagus"), P<90), aes( y=reads_sum/totalReads_mean, x= P)) +
  geom_point() +
  stat_smooth(method="lm")
  # ylab("relative abundance") +
  # xlab("P") +
  # scale_fill_brewer(palette="Paired")+
  # theme_classic()

relAbun_P <-  ggplot(relAbun, aes(fill=Taxon, y=relAbun, x=factor(round_any(P,12)))) +
  geom_bar(position="fill", stat="identity") +
  ylab("relative abundance") +
  xlab("P") +
  scale_fill_manual(values= taxonColor(relAbun$Taxon))+
  theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust=1))

relAbun_pH <-  ggplot(relAbun, aes(fill=Taxon, y=relAbun, x=factor(round_any(pH,0.5)))) +
  geom_bar(position="fill", stat="identity")  +
  ylab("relative abundance") +
  xlab("pH") +
  scale_fill_manual(values= taxonColor(relAbun$Taxon))+
  theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust=1))

relAbun_NP <-  ggplot(relAbun, aes(fill=Taxon, y=relAbun, x=factor(round_any(NP_ratio,0.5)))) +
  geom_bar(position="fill", stat="identity") +
  ylab("relative abundance") +
  xlab("N:P") +
  scale_fill_manual(values= taxonColor(relAbun$Taxon))+
  theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust=1))

relAbun_TOC <-  ggplot(relAbun, aes(fill=Taxon, y=relAbun, x=factor(round_any(TOC,0.125)))) +
  geom_bar(position="fill", stat="identity")+
  ylab("relative abundance") +
  xlab("TOC") +
  scale_fill_manual(values= taxonColor(relAbun$Taxon))+
  theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust=1))


relAbun_N <-  ggplot(relAbun, aes(fill=Taxon, y=relAbun, x=factor(round_any(N,0.015)) ) ) +
  geom_bar(position="fill", stat="identity")+
  ylab("relative abundance") +
  xlab("N") +
  scale_fill_manual(values= taxonColor(relAbun$Taxon))+
  theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust=1))


relAbun_CD <-  ggplot(relAbun, aes(fill=Taxon, y=relAbun, x=factor(round_any(cropDiversity,0.3)))) +
  geom_bar(position="fill", stat="identity")  +
  ylab("relative abundance") +
  xlab("crop diversity") +
  scale_fill_manual(values= taxonColor(relAbun$Taxon))+
  theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust=1))

relAbun_cropRichness <-  ggplot(relAbun %>% filter(cropRichness !=5), aes(fill=Taxon, y=relAbun, x=factor(round_any(cropRichness,5)))) +
  geom_bar(position="fill", stat="identity")  +
  ylab("relative abundance") +
  xlab("crop richness") +
  scale_fill_manual(values= taxonColor(relAbun$Taxon))+
  theme_classic()

relAbun_Years <-  ggplot(relAbun %>% filter(FarmKey %in% c("KY","FO","DI","CF")) , aes(fill=Taxon, y=relAbun, x=factor(Year))) +
  geom_bar(position="fill", stat="identity")  +
  ylab("relative abundance") +
  xlab("crop richness") +
  scale_fill_brewer(palette="Paired")+
  theme_classic()  

farmDiv <- c("AE", "AS", "DS", "IS", "SD", "VI", "VK", "GA", "CF", "BE", "KY", "DI","FO", "OT", "YJ")
farmRich <- c("AE", "AS", "DS", "IS", "SD", "VI", "VK", "GA", "CF","YJ","KY","OT","BE","DI","FO")

relAbun$FarmKey <- factor(relAbun$FarmKey, levels=farmRich)

relAbun_cropRichness <-  ggplot(relAbun %>% filter(cropRichness !=5), aes(fill=Taxon, y=relAbun, x= FarmKey)) +
  geom_bar(position="fill", stat="identity")  +
  ylab("relative abundance") +
  xlab("crop richness") +
  scale_fill_brewer(palette="Paired")+
  theme_classic() + facet_wrap(vars(FarmType), scales = "free", drop=TRUE)




## *****************************************************************************
## distance matrix #############################################################
## *****************************************************************************
all <- all %>% mutate(Key = as.integer(Key))
amf_otu <- all %>% dplyr::select(contains("OTU"))
dist <- vegdist(amf_otu, "bray")
# KEYS NOT MATCHING WHEN TURNING TO MATRIX!


library(phyloseq)

fung01<-amf_otu; fung01[fung01>0]=1 # fung is the OTU table ##
fung.dist01<-beta.pair(fung01, index.family = "jaccard");

jne <- as.data.frame(as.matrix(fung.dist01$beta.jne))
colnames(jne)  = all$Key
rownames(jne) = all$Key
jne <- jne %>%
  rownames_to_column("Key") %>%
  pivot_longer(cols= -Key, names_to = "pairs", values_to = "dissim") %>%
  mutate(Key = as.integer(Key), pairs = as.integer(pairs)) %>%
  left_join(all %>% dplyr::select(Key, farmCode), by= "Key") %>%
  left_join(all %>% dplyr::select(Key, farmCode), by= c("pairs" = "Key")) %>%
  mutate(scale = ifelse(farmCode.x == farmCode.y, "within","between"))

# check = summary(is.na(jne$farmCode.x))


jne_within <- jne %>%
  filter(scale == "within") %>%
  dplyr::select(-farmCode.x, -farmCode.y) %>%
  pivot_wider(id_cols=Key, names_from= pairs, values_from = dissim) %>%
  column_to_rownames("Key")

jne_between <- jne %>%
  filter(scale == "between") %>%
  dplyr::select(-farmCode.x, -farmCode.y) %>%
  pivot_wider(id_cols=Key, names_from= pairs, values_from = dissim) %>%
  column_to_rownames("Key")

jne_between <- jne_between[c(194:205,1:193)]

pH <-dist(all %>% dplyr::select(pH), method = "euclidean")
CD <-dist(all %>% dplyr::select(cropDiversity), method = "euclidean")
TOC <-dist(all %>% dplyr::select(TOC), method = "euclidean")

vegan::mantel(as.dist(jne_between), pH, na.rm=TRUE)
vegan::mantel(as.dist(jne_between), TOC, na.rm=TRUE)

# NOTE
# NEED TO CHANGE THE DIST BELOW TO WITHIN AND BETWEEN

par(mfrow=c(1,1),mar=c(2, 2, 4, 0.5))
td<-dist(all$FarmBi) ## da$TP is the variable of interest in the meta table ###
# test <- as.data.frame(as.matrix(td))
color=rgb(0,0,0,alpha=0.1)
plot(all$FarmBi,as.dist(jne_within),xlab=" ",ylab=" ", ylim=c(0,1), col=color, cex=0.2)
vegan::mantel(fung.dist01$beta.jne~td)
abline(lm(fung.dist01$beta.jne~td),col="red")

plot(jitter(td),fung.dist01$beta.jtu,xlab=" ",ylab=" ", ylim=c(0,1), col=color, cex=0.2)
mantel(fung.dist01$beta.jtu~td)
abline(lm(fung.dist01$beta.jtu~td),col="red")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# dist.h <- vegdist(decostand(amf_otu, "hellinger"), "bray")
# dist.z <- as.dist(scale(dist))
# 
# distUpper <- as.matrix(dist)
# distUpper[lower.tri(distUpper)] <- NA
# distUpper[distUpper == 0] <- NA
# 
# means <- all %>%
#   # filter(FocalCrop == "Eggplant") %>%
#   mutate(distMeans = rowMeans(distUpper, na.rm = TRUE),
#          dissIndex = rowSums(distUpper, na.rm=TRUE)/(nrow(distUpper)*(nrow(distUpper)-1)))
# 
# summary(lm(as.matrix(dist.z) ~ all$FarmType*all$Block))
# 
# 
# nest <- nestednodf(amf_otu,
#            weighted=TRUE,
#            order=FALSE) $statistic["NODF"]
# 
# 
# bd <- betadisper(dist.z, all$FTBL, type=c("centroid"))
# permutest(bd)
# 
# polyOTU <- all %>% filter(FarmType == "Polyculture") %>% dplyr::select(contains("OTU"))
# allPoly <- all %>% filter(FarmType == "Polyculture")
# 
# bd <- betadisper(vegdist(polyOTU, "bray"), allPoly$FarmKey, type=c("centroid"))
# 
# 
# monoOTU <- all %>% filter(FarmType == "Monoculture") %>% dplyr::select(contains("OTU"))
# allMono <- all %>% filter(FarmType == "Monoculture")
# 
# bd <- betadisper(vegdist(monoOTU, "bray"), allMono$FarmKey, type=c("centroid"))
# 
# 
# distDF <- as.data.frame(distUpper) %>%
#   mutate(Key = as.character(all$Key)) %>%
#   pivot_longer(cols=-Key, names_to="compared_to", values_to = "distance") %>%
#   filter(! distance %in% c(0)) %>%
#   left_join(prop %>% mutate(Key=as.character(Key)), by="Key")
# 
# test <- lmer(distance~FarmType*Block + cropDiversity + (1|FarmKey:Year), data=distDF)
# 
# amf_bp <- beta.pair(decostand(amf_otu, "pa"), index.family = "jaccard")
# 
# plot(hclust(amf_bp$beta.sne, method="average"), hang=-1, main='', sub='', xlab='')
# 
# 
# 
# PCoA <- ape::pcoa(amf_bp$beta.jtu)
# 
# axes <- as.data.frame(PCoA$vectors[,1:2])
# axes$Key <- all$Key
# axes <- axes %>%
#   left_join(prop, by= "Key") %>%
#   mutate(Year = as.factor(Year))
# 
# 
# 
# PCoA.plot <- ggplot(data=axes, aes(x=Axis.1,y=Axis.2,  colour= FarmType)) +
#   geom_point(aes(x=Axis.1,y=Axis.2, shape=Block), size=4, alpha=0.85)+
#   # scale_colour_manual(values=c('#dfc27d', '#018571')) +
#   # scale_fill_manual(values=c('#dfc27d', '#018571')) +
#   scale_shape_manual(values = c(16, 1))+
#   theme_classic()
# 
# 
# plot(nestedtemp(amf_otu))

# ## *****************************************************************************
# ## all fungi -  local
# ## *****************************************************************************
# 
# 
# 
# 
# # partial dbRDA model
# dbRDAmodel <- capscale(dist ~  cropDiversity+ pH + P  + TOC + N + NP_ratio  + Condition(farmCode) , data=all)
# 
# 
# # automatically select variables of "env" matrix that best explain "spe" matrix
# finalmodel <- ordistep(dbRDAmodel, scope=formula(dbRDAmodel)) #, direction = "forward")
# 
# # dataframe
# 
# # dbRDA_df <- as.data.frame(finalmodel$CCA$u)
# dbRDA_df <- as.data.frame(finalmodel$CCA$wa)
# dbRDA_df$Key <- as.integer(c(row.names(dbRDA_df)))
# dbRDA_df <- dbRDA_df %>%
#   left_join(ss)
# dbRDA_df_env <- as.data.frame(finalmodel$CCA$biplot)
# 
# ## ------------------------ Information about summary output -----------------------
# # Information about summary output
# # finalmodel.Summary <- head(summary(finalmodel))
# # Total Inertia = total variance in species (observations matrix) distributions
# # Constrained Inertia = variance explained by the environmental variables (gradients matrix)
# # Proportion = percentages of variance of species distributions explained by Constrained (environmental) and Unconstrained variables
# # Eigenvalues = represent the amount of variance explained by each CCA axis (graphs usually present the first two constrained axes)
# ## ----------------------------------  **  ------------------------------------- 
# 
# # Variance Inflation Factors (VIF) for each of the constraints (variables) from the "env" matrix
# ## If we find an environmental variable with VIF>10, we'll know that this variable presents colinearity with another or other variables. In that case, we would have to delete the variable from our initial dataset and redo all the analysis.
# # Collinear variables = CEC, Mg, Ca, N, pH, CN_ratio, Zn, Al,  remove above
# # rdaVIF <- vif.cca(finalmodel)
# ## ----------------------------------  **  ------------------------------------- 
# 
# # Testing the significance of the CCA model:
# # if our whole CCA model, the CCA terms (environmental varibles), and CCA axes explain more variance of "spe" (observations) matrix than expected by chance
# rdaModelSig <- anova.cca(finalmodel)
# 
# # Testing the significance of terms (environmental variables):
# #rdaTermsSig <- anova(finalmodel, by="terms", permutations = 999)
# rdaTermsSig <- anova.cca(finalmodel, by="terms", permutations = 999)
# # rdaTermsSig$R2 <- rdaTermsSig$'SumOfSqs'/sum(rdaTermsSig$'SumOfSqs'[1:13])
# 
# # Testing the significance of CCA axes (at least the first two or three should present a significant p value):
# rdaAxesSig <- anova(finalmodel, by="axis")
# 
# 
# 
# test <- tax %>% filter(Guild == "Arbuscular Mycorrhizal")
# levels(factor(test$Family))
