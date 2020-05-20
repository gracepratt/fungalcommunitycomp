########################################################################
## clean
########################################################################

########################################################################
## 1. clean and bind 2017 and 2018 soils dataset
########################################################################

# Enter 2017-2018 data as prop (since the relational database combines both now )
# prop <- rbind(data2017, data2018)

# Only include non-OTU columns 
prop <- data[1:84] # nrow=378, ncol=84
# nrow=378, ncol=84

# rename columns
colnames(prop)[4] <- c("Transect")
colnames(prop)[8] <- c("Block")

prop$farmCode <- paste(prop$Year, prop$FarmKey, sep="_")
prop$FTBL <- paste(prop$FarmType, prop$Block, sep="_")

# create N:P ratio column

prop$NP_ratio <- (prop$N/prop$P)*100

# change row.names to Key
row.names(prop) <- prop$Key # nrow=378, ncol=86
# nrow=378, ncol=86



########################################################################
## 2. adjust lat long for each point
########################################################################

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

prop$Lat_point <- latlong$Lat[match( interaction(prop$FarmKey, prop$Transect, prop$Point), interaction(latlong$FarmKey, latlong$Transect, latlong$Point))]
prop$Long_point <- latlong$Long[match( interaction(prop$FarmKey, prop$Transect, prop$Point), interaction(latlong$FarmKey, latlong$Transect, latlong$Point))] # nrow=378, ncol=88
# nrow=378, ncol=88

########################################################################
## 3. adding mono vs poly as a binary (0/1) variable
########################################################################

prop <- prop %>% mutate(FarmBi = recode(FarmType, "Monoculture" = 1,
                                          "Polyculture" = 0)) # nrow=378, ncol=89

# nrow=378, ncol=89
 
########################################################################
## 3. rarefy dataset with all fungi
########################################################################

#add key to OTUs
otu$Key <- row.names(otu)

#remove the mock community OTUs
wo_mock <- otu %>% dplyr::select(-contains("mock")) 

#rarefy to minimum number of sequences observed
species_only <- wo_mock %>% dplyr::select(contains("OTU"))

minReads <- min(rowSums(species_only))

species.rr_df <- data.frame(rrarefy(species_only, sample=minReads))

species.rr_df$Key <- otu$Key


########################################################################
## 4. Add OTU tables
########################################################################

# add rarefied OTU table to complete dataset
all_fungi <- prop %>% 
  join(species.rr_df) %>% 
  drop_na(Lat_point)

#add AMF table for AMF dataset
amf_otu$Key <- row.names(amf_otu) # nrow=378, ncol=245
# nrow=378, ncol=245
  
amf <- prop %>%
  join(amf_otu) %>% 
  drop_na(Lat_point) # nrow=372, ncol=333
# nrow=372, ncol=333

#taking out samples with no AMF
amf$rowsum <- rowSums(amf %>% dplyr::select(contains("OTU"))) # nrow=372, ncol=334
# nrow=372, ncol=334

amf <- amf %>% filter(rowsum > 0) # nrow=322, ncol=334
 # nrow=322, ncol=334

# # add vegan calculations for chao1
# richness <- t(as.data.frame(estimateR(amf %>% dplyr::select( contains("OTU")))))
# diversity <- data.frame(diversity=diversity(amf %>% dplyr::select( contains("OTU"))))
# amf <- cbind(amf, richness[,1:2], diversity)
# 


########################################################################
## 6. create input dataframes
########################################################################

# choose the variables you want

envi_factors <- c("pH", "P", "TOC", "N", "NP_ratio", "FarmBi")  #testing


########################################################################
## all fungi
########################################################################

## all farms

all_inputs<- input_tables(all_fungi, envi_factors) # nrow=69006, ncol=14
# nrow=372, ncol=3428, # nrow=372, ncol=7
all_diss <- input_diss(all_fungi, envi_factors)

#monoculture
monocultures <- all_fungi %>%
  filter(FarmType == "Monoculture") 

mono_inputs <- input_tables(monocultures, envi_factors) 
mono_diss <- input_diss(monocultures, envi_factors)

#polyculture
#nrow=192
polycultures <- all_fungi %>%
  filter(FarmType == "Polyculture")

poly_inputs <- input_tables(polycultures, envi_factors) 
poly_diss <- input_diss(polycultures, envi_factors)



########################################################################
## amf
########################################################################


all_amf <- input_tables(amf, envi_factors) # nrow=51681, ncol=14
# [[1]] nrow=322, ncol=247, nrow=322, ncol=7
all_amf_diss <- input_diss(amf, envi_factors)

#monoculture

monocultures_amf <- amf %>%
  filter(FarmType == "Monoculture") #nrow=161, ncol=334
#nrow=161, ncol=334

mono_inputs_amf <- input_tables(monocultures_amf, envi_factors) #nrow=12880, ncol=14
# [[1]] nrow=161, ncol=247, [[2]] nrow=161, ncol=7

mono_diss_amf <- input_diss(monocultures_amf, envi_factors)

#polyculture

polycultures_amf <- amf %>%
  filter(FarmType == "Polyculture") #nrow=161, ncol=334
#nrow=161, ncol=334

poly_inputs_amf <- input_tables(polycultures_amf, envi_factors) #nrow=12880, ncol=14
# [[1]] nrow=161, ncol=247, [[2]] nrow=161, ncol=7
poly_diss_amf <- input_diss(polycultures_amf, envi_factors)


########################################################################
## functional groups with function
########################################################################

#amf
amf_filter <- guild_filter(all_fungi, "Arbuscular Mycorrhizal")
amf_mono_filter <- guild_filter(monocultures, "Arbuscular Mycorrhizal")
amf_poly_filter <- guild_filter(polycultures, "Arbuscular Mycorrhizal")

amf_fd_inputs_d <- input_diss(amf_filter, envi_factors)

amf_fd_inputs <- input_tables(amf_filter, envi_factors)
amf_mono_inputs <- input_tables(amf_mono_filter, envi_factors)
amf_poly_inputs <- input_tables(amf_poly_filter, envi_factors)

#plant pathogen
plant_pathogen <- guild_filter(all_fungi, "Plant Pathogen")
plant_mono  <- guild_filter(monocultures, "Plant Pathogen")
plant_poly <- guild_filter(polycultures, "Plant Pathogen")

plant_path_inputs_d <- input_diss(plant_pathogen, envi_factors)

plant_path_inputs <- input_tables(plant_pathogen, envi_factors)
plant_mono_inputs <- input_tables(plant_mono, envi_factors)
plant_poly_inputs <- input_tables(plant_poly, envi_factors)

#saprotroph
all_saprotroph <- guild_filter(all_fungi, "Saprotroph")
sap_mono <- guild_filter(monocultures, "Saprotroph")
sap_poly <- guild_filter(polycultures, "Saprotroph")

sap_inputs_d <- input_diss(all_saprotroph, envi_factors)

sap_inputs <- input_tables(all_saprotroph, envi_factors)
sap_mono_inputs <- input_tables(sap_mono, envi_factors)
sap_poly_inputs <- input_tables(sap_poly, envi_factors)

#fungal parasite
fungal_par <- guild_filter(all_fungi, "Fungal Parasite")
fungal_mono <- guild_filter(monocultures, "Fungal Parasite")
fungal_poly <- guild_filter(polycultures, "Fungal Parasite")

fungal_par_inputs_d <- input_diss(fungal_par, envi_factors)

fungal_par_inputs <- input_tables(fungal_par, envi_factors)
fungal_mono_inputs <- input_tables(fungal_mono, envi_factors)
fungal_poly_inputs <- input_tables(fungal_poly, envi_factors)




########################################################################
## alpha diversity of functional groups
########################################################################


alpha_AMF <- as.data.frame(microbiome::alpha(t(amf_filter %>% dplyr::select(contains("OTU"))), index=c("diversity_shannon", "observed"))) %>%
  rename(obs_amf = observed, div_amf=diversity_shannon) %>%
  mutate(Key = amf_filter$Key)

alpha_pathogen <- as.data.frame(microbiome::alpha(t(plant_pathogen %>% dplyr::select(contains("OTU"))), index=c("diversity_shannon", "observed"))) %>%
  rename(obs_path = observed, div_path=diversity_shannon) %>%
  mutate(Key = plant_pathogen$Key)

alpha_sap <- as.data.frame(microbiome::alpha(t(all_saprotroph %>% dplyr::select(contains("OTU"))), index=c("diversity_shannon", "observed"))) %>%
  rename(obs_sap = observed, div_sap=diversity_shannon) %>%
  mutate(Key = all_saprotroph$Key)

alpha_par <- as.data.frame(microbiome::alpha(t(fungal_par %>% dplyr::select(contains("OTU"))), index=c("diversity_shannon", "observed"))) %>%
  rename(obs_par = observed, div_par=diversity_shannon) %>%
  mutate(Key = fungal_par$Key)


alphaDF <-all_fungi[1:89] %>%
  left_join(all_fungialpha_AMF, alpha_pathogen,alpha_sap,alpha_par, by="Key" )






########################################################################
## SCRATCH
########################################################################

########################################################################
## dissimilarity inputs
########################################################################


species_table1 <- all_fungi%>%
  dplyr::select(contains("OTU"))


dist.sp1 <- as.matrix(vegdist(species_table1, "bray"))

species_matrix1 <- cbind(all_fungi$Key, dist.sp1) 

colnames(species_matrix1)[1] <- "Key"

envi_table1 <- all_fungi %>%
  dplyr::select("Key", "Long_point", "Lat_point", envi_factors)

formated_tables1 <- formatsitepair(species_matrix1, bioFormat=3, XColumn="Long_point", YColumn="Lat_point",
                                  siteColumn="Key", predData= envi_table1, abundance = FALSE)




########################################################################
## adding functional groups
########################################################################


#pull out OTU columns and transpose to make 1 row per Key and OTU
OTURows<- all_fungi %>% 
  dplyr::select(Key, contains("OTU")) %>%
  pivot_longer(cols = starts_with("OTU"), names_to = "OTU")

#pull out names of guilds
rawguilds <- tax %>%
  dplyr::select("OTU" = "X.OTU.ID", "Guild")

#join guild names with OTU in each key 
wGuild <- OTURows %>%
  left_join(rawguilds) %>%
  drop_na()


#subsetting OTUs (need to make a function eventually probably)

amf_fd <- wGuild %>% 
  filter(str_detect(Guild, pattern = "Arbuscular Mycorrhizal")) 


#re-pivot back to wide format 

amf_wide <- amf_fd %>%
  dplyr::select("Key", "OTU", "value") %>%
  pivot_wider(names_from = OTU, values_from = value)

amf_wide[is.na(amf_wide)] <- 0

#rejoin with full table 
fdSum<- all_fungi %>%
  dplyr::select(-contains("OTU")) %>%
  join(amf_wide)


#taking out samples with no AMF
fdSum$rowsum <- rowSums(fdSum %>% dplyr::select(contains("OTU")))

fdSum <- fdSum %>% filter(rowsum > 0)


amf_fd_inputs <- input_diss(fdSum, envi_factors)

amf_fd_gdm <- gdm(amf_fd_inputs, geo = TRUE)
















########################################################################
## End
########################################################################