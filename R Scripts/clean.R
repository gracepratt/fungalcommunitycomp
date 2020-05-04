########################################################################
## clean
########################################################################

########################################################################
## 1. clean and bind 2017 and 2018 soils dataset
########################################################################

# Enter 2017-2018 data as prop (since the relational database combines both now )
# Only include non-OTU columns 
prop <- data[1:84]

# rename columns
colnames(prop)[4] <- c("Transect")
colnames(prop)[8] <- c("Block")

prop$farmCode <- paste(prop$Year, prop$FarmKey, sep="_")
prop$FTBL <- paste(prop$FarmType, prop$Block, sep="_")

# change row.names to Key
row.names(prop) <- prop$Key


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
prop$Long_point <- latlong$Long[match( interaction(prop$FarmKey, prop$Transect, prop$Point), interaction(latlong$FarmKey, latlong$Transect, latlong$Point))]

########################################################################
## 3. adding mono vs poly as a binary (0/1) variable
########################################################################

prop <- prop %>% mutate(FarmBi = recode(FarmType, "Monoculture" = 1,
                                          "Polyculture" = 0))

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
amf_otu_100$Key <- prop$Key
  
amf <- prop %>%
  join(amf_otu_100) %>% 
  drop_na(Lat_point)

#taking out samples with no AMF
amf$rowsum <- rowSums(amf %>% dplyr::select(contains("OTU")))

amf <- amf %>% filter(rowsum > 0)


########################################################################
## 6. create input dataframes
########################################################################

# choose the variables you want

envi_factors <- c("pH", "P", "CEC", "TOC","N", "CN_ratio")  #testing

########################################################################
## all fungi
########################################################################

## all farms

all_inputs<- input_tables(all_fungi, envi_factors)

#monoculture

monocultures <- all_fungi %>%
  filter(FarmType == "Monoculture")

mono_inputs <- input_tables(monocultures, envi_factors)

#polyculture

polycultures <- all_fungi %>%
  filter(FarmType == "Polyculture")

poly_inputs <- input_tables(polycultures, envi_factors)



########################################################################
## amf
########################################################################

all_amf <- input_tables(amf, envi_factors)

#monoculture

monocultures_amf <- amf %>%
  filter(FarmType == "Monoculture")

mono_inputs_amf <- input_tables(monocultures_amf, envi_factors)

#polyculture

polycultures_amf <- amf %>%
  filter(FarmType == "Polyculture")

poly_inputs_amf <- input_tables(polycultures, envi_factors)


########################################################################
## creating dissimilarity input 
########################################################################

#all fungi
all_diss <- input_diss(all_fungi, envi_factors)


########################################################################
## functional groups with function
########################################################################

#amf
amf_filter <- guild_filter(all_fungi, "Arbuscular Mycorrhizal")
amf_fd_inputs_d <- input_diss(amf_filter, envi_factors)
amf_fd_inputs <- input_tables(amf_filter, envi_factors)

#plant pathogen
plant_pathogen <- guild_filter(all_fungi, "Plant Pathogen")
plant_path_inputs_d <- input_diss(plant_pathogen, envi_factors)
plant_path_inputs <- input_tables(plant_pathogen, envi_factors)

#saprotroph
all_saprotroph <- guild_filter(all_fungi, "Saprotroph")
sap_inputs_d <- input_diss(all_saprotroph, envi_factors)
sap_inputs <- input_tables(all_saprotroph, envi_factors)

#fungal parasite
fungal_par <- guild_filter(all_fungi, "Fungal Parasite")
fungal_par_inputs_d <- input_diss(fungal_par, envi_factors)
fungal_par_inputs <- input_tables(fungal_par, envi_factors)




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