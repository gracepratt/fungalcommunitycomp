########################################################################
## clean
########################################################################

########################################################################
## 1. clean and bind 2017 and 2018 soils dataset
########################################################################

# Combine 2017 and 2018 data
prop <- rbind(data2017, data2018)

# rename columns
colnames(prop)[4] <- c("Transect")
colnames(prop)[8] <- c("Block")

prop$farmCode <- paste(prop$Year, prop$FarmKey, sep="_")
prop$FTBL <- paste(prop$FarmType, prop$Block, sep="_")

# change row.names to Key
row.names(prop) <- prop$Key

########################################################################
## 2. add crop plant functional groups
########################################################################

# add crop plant functional groups
prop <- merge(prop,plantID, by="PlantID")


########################################################################
## 3. adjust lat long for each point
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

#latlong$BlockPoint <- paste(latlong$Block, latlong$Point, sep="_")
#prop$BlockPoint <- paste(prop$Block, prop$Point, sep="_")

prop$Lat_point <- latlong$Lat[match( interaction(prop$FarmKey, prop$Transect, prop$Point), interaction(latlong$FarmKey, latlong$Transect, latlong$Point))]
prop$Long_point <- latlong$Long[match( interaction(prop$FarmKey, prop$Transect, prop$Point), interaction(latlong$FarmKey, latlong$Transect, latlong$Point))]



########################################################################
## 4. rarefy dataset with all fungi
########################################################################

#add key to OTUs
otu$Key <- row.names(otu)

#remove the mock community OTUs

wo_mock <- otu %>% dplyr::select(-contains("mock")) 

#rarefy to minimum number of species observed

species_only <- wo_mock %>% dplyr::select(contains("OTU"))

minReads <- min(rowSums(species_only))

species_only.rr <- rrarefy(species_only, sample=minReads)

species.rr_df <- data.frame(species_only.rr)

species.rr_df$Key <- otu$Key

sum(is.na(species.rr_df))


########################################################################
## 5. Add OTU tables
########################################################################

# add rarefied OTU table to complete dataset

all_fungi <- prop %>% 
  join(species.rr_df) %>% 
  drop_na(Lat_point)

#add AMF table for AMF dataset

amf_otu$Key <- prop$Key
  
amf <- prop %>%
  join(amf_otu) %>% 
  drop_na(Lat_point)

#taking out samples with no AMF
amf$rowsum <- rowSums(amf %>% dplyr::select(contains("OTU")))

amf <- amf %>% filter(rowsum > 0)


########################################################################
## 6. create input dataframes
########################################################################

# choose the variables you want

envi_factors <- c("pH", "OM", "P", "CEC")  

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
## adding functional groups
########################################################################

rawguilds <- tax %>%
  dplyr::select("X.OTU.ID", "Guild")


justOTU <- all_fungi %>% 
  dplyr::select(contains("OTU")) %>% 
  colnames() %>%
  data.frame() %>%
  rename("X.OTU.ID" = ".")

wGuilds <- justOTU %>% 
  left_join(rawguilds, by = "X.OTU.ID")

guilds <- wGuilds %>% dplyr::select("Guild")


OTUColumns <- all_fungi %>% 
  dplyr::select(contains("OTU"))



colnames(OTUColumns) <- guilds$Guild

transposed <- t(OTUColumns)

########################################################################
## End
########################################################################