########################################################################
## clean
########################################################################

########################################################################
## 1. clean and bind 2017 and 2018 soils dataset
########################################################################

# Change FALSE to Focal
prop <- rbind(data2017, data2018)

# rename columns
colnames(prop)[4] <- c("Transect")
colnames(prop)[8] <- c("Block")

prop$farmCode <- paste(prop$Year, prop$FarmKey, sep="_")
prop$FTBL <- paste(prop$FarmType, prop$Block, sep="_")

# change row.names to Key
row.names(prop) <- prop$Key






########################################################################
## 3. add crop plant functional groups
########################################################################

# add crop plant functional groups
prop <- merge(prop,plantID, by="PlantID")



########################################################################
## 4. ADD OTU table
########################################################################

# add OTU table to complete dataeset
otu$Key <- row.names(otu)
prop <- merge(prop, otu, by="Key")


########################################################################
## 5. adjust lat long for each point
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
## 6. subset rows & columns needed
########################################################################

##subset rows needed

wo_extras <- prop %>% drop_na(Lat_point)

##subset columns needed

wo_mock <- wo_extras %>% dplyr::select(-contains("mock")) #remove the mock community OTUs

#rarefy to minimum number of species observed

species_only <- wo_mock %>% dplyr::select(contains("OTU"))

minReads <- min(rowSums(species_only))

species_only.rr <- rrarefy(species_only, sample=minReads)

species.rr_df <- data.frame(species_only.rr)

keys <- wo_extras$Key

species.rr_df$Key <- c(keys)

table(is.na(species.rr_df))


########################################################################
## 7. create input dataframes
########################################################################

#select the environmental variables you want

envi_factors <- c("pH", "OM", "P")

##all farms
#species table

species_table <- wo_mock %>% 
  dplyr::select("Key", "Lat_point", "Long_point") %>% 
  join(species.rr_df)

#environment table

envi_variables <- wo_mock %>% dplyr::select(envi_factors) %>% #add relevant colnames
  lapply(function(x) scale(x, center = FALSE)) %>% 
  as.data.frame() %>%
  add_column(Key = keys)

envi_table <- wo_mock %>% 
  dplyr::select("Key", "Lat_point", "Long_point") %>% 
  join(envi_variables)

#quick checks to make sure there are no missing values
table(is.na(species_table))

table(is.na(envi_table))

which(names(df) == "") #function to find column indices

#monocultures
#species table

mono_species_table <- wo_mock %>%
  filter(FarmType == "Monoculture") %>%
  dplyr::select("Key", "Lat_point", "Long_point") %>% 
  join(species.rr_df) 

mono_keys <- mono_species_table$Key

#environment table

mono_envi_variables <- wo_mock %>% 
  filter(FarmType == "Monoculture") %>%
  dplyr::select(envi_factors) %>% #add relevant colnames
  lapply(function(x) scale(x, center = FALSE)) %>% 
  as.data.frame() %>%
  add_column(Key = mono_keys)

mono_envi_table <- wo_mock %>% 
  dplyr::select("Key", "Lat_point", "Long_point") %>% 
  join(envi_variables)

##Polycultures

#species table

poly_species_table <- wo_mock %>%
  filter(FarmType == "Polyculture") %>%
  dplyr::select("Key", "Lat_point", "Long_point") %>% 
  join(species.rr_df) 

poly_keys <- poly_species_table$Key

#environment table

poly_envi_variables <- wo_mock %>% 
  filter(FarmType == "Polyculture") %>%
  dplyr::select("pH", "OM", "P") %>% #add relevant colnames
  lapply(function(x) scale(x, center = FALSE)) %>% 
  as.data.frame() %>%
  add_column(Key = poly_keys)

poly_envi_table <- wo_mock %>% 
  dplyr::select("Key", "Lat_point", "Long_point") %>% 
  join(envi_variables)



########################################################################
## End
########################################################################
