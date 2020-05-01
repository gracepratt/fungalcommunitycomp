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

#latlong$BlockPoint <- paste(latlong$Block, latlong$Point, sep="_")
#prop$BlockPoint <- paste(prop$Block, prop$Point, sep="_")

prop$Lat_point <- latlong$Lat[match( interaction(prop$FarmKey, prop$Transect, prop$Point), interaction(latlong$FarmKey, latlong$Transect, latlong$Point))]
prop$Long_point <- latlong$Long[match( interaction(prop$FarmKey, prop$Transect, prop$Point), interaction(latlong$FarmKey, latlong$Transect, latlong$Point))]

########################################################################
## 3. adding mono vs poly as a binary (0/1) variable
########################################################################

prop_b <- prop %>% mutate(FarmBi = recode(FarmType, "Monoculture" = 1,
                                          "Polyculture" = 0))

########################################################################
## 3. rarefy dataset with all fungi
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
## 4. Add OTU tables
########################################################################

# add rarefied OTU table to complete dataset
all_fungi <- prop_b %>% 
  join(species.rr_df) %>% 
  drop_na(Lat_point)

#add AMF table for AMF dataset
amf_otu_100$Key <- prop$Key
  
amf <- prop_b %>%
  join(amf_otu) %>% 
  drop_na(Lat_point)

#taking out samples with no AMF
amf$rowsum <- rowSums(amf %>% dplyr::select(contains("OTU")))

amf <- amf %>% filter(rowsum > 0)


########################################################################
## 6. create input dataframes
########################################################################

# choose the variables you want
# Adding CN_ratio, TOC and N, removing OM

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
## creating dissimilarity input (Might remove)
########################################################################

species <- all_fungi%>%
  dplyr::select(contains("OTU"))


dist.sp <- as.matrix(vegdist(species, "bray"))

species_table1 <- cbind(all_fungi$Key, dist.sp) %>% 
  data.frame() %>% 
  rename("Key" = 'V1')

envi_table1 <- all_fungi %>%
  dplyr::select("Key", "Long_point", "Lat_point", envi_factors)

formated_tables1 <- formatsitepair(species_table1, bioFormat=3, XColumn="Long_point", YColumn="Lat_point",
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
  drop_na() %>%
  filter(value > 0)
  
#group by key and guild WRONG
grouped <- wGuild %>% 
  group_by(Key, Guild) %>% 
  summarise(count = n(), sum = sum(value)) 

#re-pivot back to wide format with guilds as column names

#count
wideCount <- grouped %>%
  dplyr::select("Key", "Guild", "count") %>%
  pivot_wider(names_from = Guild, values_from = count)

wideCount[is.na(wideCount)] <- 0 #change NAs to 0

#sum
wideSum <- grouped %>%
  dplyr::select("Key", "Guild", "sum") %>%
  pivot_wider(names_from = Guild, values_from = sum)

wideSum[is.na(wideSum)] <- 0

#rejoin with full table 
fdCount <- all_fungi %>%
  dplyr::select(-contains("OTU")) %>%
  join(wideCount)

fdSum <- all_fungi %>%
  dplyr::select(-contains("OTU")) %>%
  join(wideSum)
  




########################################################################
## input tables functional groups (might remove)
########################################################################

species_table <- fdSum %>% 
  dplyr::select("Key", "Long_point", "Lat_point", c(90:177))

envi_table <- fdSum %>% 
  dplyr::select("Key", "Long_point", "Lat_point", envi_factors)



formated_tables <- formatsitepair(species_table, bioFormat=1, XColumn="Long_point", YColumn="Lat_point",
                                  siteColumn="Key", predData= envi_table, abundance = FALSE)





















########################################################################
## End
########################################################################