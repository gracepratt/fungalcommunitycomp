########################################################################
## gdm analysis
########################################################################

##all farms

# prepare site-pair tables

test_gdm_table <- formatsitepair(species_table, bioFormat=1, XColumn="Long_point", YColumn="Lat_point",
                                 siteColumn="Key", predData=envi_table)

table(is.na(test_gdm_table))

#create model
gdmModel <- gdm(test_gdm_table, geo= TRUE)
summary(gdmModel)

#plot
plot(gdmModel, plot.layout = c(2,2))

##monocultures

mono_gdm_table <- formatsitepair(mono_species_table, bioFormat=1, XColumn="Long_point", YColumn="Lat_point",
                            siteColumn="Key", predData=mono_envi_table)

#create model
gdm_mono_model <- gdm(mono_gdm_table, geo= TRUE)
summary(gdm_mono_model)

#plot
plot(gdm_mono_model, plot.layout = c(2,2))

##polycultures

poly_gdm_table <- formatsitepair(poly_species_table, bioFormat=1, XColumn="Long_point", YColumn="Lat_point",
                                 siteColumn="Key", predData=poly_envi_table)

#create model
gdm_poly_model <- gdm(poly_gdm_table, geo= TRUE)
summary(gdm_poly_model)

#plot
plot(gdm_poly_model, plot.layout = c(2,2))


gdm.varImp(test_gdm_table, geo = TRUE, nPerm = 10)


########################################################################
## mantel tests
########################################################################

# transformed OTU table 
h <- decostand(OTUcomp, method = "hellinger")
pa <- decostand(OTUcomp, method = "pa")

# CREATE DISSIMILARITY MATRIX
dist.h = as.matrix((vegdist(h, "bray")))






