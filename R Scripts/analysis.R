########################################################################
## gdm analysis
########################################################################


########################################################################
## 1. prepare site-pair tables
########################################################################

test_gdm_table <- formatsitepair(species_table, bioFormat=1, XColumn="Long_point", YColumn="Lat_point",
                                 siteColumn="Key", predData=envi_table)

table(is.na(test_gdm_table))

gdmModel <- gdm(test_gdm_table, geo= TRUE)
summary(gdmModel)


length(gdmModel$predictors)

plot(gdmModel, plot.layout = c(2,2))


########################################################################
## mantel tests
########################################################################

# transformed OTU table 
h <- decostand(OTUcomp, method = "hellinger")
pa <- decostand(OTUcomp, method = "pa")

# CREATE DISSIMILARITY MATRIX
dist.h = as.matrix((vegdist(h, "bray")))






