########################################################################
## gdm analysis
########################################################################

########################################################################
## all fungi
########################################################################

#all farms 
all_farms_model <- gdmModel(all_inputs)
summary(all_farms_model)

#gdm.varImp(test_gdm_table, geo = TRUE, nPerm = 10) #very slow

#gdm plot
plot(gdmModel, plot.layout = c(1,2))

#monocultures
mono_model <- gdmModel(mono_inputs)
summary(mono_model)

#polycultures
poly_model <- gdmModel(poly_inputs)
summary(poly_model)

#building table function

Predictors <- c(poly_model$predictors, "Percent Deviance Explained")

poly_coef <- poly_model$coefficients

i <- 1
coeffs <- c()
while(i < length(poly_coef)){
  coeffs <- c(coeffs, sum(poly_coef[i:(i+2)]))
  i <- i+3
}

Coefficients <- c(coeffs, poly_model$explained)

table <- data.frame(Predictors, Coefficients)
  

  


########################################################################
## amf
########################################################################

amf_allfarms <- formatsitepair(all_amf[[1]], bioFormat=1, XColumn="Long_point", YColumn="Lat_point",
                                 siteColumn="Key", predData=  all_amf[[2]])

#create model
amfModel <- gdm(amf_allfarms, geo= TRUE)
summary(amfModel)




########################################################################
## mantel tests
########################################################################


# transformed OTU table 
h <- decostand(species.rr_df, method = "hellinger")
pa <- decostand(species.rr_df, method = "pa")

# CREATE DISSIMILARITY MATRIX
dist.h <- as.matrix((vegdist(h, "bray")))

dist.pa <- as.matrix((vegdist(pa, "bray")))

#dist.envi <- as.matrix((vegdist(envi_table, "bray")))

dist.envi <- as.matrix(dist(envi_table, method = "euclidean"))

#geo table
geo <- wo_mock %>% dplyr::select("Long_point", "Lat_point")

dist.geo <- distm(geo, fun = distHaversine) #not sure if I should be using this


#abundace vs. environment

mantel_envi_h <- mantel(dist.h, dist.envi, method = "spearman")

mantel_envi_pa <- mantel(dist.pa, dist.envi)


#abundance vs. geography

mantel(dist.h, dist.geo, method = "spearman")

mantel(dist.pa, dist.geo)






