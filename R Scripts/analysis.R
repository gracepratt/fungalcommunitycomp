########################################################################
## gdm analysis
########################################################################

########################################################################
## all fungi
########################################################################

#all farms 
all_farms_model <- gdmModel(all_inputs)
#summary(all_farms_model)
table(all_farms_model)


gdm.varImp(formated_tables, geo = TRUE, nPerm = 40) #very slow

#gdm plot
plot(all_farms_model, plot.layout = c(1,2))

#monocultures
mono_model <- gdmModel(mono_inputs)
#summary(mono_model)
table(mono_model)

#polycultures
poly_model <- gdmModel(poly_inputs)
#summary(poly_model)
table(poly_model)


########################################################################
## amf
########################################################################


#create model
all_amf_model <- gdmModel(all_amf)
#summary(mono_model)
table(all_amf_model)


#gdm plot
plot(all_amf_model, plot.layout = c(1,2))


#monocultures
mono_model_amf <- gdmModel(mono_inputs_amf)
#summary(mono_model)
table(mono_model_amf)

#gdm plot
plot(mono_model_amf, plot.layout = c(1,2))


#polycultures
poly_model_amf <- gdmModel(poly_inputs_amf)
#summary(poly_model)
table(poly_model_amf)

#gdm plot
plot(poly_model_amf, plot.layout = c(1,2))





########################################################################
## mantel tests
########################################################################

all_mantel <- mantel_func(all_inputs)
mono_mantel <- mantel_func(mono_inputs)
poly_mantel <- mantel_func(poly_inputs)



#environment vs distance

#all farms

envi <- all_inputs[[2]] %>% dplyr::select(-"Key", -"Lat_point", -"Long_point")
geo <- all_inputs[[2]] %>% dplyr::select("Long_point", "Lat_point")


dist.envi <- as.matrix(dist(envi, method = "euclidean"))
dist.geo <- distm(geo, fun = distHaversine)

all_mantel_envi <- mantel(dist.envi, dist.geo, method = "spearman")


#monoculture
envi <- mono_inputs[[2]] %>% dplyr::select(-"Key", -"Lat_point", -"Long_point")
geo <- mono_inputs[[2]] %>% dplyr::select("Long_point", "Lat_point")


dist.envi <- as.matrix(dist(envi, method = "euclidean"))
dist.geo <- distm(geo, fun = distHaversine)

mono_mantel_envi <- mantel(dist.envi, dist.geo, method = "spearman")


#polyculture
envi <- poly_inputs[[2]] %>% dplyr::select(-"Key", -"Lat_point", -"Long_point")
geo <- poly_inputs[[2]] %>% dplyr::select("Long_point", "Lat_point")


dist.envi <- as.matrix(dist(envi, method = "euclidean"))
dist.geo <- distm(geo, fun = distHaversine)

poly_mantel_envi <- mantel(dist.envi, dist.geo, method = "spearman")




########################################################################
## functional groups
########################################################################


model <- gdm(formated_tables, geo = TRUE)

table(model)



########################################################################
## dissimilarity matrix
########################################################################

model1 <- gdm(formated_tables1, geo = TRUE)

table(model1)










