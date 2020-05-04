########################################################################
## gdm analysis
########################################################################

########################################################################
## all fungi
########################################################################

#all farms 
all_farms_model <- gdm(all_inputs, geo = TRUE)
table(all_farms_model)


#gdm.varImp(formated_tables, geo = TRUE, nPerm = 50, cores=4) #very slow

#gdm plot
plot(all_farms_model, plot.layout = c(1,2))

#monocultures
mono_model <- gdm(mono_inputs, geo = TRUE)
table(mono_model)

#gdm plot
plot(mono_model, plot.layout = c(1,2))


#polycultures
poly_model <- gdm(poly_inputs, geo = TRUE)
#summary(poly_model)
table(poly_model)

#gdm plot
plot(poly_model, plot.layout = c(1,2))


########################################################################
## amf
########################################################################


# create model
all_amf_model <- gdm(all_amf, geo = TRUE)
table(all_amf_model)

#gdm plot
plot(all_amf_model, plot.layout = c(1,2))


# monocultures
## Given how little deviance is explained by this model, this it make more sense to try a bunch a different parameters and see if those explain a bit more?
## I wonder whether this is heavily influenced by host plant? Since it will be the same host plant throughout the site, but might differ across sites?
## Might expect geographic to help model this, but might be messed up if the same crop is planted at different distances

mono_model_amf <- gdm(mono_inputs_amf, geo = TRUE)
table(mono_model_amf)

#gdm plot
plot(mono_model_amf, plot.layout = c(1,2))


#polycultures
poly_model_amf <- gdm(poly_inputs_amf, geo = TRUE)
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


modeldiss <- gdm(formated_tables, geo = TRUE)

table(modeldiss)

modelreg <- gdmModel(amf_fd)
table(modelreg)


########################################################################
## dissimilarity matrix
########################################################################

model1 <- gdm(formated_tables1, geo = TRUE)

table(model1)

plot(model1, plot.layout = c(1,2))










