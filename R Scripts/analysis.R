########################################################################
## gdm analysis
########################################################################

########################################################################
## all fungi
########################################################################

#all farms 
all_farms_model <- gdm(all_inputs, geo = TRUE)
table(all_farms_model) #better by 14%

all_diss_model <- gdm(all_diss, geo = TRUE)
table(all_diss_model)


#gdm.varImp(formated_tables, geo = TRUE, nPerm = 50, cores=4) #very slow


#monocultures
mono_model <- gdm(mono_inputs, geo = TRUE)
table(mono_model) #better by 14%

mono_diss_model <- gdm(mono_diss, geo = TRUE)
table(mono_diss_model)


#polycultures
poly_model <- gdm(poly_inputs, geo = TRUE)
table(poly_model) # better by 19%

poly_diss_model <- gdm(poly_diss, geo = TRUE)
table(poly_diss_model)




########################################################################
## amf
########################################################################


# create model
all_amf_model <- gdm(all_amf, geo = TRUE)
table(all_amf_model)

amf_diss_model <- gdm(all_amf_diss, geo = TRUE)
table(amf_diss_model) # Better by 1%



# monocultures
## Given how little deviance is explained by this model, this it make more sense to try a bunch a different parameters and see if those explain a bit more?
## I wonder whether this is heavily influenced by host plant? Since it will be the same host plant throughout the site, but might differ across sites?
## Might expect geographic to help model this, but might be messed up if the same crop is planted at different distances

mono_model_amf <- gdm(mono_inputs_amf, geo = TRUE)
table(mono_model_amf) 

mono_amf_diss_model <- gdm(mono_diss_amf, geo = TRUE)
table(mono_amf_diss_model) #better by 3%


#polycultures
poly_model_amf <- gdm(poly_inputs_amf, geo = TRUE)
table(poly_model_amf) #better by 20%

poly_amf_diss_model <- gdm(poly_diss_amf, geo = TRUE)
table(poly_amf_diss_model)






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

#AMF
amf_fd <- gdm(amf_fd_inputs, geo = TRUE)
table(amf_fd)

amf_fd_d <- gdm(amf_fd_inputs_d, geo = TRUE)
table(amf_fd_d) #better by 2%


#plant pathogens
plant_path <- gdm(plant_path_inputs, geo = TRUE)
table(plant_path) #better by 7%

plant_path_d <- gdm(plant_path_inputs_d, geo = TRUE)
table(plant_path_d)

#saprotroph
saprotroph <- gdm(sap_inputs, geo = TRUE)
table(saprotroph) #better by 10%

saprotroph_d <- gdm(sap_inputs_d, geo = TRUE)
table(saprotroph_d)

#fungal parasite
fungal_parasite <- gdm(fungal_par_inputs, geo = TRUE)
table(fungal_parasite) #better by 2%

fungal_parasite_d <- gdm(fungal_par_inputs_d, geo = TRUE)
table(fungal_parasite_d)












