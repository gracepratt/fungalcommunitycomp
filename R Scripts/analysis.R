########################################################################
## gdm analysis
########################################################################

########################################################################
## all fungi
########################################################################

#all farms 
all_farms_model <- gdmModel(all_inputs)

table(all_farms_model)


#gdm.varImp(formated_tables, geo = TRUE, nPerm = 50, cores=4) #very slow

#gdm plot
plot(all_farms_model, plot.layout = c(1,2))

#monocultures
mono_model <- gdmModel(mono_inputs)

table(mono_model)

#gdm plot
plot(mono_model, plot.layout = c(1,2))


#polycultures
poly_model <- gdmModel(poly_inputs)
#summary(poly_model)
table(poly_model)

#gdm plot
plot(poly_model, plot.layout = c(1,2))


########################################################################
## amf
########################################################################


# create model
all_amf_model <- gdmModel(all_amf)
#summary(mono_model)
table(all_amf_model)

#gdm plot
plot(all_amf_model, plot.layout = c(1,2))


# monocultures
## Given how little deviance is explained by this model, this it make more sense to try a bunch a different parameters and see if those explain a bit more?
## I wonder whether this is heavily influenced by host plant? Since it will be the same host plant throughout the site, but might differ across sites?
## Might expect geographic to help model this, but might be messed up if the same crop is planted at different distances

mono_model_amf <- gdmModel(mono_inputs_amf)

table(mono_model_amf)

#gdm plot
plot(mono_model_amf, plot.layout = c(1,2))


#polycultures
poly_model_amf <- gdmModel(poly_inputs_amf)

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



#TESTING to figure out these euclidean distances WILL BE REMOVED!
envi_1 <- envi[1:2,]
envi_ph <- envi_1$pH
envi_p <- envi_1$P
envi_cec <- envi_1$CEC
envi_toc <- envi_1$TOC
envi_n <- envi_1$N
envi_cn <- envi_1$CN_ratio

dist.envi_1 <- dist(envi_1, method = "euclidean")
dist.ph <- dist(envi_ph, method = "euclidean")
dist.p <- dist(envi_p, method = "euclidean")
dist.cec <- dist(envi_cec, method = "euclidean")
dist.toc <- dist(envi_toc, method = "euclidean")
dist.n <- dist(envi_n, method = "euclidean")
dist.cn <- dist(envi_cn, method = "euclidean")

geo_1 <- geo[1:2,]
dist.geo_1 <- dist(geo_1, method = "euclidean")

#this confirms something I already thought, which is that the euclidean distance between 2 points is not the sum of the euclidean distances between all of the axes
sum(dist.ph, dist.p, dist.cec, dist.toc, dist.n, dist.cn)


ecologic_dist <- all_farms_model$ecological

isplines <- isplineExtract(all_farms_model)

#pH 
isplines$x[178,2]
isplines$y[178,2]

isplines$y[174,2]

dist_ph <- isplines$y[178,2]-isplines$y[174,2]

#P
dist_p <- isplines$y[33,3] - isplines$y[29,3]

sum(dist_p, dist_ph)



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

plot(model1, plot.layout = c(1,2))










