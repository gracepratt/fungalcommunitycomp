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

amf_allfarms <- formatsitepair(all_amf[[1]], bioFormat=1, XColumn="Long_point", YColumn="Lat_point",
                                 siteColumn="Key", predData=  all_amf[[2]])

#create model
amfModel <- gdm(amf_allfarms, geo= TRUE)
summary(amfModel)




########################################################################
## mantel tests
########################################################################

all_mantel <- mantel_func(all_inputs)
mono_mantel <- mantel_func(mono_inputs)
poly_mantel <- mantel_func(poly_inputs)


