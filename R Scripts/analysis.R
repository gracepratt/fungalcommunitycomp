########################################################################
## gdm analysis
########################################################################

########################################################################
## all fungi
########################################################################

#all farms 
all_farms_model <- gdmModel(all_inputs)
summary(all_farms_model)
table(all_farms_model)

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


