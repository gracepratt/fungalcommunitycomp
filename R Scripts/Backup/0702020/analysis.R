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

#monocultures
mono_model <- gdm(mono_inputs, geo = TRUE)
table(mono_model) 

#polycultures
poly_model <- gdm(poly_inputs, geo = TRUE)
table(poly_model) 


########################################################################
## amf
########################################################################


# create model
all_amf_model <- gdm(all_amf, geo = TRUE)
table(all_amf_model)

# monocultures
mono_model_amf <- gdm(mono_inputs_amf, geo = TRUE)
table(mono_model_amf) 

#polycultures
poly_model_amf <- gdm(poly_inputs_amf, geo = TRUE)
table(poly_model_amf) 




########################################################################
## amf functional groups
########################################################################

#AMF
amf_fd <- gdm(amf_fd_inputs, geo = TRUE)
table(amf_fd)

amf_fd_mono <- gdm(amf_mono_inputs, geo = TRUE)
table(amf_fd_mono)

amf_fd_poly <- gdm(amf_poly_inputs, geo = TRUE)
table(amf_fd_poly)


########################################################################
## FTBL
########################################################################

#all fungi

mono_f_model <- gdm(mono_f_inputs, geo = TRUE)
table(mono_f_model)

mono_n_model <- gdm(mono_n_inputs, geo = TRUE)
table(mono_n_model)

poly_f_model <- gdm(poly_f_inputs, geo = TRUE)
table(poly_f_model)

poly_n_model <- gdm(poly_n_inputs, geo = TRUE)
table(poly_n_model)


#AMF

mono_f_amf_model <- gdm(mono_f_amf_inputs, geo = TRUE)
table(mono_f_amf_model)

mono_n_amf_model <- gdm(mono_n_amf_inputs, geo = TRUE)
table(mono_n_amf_model)

poly_f_amf_model <- gdm(poly_f_amf_inputs, geo = TRUE)
table(poly_f_amf_model)

poly_n_amf_model <- gdm(poly_n_amf_inputs, geo = TRUE)
table(poly_n_amf_model)



#########################################################################
### mantel tests
#########################################################################

#all fungi
all_mantel <- mantel_func(all_fungi, envi_factors)
mono_mantel <- mantel_func(monocultures, envi_factors)
poly_mantel <- mantel_func(polycultures, envi_factors)

#amf
amf_mantel <- mantel_func(amf_filter, envi_factors)
amf_mono_mantel <- mantel_func(amf_mono_filter, envi_factors)
amf_poly_mantel <- mantel_func(amf_poly_filter, envi_factors)


########################################################################
## alpha tests
########################################################################


divIndices <- c("obs_all","obs_amf", "obs_path","obs_sap","obs_par", "div_all","div_amf", "div_path","div_sap","div_par")

options(contrasts = c("contr.sum","contr.poly"))

alphaModels <- sapply(c("obs_all","obs_amf","obs_path", "obs_sap","obs_par"), USE.NAMES=TRUE, simplify = FALSE,
       function(x) {
         model <- glmer.nb(substitute(round(i,0) ~ FarmType*Block+   scale(pH) + scale(P) + scale(NP_ratio) + scale(TOC) + scale(N)  + (1|farmCode), list(i = as.name(x))), data=alphaDF, nAGQ=1, na.action=na.fail)
         list(summary(model))
       })

envModels <- sapply(c(envi_factors), USE.NAMES=TRUE, simplify = FALSE,
                      function(x) {
                        model <- lmer(substitute(log(i+1) ~ FarmType*Block + (1|farmCode), list(i = as.name(x))), data=alphaDF, na.action=na.exclude)
                        anova(model)
                      })

divModels <- sapply(c("div_all","div_amf","div_path", "div_sap","div_par"), USE.NAMES=TRUE, simplify = FALSE,
                      function(x) {
                        model <- lmer(substitute(log(i + 1) ~ FarmType*Block+   scale(pH) + scale(P) + scale(NP_ratio) + scale(TOC) + scale(N)  + (1|farmCode), list(i = as.name(x))), data=alphaDF,  na.action=na.exclude)
                        list(summary(model))
                      })

envModels <- sapply(c(envi_factors), USE.NAMES=TRUE, simplify = FALSE,
                    function(x) {
                      model <- lmer(substitute(log(i+1) ~ FarmType*Block + (1|farmCode), list(i = as.name(x))), data=alphaDF, na.action=na.exclude)
                      summary(model)
                    })

# The test reveals a p-value greater than 0.05, indicating that there is no significant difference between the group variances
pH_lt <- as.data.frame(car::leveneTest(pH ~ FarmType, data=alphaDF)[1,2:3], row.names="pH")
NP_ratio_lt <-  as.data.frame(car::leveneTest(NP_ratio ~ FarmType, data=alphaDF)[1,2:3], row.names="NP_ratio")
P_lt <- as.data.frame(car::leveneTest(P ~ FarmType, data=alphaDF)[1,2:3], row.names="P")
TOC_lt <-  as.data.frame(car::leveneTest(TOC ~ FarmType, data=alphaDF)[1,2:3], row.names="TOC")
N_lt <- as.data.frame(car::leveneTest(N ~ FarmType, data=alphaDF)[1,2:3], row.names="N")

leveneTest_table <- round(rbind(pH_lt,NP_ratio_lt,P_lt,TOC_lt,N_lt),3)

# soil_leveneTest <- sapply(envi_factors, USE.NAMES=TRUE, simplify = FALSE, function(x) { car::leveneTest(substitute(i ~ FarmType, list(i = as.name(x))), data=alphaDF)})

alphaSummary <-  alphaDF[, names(alphaDF) %in% c("FarmType", divIndices)] %>%
  gather(key = "variable", value = "value", -c(FarmType)) %>%
  group_by(FarmType, variable) %>%
  summarize_at("value", list(mean = mean, SE=std.error, min = min, max = max), na.rm=TRUE) %>%
  ungroup() %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, 3) %>%
  arrange(variable)


alphaBlockSummary <-  alphaDF[, names(alphaDF) %in% c("Block", divIndices)] %>%
  gather(key = "variable", value = "value", -c(Block)) %>%
  group_by(Block, variable) %>%
  summarize_at("value", list(mean = mean, SE=std.error, min = min, max = max), na.rm=TRUE) %>%
  ungroup() %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, 3) %>%
  arrange(variable)


alphaFTBLSummary <-  alphaDF[, names(alphaDF) %in% c("FTBL", divIndices)] %>%
  gather(key = "variable", value = "value", -c(FTBL)) %>%
  group_by(FTBL, variable) %>%
  summarize_at("value", list(mean = mean, SE=std.error, min = min, max = max), na.rm=TRUE) %>%
  ungroup() %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, 3) %>%
  arrange(variable)


alphaEnvSummary <-  alphaDF[, names(alphaDF) %in% c("FarmType", envi_factors)] %>%
  gather(key = "variable", value = "value", -c(FarmType)) %>%
  group_by(FarmType, variable) %>%
  summarize_at("value", list(mean = mean, SE=std.error, min = min, max = max), na.rm=TRUE) %>%
  ungroup() %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, 3) %>%
  arrange(variable)


alphaEnvBlockSummary <-  alphaDF[, names(alphaDF) %in% c("Block", envi_factors)] %>%
  gather(key = "variable", value = "value", -c(Block)) %>%
  group_by(Block, variable) %>%
  summarize_at("value", list(mean = mean, SE=std.error, min = min, max = max), na.rm=TRUE) %>%
  ungroup() %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, 3) %>%
  arrange(variable)


alphaEnvFTBLSummary <-  alphaDF[, names(alphaDF) %in% c("FTBL", envi_factors)] %>%
  gather(key = "variable", value = "value", -c(FTBL)) %>%
  group_by(FTBL, variable) %>%
  summarize_at("value", list(mean = mean, SE=std.error, min = min, max = max), na.rm=TRUE) %>%
  ungroup() %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, 3) %>%
  arrange(variable)




########################################################################
## envi variables
########################################################################


all_envi <- enviRange(all_fungi)

mono_envi <- enviRange(monocultures)

poly_envi <- enviRange(polycultures)










########################################################################
## supplementary section: functional groups
########################################################################

#original models

#plant pathogens
plant_path <- gdm(plant_path_inputs, geo = TRUE)
table(plant_path) #better by 7%

plant_path_mono <- gdm(plant_mono_inputs, geo = TRUE)
table(plant_path_mono)

plant_path_poly <- gdm(plant_poly_inputs, geo = TRUE)
table(plant_path_poly)

plant_path_d <- gdm(plant_path_inputs_d, geo = TRUE)
table(plant_path_d)

#saprotroph
saprotroph <- gdm(sap_inputs, geo = TRUE)
table(saprotroph) #better by 10%

saprotroph_mono <- gdm(sap_mono_inputs, geo = TRUE)
table(saprotroph_mono)

saprotroph_poly<- gdm(sap_poly_inputs, geo = TRUE)
table(saprotroph_poly)

saprotroph_d <- gdm(sap_inputs_d, geo = TRUE)
table(saprotroph_d)

#fungal parasite
fungal_parasite <- gdm(fungal_par_inputs, geo = TRUE)
table(fungal_parasite) #better by 2%

fungal_par_mono <- gdm(fungal_mono_inputs, geo = TRUE)
table(fungal_par_mono)

fungal_par_poly <- gdm(fungal_poly_inputs, geo = TRUE)
table(fungal_par_poly)

fungal_parasite_d <- gdm(fungal_par_inputs_d, geo = TRUE)
table(fungal_parasite_d)


#backwards selection models

#plant path across
across_plant <- backwardsSelection(df=all_fungi, guild= "Plant Pathogen", block= "F" ,focalcrop= "Eggplant", farmtype=c("Monoculture","Polyculture"), env_factors=envi_factors, geo=TRUE, maxDist = "across")
across_mono_plant <- backwardsSelection(df=all_fungi, guild= "Plant Pathogen", block= "F" ,focalcrop= "Eggplant", farmtype=c("Monoculture"), env_factors=envi_factors, geo=TRUE, maxDist = "across")
across_poly_plant <- backwardsSelection(df=all_fungi, guild= "Plant Pathogen", block= "F" ,focalcrop= "Eggplant", farmtype=c("Polyculture"), env_factors=envi_factors, geo=TRUE, maxDist = "across")

#plant path within
win_plant <- backwardsSelection(df=all_fungi, guild= "Plant Pathogen", block= "F" ,focalcrop= "Eggplant", farmtype=c("Monoculture","Polyculture"), env_factors=envi_factors, geo=TRUE, maxDist = "within")
win_mono_plant <- backwardsSelection(df=all_fungi, guild= "Plant Pathogen", block= "F" ,focalcrop= "Eggplant", farmtype=c("Monoculture"), env_factors=envi_factors, geo=TRUE, maxDist = "within")
win_poly_plant <- backwardsSelection(df=all_fungi, guild= "Plant Pathogen", block= "F" ,focalcrop= "Eggplant", farmtype=c("Polyculture"), env_factors=envi_factors, geo=TRUE, maxDist = "within")


#saprotrophs
across_sap <- backwardsSelection(df=all_fungi, guild= "Saprotroph", block= "F" ,focalcrop= "Eggplant", farmtype=c("Monoculture","Polyculture"), env_factors=envi_factors, geo=TRUE, maxDist = "across")
across_mono_sap <- backwardsSelection(df=all_fungi, guild= "Saprotroph", block= "F" ,focalcrop= "Eggplant", farmtype=c("Monoculture"), env_factors=envi_factors, geo=TRUE, maxDist = "across")
across_poly_sap <- backwardsSelection(df=all_fungi, guild= "Saprotroph", block= "F" ,focalcrop= "Eggplant", farmtype=c("Polyculture"), env_factors=envi_factors, geo=TRUE, maxDist = "across")

win_sap <- backwardsSelection(df=all_fungi, guild= "Saprotroph", block= "F" ,focalcrop= "Eggplant", farmtype=c("Monoculture","Polyculture"), env_factors=envi_factors, geo=TRUE, maxDist = "within")
win_mono_sap <- backwardsSelection(df=all_fungi, guild= "Saprotroph", block= "F" ,focalcrop= "Eggplant", farmtype=c("Monoculture"), env_factors=envi_factors, geo=TRUE, maxDist = "within")
win_poly_sap <- backwardsSelection(df=all_fungi, guild= "Saprotroph", block= "F" ,focalcrop= "Eggplant", farmtype=c("Polyculture"), env_factors=envi_factors, geo=TRUE, maxDist = "within")



#fungal parasite
across_fungal <- backwardsSelection(df=all_fungi, guild= "Fungal Parasite", block= "F" ,focalcrop= "Eggplant", farmtype=c("Monoculture","Polyculture"), env_factors=envi_factors, geo=TRUE, maxDist = "across")
across_mono_fungal <- backwardsSelection(df=all_fungi, guild= "Fungal Parasite", block= "F" ,focalcrop= "Eggplant", farmtype=c("Monoculture"), env_factors=envi_factors, geo=TRUE, maxDist = "across")
across_poly_fungal <- backwardsSelection(df=all_fungi, guild= "Fungal Parasite", block= "F" ,focalcrop= "Eggplant", farmtype=c("Polyculture"), env_factors=envi_factors, geo=TRUE, maxDist = "across")

win_fungal <- backwardsSelection(df=all_fungi, guild= "Fungal Parasite", block= "F" ,focalcrop= "Eggplant", farmtype=c("Monoculture","Polyculture"), env_factors=envi_factors, geo=TRUE, maxDist = "within")
win_mono_fungal <- backwardsSelection(df=all_fungi, guild= "Fungal Parasite", block= "F" ,focalcrop= "Eggplant", farmtype=c("Monoculture"), env_factors=envi_factors, geo=TRUE, maxDist = "within")
win_poly_fungal <- backwardsSelection(df=all_fungi, guild= "Fungal Parasite", block= "F" ,focalcrop= "Eggplant", farmtype=c("Polyculture"), env_factors=envi_factors, geo=TRUE, maxDist = "within")


#mantel tests

#plant pathogen
plant_mantel <- mantel_func(plant_pathogen, envi_factors)
plant_mono_mantel <- mantel_func(plant_mono, envi_factors)
plant_poly_mantel <- mantel_func(plant_poly, envi_factors)

#saprotroph
sap_mantel <- mantel_func(all_saprotroph, envi_factors)
sap_mono_mantel <- mantel_func(sap_mono, envi_factors)
sap_poly_mantel <- mantel_func(sap_poly, envi_factors)

#fungal parasite
fungal_mantel <- mantel_func(fungal_par, envi_factors)
fungal_mono_mantel <- mantel_func(fungal_mono, envi_factors)
fungal_poly_mantel <- mantel_func(fungal_poly, envi_factors)














