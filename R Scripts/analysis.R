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
## functional groups
########################################################################

#AMF
amf_fd <- gdm(amf_fd_inputs, geo = TRUE)
table(amf_fd)

amf_fd_mono <- gdm(amf_mono_inputs, geo = TRUE)
table(amf_fd_mono)

amf_fd_poly <- gdm(amf_poly_inputs, geo = TRUE)
table(amf_fd_poly)

amf_fd_d <- gdm(amf_fd_inputs_d, geo = TRUE)
table(amf_fd_d) #better by 2%


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
                      anova(model)
                    })

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



all_fungi %>%
  group_by(FarmKey) %>%
  summarise(n())






