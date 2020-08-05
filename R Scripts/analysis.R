########################################################################
## gdm analysis
########################################################################

########################################################################
## amf coefficient tables - no scale
########################################################################


########################################################################
## amf coefficient tables - ALL
########################################################################
landscape_AMF_table <- landscape_amf$tables$`item:1`

local_AMF_table <- local_amf$tables$`item:1`

########################################################################
## amf coefficient tables
########################################################################
landscape_AMF_f_table <- landscape_AMF_f$tables$`item:1`

landscape_AMF_n_table <- landscape_AMF_n$tables$`item:1`


local_AMF_f_table <- local_AMF_f$tables$`item:1`

local_AMF_n_table <- local_AMF_n$tables$`item:1`

########################################################################
## amf variance partitioning - ALL
########################################################################

# landscape - focal
noscale_AMF_var <- data.frame(gdm.varImp(noscale_amf$spTable$`item:1`, geo=TRUE, nPerm=5, cores=5, fullModelOnly = TRUE)[[2]]) %>%
  rownames_to_column("predictor") %>%
  rename(variance = fullModel) %>%
  mutate(scaleLevel = "all") %>%
  dplyr::select(scaleLevel, predictor, variance)




# landscape - focal
noscale_mono_AMF_var <- data.frame(gdm.varImp(noscale_mono_amf$spTable, geo=TRUE, nPerm=5, cores=5, fullModelOnly = TRUE)[[2]]) %>%
  rownames_to_column("predictor") %>%
  rename(variance = fullModel) %>%
  mutate(scaleLevel = "monoculture") %>%
  dplyr::select(scaleLevel, predictor, variance)


# landscape - focal
noscale_poly_AMF_var <- data.frame(gdm.varImp(noscale_poly_amf$spTable$`item:1`,geo=TRUE, nPerm=5, cores=5, fullModelOnly = TRUE)[[2]]) %>%
  rownames_to_column("predictor") %>%
  rename(variance = fullModel) %>%
  mutate(scaleLevel = "polyculture") %>%
  dplyr::select(scaleLevel, predictor, variance)

noscale_amf_var <- rbind(noscale_AMF_var,noscale_mono_AMF_var,noscale_poly_AMF_var)

########################################################################
## amf variance partitioning
########################################################################

# landscape - focal
landscape_AMF_f_var <- data.frame(gdm.varImp(landscape_AMF_f$spTable$`item:1`, geo=TRUE, nPerm=5, cores=5, fullModelOnly = TRUE)[[2]]) %>%
  rownames_to_column("predictor") %>%
  rename(variance = fullModel) %>%
  mutate(scaleLevel = "landscape_F") %>%
  dplyr::select(scaleLevel, predictor, variance)

# landscape - non-focal
landscape_AMF_n_var <-data.frame(gdm.varImp(landscape_AMF_n$spTable$`item:1`, geo=TRUE, nPerm=5, cores=5, fullModelOnly = TRUE)[[2]]) %>%
  rownames_to_column("predictor") %>%
  rename(variance = fullModel) %>%
  mutate(scaleLevel = "landscape_N") %>%
  dplyr::select(scaleLevel, predictor, variance)


# local -focal
local_AMF_f_var <- data.frame(gdm.varImp(local_AMF_f$spTable$`item:1`, geo=TRUE, nPerm=5, cores=5, fullModelOnly = TRUE)[[2]]) %>%
  rownames_to_column("predictor") %>%
  rename(variance = fullModel) %>%
  mutate(scaleLevel = "local_F") %>%
  dplyr::select(scaleLevel, predictor, variance)

# local - non-focal
local_AMF_n_var <- data.frame(gdm.varImp(local_AMF_n$spTable$`item:1`, geo=TRUE, nPerm=5, cores=5, fullModelOnly = TRUE)[[2]]) %>%
  rownames_to_column("predictor") %>%
  rename(variance = fullModel) %>%
  mutate(scaleLevel = "local_N") %>%
  dplyr::select(scaleLevel, predictor, variance)


varianceImportance <- rbind(landscape_AMF_f_var, landscape_AMF_n_var, local_AMF_f_var, local_AMF_n_var)



########################################################################
## amf variance partitioning - ALL 
########################################################################

# landscape - focal
landscape_AMF_var <- data.frame(gdm.varImp(landscape_amf$spTable$`item:1`, geo=TRUE, nPerm=5, cores=5, fullModelOnly = TRUE)[[2]]) %>%
  rownames_to_column("predictor") %>%
  rename(variance = fullModel) %>%
  mutate(scaleLevel = "landscape") %>%
  dplyr::select(scaleLevel, predictor, variance)

# local -focal
local_AMF_var <- data.frame(gdm.varImp(local_amf$spTable$`item:1`, geo=TRUE, nPerm=5, cores=5, fullModelOnly = TRUE)[[2]]) %>%
  rownames_to_column("predictor") %>%
  rename(variance = fullModel) %>%
  mutate(scaleLevel = "local") %>%
  dplyr::select(scaleLevel, predictor, variance)



varianceImportance_all <- rbind(landscape_AMF_var,  local_AMF_var)








#########################################################################
### mantel tests
#########################################################################

#amf
amf_mantel <- mantel_func(all_amf, envi_factors)
# amf_mono_mantel <- mantel_func(amf_mono_filter, envi_factors)
# amf_poly_mantel <- mantel_func(amf_poly_filter, envi_factors)

########################################################################
## alpha models
########################################################################

options(contrasts = c("contr.sum","contr.poly"))

richnessModel <- glmer.nb(round(observed,0) ~ FarmType*Block+   scale(pH) + scale(P) + scale(NP_ratio) + scale(TOC) + scale(N) + cropDiversity  + (1|farmCode), data= all_amf, nAGQ=1, na.action=na.fail)


shannonModels <-  lmer(log(shannon + 1) ~ FarmType*Block+   scale(pH) + scale(P) + scale(NP_ratio) + scale(TOC) + scale(N) + cropDiversity + (1|farmCode), data=all_amf,  na.action=na.exclude)
     

########################################################################
## environmental predictor models
########################################################################

envModels <- sapply(c(envi_factors), USE.NAMES=TRUE, simplify = FALSE,
                    function(x) {
                      model <- lmer(substitute(log(i+1) ~ FarmType*Block + (1|farmCode), 
                                               list(i = as.name(x))), data=all_amf, na.action=na.exclude)
                      summary(model)
                    })

########################################################################
## levene's test
## The test reveals a p-value greater than 0.05, indicating that there 
## is no significant difference between the group variances
########################################################################

pH_lt <- as.data.frame(car::leveneTest(pH ~ factor(FarmType), data=all_amf)[1,2:3], row.names="pH")
NP_ratio_lt <-  as.data.frame(car::leveneTest(NP_ratio ~ factor(FarmType), data=all_amf)[1,2:3], row.names="NP_ratio")
P_lt <- as.data.frame(car::leveneTest(P ~ factor(FarmType), data=all_amf)[1,2:3], row.names="P")
TOC_lt <-  as.data.frame(car::leveneTest(TOC ~ factor(FarmType), data=all_amf)[1,2:3], row.names="TOC")
N_lt <- as.data.frame(car::leveneTest(N ~ factor(FarmType), data=all_amf)[1,2:3], row.names="N")

leveneTest_table <- round(rbind(pH_lt,NP_ratio_lt,P_lt,TOC_lt,N_lt),3)









