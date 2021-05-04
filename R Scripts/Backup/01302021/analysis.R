## *****************************************************************************
## gdm analysis ###############################################################
## *****************************************************************************


## *****************************************************************************
## 1. coefficient tables #######################################################
## *****************************************************************************

## *****************************************************************************
## 1a. ALL FARMS ###############################################################
## *****************************************************************************

# both transect blocks
all_wa_table <- all_wa$tables

# within-rows
all_w_table <- all_w$tables

# across-rows
all_a_table <- all_a$tables

## *****************************************************************************
## 1b. MONOCULTURE #############################################################
## *****************************************************************************

# both transect blocks
mono_wa_table <- mono_wa$tables

# within-rows
mono_w_table <- mono_w$tables

# across-rows
mono_a_table <- mono_a$tables

## *****************************************************************************
## 1c. POLYCULTURE #############################################################
## *****************************************************************************

# both transect blocks
poly_wa_table <- poly_wa$tables

# within-rows
poly_w_table <- poly_w$tables

# across-rows
poly_a_table <- poly_a$tables

## *****************************************************************************
## 2. variance explained #######################################################
## Note: these analyses take a long time to run.
## *****************************************************************************

## *****************************************************************************
## 2a. ALL FARMS ###############################################################
## *****************************************************************************

all_wa_var_df <- gdm.varImp(all_wa$spTable, geo=TRUE, nPerm=10, cores=10, fullModelOnly = FALSE)


all_wa_var <- as.data.frame(cbind(variance = all_wa_var_df[[2]][,1], pvalue=all_wa_var_df[[3]][,1])) %>%
  rownames_to_column("predictor") %>%
  mutate(dataset = "all farms",
         location = "whole farm") %>%
  dplyr::select(dataset, location, predictor, variance, pvalue)

all_w_var_df <- gdm.varImp(all_w$spTable, geo=TRUE, nPerm=10, cores=10, fullModelOnly = FALSE)

all_w_var <- as.data.frame(cbind(variance = all_w_var_df[[2]][,1], pvalue=all_w_var_df[[3]][,1])) %>%
  rownames_to_column("predictor") %>%
  mutate(dataset = "all farms",
         location = "within rows") %>%
  dplyr::select(dataset, location, predictor, variance, pvalue)


all_a_var_df <- gdm.varImp(all_a$spTable,geo=TRUE, nPerm=10, cores=10, fullModelOnly = FALSE)

all_a_var <- as.data.frame(cbind(variance = all_a_var_df[[2]][,1], pvalue=all_a_var_df[[3]][,1])) %>%
  rownames_to_column("predictor") %>%
  mutate(dataset = "all farms",
         location = "across rows") %>%
  dplyr::select(dataset, location, predictor, variance, pvalue)

all_varImp <- rbind(all_wa_var, all_w_var, all_a_var) %>% mutate(variance = round(variance, 3))


## *****************************************************************************
## 2b. MONOCULTURE #############################################################
## *****************************************************************************


mono_wa_var_df <- gdm.varImp(mono_wa$spTable, geo=TRUE, nPerm=10, cores=10, fullModelOnly = FALSE)


mono_wa_var <- as.data.frame(cbind(variance = mono_wa_var_df[[2]][,1], pvalue=mono_wa_var_df[[3]][,1])) %>%
  rownames_to_column("predictor") %>%
  mutate(dataset = "monoculture farms",
         location = "whole farm") %>%
  dplyr::select(dataset, location, predictor, variance, pvalue)

mono_w_var_df <- gdm.varImp(mono_w$spTable, geo=TRUE, nPerm=10, cores=10, fullModelOnly = FALSE)

mono_w_var <- as.data.frame(cbind(variance = mono_w_var_df[[2]][,1], pvalue=mono_w_var_df[[3]][,1])) %>%
  rownames_to_column("predictor") %>%
  mutate(dataset = "monoculture farms",
         location = "within rows") %>%
  dplyr::select(dataset, location, predictor, variance, pvalue)


mono_a_var_df <- gdm.varImp(mono_a$spTable,geo=TRUE, nPerm=10, cores=10, fullModelOnly = FALSE)

mono_a_var <- as.data.frame(cbind(variance = mono_a_var_df[[2]][,1], pvalue=mono_a_var_df[[3]][,1])) %>%
  rownames_to_column("predictor") %>%
  mutate(dataset = "monoculture farms",
         location = "across rows") %>%
  dplyr::select(dataset, location, predictor, variance, pvalue)

mono_varImp <- rbind(mono_wa_var, mono_w_var, mono_a_var) %>% mutate(variance = round(variance, 3))


## *****************************************************************************
## 2c. POLYCULTURE #############################################################
## *****************************************************************************

poly_wa_var_df <- gdm.varImp(poly_wa$spTable, geo=TRUE, nPerm=10, cores=10, fullModelOnly = FALSE)


poly_wa_var <- as.data.frame(cbind(variance = poly_wa_var_df[[2]][,1], pvalue=poly_wa_var_df[[3]][,1])) %>%
  rownames_to_column("predictor") %>%
  mutate(dataset = "polyculture farms",
         location = "whole farm") %>%
  dplyr::select(dataset, location, predictor, variance, pvalue)

poly_w_var_df <- gdm.varImp(poly_w$spTable, geo=TRUE, nPerm=10, cores=10, fullModelOnly = FALSE)

poly_w_var <- as.data.frame(cbind(variance = poly_w_var_df[[2]][,1], pvalue=poly_w_var_df[[3]][,1])) %>%
  rownames_to_column("predictor") %>%
  mutate(dataset = "polyculture farms",
         location = "within rows") %>%
  dplyr::select(dataset, location, predictor, variance, pvalue)


poly_a_var_df <- gdm.varImp(poly_a$spTable,geo=TRUE, nPerm=10, cores=10, fullModelOnly = FALSE)

poly_a_var <- as.data.frame(cbind(variance = poly_a_var_df[[2]][,1], pvalue=poly_a_var_df[[3]][,1])) %>%
  rownames_to_column("predictor") %>%
  mutate(dataset = "polyculture farms",
         location = "across rows") %>%
  dplyr::select(dataset, location, predictor, variance, pvalue)

poly_varImp <- rbind(poly_wa_var, poly_w_var, poly_a_var) %>% mutate(variance = round(variance, 3))


# final dataset

varImp <- rbind(all_varImp, mono_varImp, poly_varImp)

## *****************************************************************************
## 3. MRM #############################################################
## *****************************************************************************

## *****************************************************************************
## 3a. ALL FARMS ###############################################################
## *****************************************************************************

# both transecets
all_wa_mantel <- mantel_func(all_wa$df, edaphic_variables, type = "composition")
all_wa_mantel_nestedness <- mantel_func(all_wa$df, edaphic_variables, type = "nestedness")
all_wa_mantel_turnover <- mantel_func(all_wa$df, edaphic_variables, type = "turnover")

# within
all_w_mantel <- mantel_func(all_w$df, edaphic_variables, type = "composition")
all_w_mantel_nestedness <- mantel_func(all_w$df, edaphic_variables, type = "nestedness")
all_w_mantel_turnover <- mantel_func(all_w$df, edaphic_variables, type = "turnover")

# across
all_a_mantel <- mantel_func(all_a$df, edaphic_variables, type = "composition")
all_a_mantel_nestedness <- mantel_func(all_a$df, edaphic_variables, type = "nestedness")
all_a_mantel_turnover <- mantel_func(all_a$df, edaphic_variables, type = "turnover")

## *****************************************************************************
## 3b. MONOCULTURE ###############################################################
## *****************************************************************************

# both transecets
mono_wa_mantel <- mantel_func(mono_wa$df, edaphic_variables, type = "composition")
mono_wa_mantel_nestedness <- mantel_func(mono_wa$df, edaphic_variables, type = "nestedness")
mono_wa_mantel_turnover <- mantel_func(mono_wa$df, edaphic_variables, type = "turnover")

# within
mono_w_mantel <- mantel_func(mono_w$df, edaphic_variables, type = "composition")
mono_w_mantel_nestedness <- mantel_func(mono_w$df, edaphic_variables, type = "nestedness")
mono_w_mantel_turnover <- mantel_func(mono_w$df, edaphic_variables, type = "turnover")

# across
mono_a_mantel <- mantel_func(mono_a$df, edaphic_variables, type = "composition")
mono_a_mantel_nestedness <- mantel_func(mono_a$df, edaphic_variables, type = "nestedness")
mono_a_mantel_turnover <- mantel_func(mono_a$df, edaphic_variables, type = "turnover")

## *****************************************************************************
## 3c. POLYCULTURE ###############################################################
## *****************************************************************************

# both transecets
poly_wa_mantel <- mantel_func(poly_wa$df, edaphic_variables, type = "composition")
poly_wa_mantel_nestedness <- mantel_func(poly_wa$df, edaphic_variables, type = "nestedness")
poly_wa_mantel_turnover <- mantel_func(poly_wa$df, edaphic_variables, type = "turnover")

# within
poly_w_mantel <- mantel_func(poly_w$df, edaphic_variables, type = "composition")
poly_w_mantel_nestedness <- mantel_func(poly_w$df, edaphic_variables, type = "nestedness")
poly_w_mantel_turnover <- mantel_func(poly_w$df, edaphic_variables, type = "turnover")

# across
poly_a_mantel <- mantel_func(poly_a$df, edaphic_variables, type = "composition")
poly_a_mantel_nestedness <- mantel_func(poly_a$df, edaphic_variables, type = "nestedness")
poly_a_mantel_turnover <- mantel_func(poly_a$df, edaphic_variables, type = "turnover")

## *****************************************************************************
## 3d. mantel test table #######################################################
## *****************************************************************************

# composition table
mantelTests <- as.data.frame(
  rbind(all_wa_mantel$table, mono_wa_mantel$table, poly_wa_mantel$table,
        all_w_mantel$table, mono_w_mantel$table, poly_w_mantel$table,
        all_a_mantel$table, mono_a_mantel$table, poly_a_mantel$table)) %>%
  mutate(Test = "compositional", 
         Data = rep(c("All","Mono","Poly",
                      "All within", "Mono within", "Poly within",
                      "All across", "Mono across", "Poly across"), each=10),
         sig = case_when(
           Significance <= 0.001 ~ "***",
           Significance <= 0.01 &  Significance > 0.001 ~ "**",
           Significance <= 0.05 &  Significance > 0.01 ~ "*",
           Significance > 0.05  ~ ""),
         stat = paste(round(Statistic, 3),sig, space="")) %>%
  # filter(Factor %in% c("Species v Crop Diversity","Species v Geography","Species v pH", "Species v P","Species v N","Species v TOC")) %>% 
  pivot_wider(id_cols = "Data", names_from = "Factor", values_from = "stat")%>%
  arrange(Data) %>% as.data.frame()


# nestedness
mantel_nestednessTests <- as.data.frame(
  rbind(all_wa_mantel_nestedness$table, mono_wa_mantel_nestedness$table, poly_wa_mantel_nestedness$table,
        all_w_mantel_nestedness$table, mono_w_mantel_nestedness$table, poly_w_mantel_nestedness$table,
        all_a_mantel_nestedness$table, mono_a_mantel_nestedness$table, poly_a_mantel_nestedness$table)) %>%
  mutate(Test = "nestedness", 
         Data = rep(c("All","Mono","Poly",
                      "All within", "Mono within", "Poly within",
                      "All across", "Mono across", "Poly across"), each=10),
         sig = case_when(
           Significance <= 0.001 ~ "***",
           Significance <= 0.01 &  Significance > 0.001 ~ "**",
           Significance <= 0.05 &  Significance > 0.01 ~ "*",
           Significance > 0.05  ~ ""),
         stat = paste(round(Statistic, 3),sig, space="")) %>%
  filter(Factor %in% c("Species v Crop Diversity","Species v Geography","Species v pH", "Species v P","Species v N","Species v TOC")) %>% pivot_wider(id_cols = "Data", names_from = "Factor", values_from = "stat") %>%
  arrange(Data)

# turnover
mantel_turnoverTests <- as.data.frame(
  rbind(all_wa_mantel_turnover$table, mono_wa_mantel_turnover$table, poly_wa_mantel_turnover$table,
        all_w_mantel_turnover$table, mono_w_mantel_turnover$table, poly_w_mantel_turnover$table,
        all_a_mantel_turnover$table, mono_a_mantel_turnover$table, poly_a_mantel_turnover$table)) %>%
  mutate(Test = "turnover", 
         Data = rep(c("All","Mono","Poly",
                      "All within", "Mono within", "Poly within",
                      "All across", "Mono across", "Poly across"), each=10),
         sig = case_when(
           Significance <= 0.001 ~ "***",
           Significance <= 0.01 &  Significance > 0.001 ~ "**",
           Significance <= 0.05 &  Significance > 0.01 ~ "*",
           Significance > 0.05  ~ ""),
         stat = paste(round(Statistic, 3),sig, space="")) %>%
  filter(Factor %in% c("Species v Crop Diversity","Species v Geography","Species v pH", "Species v P","Species v N","Species v TOC")) %>% pivot_wider(id_cols = "Data", names_from = "Factor", values_from = "stat")%>%
  arrange(Data)




allMantel <- rbind(mantelTests, mantel_nestednessTests, mantel_turnoverTests)

write.csv(allMantel, "Outputs/Tables/allMantel.csv", row.names = FALSE)


## *****************************************************************************
## 4. composition models #######################################################
## *****************************************************************************

all_wa_composition <- pcoaFun(all_wa$df, color = "FarmType", shape = "Block", formula = "FarmType*Block", strata = "farmCode")

# mono_wa_composition <- pcoaFun(mono_wa$df, color = "FarmType", shape = "Block", formula = "farmCode", strata = "farmCode")

# poly_wa_composition <- pcoaFun(poly_wa$df, color = "FarmType", shape = "Block", formula = "Block", strata = "farmCode")

## *****************************************************************************
## 5. alpha models #############################################################
## crude test for collinearity: vif(), see https://www.statisticshowto.com/variance-inflation-factor/
## *****************************************************************************

## *****************************************************************************
## 5a. richnesss ###############################################################
## *****************************************************************************

# options(contrasts = c("contr.sum","contr.poly"))

## *****************************************************************************
## 5a_1. richnesss ~ crop diversity ############################################
## *****************************************************************************

# ALL FARMS
all_wa_richness_cd <- glmer.nb(observed ~ cropDiversity*scale(pH) + scale(P)*cropDiversity +  scale(TOC)*cropDiversity + scale(N)*cropDiversity  + (1|FarmKey:Year), data= all_wa$df, nAGQ=1, na.action=na.fail)

all_w_richness_cd <- glmer.nb(observed ~ cropDiversity*scale(pH) + scale(P)*cropDiversity +  scale(TOC)*cropDiversity + scale(N)*cropDiversity  + (1|FarmKey:Year), data= all_w$df, nAGQ=1, na.action=na.fail)

all_a_richness_cd <- glmer.nb(observed ~ cropDiversity*scale(pH) + scale(P)*cropDiversity +  scale(TOC)*cropDiversity + scale(N)*cropDiversity  + (1|FarmKey:Year), data= all_a$df, nAGQ=1, na.action=na.fail)


## *****************************************************************************
## 5a_2. richnesss ~ farm type ############################################
## *****************************************************************************

# ALL FARMS
all_wa_richness_ft <- glmer.nb(observed ~ FarmType*scale(pH) + scale(P)*FarmType +  scale(TOC)*FarmType + scale(N)*FarmType  + (1|FarmKey:Year), data= all_wa$df, nAGQ=1, na.action=na.fail)

all_w_richness_ft <- glmer.nb(observed ~ FarmType*scale(pH) + scale(P)*FarmType +  scale(TOC)*FarmType + scale(N)*FarmType  + (1|FarmKey:Year), data= all_w$df, nAGQ=1, na.action=na.fail)

all_a_richness_ft <- glmer.nb(observed ~ FarmType*scale(pH) + scale(P)*FarmType +  scale(TOC)*FarmType + scale(N)*FarmType  + (1|FarmKey:Year), data= all_a$df, nAGQ=1, na.action=na.fail)

## *****************************************************************************
## 5b_1. shannon ~ crop diversity ###################################################
## *****************************************************************************

all_wa_shannon_cd <-  lmer(shannon ~ cropDiversity*scale(pH) + scale(P)*cropDiversity + scale(TOC)*cropDiversity + scale(N)*cropDiversity  + (1|FarmKey:Year), data= all_wa$df,  na.action=na.exclude)

all_w_shannon_cd <-  lmer(shannon ~ cropDiversity*scale(pH) + scale(P)*cropDiversity +  scale(TOC)*cropDiversity + scale(N)*cropDiversity  + (1|FarmKey:Year), data= all_w$df,  na.action=na.exclude)

all_a_shannon_cd <-  lmer(shannon ~ cropDiversity*scale(pH) + scale(P)*cropDiversity +  scale(TOC)*cropDiversity + scale(N)*cropDiversity  + (1|FarmKey:Year), data= all_a$df,  na.action=na.exclude)

## *****************************************************************************
## 5b_2. shannon ~ farm type ###################################################
## *****************************************************************************

all_wa_shannon_ft <-  lmer(log(shannon + 1) ~ FarmType*scale(pH) + scale(P)*FarmType +  scale(TOC)*FarmType + scale(N)*FarmType  + (1|FarmKey:Year), data= all_wa$df,  na.action=na.exclude)

all_w_shannon_ft <-  lmer(log(shannon + 1) ~ FarmType*scale(pH) + scale(P)*FarmType +  scale(TOC)*FarmType + scale(N)*FarmType  + (1|FarmKey:Year), data= all_w$df,  na.action=na.exclude)

all_a_shannon_ft <-  lmer(log(shannon + 1) ~ FarmType*scale(pH) + scale(P)*FarmType +  scale(TOC)*FarmType + scale(N)*FarmType  + (1|FarmKey:Year), data= all_a$df,  na.action=na.exclude)

## *****************************************************************************
## 6. edaphic predictor models ###########################################
## *****************************************************************************

## *****************************************************************************
## 6a. edaphic predictor ~ crop diversity ###########################################
## *****************************************************************************

edaphicModels_cd <- sapply(c(edaphic_variables), USE.NAMES=TRUE, simplify = FALSE,
                    function(x) {
                      model <- lmer(substitute(scale(i) ~ cropDiversity*Block + (1|farmCode), 
                                               list(i = as.name(x))), data=all, na.action=na.exclude)
                      modelSummary(model)$summary
                    })

## *****************************************************************************
## 6b. edaphic predictor ~ farm type ###########################################
## *****************************************************************************

edaphicModels_ft <- sapply(c(edaphic_variables), USE.NAMES=TRUE, simplify = FALSE,
                           function(x) {
                             model <- lmer(substitute(scale(i) ~ FarmType*Block + (1|farmCode), 
                                                      list(i = as.name(x))), data=all, na.action=na.exclude)
                             modelSummary(model)$summary
                           })

## *****************************************************************************
## 7. dispersion of soil properties ############################################
## *****************************************************************************
# 
# all_wa_soil <- pcaFun(all_wa$df, edaphic_variables, "Block", "FarmType",  testGroup = "FarmType")
# mono_wa_soil <- pcaFun(mono_wa$df, edaphic_variables, "Block", "FarmType",  testGroup = "Block")
# poly_wa_soil <- pcaFun(poly_wa$df, edaphic_variables, "Block", "FarmType",  testGroup = "Block")

## *****************************************************************************
## 8. levene's test ############################################################
## The test reveals a p-value greater than 0.05, indicating that there 
## is no significant difference between the group variances
## *****************************************************************************
# 
# pH_lt <- as.data.frame(car::leveneTest(pH ~ factor(FarmType), data=all)[1,2:3], row.names="pH")
# NP_ratio_lt <-  as.data.frame(car::leveneTest(NP_ratio ~ factor(FarmType), data=all)[1,2:3], row.names="NP_ratio")
# P_lt <- as.data.frame(car::leveneTest(P ~ factor(FarmType), data=all)[1,2:3], row.names="P")
# TOC_lt <-  as.data.frame(car::leveneTest(TOC ~ factor(FarmType), data=all)[1,2:3], row.names="TOC")
# N_lt <- as.data.frame(car::leveneTest(N ~ factor(FarmType), data=all)[1,2:3], row.names="N")
# 
# leveneTest_table <- round(rbind(pH_lt,NP_ratio_lt,P_lt,TOC_lt,N_lt),3)


## *****************************************************************************
## 9. TITAN2 ######################################################################
## *****************************************************************************
# 
# # cropDiversity
# titan_cd <- run.titan(all_wa$df, "cropDiversity")
# 
# # N
# titan_N <- run.titan(all_wa$df, "N")
# 
# # pH
# titan_pH <- run.titan(all_wa$df, "pH")
# 
# # P
# titan_P <- run.titan(all_wa$df, "P")
# 
# # TOC
# titan_TOC <- run.titan(all_wa$df, "TOC")

