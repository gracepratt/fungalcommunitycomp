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
## coeff table 
## *****************************************************************************


gdm_coeffTable_cd <- rbind(
  all_wa_table %>% mutate(dataset = "all farms", location = "whole farm"),
  all_w_table %>% mutate(dataset = "all farms", location = "within rows"),
  all_a_table %>% mutate(dataset = "all farms", location = "across rows"),
  mono_wa_table %>% mutate(dataset = "monoculture farms", location = "whole farm"),
  mono_w_table %>% mutate(dataset = "monoculture farms", location = "within rows"),
  mono_a_table %>% mutate(dataset = "monoculture farms", location = "across rows"),
  poly_wa_table %>% mutate(dataset = "polyculture farms", location = "whole farm"),
  poly_w_table %>% mutate(dataset = "polyculture farms", location = "within rows"),
  poly_a_table %>% mutate(dataset = "polyculture farms", location = "across rows")) %>%
  filter(!Predictors %in% c("DIC"))


## *****************************************************************************
## 1. coefficient tables -- farm type ##########################################
## *****************************************************************************

## *****************************************************************************
## 1a. ALL FARMS 
## *****************************************************************************

# both transect blocks
all_wa_table_ft <- all_wa_ft$tables

# within-rows
all_w_table_ft <- all_w_ft$tables

# across-rows
all_a_table_ft <- all_a_ft$tables

## *****************************************************************************
## 1b. MONOCULTURE 
## *****************************************************************************

# both transect blocks
mono_wa_table_ft <- mono_wa_ft$tables

# within-rows
mono_w_table_ft <- mono_w_ft$tables

# across-rows
mono_a_table_ft <- mono_a_ft$tables

## *****************************************************************************
## 1c. POLYCULTURE
## *****************************************************************************

# both transect blocks
poly_wa_table_ft <- poly_wa_ft$tables

# within-rows
poly_w_table_ft <- poly_w_ft$tables

# across-rows
poly_a_table_ft <- poly_a_ft$tables


## *****************************************************************************
## coeff table ~ farm type
## *****************************************************************************


gdm_coeffTable_ft <- rbind(all_wa_table_ft %>% mutate(dataset = "all farms", location = "whole farm"),
                        all_w_table_ft %>% mutate(dataset = "all farms", location = "within rows"),
                        all_a_table_ft %>% mutate(dataset = "all farms", location = "across rows"),
                        mono_wa_table_ft %>% mutate(dataset = "monoculture farms", location = "whole farm"),
                        mono_w_table_ft %>% mutate(dataset = "monoculture farms", location = "within rows"),
                        mono_a_table_ft %>% mutate(dataset = "monoculture farms", location = "across rows"),
                        poly_wa_table_ft %>% mutate(dataset = "polyculture farms", location = "whole farm"),
                        poly_w_table_ft %>% mutate(dataset = "polyculture farms", location = "within rows"),
                        poly_a_table_ft %>% mutate(dataset = "polyculture farms", location = "across rows")) %>%
  filter(!Predictors %in% c("DIC"))

## *****************************************************************************
## 2. variance importance by GDM ###############################################
## Note: these analyses take a long time to run.
## *****************************************************************************

## *****************************************************************************
## 2a. ALL FARMS ###############################################################
## *****************************************************************************

# all_wa_var_df <- gdm.varImp(all_wa$spTable, geo=TRUE, cores=10, fullModelOnly = TRUE, outFile = "all_wa_varImp")
# saveRDS(all_wa_var_df, "all_wa_var_df.rds")
all_wa_var_df <- readRDS("all_wa_var_df.rds")

all_wa_var <- as.data.frame(cbind(variance = all_wa_var_df[[2]][,1], pvalue=all_wa_var_df[[3]][,1])) %>%
  rownames_to_column("predictor") %>%
  mutate(dataset = "all farms",
         location = "whole farm") %>%
  dplyr::select(dataset, location, predictor, variance, pvalue)


# all_w_var_df <- gdm.varImp(all_w$spTable, geo=TRUE, cores=10, fullModelOnly = TRUE, outFile = "all_w_varImp")
# saveRDS(all_w_var_df, "all_w_var_df.rds")
all_w_var_df <- readRDS("all_w_var_df.rds")

all_w_var <- as.data.frame(cbind(variance = all_w_var_df[[2]][,1], pvalue=all_w_var_df[[3]][,1])) %>%
  rownames_to_column("predictor") %>%
  mutate(dataset = "all farms",
         location = "within rows") %>%
  dplyr::select(dataset, location, predictor, variance, pvalue)


# all_a_var_df <- gdm.varImp(all_a$spTable, geo=TRUE, cores=10, fullModelOnly = TRUE, outFile = "all_a_varImp")
# saveRDS(all_a_var_df, "all_a_var_df.rds")
all_a_var_df <- readRDS("all_a_var_df.rds")

all_a_var <- as.data.frame(cbind(variance = all_a_var_df[[2]][,1], pvalue=all_a_var_df[[3]][,1])) %>%
  rownames_to_column("predictor") %>%
  mutate(dataset = "all farms",
         location = "across rows") %>%
  dplyr::select(dataset, location, predictor, variance, pvalue)

all_varImp <- rbind(all_wa_var, all_w_var, all_a_var) %>% mutate(variance = round(variance, 3))


## *****************************************************************************
## 2b. MONOCULTURE #############################################################
## *****************************************************************************


# mono_wa_var_df <- gdm.varImp(mono_wa$spTable, geo=TRUE, cores=10, fullModelOnly = TRUE, outFile = "mono_wa_varImp")
# saveRDS(mono_wa_var_df, "mono_wa_var_df.rds")
mono_wa_var_df <- readRDS("mono_wa_var_df.rds")

mono_wa_var <- as.data.frame(cbind(variance = mono_wa_var_df[[2]][,1], pvalue=mono_wa_var_df[[3]][,1])) %>%
  rownames_to_column("predictor") %>%
  mutate(dataset = "monoculture farms",
         location = "whole farm") %>%
  dplyr::select(dataset, location, predictor, variance, pvalue)

# mono_w_var_df <- gdm.varImp(mono_w$spTable, geo=TRUE, cores=10, fullModelOnly = TRUE, outFile = "mono_w_varImp")
# saveRDS(mono_w_var_df, "mono_w_var_df.rds")
mono_w_var_df <- readRDS("mono_w_var_df.rds")


mono_w_var <- as.data.frame(cbind(variance = mono_w_var_df[[2]][,1], pvalue=mono_w_var_df[[3]][,1])) %>%
  rownames_to_column("predictor") %>%
  mutate(dataset = "monoculture farms",
         location = "within rows") %>%
  dplyr::select(dataset, location, predictor, variance, pvalue)


# mono_a_var_df <- gdm.varImp(mono_a$spTable, geo=TRUE, cores=10, fullModelOnly = TRUE, outFile = "mono_a_varImp")
# saveRDS(mono_a_var_df, "mono_a_var_df.rds")
mono_a_var_df <- readRDS("mono_a_var_df.rds")


mono_a_var <- as.data.frame(cbind(variance = mono_a_var_df[[2]][,1], pvalue=mono_a_var_df[[3]][,1])) %>%
  rownames_to_column("predictor") %>%
  mutate(dataset = "monoculture farms",
         location = "across rows") %>%
  dplyr::select(dataset, location, predictor, variance, pvalue)

mono_varImp <- rbind(mono_wa_var, mono_w_var, mono_a_var) %>% mutate(variance = round(variance, 3))


## *****************************************************************************
## 2c. POLYCULTURE #############################################################
## *****************************************************************************

# poly_wa_var_df <- gdm.varImp(poly_wa$spTable, geo=TRUE, cores=10, fullModelOnly = TRUE, outFile = "poly_wa_varImp")
# saveRDS(poly_wa_var_df, "poly_wa_var_df.rds")
poly_wa_var_df <- readRDS("poly_wa_var_df.rds")


poly_wa_var <- as.data.frame(cbind(variance = poly_wa_var_df[[2]][,1], pvalue=poly_wa_var_df[[3]][,1])) %>%
  rownames_to_column("predictor") %>%
  mutate(dataset = "polyculture farms",
         location = "whole farm") %>%
  dplyr::select(dataset, location, predictor, variance, pvalue)

# poly_w_var_df <- gdm.varImp(poly_w$spTable, geo=TRUE, cores=10, fullModelOnly = TRUE, outFile = "poly_w_varImp")
# saveRDS(poly_w_var_df, "poly_w_var_df.rds")
poly_w_var_df <- readRDS("poly_w_var_df.rds")

poly_w_var <- as.data.frame(cbind(variance = poly_w_var_df[[2]][,1], pvalue=poly_w_var_df[[3]][,1])) %>%
  rownames_to_column("predictor") %>%
  mutate(dataset = "polyculture farms",
         location = "within rows") %>%
  dplyr::select(dataset, location, predictor, variance, pvalue)


# poly_a_var_df <- gdm.varImp(poly_a$spTable, geo=TRUE, cores=10, fullModelOnly = TRUE, outFile = "poly_a_varImp")
# saveRDS(poly_a_var_df, "poly_a_var_df.rds")
poly_a_var_df <- readRDS("poly_a_var_df.rds")

poly_a_var <- as.data.frame(cbind(variance = poly_a_var_df[[2]][,1], pvalue=poly_a_var_df[[3]][,1])) %>%
  rownames_to_column("predictor") %>%
  mutate(dataset = "polyculture farms",
         location = "across rows") %>%
  dplyr::select(dataset, location, predictor, variance, pvalue)

poly_varImp <- rbind(poly_wa_var, poly_w_var, poly_a_var) %>% mutate(variance = round(variance, 3)) 


# final dataset

varImp_cd <- rbind(all_varImp, mono_varImp, poly_varImp) %>%
  mutate(location = factor(location, levels=c("whole farm","within rows","across rows")))


gdm_cd <- gdm_coeffTable_cd %>%
  left_join(varImp_cd, by = c("Predictors" = "predictor","dataset","location"))

gdm_cd_wide <- gdm_cd %>%
  mutate(sig = case_when(
    pvalue <= 0.001 ~ "***",
    pvalue <= 0.01 &  pvalue > 0.001 ~ "**",
    pvalue <= 0.05 &  pvalue > 0.01 ~ "*",
    pvalue > 0.05  ~ ""),
    stat = paste(round(Coefficients, 3),sig, space="")) %>%
  pivot_wider(id_cols = c("dataset","location"),names_from = "Predictors",values_from = "Coefficients")  

## *****************************************************************************
## 2. variance importance by GDM - farm type ###################################
## Note: these analyses take a long time to run.
## *****************************************************************************

## *****************************************************************************
## 2a. ALL FARMS
## *****************************************************************************

# all_wa_var_df_ft <- gdm.varImp(all_wa_ft$spTable, geo=TRUE, cores=10, fullModelOnly = TRUE, outFile = "all_wa_varImp")
# saveRDS(all_wa_var_df_ft, "all_wa_var_df_ft.rds")
all_wa_var_df_ft <- readRDS("all_wa_var_df_ft.rds")

all_wa_var_ft <- as.data.frame(cbind(variance = all_wa_var_df_ft[[2]][,1], pvalue=all_wa_var_df_ft[[3]][,1])) %>%
  rownames_to_column("predictor") %>%
  mutate(dataset = "all farms",
         location = "whole farm") %>%
  dplyr::select(dataset, location, predictor, variance, pvalue)


# all_w_var_df_ft <- gdm.varImp(all_w_ft$spTable, geo=TRUE, cores=10, fullModelOnly = TRUE, outFile = "all_w_varImp")
# saveRDS(all_w_var_df_ft, "all_w_var_df_ft.rds")
all_w_var_df_ft <- readRDS("all_w_var_df_ft.rds")

all_w_var_ft <- as.data.frame(cbind(variance = all_w_var_df_ft[[2]][,1], pvalue=all_w_var_df_ft[[3]][,1])) %>%
  rownames_to_column("predictor") %>%
  mutate(dataset = "all farms",
         location = "within rows") %>%
  dplyr::select(dataset, location, predictor, variance, pvalue)


# all_a_var_df_ft <- gdm.varImp(all_a_ft$spTable, geo=TRUE, cores=10, fullModelOnly = TRUE, outFile = "all_a_varImp")
# saveRDS(all_a_var_df_ft, "all_a_var_df_ft.rds")
all_a_var_df_ft <- readRDS("all_a_var_df_ft.rds")

all_a_var_ft <- as.data.frame(cbind(variance = all_a_var_df_ft[[2]][,1], pvalue=all_a_var_df_ft[[3]][,1])) %>%
  rownames_to_column("predictor") %>%
  mutate(dataset = "all farms",
         location = "across rows") %>%
  dplyr::select(dataset, location, predictor, variance, pvalue)

all_varImp_ft <- rbind(all_wa_var_ft, all_w_var_ft, all_a_var_ft) %>% mutate(variance = round(variance, 3))


## *****************************************************************************
## 2b. MONOCULTURE
## *****************************************************************************


# mono_wa_var_df_ft <- gdm.varImp(mono_wa_ft$spTable, geo=TRUE, cores=10, fullModelOnly = TRUE, outFile = "mono_wa_varImp")
# saveRDS(mono_wa_var_df_ft, "mono_wa_var_df_ft.rds")
mono_wa_var_df_ft <- readRDS("mono_wa_var_df_ft.rds")

mono_wa_var_ft <- as.data.frame(cbind(variance = mono_wa_var_df_ft[[2]][,1], pvalue=mono_wa_var_df_ft[[3]][,1])) %>%
  rownames_to_column("predictor") %>%
  mutate(dataset = "monoculture farms",
         location = "whole farm") %>%
  dplyr::select(dataset, location, predictor, variance, pvalue)

# mono_w_var_df_ft <- gdm.varImp(mono_w_ft$spTable, geo=TRUE, cores=10, fullModelOnly = TRUE, outFile = "mono_w_varImp")
# saveRDS(mono_w_var_df_ft, "mono_w_var_df_ft.rds")
mono_w_var_df_ft <- readRDS("mono_w_var_df_ft.rds")


mono_w_var_ft <- as.data.frame(cbind(variance = mono_w_var_df_ft[[2]][,1], pvalue=mono_w_var_df_ft[[3]][,1])) %>%
  rownames_to_column("predictor") %>%
  mutate(dataset = "monoculture farms",
         location = "within rows") %>%
  dplyr::select(dataset, location, predictor, variance, pvalue)


# mono_a_var_df_ft <- gdm.varImp(mono_a_ft$spTable, geo=TRUE, cores=10, fullModelOnly = TRUE, outFile = "mono_a_varImp")
# saveRDS(mono_a_var_df_ft, "mono_a_var_df_ft.rds")
mono_a_var_df_ft <- readRDS("mono_a_var_df_ft.rds")


mono_a_var_ft <- as.data.frame(cbind(variance = mono_a_var_df_ft[[2]][,1], pvalue=mono_a_var_df_ft[[3]][,1])) %>%
  rownames_to_column("predictor") %>%
  mutate(dataset = "monoculture farms",
         location = "across rows") %>%
  dplyr::select(dataset, location, predictor, variance, pvalue)

mono_varImp_ft <- rbind(mono_wa_var_ft, mono_w_var_ft, mono_a_var_ft) %>% mutate(variance = round(variance, 3))


## *****************************************************************************
## 2c. POLYCULTURE
## *****************************************************************************

# poly_wa_var_df_ft <- gdm.varImp(poly_wa_ft$spTable, geo=TRUE, cores=10, fullModelOnly = TRUE, outFile = "poly_wa_varImp")
# saveRDS(poly_wa_var_df_ft, "poly_wa_var_df_ft.rds")
poly_wa_var_df_ft <- readRDS("poly_wa_var_df_ft.rds")


poly_wa_var_ft <- as.data.frame(cbind(variance = poly_wa_var_df_ft[[2]][,1], pvalue=poly_wa_var_df_ft[[3]][,1])) %>%
  rownames_to_column("predictor") %>%
  mutate(dataset = "polyculture farms",
         location = "whole farm") %>%
  dplyr::select(dataset, location, predictor, variance, pvalue)

# poly_w_var_df_ft <- gdm.varImp(poly_w_ft$spTable, geo=TRUE, cores=10, fullModelOnly = TRUE, outFile = "poly_w_varImp")
# saveRDS(poly_w_var_df_ft, "poly_w_var_df_ft.rds")
poly_w_var_df_ft <- readRDS("poly_w_var_df_ft.rds")

poly_w_var_ft <- as.data.frame(cbind(variance = poly_w_var_df_ft[[2]][,1], pvalue=poly_w_var_df_ft[[3]][,1])) %>%
  rownames_to_column("predictor") %>%
  mutate(dataset = "polyculture farms",
         location = "within rows") %>%
  dplyr::select(dataset, location, predictor, variance, pvalue)


# poly_a_var_df_ft <- gdm.varImp(poly_a_ft$spTable, geo=TRUE, cores=10, fullModelOnly = TRUE, outFile = "poly_a_varImp")
# saveRDS(poly_a_var_df_ft, "poly_a_var_df_ft.rds")
poly_a_var_df_ft <- readRDS("poly_a_var_df_ft.rds")

poly_a_var_ft <- as.data.frame(cbind(variance = poly_a_var_df_ft[[2]][,1], pvalue=poly_a_var_df_ft[[3]][,1])) %>%
  rownames_to_column("predictor") %>%
  mutate(dataset = "polyculture farms",
         location = "across rows") %>%
  dplyr::select(dataset, location, predictor, variance, pvalue)

poly_varImp_ft <- rbind(poly_wa_var_ft, poly_w_var_ft, poly_a_var_ft) %>% mutate(variance = round(variance, 3)) 


# final dataset

varImp_ft <- rbind(all_varImp_ft, mono_varImp_ft, poly_varImp_ft) %>%
  mutate(location = factor(location, levels=c("whole farm","within rows","across rows")))

gdm_ft <- gdm_coeffTable_ft %>%
  left_join(varImp_ft, by = c("Predictors" = "predictor","dataset","location"))

gdm_ft_wide <- gdm_ft %>%
  mutate(sig = case_when(
    pvalue <= 0.001 ~ "***",
    pvalue <= 0.01 &  pvalue > 0.001 ~ "**",
    pvalue <= 0.05 &  pvalue > 0.01 ~ "*",
    pvalue > 0.05  ~ ""),
    stat = paste(round(Coefficients, 3),sig, space="")) %>%
  pivot_wider(id_cols = c("dataset","location"),names_from = "Predictors",values_from = "stat") %>%
  rename(deviance = `Percent Deviance Explained`)

## *****************************************************************************
## 3. MRM #############################################################
## *****************************************************************************

## *****************************************************************************
## 3a. ALL FARMS ###############################################################
## *****************************************************************************

# both transecets
# all_wa_MRM <- run.MMMR(all_wa$df, vars=c("pH","P", "N", "TOC","cropDiversity"))
# saveRDS(all_wa_MRM, "all_wa_MRM.rds")
all_wa_MRM <- readRDS("all_wa_MRM.rds")


# within
# all_w_MRM <- run.MMMR(all_w$df, c("pH","P", "N", "TOC","cropDiversity"))
# saveRDS(all_w_MRM, "all_w_MRM.rds")
all_w_MRM <- readRDS("all_w_MRM.rds")


# across
# all_a_MRM <- run.MMMR(all_a$df, c("pH","P", "N", "TOC","cropDiversity"))
# saveRDS(all_a_MRM, "all_a_MRM.rds")
all_a_MRM <- readRDS("all_a_MRM.rds")


## *****************************************************************************
## 3b. MONOCULTURE ###############################################################
## *****************************************************************************

# both transecets
# mono_wa_MRM <- run.MMMR(mono_wa$df, c("pH","P", "N", "TOC"))
# saveRDS(mono_wa_MRM, "mono_wa_MRM.rds")
mono_wa_MRM <- readRDS("mono_wa_MRM.rds")

# within
# mono_w_MRM <- run.MMMR(mono_w$df, c("pH","P", "N", "TOC"))
# saveRDS(mono_w_MRM, "mono_w_MRM.rds")
mono_w_MRM <- readRDS("mono_w_MRM.rds")

# across
# mono_a_MRM <- run.MMMR(mono_a$df, c("pH","P", "N", "TOC"))
# saveRDS(mono_a_MRM, "mono_a_MRM.rds")
mono_a_MRM <- readRDS("mono_a_MRM.rds")

## *****************************************************************************
## 3c. POLYCULTURE ###############################################################
## *****************************************************************************

# both transecets
# poly_wa_MRM <- run.MMMR(poly_wa$df, c("pH","P", "N", "TOC","cropDiversity"))
# saveRDS(poly_wa_MRM, "poly_wa_MRM.rds")
poly_wa_MRM <- readRDS("poly_wa_MRM.rds")

# within
# poly_w_MRM <- run.MMMR(poly_w$df, c("pH","P", "N", "TOC","cropDiversity"))
# saveRDS(poly_w_MRM, "poly_w_MRM.rds")
poly_w_MRM <- readRDS("poly_w_MRM.rds")

# across
# poly_a_MRM <- run.MMMR(poly_a$df, c("pH","P", "N", "TOC","cropDiversity"))
# saveRDS(poly_a_MRM, "poly_a_MRM.rds")
poly_a_MRM <- readRDS("poly_a_MRM.rds")

## *****************************************************************************
## 3d. MRM table #######################################################
## *****************************************************************************

mrmTests <- as.data.frame(
  rbind(all_wa_MRM$MMRR, all_w_MRM$MMRR, all_a_MRM$MMRR,
        mono_wa_MRM$MMRR, mono_w_MRM$MMRR, mono_a_MRM$MMRR,
        poly_wa_MRM$MMRR, poly_w_MRM$MMRR, poly_a_MRM$MMRR)) %>%
  mutate(Test = "MRM", 
         Data = c(rep(c("All","All within","All across"), each=7),
                  rep(c("Mono", "Mono within", "Mono across"), each=6),
                  rep(c("Poly", "Poly within", "Poly across"), each=7)),
         sig = case_when(
           p <= 0.001 ~ "***",
           p <= 0.01 &  p > 0.001 ~ "**",
           p <= 0.05 &  p > 0.01 ~ "*",
           p > 0.05  ~ ""),
         stat = paste(round(coefficent, 3),sig, space="")) %>%
  filter(! variable %in% c("Intercept")) %>%
  pivot_wider(id_cols = "Data", names_from = "variable", values_from = "stat") %>%
  mutate(variance = substring(c(all_wa_MRM$variance, all_w_MRM$variance, all_a_MRM$variance,
                                mono_wa_MRM$variance, mono_w_MRM$variance, mono_a_MRM$variance,
                                poly_wa_MRM$variance, poly_w_MRM$variance, poly_a_MRM$variance),44)) %>%
  arrange(Data) %>% 
  as.data.frame()

## *****************************************************************************
## 3d. variance importance by MRM ##############################################
## *****************************************************************************

relImpTable <- as.data.frame(
  rbind(all_wa_MRM$relative_importance, all_w_MRM$relative_importance, all_a_MRM$relative_importance,
        mono_wa_MRM$relative_importance, mono_w_MRM$relative_importance, mono_a_MRM$relative_importance,
        poly_wa_MRM$relative_importance, poly_w_MRM$relative_importance, poly_a_MRM$relative_importance)) %>%
  mutate(Test = "MRM", 
         location = c(rep(c("whole farm","within rows","across rows"), each=6),
                   rep(c("whole farm", "within rows", "across rows"), each=5),
                   rep(c("whole farm", "within rows", "across rows"), each=6)),
         dataset = c(rep(c("all farms","all farms","all farms"), each=6),
                  rep(c("monoculture farms", "monoculture farms", "monoculture farms"), each=5),
                  rep(c("polyculture farms", "polyculture farms", "polyculture farms"), each=6)),
         location = factor(location, levels=c("whole farm","within rows","across rows"))) %>%
  arrange(dataset) %>% 
  as.data.frame()



relImpTable_wide <- as.data.frame(
  rbind(all_wa_MRM$relative_importance, all_w_MRM$relative_importance, all_a_MRM$relative_importance,
        mono_wa_MRM$relative_importance, mono_w_MRM$relative_importance, mono_a_MRM$relative_importance,
        poly_wa_MRM$relative_importance, poly_w_MRM$relative_importance, poly_a_MRM$relative_importance)) %>%
  mutate(Test = "MRM", 
         Data = c(rep(c("All","All within","All across"), each=6),
                  rep(c("Mono", "Mono within", "Mono across"), each=5),
                  rep(c("Poly", "Poly within", "Poly across"), each=5))) %>%
  pivot_wider(id_cols = "Data", names_from = "variable", values_from = "lmg") %>%
  mutate(variance = substring(c(all_wa_MRM$variance, all_w_MRM$variance, all_a_MRM$variance,
                                mono_wa_MRM$variance, mono_w_MRM$variance, mono_a_MRM$variance,
                                poly_wa_MRM$variance, poly_w_MRM$variance, poly_a_MRM$variance),44)) %>%
  arrange(Data) %>% 
  as.data.frame()




# write.csv(allMantel, "Outputs/Tables/allMantel.csv", row.names = FALSE)


## *****************************************************************************
## 3. MRM - farm type ##########################################################
## *****************************************************************************

## *****************************************************************************
## 3a. ALL FARMS
## *****************************************************************************

# both transecets
# all_wa_MRM_ft <- run.MMMR(all_wa_ft$df, vars=c("pH","P", "N", "TOC","FarmBi"))
# saveRDS(all_wa_MRM_ft, "all_wa_MRM_ft.rds")
all_wa_MRM_ft <- readRDS("all_wa_MRM_ft.rds")


# within
# all_w_MRM_ft <- run.MMMR(all_w_ft$df, c("pH","P", "N", "TOC","FarmBi"))
# saveRDS(all_w_MRM_ft, "all_w_MRM_ft.rds")
all_w_MRM_ft <- readRDS("all_w_MRM_ft.rds")


# across
# all_a_MRM_ft <- run.MMMR(all_a_ft$df, c("pH","P", "N", "TOC","FarmBi"))
# saveRDS(all_a_MRM_ft, "all_a_MRM_ft.rds")
all_a_MRM_ft <- readRDS("all_a_MRM_ft.rds")


## *****************************************************************************
## 3b. MONOCULTURE
## *****************************************************************************

# both transecets
# mono_wa_MRM_ft <- run.MMMR(mono_wa_ft$df, c("pH","P", "N", "TOC"))
# saveRDS(mono_wa_MRM_ft, "mono_wa_MRM_ft.rds")
mono_wa_MRM_ft <- readRDS("mono_wa_MRM_ft.rds")

# within
# mono_w_MRM_ft <- run.MMMR(mono_w_ft$df, c("pH","P", "N", "TOC"))
# saveRDS(mono_w_MRM_ft, "mono_w_MRM_ft.rds")
mono_w_MRM_ft <- readRDS("mono_w_MRM_ft.rds")

# across
# mono_a_MRM_ft <- run.MMMR(mono_a_ft$df, c("pH","P", "N", "TOC"))
# saveRDS(mono_a_MRM_ft, "mono_a_MRM_ft.rds")
mono_a_MRM_ft <- readRDS("mono_a_MRM_ft.rds")

## *****************************************************************************
## 3c. POLYCULTURE
## *****************************************************************************

# both transecets
# poly_wa_MRM_ft <- run.MMMR(poly_wa_ft$df, c("pH","P", "N", "TOC"))
# saveRDS(poly_wa_MRM_ft, "poly_wa_MRM_ft.rds")
poly_wa_MRM_ft <- readRDS("poly_wa_MRM_ft.rds")

# within
# poly_w_MRM_ft <- run.MMMR(poly_w_ft$df, c("pH","P", "N", "TOC"))
# saveRDS(poly_w_MRM_ft, "poly_w_MRM_ft.rds")
poly_w_MRM_ft <- readRDS("poly_w_MRM_ft.rds")

# across
# poly_a_MRM_ft <- run.MMMR(poly_a_ft$df, c("pH","P", "N", "TOC"))
# saveRDS(poly_a_MRM_ft, "poly_a_MRM_ft.rds")
poly_a_MRM_ft <- readRDS("poly_a_MRM_ft.rds")

## *****************************************************************************
## 3d. MRM table
## *****************************************************************************

mrmTests_ft <- as.data.frame(
  rbind(all_wa_MRM_ft$MMRR, all_w_MRM_ft$MMRR, all_a_MRM_ft$MMRR,
        mono_wa_MRM_ft$MMRR, mono_w_MRM_ft$MMRR, mono_a_MRM_ft$MMRR,
        poly_wa_MRM_ft$MMRR, poly_w_MRM_ft$MMRR, poly_a_MRM_ft$MMRR)) %>%
  mutate(Test = "MRM", 
         Data = c(rep(c("All","All within","All across"), each=7),
                  rep(c("Mono", "Mono within", "Mono across"), each=6),
                  rep(c("Poly", "Poly within", "Poly across"), each=6)),
         sig = case_when(
           p <= 0.001 ~ "***",
           p <= 0.01 &  p > 0.001 ~ "**",
           p <= 0.05 &  p > 0.01 ~ "*",
           p > 0.05  ~ ""),
         stat = paste(round(coefficent, 3),sig, space="")) %>%
  filter(! variable %in% c("Intercept")) %>%
  pivot_wider(id_cols = "Data", names_from = "variable", values_from = "stat") %>%
  mutate(variance = substring(c(all_wa_MRM_ft$variance, all_w_MRM_ft$variance, all_a_MRM_ft$variance,
                                mono_wa_MRM_ft$variance, mono_w_MRM_ft$variance, mono_a_MRM_ft$variance,
                                poly_wa_MRM_ft$variance, poly_w_MRM_ft$variance, poly_a_MRM_ft$variance),44)) %>%
  arrange(Data) %>% 
  dplyr::select(Data, variance, geography, pH, P, TOC, N, FarmBi) %>%
  as.data.frame()

## *****************************************************************************
## 3d. variance importance by MRM
## *****************************************************************************

relImpTable_ft <- as.data.frame(
  rbind(all_wa_MRM_ft$relative_importance, all_w_MRM_ft$relative_importance, all_a_MRM_ft$relative_importance,
        mono_wa_MRM_ft$relative_importance, mono_w_MRM_ft$relative_importance, mono_a_MRM_ft$relative_importance,
        poly_wa_MRM_ft$relative_importance, poly_w_MRM_ft$relative_importance, poly_a_MRM_ft$relative_importance)) %>%
  mutate(Test = "MRM", 
         location = c(rep(c("whole farm","within rows","across rows"), each=6),
                      rep(c("whole farm", "within rows", "across rows"), each=5),
                      rep(c("whole farm", "within rows", "across rows"), each=5)),
         dataset = c(rep(c("all farms","all farms","all farms"), each=6),
                     rep(c("monoculture farms", "monoculture farms", "monoculture farms"), each=5),
                     rep(c("polyculture farms", "polyculture farms", "polyculture farms"), each=5)),
         location = factor(location, levels=c("whole farm","within rows","across rows"))) %>%
  arrange(dataset) %>% 
  as.data.frame()



relImpTable_wide_ft <- as.data.frame(
  rbind(all_wa_MRM_ft$relative_importance, all_w_MRM_ft$relative_importance, all_a_MRM_ft$relative_importance,
        mono_wa_MRM_ft$relative_importance, mono_w_MRM_ft$relative_importance, mono_a_MRM_ft$relative_importance,
        poly_wa_MRM_ft$relative_importance, poly_w_MRM_ft$relative_importance, poly_a_MRM_ft$relative_importance)) %>%
  mutate(Test = "MRM", 
         Data = c(rep(c("All","All within","All across"), each=6),
                  rep(c("Mono", "Mono within", "Mono across"), each=5),
                  rep(c("Poly", "Poly within", "Poly across"), each=5))) %>%
  pivot_wider(id_cols = "Data", names_from = "variable", values_from = "lmg") %>%
  mutate(variance = substring(c(all_wa_MRM_ft$variance, all_w_MRM_ft$variance, all_a_MRM_ft$variance,
                                mono_wa_MRM_ft$variance, mono_w_MRM_ft$variance, mono_a_MRM_ft$variance,
                                poly_wa_MRM_ft$variance, poly_w_MRM_ft$variance, poly_a_MRM_ft$variance),44)) %>%
  arrange(Data) %>% 
  as.data.frame()




# write.csv(allMantel, "Outputs/Tables/allMantel.csv", row.names = FALSE)


## *****************************************************************************
## 3e. MRM table for environmental predictors ##################################
## *****************************************************************************

# all_wa_env_MRM <-  run.MMMR(all_wa$df, vars=c("cropDiversity"), contains = c("pH","P","N","TOC"), type="euclidean")
# saveRDS(all_wa_env_MRM, "all_wa_env_MRM.rds")
all_wa_env_MRM <- readRDS("all_wa_env_MRM.rds")

# all_w_env_MRM <-  run.MMMR(all_w$df, vars=c("cropDiversity"), contains = c("pH","P","N","TOC"), type="euclidean")
# saveRDS(all_w_env_MRM, "all_w_env_MRM.rds")
all_w_env_MRM <- readRDS("all_w_env_MRM.rds")

# all_a_env_MRM <-  run.MMMR(all_a$df, vars=c("cropDiversity"), contains = c("pH","P","N","TOC"), type="euclidean")
# saveRDS(all_a_env_MRM, "all_a_env_MRM.rds")
all_a_env_MRM <- readRDS("all_a_env_MRM.rds")

# mono_wa_env_MRM <-  run.MMMR(mono_wa$df, contains = c("pH","P","N","TOC"), type="euclidean")
# saveRDS(mono_wa_env_MRM, "mono_wa_env_MRM.rds")
mono_wa_env_MRM <- readRDS("mono_wa_env_MRM.rds")

# mono_w_env_MRM <-  run.MMMR(mono_w$df, contains = c("pH","P","N","TOC"), type="euclidean")
# saveRDS(mono_w_env_MRM, "mono_w_env_MRM.rds")
mono_w_env_MRM <- readRDS("mono_w_env_MRM.rds")

# mono_a_env_MRM <-  run.MMMR(mono_a$df, contains = c("pH","P","N","TOC"), type="euclidean")
# saveRDS(mono_a_env_MRM, "mono_a_env_MRM.rds")
mono_a_env_MRM <- readRDS("mono_a_env_MRM.rds")

# poly_wa_env_MRM <-  run.MMMR(poly_wa$df, contains = c("pH","P","N","TOC"), type="euclidean")
# saveRDS(poly_wa_env_MRM, "poly_wa_env_MRM.rds")
poly_wa_env_MRM <- readRDS("poly_wa_env_MRM.rds")

# poly_w_env_MRM <-  run.MMMR(poly_w$df, contains = c("pH","P","N","TOC"), type="euclidean")
# saveRDS(poly_w_env_MRM, "poly_w_env_MRM.rds")
poly_w_env_MRM <- readRDS("poly_w_env_MRM.rds")

# poly_a_env_MRM <-  run.MMMR(poly_a$df, contains = c("pH","P","N","TOC"), type="euclidean")
# saveRDS(poly_a_env_MRM, "poly_a_env_MRM.rds")
poly_a_env_MRM <- readRDS("poly_a_env_MRM.rds")


mrmTests_env <- as.data.frame(
  rbind(all_wa_env_MRM$MMRR, all_w_env_MRM$MMRR, all_a_env_MRM$MMRR,
        mono_wa_env_MRM$MMRR, mono_w_env_MRM$MMRR, mono_a_env_MRM$MMRR,
        poly_wa_env_MRM$MMRR, poly_w_env_MRM$MMRR, poly_a_env_MRM$MMRR)) %>%
  mutate(Test = "MRM", 
         Data = c(rep(c("All","All within","All across"), each=3),
                  rep(c("Mono", "Mono within", "Mono across"), each=2),
                  rep(c("Poly", "Poly within", "Poly across"), each=2)),
         sig = case_when(
           p <= 0.001 ~ "***",
           p <= 0.01 &  p > 0.001 ~ "**",
           p <= 0.05 &  p > 0.01 ~ "*",
           p > 0.05  ~ ""),
         stat = paste(round(coefficent, 3),sig, space="")) %>%
  filter(! variable %in% c("Intercept")) %>%
  pivot_wider(id_cols = "Data", names_from = "variable", values_from = "stat") %>%
  mutate(variance = substring(c(all_wa_env_MRM$variance, all_w_env_MRM$variance, all_a_env_MRM$variance,
                          mono_wa_env_MRM$variance, mono_w_env_MRM$variance, mono_a_env_MRM$variance,
                          poly_wa_env_MRM$variance, poly_w_env_MRM$variance, poly_a_env_MRM$variance),44)) %>%
  arrange(Data) %>% 
  as.data.frame()


## *****************************************************************************
## 4. variation paritioning ####################################################
## *****************************************************************************

all_wa_varpart <- run.varpart(all_wa$df)
all_w_varpart <- run.varpart(all_w$df)
all_a_varpart <- run.varpart(all_a$df)
mono_wa_varpart <- run.varpart(mono_wa$df, cd = FALSE)
mono_w_varpart <- run.varpart(mono_w$df, cd = FALSE)
mono_a_varpart <- run.varpart(mono_a$df, cd = FALSE)
poly_wa_varpart <- run.varpart2(poly_wa$df)
poly_w_varpart <- run.varpart2(poly_w$df)
poly_a_varpart <- run.varpart2(poly_a$df)



varpart_table <- rbind(all_wa_varpart$varpart %>% mutate(dataset = "all farms", location = "whole farm"),
                           all_w_varpart$varpart %>% mutate(dataset = "all farms", location = "within rows"),
                           all_a_varpart$varpart %>% mutate(dataset = "all farms", location = "across rows"),
                           mono_wa_varpart$varpart %>% mutate(dataset = "monoculture farms", location = "whole farm"),
                           mono_w_varpart$varpart %>% mutate(dataset = "monoculture farms", location = "within rows"),
                           mono_a_varpart$varpart %>% mutate(dataset = "monoculture farms", location = "across rows"),
                           poly_wa_varpart$varpart %>% mutate(dataset = "polyculture farms", location = "whole farm"),
                           poly_w_varpart$varpart %>% mutate(dataset = "polyculture farms", location = "within rows"),
                           poly_a_varpart$varpart %>% mutate(dataset = "polyculture farms", location = "across rows")) 

varpart_table_wide <- varpart_table %>%
  pivot_wider(id_cols = c("dataset", "location"), names_from = "parts", values_from = "Adj.R")



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

