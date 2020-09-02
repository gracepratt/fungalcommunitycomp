## *****************************************************************************
## gdm analysis ###############################################################
## *****************************************************************************


## *****************************************************************************
## coefficient tables ##########################################################
## *****************************************************************************

## *****************************************************************************
## no scale ####################################################################
## *****************************************************************************

# Monoculture + Polyculture
noscale_table <- noscale$tables

# Monoculture
noscale_mono_table <- noscale_mono$tables

# Polyculture
noscale_poly_table <- noscale_poly$tables

## *****************************************************************************
## all blocks ##################################################################
## *****************************************************************************

# Monoculture + Polyculture
landscape_table <- landscape$tables
local_table <- local$tables

# Monoculture
landscape_mono_table <- landscape_mono$tables
local_mono_table <- local_mono$tables

# Polyculture
landscape_poly_table <- landscape_poly$tables
local_poly_table <- local_poly$tables

## *****************************************************************************
## Focal block #################################################################
## *****************************************************************************

# Monoculture + Polyculture
landscape_f_table <- landscape_f$tables 
local_f_table <- local_f$tables 

# Monoculture
landscape_mono_f_table <- landscape_mono_f$tables 
local_mono_f_table <- local_mono_f$tables 

# Polyculture
landscape_poly_f_table <- landscape_poly_f$tables 
local_poly_f_table <- local_poly_f$tables 


## *****************************************************************************
## Non-focal block #############################################################
## *****************************************************************************

# Monoculture + Polyculture
landscape_n_table <- landscape_f$tables 
local_n_table <- local_f$tables 

# Monoculture
landscape_mono_n_table <- landscape_mono_f$tables 
local_mono_n_table <- local_mono_f$tables 

# Polyculture
landscape_poly_n_table <- landscape_poly_f$tables 
local_poly_n_table <- local_poly_f$tables 


## *****************************************************************************
## variance partitioning #######################################################
## *****************************************************************************

## *****************************************************************************
## no scale ####################################################################
## *****************************************************************************



# Monoculture + Polyculture
noscale_var <- data.frame(gdm.varImp(noscale$spTable, geo=TRUE, nPerm=5, cores=6, fullModelOnly = TRUE)[[2]]) %>%
  rownames_to_column("predictor") %>%
  rename(variance = fullModel) %>%
  mutate(scaleLevel = "all") %>%
  dplyr::select(scaleLevel, predictor, variance)



# Monoculture
noscale_mono_var <- data.frame(gdm.varImp(noscale_mono$spTable, geo=TRUE, nPerm=5, cores=6, fullModelOnly = TRUE)[[2]]) %>%
  rownames_to_column("predictor") %>%
  rename(variance = fullModel) %>%
  mutate(scaleLevel = "monoculture") %>%
  dplyr::select(scaleLevel, predictor, variance)


# Polyculture
noscale_poly_var <- data.frame(gdm.varImp(noscale_poly$spTable,geo=TRUE, nPerm=5, cores=6, fullModelOnly = TRUE)[[2]]) %>%
  rownames_to_column("predictor") %>%
  rename(variance = fullModel) %>%
  mutate(scaleLevel = "polyculture") %>%
  dplyr::select(scaleLevel, predictor, variance)

varImp_noscale <- rbind(noscale_var,noscale_mono_var,noscale_poly_var)


## *****************************************************************************
## All blocks ##################################################################
## *****************************************************************************

# Monoculture + Polyculture
landscape_var <- data.frame(gdm.varImp(landscape$spTable, geo=TRUE, nPerm=5, cores=6, fullModelOnly = TRUE)[[2]]) %>%
  rownames_to_column("predictor") %>%
  rename(variance = fullModel) %>%
  mutate(scaleLevel = "landscape") %>%
  dplyr::select(scaleLevel, predictor, variance)

local_var <- data.frame(gdm.varImp(local$spTable, geo=TRUE, nPerm=5, cores=6, fullModelOnly = TRUE)[[2]]) %>%
  rownames_to_column("predictor") %>%
  rename(variance = fullModel) %>%
  mutate(scaleLevel = "local") %>%
  dplyr::select(scaleLevel, predictor, variance)

varImp_allBlocks <- rbind(landscape_var,  local_var)


## *****************************************************************************
## Focal #######################################################################
## *****************************************************************************

# Monoculture + Polyculture
landscape_f_var <- data.frame(gdm.varImp(landscape_f$spTable, geo=TRUE, nPerm=5, cores=6, fullModelOnly = TRUE)[[2]]) %>%
  rownames_to_column("predictor") %>%
  rename(variance = fullModel) %>%
  mutate(scaleLevel = "landscape_f") %>%
  dplyr::select(scaleLevel, predictor, variance)

local_f_var <- data.frame(gdm.varImp(local_f$spTable, geo=TRUE, nPerm=5, cores=6, fullModelOnly = TRUE)[[2]]) %>%
  rownames_to_column("predictor") %>%
  rename(variance = fullModel) %>%
  mutate(scaleLevel = "local_f") %>%
  dplyr::select(scaleLevel, predictor, variance)

# Monoculture
landscape_mono_f_var <- data.frame(gdm.varImp(landscape_mono_f$spTable, geo=TRUE, nPerm=5, cores=6, fullModelOnly = TRUE)[[2]]) %>%
  rownames_to_column("predictor") %>%
  rename(variance = fullModel) %>%
  mutate(scaleLevel = "landscape_mono_f") %>%
  dplyr::select(scaleLevel, predictor, variance)

local_mono_f_var <- data.frame(gdm.varImp(local_mono_f$spTable, geo=TRUE, nPerm=5, cores=6, fullModelOnly = TRUE)[[2]]) %>%
  rownames_to_column("predictor") %>%
  rename(variance = fullModel) %>%
  mutate(scaleLevel = "local_mono_f") %>%
  dplyr::select(scaleLevel, predictor, variance)

# Polyculture
landscape_poly_f_var <- data.frame(gdm.varImp(landscape_poly_f$spTable, geo=TRUE, nPerm=5, cores=6, fullModelOnly = TRUE)[[2]]) %>%
  rownames_to_column("predictor") %>%
  rename(variance = fullModel) %>%
  mutate(scaleLevel = "landscape_poly_f") %>%
  dplyr::select(scaleLevel, predictor, variance)

local_poly_f_var <- data.frame(gdm.varImp(local_poly_f$spTable, geo=TRUE, nPerm=5, cores=6, fullModelOnly = TRUE)[[2]]) %>%
  rownames_to_column("predictor") %>%
  rename(variance = fullModel) %>%
  mutate(scaleLevel = "local_poly_f") %>%
  dplyr::select(scaleLevel, predictor, variance)

varImp_Focal <- rbind(landscape_f_var,  local_f_var,landscape_mono_f_var, local_mono_f_var, landscape_poly_f_var,local_poly_f_var )

## *****************************************************************************
## Non-focal ###################################################################
## *****************************************************************************

# Monoculture + Polyculture
landscape_n_var <- data.frame(gdm.varImp(landscape_n$spTable, geo=TRUE, nPerm=5, cores=6, fullModelOnly = TRUE)[[2]]) %>%
  rownames_to_column("predictor") %>%
  rename(variance = fullModel) %>%
  mutate(scaleLevel = "landscape_n") %>%
  dplyr::select(scaleLevel, predictor, variance)

local_n_var <- data.frame(gdm.varImp(local_n$spTable, geo=TRUE, nPerm=5, cores=6, fullModelOnly = TRUE)[[2]]) %>%
  rownames_to_column("predictor") %>%
  rename(variance = fullModel) %>%
  mutate(scaleLevel = "local_n") %>%
  dplyr::select(scaleLevel, predictor, variance)

# Monoculture
landscape_mono_n_var <- data.frame(gdm.varImp(landscape_mono_n$spTable, geo=TRUE, nPerm=5, cores=6, fullModelOnly = TRUE)[[2]]) %>%
  rownames_to_column("predictor") %>%
  rename(variance = fullModel) %>%
  mutate(scaleLevel = "landscape_mono_n") %>%
  dplyr::select(scaleLevel, predictor, variance)

local_mono_n_var <- data.frame(gdm.varImp(local_mono_n$spTable, geo=TRUE, nPerm=5, cores=6, fullModelOnly = TRUE)[[2]]) %>%
  rownames_to_column("predictor") %>%
  rename(variance = fullModel) %>%
  mutate(scaleLevel = "local_mono_n") %>%
  dplyr::select(scaleLevel, predictor, variance)

# Polyculture
landscape_poly_n_var <- data.frame(gdm.varImp(landscape_poly_n$spTable, geo=TRUE, nPerm=5, cores=6, fullModelOnly = TRUE)[[2]]) %>%
  rownames_to_column("predictor") %>%
  rename(variance = fullModel) %>%
  mutate(scaleLevel = "landscape_poly_n") %>%
  dplyr::select(scaleLevel, predictor, variance)

local_poly_n_var <- data.frame(gdm.varImp(local_poly_n$spTable, geo=TRUE, nPerm=5, cores=6, fullModelOnly = TRUE)[[2]]) %>%
  rownames_to_column("predictor") %>%
  rename(variance = fullModel) %>%
  mutate(scaleLevel = "local_poly_n") %>%
  dplyr::select(scaleLevel, predictor, variance)


varImp_nonfocal <- rbind(landscape_n_var,  local_n_var,landscape_mono_n_var, local_mono_n_var, landscape_poly_n_var,local_poly_n_var )




## *****************************************************************************
## mantel tests ################################################################
## *****************************************************************************

# Composition
mantel <- mantel_func(all, edaphic_variables, type = "composition", mantelType = "vegan")
mono_mantel <- mantel_func(all %>% filter(FarmType == "Monoculture"), envi_factors, type = "composition", mantelType = "vegan")
poly_mantel <- mantel_func(all %>% filter(FarmType == "Polyculture"), envi_factors, type = "composition", mantelType = "ecodist")

# Nestedness
mantel_nestedness <- mantel_func(all, edaphic_variables, type = "nestedness", mantelType = "vegan")
mono_mantel_nestedness <- mantel_func(all %>% filter(FarmType == "Monoculture"), envi_factors, type = "nestedness", mantelType = "ecodist")
poly_mantel_nestedness <- mantel_func(all %>% filter(FarmType == "Polyculture"), envi_factors, type = "nestedness", mantelType = "ecodist")

# Turnover
mantel_turnover <- mantel_func(all, edaphic_variables, type = "turnover", mantelType = "ecodist")
mono_mantel_turnover <- mantel_func(all %>% filter(FarmType == "Monoculture"), envi_factors, type = "turnover", mantelType = "ecodist")
poly_mantel_turnover <- mantel_func(all %>% filter(FarmType == "Polyculture"), envi_factors, type = "turnover", mantelType = "ecodist")


## *****************************************************************************
## alpha models ################################################################
## *****************************************************************************

# options(contrasts = c("contr.sum","contr.poly"))
# 
# richnessModel <- glmer.nb(round(observed,0) ~ FarmType*Block+   scale(pH) + scale(P) + scale(NP_ratio) + scale(TOC) + scale(N) + cropDiversity  + (1|farmCode), data= all, nAGQ=1, na.action=na.fail)
# 
# 
# shannonModels <-  lmer(log(shannon + 1) ~ FarmType*Block+   scale(pH) + scale(P) + scale(NP_ratio) + scale(TOC) + scale(N) + cropDiversity + (1|farmCode), data=all,  na.action=na.exclude)
#      

## *****************************************************************************
## environmental predictor models ##############################################
## *****************************************************************************

envModels <- sapply(c(envi_factors), USE.NAMES=TRUE, simplify = FALSE,
                    function(x) {
                      model <- lmer(substitute(log(i+1) ~ FarmType*Block + (1|farmCode), 
                                               list(i = as.name(x))), data=all, na.action=na.exclude)
                      summary(model)
                    })

## *****************************************************************************
## levene's test ###############################################################
## The test reveals a p-value greater than 0.05, indicating that there 
## is no significant difference between the group variances
## *****************************************************************************

pH_lt <- as.data.frame(car::leveneTest(pH ~ factor(FarmType), data=all)[1,2:3], row.names="pH")
NP_ratio_lt <-  as.data.frame(car::leveneTest(NP_ratio ~ factor(FarmType), data=all)[1,2:3], row.names="NP_ratio")
P_lt <- as.data.frame(car::leveneTest(P ~ factor(FarmType), data=all)[1,2:3], row.names="P")
TOC_lt <-  as.data.frame(car::leveneTest(TOC ~ factor(FarmType), data=all)[1,2:3], row.names="TOC")
N_lt <- as.data.frame(car::leveneTest(N ~ factor(FarmType), data=all)[1,2:3], row.names="N")

leveneTest_table <- round(rbind(pH_lt,NP_ratio_lt,P_lt,TOC_lt,N_lt),3)


## *****************************************************************************
## TITAN2 ######################################################################
## *****************************************************************************

add3 <- function(x, na.rm = FALSE) ifelse(x>0, x+3, x)
add3_all <- function(x, na.rm = FALSE) x+3



taxa <- all %>% dplyr::select(contains("OTU"))

taxa.pa <- decostand(taxa, "pa")

plus3 <- data.frame(sums=colSums(taxa.pa)) %>%
  rownames_to_column("OTU") %>%
  filter(sums > 3) %>% pull(OTU)


taxa <- taxa[, colSums(taxa != 0) > 0]
taxa <- taxa[c(plus3)]


# taxa <- taxa %>% 
  # mutate_all(add3_all)



pH.titanOutput <- titanOutput(pH.titan)



env.pH <- all %>% dplyr::select(pH)
env.N <- all %>% dplyr::select(N)
env.P <- all %>% dplyr::select(P)
env.NP <- all %>% dplyr::select(NP_ratio)
env.TOC <- all %>% dplyr::select(TOC)
env.CD <- all %>% dplyr::select(cropDiversity)


pH.titan <- titan(env.pH, taxa, ncpus = 8)
N.titan <- titan(env.N, taxa, ncpus = 8)
P.titan <- titan(env.P, taxa, ncpus = 8)
NP.titan <- titan(env.NP, taxa, ncpus = 8)
TOC.titan <- titan(env.TOC, taxa, ncpus = 8)
CD.titan <- titan(env.CD, taxa, ncpus = 8)


pH.titan_table <- pH.titan$sppmax %>%
  as.data.frame() %>% 
  rownames_to_column("OTU") %>%
  left_join(tax, by = "OTU") %>%
  filter(filter >0) %>%
  mutate(lowSE = zenv.cp-`5%`, highSE = abs( zenv.cp-`95%`), filter = factor(filter), organization = ifelse(filter == 1, zenv.cp*1, zenv.cp*-1)) %>%
  group_by(filter) %>%
    mutate(rank = order(order(organization, decreasing=TRUE)), 
           rank2 = ifelse(filter != 1, rank+0.5, rank)) %>%
  ungroup() %>%
  arrange( rank2) %>%
  mutate(OTU = factor(OTU, levels=OTU))

ggplot(pH.titan_table , aes(y= zenv.cp, x= OTU, color=Taxon )) +
  geom_errorbar(aes(ymin=zenv.cp-lowSE, ymax=zenv.cp+highSE), width=0, size=0.75, position=position_dodge(0.05)) +
  geom_point(aes(shape=factor(filter), size=zscore), fill = "white", stroke=0.75) +
  scale_shape_manual(values = c(16,21) ) + 
  scale_color_manual(values = taxonColor(taxons=pH.titan_table$Taxon)) + 
  coord_flip() + theme_classic() #+ facet_wrap(~filter)


# ggplot(pH.titan_table , aes(y= zenv.cp, x= rank2, color=Taxon )) +
#   geom_errorbar(data=pH.titan_table %>% filter(filter == 1) , aes(ymin=zenv.cp-lowSE, ymax=zenv.cp+highSE), width=0, size=0.75, position=position_dodge(0.05)) +
#   geom_point(data=pH.titan_table %>% filter(filter == 1) ,aes(shape=factor(filter), size=zscore), fill = "white", stroke=0.75) +
#   geom_errorbar(data=pH.titan_table %>% filter(filter != 1) , aes(ymin=zenv.cp-lowSE, ymax=zenv.cp+highSE), width=0, size=0.75, position=position_dodge(0.05)) +
#   geom_point(data=pH.titan_table %>% filter(filter != 1) ,aes(shape=factor(filter), size=zscore), fill = "white", stroke=0.75) +
#   scale_shape_manual(values = c(16,21) ) + 
#   scale_color_brewer(palette = "Dark2") +
#   scale_x_continuous(sec.axis = sec_axis(~./1, breaks = c(1:31), labels=pH.titan_table$OTU), breaks = c(1:31), labels=pH.titan_table$OTU) +
#   coord_flip() + theme_classic() #+ facet_wrap(~filter)


P.titan_table <- P.titan$sppmax %>%
  as.data.frame() %>% 
  rownames_to_column("OTU") %>%
  left_join(tax, by = "OTU") %>%
  filter(filter >0) %>%
  mutate(lowSE = zenv.cp-`5%`, highSE = abs( zenv.cp-`95%`), filter = factor(filter), organization = ifelse(filter == 1, zenv.cp*-1, zenv.cp)) %>%
  arrange( organization, filter) %>%
  mutate(OTU = factor(OTU, levels=OTU))

ggplot(P.titan_table , aes(y= zenv.cp, x= OTU, color=Taxon )) +
  geom_errorbar(aes(ymin=zenv.cp-lowSE, ymax=zenv.cp+highSE, linetype=factor(filter)), width=0, size=0.75, position=position_dodge(0.05)) +
  geom_point(aes(shape=factor(filter), size=zscore), fill = "white", stroke=0.75) +
  scale_shape_manual(values = c(16,21) ) + 
  scale_color_manual(values = taxonColor(taxons=P.titan_table$Taxon)) + 
  coord_flip() + theme_classic() #+ facet_wrap(~filter,scales = "free")




CD.titan_table <- CD.titan$sppmax %>%
  as.data.frame() %>% 
  rownames_to_column("OTU") %>%
  left_join(tax, by = "OTU") %>%
  filter(filter >0) %>%
  mutate(lowSE = zenv.cp-`5%`, highSE = abs( zenv.cp-`95%`), filter = factor(filter), organization = ifelse(filter == 1, zenv.cp*-1, zenv.cp)) %>%
  arrange( organization, filter) %>%
  mutate(OTU = factor(OTU, levels=OTU))

ggplot(CD.titan_table , aes(y= zenv.cp, x= OTU, color=Taxon )) +
  geom_errorbar(aes(ymin=zenv.cp-lowSE, ymax=zenv.cp+highSE, linetype=factor(filter)), width=0, size=0.75, position=position_dodge(0.05)) +
  geom_point(aes(shape=factor(filter), size=zscore), fill = "white", stroke=0.75) +
  scale_shape_manual(values = c(16,21) ) + 
  # scale_color_brewer(palette = "Dark2") +
  coord_flip() + theme_classic() #+ facet_wrap(~filter,scales = "free")




NP.titan_table <- NP.titan$sppmax %>%
  as.data.frame() %>% 
  rownames_to_column("OTU") %>%
  left_join(tax, by = "OTU") %>%
  filter(filter >0) %>%
  mutate(lowSE = zenv.cp-`5%`, highSE = abs( zenv.cp-`95%`), filter = factor(filter), organization = ifelse(filter == 1, zenv.cp*-1, zenv.cp)) %>%
  arrange( organization, filter) %>%
  mutate(OTU = factor(OTU, levels=OTU))

ggplot(NP.titan_table , aes(y= zenv.cp, x= OTU, color=Taxon )) +
  geom_errorbar(aes(ymin=zenv.cp-lowSE, ymax=zenv.cp+highSE, linetype=factor(filter)), width=0, size=0.75, position=position_dodge(0.05)) +
  geom_point(aes(shape=factor(filter), size=zscore), fill = "white", stroke=0.75) +
  scale_shape_manual(values = c(16,21) ) + 
  # scale_color_brewer(palette = "Dark2") +
  coord_flip() + theme_classic() #+ facet_wrap(~filter,scales = "free")



TOC.titan_table <- TOC.titan$sppmax %>%
  as.data.frame() %>% 
  rownames_to_column("OTU") %>%
  left_join(tax, by = "OTU") %>%
  filter(filter >0) %>%
  mutate(lowSE = zenv.cp-`5%`, highSE = abs( zenv.cp-`95%`), filter = factor(filter), organization = ifelse(filter == 1, zenv.cp*-1, zenv.cp)) %>%
  arrange( organization, filter) %>%
  mutate(OTU = factor(OTU, levels=OTU))

ggplot(TOC.titan_table , aes(y= zenv.cp, x= OTU, color=Taxon )) +
  geom_errorbar(aes(ymin=zenv.cp-lowSE, ymax=zenv.cp+highSE, linetype=factor(filter)), width=0, size=0.75, position=position_dodge(0.05)) +
  geom_point(aes(shape=factor(filter), size=zscore), fill = "white", stroke=0.75) +
  scale_shape_manual(values = c(16,21) ) + 
  # scale_color_brewer(palette = "Dark2") +
  coord_flip() + theme_classic() #+ facet_wrap(~filter,scales = "free")



# ggplot(pH.titan_table , aes(x= zenv.cp, y= OTU, color=Taxon )) +
#   geom_point(data=pH.titan_table %>% filter(filter == 1) , aes(x= zenv.cp, y= OTU, color=Taxon , shape=factor(filter), size=zscore), color = "black") +
#   geom_errorbar(data=pH.titan_table %>% filter(filter == 1) , aes(xmin=zenv.cp-lowSE, xmax=zenv.cp+highSE), width=.2, position=position_dodge(0.05), color = "black") +
#   geom_point(data=pH.titan_table %>% filter(filter == 2) , aes(x= zenv.cp, y= OTU, color=Taxon , shape=factor(filter), size=zscore), color = "black") +
#   geom_errorbar(data=pH.titan_table %>% filter(filter == 2) , aes(xmin=zenv.cp-lowSE, xmax=zenv.cp+highSE), width=.2, position=position_dodge(0.05), color = "black") +
#   scale_shape_manual(values = c(16,1) ) +
#   # scale_y_continuous(sec.axis = sec_axis(~./1)) +
#   # coord_flip() + 
#   theme_classic()

