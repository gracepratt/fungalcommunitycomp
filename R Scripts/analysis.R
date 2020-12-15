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
poly_mantel <- mantel_func(all %>% filter(FarmType == "Polyculture"), envi_factors, type = "composition", mantelType = "vegan")

# Nestedness
mantel_nestedness <- mantel_func(all, edaphic_variables, type = "nestedness", mantelType = "vegan")
mono_mantel_nestedness <- mantel_func(all %>% filter(FarmType == "Monoculture"), envi_factors, type = "nestedness", mantelType = "vegan")
poly_mantel_nestedness <- mantel_func(all %>% filter(FarmType == "Polyculture"), envi_factors, type = "nestedness", mantelType = "vegan")

# Turnover
mantel_turnover <- mantel_func(all, edaphic_variables, type = "turnover", mantelType = "vegan")
mono_mantel_turnover <- mantel_func(all %>% filter(FarmType == "Monoculture"), envi_factors, type = "turnover", mantelType = "vegan")
poly_mantel_turnover <- mantel_func(all %>% filter(FarmType == "Polyculture"), envi_factors, type = "turnover", mantelType = "vegan")


mantelTests <- as.data.frame(rbind(mantel$table,mono_mantel$table,poly_mantel$table)) %>%
  mutate(Test = rep("compositional",27), 
         Data = rep(c("All","Mono","Poly"), each=9))


nestednessTests <- as.data.frame(rbind(mantel_nestedness$table,mono_mantel_nestedness$table,poly_mantel_nestedness$table)) %>%
  mutate(Test = rep("nestedness",27), 
         Data = rep(c("All","Mono","Poly"), each=9))

turnoverTests <- as.data.frame(rbind(mantel_turnover$table,mono_mantel_turnover$table,poly_mantel_turnover$table)) %>%
  mutate(Test = rep("turnover",27), 
         Data = rep(c("All","Mono","Poly"), each=9))


allMantel <- rbind(mantelTests, nestednessTests, turnoverTests)

write.csv(allMantel, "Outputs/Tables/allMantel.csv", row.names = FALSE)


ge## *****************************************************************************
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
                      model <- lmer(substitute(log(i+1) ~ cropDiversity + (1|farmCode), 
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


taxa <- all %>% dplyr::select(contains("OTU"))

taxa.pa <- decostand(taxa, "pa")

plus3 <- data.frame(sums=colSums(taxa.pa)) %>%
  rownames_to_column("OTU") %>%
  filter(sums > 3) %>% pull(OTU)


taxa <- taxa[, colSums(taxa != 0) > 0]
taxa <- taxa[c(plus3)]



# select variables
env.pH <- all %>% dplyr::select(pH)
env.N <- all %>% dplyr::select(N)
env.P <- all %>% dplyr::select(P)
env.NP <- all %>% dplyr::select(NP_ratio)
env.TOC <- all %>% dplyr::select(TOC)
env.CD <- all %>% dplyr::select(cropDiversity)

# TITAN analysis

pH.titan <- titan(env.pH, taxa, ncpus = 8)
N.titan <- titan(env.N, taxa, ncpus = 8)
P.titan <- titan(env.P, taxa, ncpus = 8)
NP.titan <- titan(env.NP, taxa, ncpus = 8)
TOC.titan <- titan(env.TOC, taxa, ncpus = 8)
CD.titan <- titan(env.CD, taxa, ncpus = 8)

# TITAN Output
pH.titanOutput <- titanOutput(pH.titan)
N.titanOutput <- titanOutput(N.titan)
P.titanOutput <- titanOutput(P.titan)
NP.titanOutput <- titanOutput(NP.titan)
TOC.titanOutput <- titanOutput(TOC.titan)
CD.titanOutput <- titanOutput(CD.titan)


legend <- get_legend(relAbun_CD + guides(fill=guide_legend(ncol=5)))

compositionalPlots <- plot_grid(
  relAbun_pH + theme(legend.position="none"), 
  relAbun_N + theme(legend.position="none"), 
  relAbun_P+ theme(legend.position="none"), 
  relAbun_NP+ theme(legend.position="none"), 
  relAbun_TOC+ theme(legend.position="none"), 
  relAbun_CD + theme(legend.position="none"), ncol=1)

compositionalPlots_h <- plot_grid(
  relAbun_pH + theme(legend.position="none"), 
  relAbun_N + theme(legend.position="none", axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()),
  relAbun_P + theme(legend.position="none", axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()),
  relAbun_NP + theme(legend.position="none", axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()),
  relAbun_TOC + theme(legend.position="none", axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()),
  relAbun_CD + theme(legend.position="none", axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()), nrow=1, align="v")

ggsave( "compositionalPlots.pdf",plot=compositionalPlots, path=fig.path, width=4, height=22, useDingbats = FALSE)


titanPlots <- plot_grid(pH.titanOutput$plot + theme(legend.position="none"), N.titanOutput$plot + theme(legend.position="none"), P.titanOutput$plot + theme(legend.position="none"), NP.titanOutput$plot + theme(legend.position="none"), TOC.titanOutput$plot + theme(legend.position="none"), CD.titanOutput$plot + theme(legend.position="none"), ncol=1)



titalComposition_h <- plot_grid(
  relAbun_pH + theme(legend.position="none"), 
  relAbun_N + theme(legend.position="none", axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()),
  relAbun_P + theme(legend.position="none", axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()),
  relAbun_NP + theme(legend.position="none", axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()),
  relAbun_TOC + theme(legend.position="none", axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()),
  relAbun_CD + theme(legend.position="none", axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()),
  pH.titanOutput$plot + theme(legend.position="none"), 
  N.titanOutput$plot + theme(legend.position="none", axis.title.y=element_blank()), 
  P.titanOutput$plot + theme(legend.position="none", axis.title.y=element_blank()), 
  NP.titanOutput$plot + theme(legend.position="none", axis.title.y=element_blank()), 
  TOC.titanOutput$plot + theme(legend.position="none", axis.title.y=element_blank()), 
  CD.titanOutput$plot + theme(legend.position="none", axis.title.y=element_blank()), nrow=2, align="hv")




titalComposition_h <- egg::ggarrange(
  pH.titanOutput$plot + theme(legend.position="none"), 
  N.titanOutput$plot + theme(legend.position="none", axis.title.y=element_blank()), 
  P.titanOutput$plot + theme(legend.position="none", axis.title.y=element_blank()), 
  NP.titanOutput$plot + theme(legend.position="none", axis.title.y=element_blank()), 
  TOC.titanOutput$plot + theme(legend.position="none", axis.title.y=element_blank()), 
  CD.titanOutput$plot + theme(legend.position="none", axis.title.y=element_blank()),
  relAbun_pH + theme(legend.position="none"), 
  relAbun_N + theme(legend.position="none", axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()),
  relAbun_P + theme(legend.position="none", axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()),
  relAbun_NP + theme(legend.position="none", axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()),
  relAbun_TOC + theme(legend.position="none", axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()),
  relAbun_CD + theme(legend.position="none", axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()),
  nrow=2)

titalComposition <- plot_grid(compositionalPlots,titanPlots, axis="top" )

# titalComposition_h <- plot_grid(compositionalPlots_h,titanPlots_h, nrow=2, align = "hv")

ggsave( "titanPlots.pdf",plot=titanPlots, path=fig.path, width=4, height=22, useDingbats = FALSE)

ggsave( "titalComposition.pdf",plot=titalComposition, path=fig.path, width=7.5, height=16, useDingbats = FALSE)

ggsave( "titalComposition_h.pdf",plot=titalComposition_h, path=fig.path, width=16, height=6, useDingbats = FALSE)

ggsave("titalComposition_h.tiff",plot=titalComposition_h, path=fig.path, width=16, height=6)
