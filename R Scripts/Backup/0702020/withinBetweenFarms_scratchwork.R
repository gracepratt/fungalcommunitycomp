# distance

envi_factors <- c("pH", "P", "TOC", "N", "NP_ratio", "FarmBi", "CropBi")


# ALL

# across farms
across <- backwardsSelection(df=all_fungi, guild= "all fungi", block= c("F") ,focalcrop= c("Eggplant", "Squash"), farmtype=c("Monoculture","Polyculture"), env_factors=envi_factors, geo=TRUE, maxDist = "across")

# tables 
# best model is `item:4` w/ Geographic, pH, P, N
across_table <- across$tables$`item:4`

#plot
across_plots <- across$plotList$`item:4` + theme(legend.position="none")

# dissimilarity within farms
within <- backwardsSelection(df=all_fungi, guild= "all fungi", block= "F" ,focalcrop=  c("Eggplant", "Squash"), farmtype=c("Monoculture","Polyculture"), env_factors=envi_factors, geo=TRUE, maxDist = "within")

# tables 
# best model is `item:3` w/ pH, P, N, TOC, N
within_table <- within$tables$`item:3`

#plot
within_plots <- within$plotList$`item:3` + theme(legend.position="none")

# legend for all plots
all_legend <- get_legend(across$plotList$`item:1`)

# all farms plots
all_spatial <- cowplot::plot_grid(plot_grid(across_plots, within_plots, nrow=1), all_legend, rel_widths = c(2, .4) )


# MONO


# across farms
mono_across <- backwardsSelection(df=all_fungi, guild= "all fungi", block= "F" ,focalcrop= "Eggplant", farmtype=c("Monoculture"), env_factors=envi_factors, geo=TRUE, maxDist = "across")

# tables 
# best model is `item:3` w/ Geographic, pH, P, TOC, N
mono_across_table <- mono_across$tables$`item:3`

#plot
mono_across_plots <- mono_across$plotList$`item:3` + theme(legend.position="none")

# dissimilarity within farms
mono_within <- backwardsSelection(df=all_fungi, guild= "all fungi", block= "F" ,focalcrop= "Eggplant", farmtype=c("Monoculture"), env_factors=envi_factors, geo=TRUE, maxDist = "within")

# tables 
# best model is `item:3` w/ pH, P, N, TOC, N
mono_within_table <- mono_within$tables$`item:3`

#plot
mono_within_plots <- mono_within$plotList$`item:3` + theme(legend.position="none")



# all farms plots
mono_spatial_plots <- cowplot::plot_grid(plot_grid(mono_across_plots, mono_within_plots, nrow=1), all_legend, rel_widths = c(2, .4) )



# POLY


# across farms
poly_across <- backwardsSelection(df=all_fungi, guild= "all fungi", block= "F" ,focalcrop= "Eggplant", farmtype=c("Polyculture"), env_factors=envi_factors, geo=TRUE, maxDist = "across")

# tables 
# best model is `item:4` w/ Geographic, pH, P, N
poly_across_table <- poly_across$tables$`item:4`

#plot
poly_across_plots <- poly_across$plotList$`item:4` + theme(legend.position="none")

# dissimilarity within farms
poly_within <- backwardsSelection(df=all_fungi, guild= "all fungi", block= "F" ,focalcrop= "Eggplant", farmtype=c("Polyculture"), env_factors=envi_factors, geo=TRUE, maxDist = "within")

# tables 
# best model is `item:3` w/ pH, P, TOC
poly_within_table <- poly_within$tables$`item:4`

#plot
poly_within_plots <- poly_within$plotList$`item:4` + theme(legend.position="none")



# all farms plots
poly_spatial_plots <- cowplot::plot_grid(plot_grid(poly_across_plots, poly_within_plots, nrow=1), all_legend, rel_widths = c(2, .4) )




allplots_spatial <- plot_grid(all_spatial,mono_spatial_plots, poly_spatial_plots, labels=c("All","MONO","POLY"), ncol=1)



# models of alpha
options(contrasts = c("contr.sum","contr.poly"))

alpha <- as.data.frame(microbiome::alpha(t(amf %>% dplyr::select(contains("OTU"))), index=c("diversity_shannon", "observed"))) %>%
  rename(obs = observed, div = diversity_shannon) %>%
  mutate(Key = amf$Key) %>%
  right_join(prop, by="Key" ) %>%
  mutate(NP_ratio = (N/P)*100)

row.names(alpha) <- alpha$Key
# soil texture index



textPCA <- prcomp(alpha[c("CLAY","SILT","SAND")], scale.=TRUE)
textPCAvalues <- data.frame(textPCA$x)[1:2]
vscores <- data.frame(textPCA$rotation)[1:2]
alpha$soilTexture <- textPCAvalues$PC1[match(alpha$Key, row.names(textPCAvalues))]
alpha$soilTexture2 <- textPCAvalues$PC2[match(alpha$Key, row.names(textPCAvalues))]


alphaModels <-  glmer.nb(obs ~ FarmType*Block+   scale(pH) + scale(P) + scale(NP_ratio) + scale(TOC) + scale(N) + scale(soilTexture) + (1|farmCode), data=alpha, nAGQ=1, na.action=na.exclude)
alphaModels_summary <- summary(alphaModels)   


envModels <- sapply(c(envi_factors), USE.NAMES=TRUE, simplify = FALSE,
                    function(x) {
                      model <- lmer(substitute(log(i+1) ~ FarmType*Block + (1|farmCode), list(i = as.name(x))), data=alphaDF, na.action=na.exclude)
                      summary(model)
                    })


divIndices <- c("obs_all","obs_amf", "obs_path","obs_sap","obs_par", "div_all","div_amf", "div_path","div_sap","div_par")





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


alphaEnvSummary <-  alphaDF[, names(alphaDF) %in% c("FarmKey","FarmType", envi_factors)] %>%
  gather(key = "variable", value = "value", -c(FarmKey,FarmType)) %>%
  group_by(FarmKey, FarmType, variable) %>%
  summarize_at("value", list(mean = mean, SE=std.error,sd=sd, min = min, max = max), na.rm=TRUE) %>%
  ungroup() %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, 3) %>%
  arrange(FarmType,variable)


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


# The test reveals a p-value greater than 0.05, indicating that there is no significant difference between the group variances
pH_lt <- as.data.frame(car::leveneTest(pH ~ FarmType, data=alphaDF)[1,2:3], row.names="pH")
NP_ratio_lt <-  as.data.frame(car::leveneTest(NP_ratio ~ FarmType, data=alphaDF)[1,2:3], row.names="NP_ratio")
P_lt <- as.data.frame(car::leveneTest(P ~ FarmType, data=alphaDF)[1,2:3], row.names="P")
TOC_lt <-  as.data.frame(car::leveneTest(TOC ~ FarmType, data=alphaDF)[1,2:3], row.names="TOC")
N_lt <- as.data.frame(car::leveneTest(N ~ FarmType, data=alphaDF)[1,2:3], row.names="N")

leveneTest_table <- round(rbind(pH_lt,NP_ratio_lt,P_lt,TOC_lt,N_lt),3)


pH_lt_FTBL <- as.data.frame(car::leveneTest(pH ~ FTBL, data=alphaDF)[1,2:3], row.names="pH")
NP_ratio_lt_FTBL <-  as.data.frame(car::leveneTest(NP_ratio ~ FTBL, data=alphaDF)[1,2:3], row.names="NP_ratio")
P_lt_FTBL <- as.data.frame(car::leveneTest(P ~ FTBL, data=alphaDF)[1,2:3], row.names="P")
TOC_lt_FTBL <-  as.data.frame(car::leveneTest(TOC ~ FTBL, data=alphaDF)[1,2:3], row.names="TOC")
N_lt_FTBL <- as.data.frame(car::leveneTest(N ~ FTBL, data=alphaDF)[1,2:3], row.names="N")

leveneTest_table_FTBL <- round(rbind(pH_lt_FTBL,NP_ratio_lt_FTBL,P_lt_FTBL,TOC_lt_FTBL,N_lt_FTBL),3)



pH_lt_Block <- as.data.frame(car::leveneTest(pH ~ Block, data=alphaDF)[1,2:3], row.names="pH")
NP_ratio_lt_Block <-  as.data.frame(car::leveneTest(NP_ratio ~ Block, data=alphaDF)[1,2:3], row.names="NP_ratio")
P_lt_Block <- as.data.frame(car::leveneTest(P ~ Block, data=alphaDF)[1,2:3], row.names="P")
TOC_lt_Block <-  as.data.frame(car::leveneTest(TOC ~ Block, data=alphaDF)[1,2:3], row.names="TOC")
N_lt_Block <- as.data.frame(car::leveneTest(N ~ Block, data=alphaDF)[1,2:3], row.names="N")

leveneTest_table_Block <- round(rbind(pH_lt_Block,NP_ratio_lt_Block,P_lt_Block,TOC_lt_Block,N_lt_Block),3)

                      