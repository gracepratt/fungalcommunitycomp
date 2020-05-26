# distance

# FarmBi, Geographic, N, NP_ratio, P, pH, TOC
colors1 <- c("#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")
colors2 <- c("#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")
# colors1 <- c("#332288","#117733","#44AA99","#88CCEE", "#DDCC77", "#CC6677", "#AA4499", "#882255")
# colors2 <- c("#117733","#44AA99","#88CCEE", "#DDCC77", "#CC6677", "#AA4499", "#882255")

envi_factors <- c("pH", "P", "TOC", "N", "NP_ratio")


df <- guild_filter(all_fungi, "Plant Pathogen") %>%
  filter(Block == "F", FocalCrop == "Eggplant") 


df <- all_fungi %>%
  filter(Block == "F", FocalCrop == "Eggplant") 


spatial <- input_diss(df, c("pH", "P", "TOC", "N", "NP_ratio", "FarmBi") )
spatial_diss <- input_diss(df, c("pH", "P", "TOC", "N", "NP_ratio", "FarmBi") )
spatial$distanceM <-distHaversine(spatial[,3:4],spatial[,5:6])

#monoculture
monocultures_amf <- df %>%
  filter(FarmType == "Monoculture") 

spatial_mono <- input_diss(monocultures_amf, envi_factors) 
spatial_mono_diss <- input_diss(monocultures_amf, envi_factors)
spatial_mono$distanceM <-distHaversine(spatial_mono[,3:4],spatial_mono[,5:6])

#polyculture
polycultures_amf <- df %>%
  filter(FarmType == "Polyculture")

spatial_poly <- input_diss(polycultures_amf, envi_factors)
spatial_poly_diss <- input_diss(polycultures_amf, envi_factors)
spatial_poly$distanceM <-distHaversine(spatial_poly[,3:4],spatial_poly[,5:6])



# ALL

# across farms

across <- spatial %>% 
  dplyr::select(-"distanceM")

amf_across <- gdm(across, geo = TRUE)
table(amf_across)

# varImp
# acrossVarImp <- gdm.varImp(across, geo=TRUE, nPerm=5, cores=8)
  
#plot
amf_across_plots <- predictors_plot(amf_across) + scale_color_manual(values=colors1) + ggtitle("Across") + theme(legend.position="none", plot.title = element_text(hjust = 0.5))

# dissimilarity within farms
within <- spatial %>% 
  filter(distanceM < 60) %>%
  dplyr::select(-"distanceM")

amf_within <- gdm(within, geo = TRUE)
table(amf_within)


amf_within_plots <- predictors_plot(amf_within) + scale_color_manual(values=colors1) + ggtitle("Within") + theme(legend.position="none", plot.title = element_text(hjust = 0.5),axis.title.y = element_blank())

# dissimilarity between farms
between <- spatial %>% 
  filter(distanceM > 60) %>%
  dplyr::select(-"distanceM")


amf_between <- gdm(between, geo = TRUE)
table(amf_between)

amf_between_plots <- predictors_plot(amf_between) + scale_color_manual(values=colors1) + ggtitle("Between") + theme(legend.position="none",plot.title = element_text(hjust = 0.5),axis.title.y = element_blank())

all_legend <- get_legend(predictors_plot(amf_across)+ scale_color_manual(values=colors1))

all_spatial <- cowplot::plot_grid(plot_grid(amf_across_plots, amf_within_plots, amf_between_plots, nrow=1), all_legend, rel_widths = c(3, .4) )


# MONO


# across farms

across_mono <- gdm(spatial_mono, geo = TRUE)
table(across_mono)

mono_across_plots <- predictors_plot(across_mono) + scale_color_manual(values=colors2)  + ggtitle("Across") + theme(legend.position="none", plot.title = element_text(hjust = 0.5))

# dissimilarity within farms
within_mono <- spatial_mono %>% 
  filter(distanceM < 60) %>%
  dplyr::select(-"distanceM")

amf_within_mono <- gdm(within_mono, geo = TRUE)
table(amf_within_mono)


mono_within_plots <- predictors_plot(amf_within_mono) + scale_color_manual(values=colors2) + ggtitle("Within") + theme(legend.position="none", plot.title = element_text(hjust = 0.5),axis.title.y = element_blank())


# dissimilarity between farms
between_mono <- spatial_mono %>% 
  filter(distanceM > 60) %>%
  dplyr::select(-"distanceM")


amf_between_mono <- gdm(between_mono, geo = TRUE)
table(amf_between_mono)

mono_between_plots <- predictors_plot(amf_between_mono)+ scale_color_manual(values=colors2) + ggtitle("Between") + theme(legend.position="none",plot.title = element_text(hjust = 0.5),axis.title.y = element_blank())

mono_legend <- get_legend(predictors_plot(across_mono)+ scale_color_manual(values=colors2))

mono_plots <- cowplot::plot_grid(plot_grid(mono_across_plots, mono_within_plots, mono_between_plots, nrow=1), mono_legend, rel_widths = c(3, .4) )


# POLY

# across farms

across_poly <- gdm(spatial_poly, geo = TRUE)
table(across_poly)

poly_across_plots <- predictors_plot(across_poly)+ scale_color_manual(values=colors2) + ggtitle("Across") + theme(legend.position="none", plot.title = element_text(hjust = 0.5))

# dissimilarity within farms
within_poly <- spatial_poly %>% 
  filter(distanceM < 60) %>%
  dplyr::select(-"distanceM")

amf_within_poly <- gdm(within_poly, geo = TRUE)
table(amf_within_poly)


poly_within_plots <- predictors_plot(amf_within_poly)+ scale_color_manual(values=colors2) + ggtitle("Within") + theme(legend.position="none", plot.title = element_text(hjust = 0.5),axis.title.y = element_blank())


# dissimilarity between farms
between_poly <- spatial_poly %>% 
  filter(distanceM > 60) %>%
  dplyr::select(-"distanceM")


amf_between_poly <- gdm(between_poly, geo = TRUE)
table(amf_between_poly)

poly_between_plots <- predictors_plot(amf_between_poly)+ scale_color_manual(values=colors2) + ggtitle("Between") + theme(legend.position="none",plot.title = element_text(hjust = 0.5),axis.title.y = element_blank())

poly_legend <- get_legend(predictors_plot(across_poly)+ scale_color_manual(values=colors2))

poly_plots <- cowplot::plot_grid(plot_grid(poly_across_plots, poly_within_plots, poly_between_plots, nrow=1), poly_legend, rel_widths = c(3, .4) )



allplots <- plot_grid(all_spatial,mono_plots, poly_plots, labels=c("All","MONO","POLY"), ncol=1)
# formatsitepair(within, bioFormat=4, XColumn="Long_point", YColumn="Lat_point",
# siteColumn="Key", predData= envi_table, abundance = FALSE)



# models of alpha

alpha <- as.data.frame(microbiome::alpha(t(amf %>% dplyr::select(contains("OTU"))), index=c("diversity_shannon", "observed"))) %>%
  rename(obs = observed, div = diversity_shannon) %>%
  mutate(Key = amf$Key) %>%
  right_join(prop, by="Key" ) %>%
  mutate(NP_ratio = (N/P)*100)

row.names(alpha) <- alpha$Key
# soil texture index

library(ggfortify)

textPCA <- prcomp(alpha[c("CLAY","SILT","SAND")], scale.=TRUE)
textPCAvalues <- data.frame(textPCA$x)[1:2]
vscores <- data.frame(textPCA$rotation)[1:2]
alpha$soilTexture <- textPCAvalues$PC1[match(alpha$Key, row.names(textPCAvalues))]
alpha$soilTexture2 <- textPCAvalues$PC2[match(alpha$Key, row.names(textPCAvalues))]


alphaModels <-  glmer.nb(obs ~ FarmType*Block+   scale(pH) + scale(P) + scale(NP_ratio) + scale(TOC) + scale(N) + scale(soilTexture) + (1|farmCode), data=alpha, nAGQ=1, na.action=na.exclude)
summary(alphaModels)   


envModels <- sapply(c(envi_factors), USE.NAMES=TRUE, simplify = FALSE,
                    function(x) {
                      model <- lmer(substitute(log(i+1) ~ FarmType*Block + (1|farmCode), list(i = as.name(x))), data=alphaDF, na.action=na.exclude)
                      anova(model)
                    })





alphaSummary <-  alphaDF[, names(alphaDF) %in% c("FarmType", "obs","div")] %>%
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
                      