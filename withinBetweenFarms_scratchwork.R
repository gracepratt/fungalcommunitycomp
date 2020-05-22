# distance


spatial <- input_diss(amf, c("pH", "P", "TOC", "N", "NP_ratio", "FarmBi")  )
spatial_diss <- input_diss(amf, c("pH", "P", "TOC", "N", "NP_ratio", "FarmBi") )
spatial$distanceM <-distHaversine(spatial[,3:4],spatial[,5:6])

#monoculture
monocultures_amf <- amf %>%
  filter(FarmType == "Monoculture") 

spatial_mono <- input_diss(monocultures_amf, envi_factors) 
spatial_mono_diss <- input_diss(monocultures_amf, envi_factors)
spatial_mono$distanceM <-distHaversine(spatial_mono[,3:4],spatial_mono[,5:6])

#polyculture
polycultures_amf <- amf %>%
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
amf_across_plots <- predictors_plot(amf_across) + ggtitle("Across") + theme(legend.position="none", plot.title = element_text(hjust = 0.5))

# dissimilarity within farms
within <- spatial %>% 
  filter(distanceM < 60) %>%
  dplyr::select(-"distanceM")

amf_within <- gdm(within, geo = TRUE)
table(amf_within)


amf_within_plots <- predictors_plot(amf_within) + ggtitle("Within") + theme(legend.position="none", plot.title = element_text(hjust = 0.5),axis.title.y = element_blank())

# dissimilarity between farms
between <- spatial %>% 
  filter(distanceM > 60) %>%
  dplyr::select(-"distanceM")


amf_between <- gdm(between, geo = TRUE)
table(amf_between)

amf_between_plots <- predictors_plot(amf_between) + ggtitle("Between") + theme(legend.position="none",plot.title = element_text(hjust = 0.5),axis.title.y = element_blank())

all_legend <- get_legend(predictors_plot(amf_across))

all_spatial <- cowplot::plot_grid(plot_grid(amf_across_plots, amf_within_plots, amf_between_plots, nrow=1), all_legend, rel_widths = c(3, .4) )


# MONO


# across farms

across_mono <- gdm(spatial_mono, geo = TRUE)
table(across_mono)

mono_across_plots <- predictors_plot(across_mono) + ggtitle("Across") + theme(legend.position="none", plot.title = element_text(hjust = 0.5))

# dissimilarity within farms
within_mono <- spatial_mono %>% 
  filter(distanceM < 60) %>%
  dplyr::select(-"distanceM")

amf_within_mono <- gdm(within_mono, geo = TRUE)
table(amf_within_mono)


mono_within_plots <- predictors_plot(amf_within_mono) + ggtitle("Within") + theme(legend.position="none", plot.title = element_text(hjust = 0.5),axis.title.y = element_blank())


# dissimilarity between farms
between_mono <- spatial_mono %>% 
  filter(distanceM > 60) %>%
  dplyr::select(-"distanceM")


amf_between_mono <- gdm(between_mono, geo = TRUE)
table(amf_between_mono)

mono_between_plots <- predictors_plot(amf_between_mono) + ggtitle("Between") + theme(legend.position="none",plot.title = element_text(hjust = 0.5),axis.title.y = element_blank())

mono_legend <- get_legend(predictors_plot(across_mono))

mono_plots <- cowplot::plot_grid(plot_grid(mono_across_plots, mono_within_plots, mono_between_plots, nrow=1), mono_legend, rel_widths = c(3, .4) )


# POLY

# across farms

across_poly <- gdm(spatial_poly, geo = TRUE)
table(across_poly)

poly_across_plots <- predictors_plot(across_poly) + ggtitle("Across") + theme(legend.position="none", plot.title = element_text(hjust = 0.5))

# dissimilarity within farms
within_poly <- spatial_poly %>% 
  filter(distanceM < 60) %>%
  dplyr::select(-"distanceM")

amf_within_poly <- gdm(within_poly, geo = TRUE)
table(amf_within_poly)


poly_within_plots <- predictors_plot(amf_within_poly) + ggtitle("Within") + theme(legend.position="none", plot.title = element_text(hjust = 0.5),axis.title.y = element_blank())


# dissimilarity between farms
between_poly <- spatial_poly %>% 
  filter(distanceM > 60) %>%
  dplyr::select(-"distanceM")


amf_between_poly <- gdm(between_poly, geo = TRUE)
table(amf_between_poly)

poly_between_plots <- predictors_plot(amf_between_poly) + ggtitle("Between") + theme(legend.position="none",plot.title = element_text(hjust = 0.5),axis.title.y = element_blank())

poly_legend <- get_legend(predictors_plot(across_poly))

poly_plots <- cowplot::plot_grid(plot_grid(poly_across_plots, poly_within_plots, poly_between_plots, nrow=1), poly_legend, rel_widths = c(3, .4) )


# formatsitepair(within, bioFormat=4, XColumn="Long_point", YColumn="Lat_point",
# siteColumn="Key", predData= envi_table, abundance = FALSE)
