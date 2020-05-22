########################################################################
## plotting
########################################################################

########################################################################
## predictor coefficient plots
########################################################################

#all fungi
all_farms_plots <- predictors_plot(all_farms_model)

# all AMF
all_amf_plots <- predictors_plot(amf_diss_model) #+ ggtitle("All farms") + theme(legend.position="none", plot.title = element_text(hjust = 0.5))
# ggsave("all_amf_plots.pdf", plot=all_amf_plots, path=fig.path, width = 5, height=4, useDingbats=FALSE)

# monoculture
mono_amf_plots <- predictors_plot(mono_amf_diss_model) + ggtitle("Monoculture") + theme(legend.position="none", plot.title = element_text(hjust = 0.5),axis.title.y = element_blank())
# ggsave("mono_amf_plots.pdf", plot=mono_amf_plots, path=fig.path,  width = 5, height=4, useDingbats=FALSE)


# polyculture
poly_amf_plots <- predictors_plot(poly_amf_diss_model) + ggtitle("Polyculture") + theme(legend.position="none",plot.title = element_text(hjust = 0.5),axis.title.y = element_blank())
# ggsave("poly_amf_plots.pdf", plot=poly_amf_plots, path=fig.path,  width = 5, height=4, useDingbats=FALSE)

# get legend
legend <- get_legend(predictors_plot(poly_model_amf))

amf_plots <- cowplot::plot_grid(plot_grid(all_amf_plots, mono_amf_plots, poly_amf_plots, nrow=1), legend, rel_widths = c(3, .4) )

########################################################################
## predicted vs observed compositional dissimilarity
########################################################################


#all fungi
all_fungi_comp <- comp_plot(all_farms_model)

## AMF 

# all AMF
all_amf_comp_plot <- comp_plot(all_amf_model)
# ggsave("all_amf_comp_plot.pdf", plot=all_amf_comp_plot, path=fig.path, width = 6, height=4, useDingbats=FALSE)


# monoculture 
mono_amf_comp_plot <- comp_plot(mono_model_amf)
# ggsave("mono_amf_comp_plot.pdf", plot=mono_amf_comp_plot, path=fig.path, width = 6, height=4, useDingbats=FALSE)


# polyculture
poly_amf_comp <- comp_plot(poly_model_amf)
# ggsave("poly_amf_comp_plot.pdf", plot=poly_amf_comp_plot, path=fig.path, width = 6, height=4, useDingbats=FALSE)


########################################################################
## pred ecological dist vs observed compositional dissimilarity
########################################################################

ecodist_plot(all_farms_model)


########################################################################
## functional diveristy group plots
########################################################################

#All farms 
all_farms_plot <- predictors_plot(all_farms_model) + ggtitle("All farms") + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

mono_plots <- predictors_plot(mono_model) + ggtitle("Monoculture") + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5),axis.title.y = element_blank())

poly_plots <- predictors_plot(poly_model) + ggtitle("Polyculture") + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5),axis.title.y = element_blank())

legend <- get_legend(predictors_plot(all_farms_model))

all_fungi_plots <- cowplot::plot_grid(plot_grid(all_farms_plot, mono_plots, poly_plots, nrow=1), 
                                   legend, rel_widths = c(3, .4) )


#AMF

amf_fd_plot <- predictors_plot(amf_fd) + ggtitle("All farms") + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

amf_mono_fd_plots <- predictors_plot(amf_fd_mono) + ggtitle("Monoculture") + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5),axis.title.y = element_blank())

amf_poly_fd_plots <- predictors_plot(amf_fd_poly) + ggtitle("Polyculture") + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5),axis.title.y = element_blank())

legend <- get_legend(predictors_plot(amf_fd))

amf_fd_plots <- cowplot::plot_grid(plot_grid(amf_fd_plot, amf_mono_fd_plots, amf_poly_fd_plots, nrow=1), 
                                legend, rel_widths = c(3, .4) )

amf_fd_comp <- comp_plot(amf_fd)

#Plant Pathogens

plant_path_plot <- predictors_plot(plant_path) + ggtitle("All farms") + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

plant_mono_plot <- predictors_plot(plant_path_mono) + ggtitle("Monoculture") + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5),axis.title.y = element_blank())

plant_poly_plot <- predictors_plot(plant_path_poly) + ggtitle("Polyculture") + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5),axis.title.y = element_blank())

legend <- get_legend(predictors_plot(plant_path))

plant_path_plots <- cowplot::plot_grid(plot_grid(plant_path_plot, plant_mono_plot, plant_poly_plot, nrow=1), 
                                   legend, rel_widths = c(3, .4) )



#saprotroph

sap_plot <- predictors_plot(saprotroph) + ggtitle("All farms") + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

sap_mono_plot <- predictors_plot(saprotroph_mono) + ggtitle("Monoculture") + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5),axis.title.y = element_blank())

sap_poly_plot <- predictors_plot(saprotroph_poly) + ggtitle("Polyculture") + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5),axis.title.y = element_blank())

legend <- get_legend(predictors_plot(saprotroph))

saprotroph_plots <- cowplot::plot_grid(plot_grid(sap_plot, sap_mono_plot, sap_poly_plot, nrow=1), 
                                   legend, rel_widths = c(3, .4) )


#fungal parasite 


fungal_par_plot <- predictors_plot(fungal_parasite) + ggtitle("All farms") + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

fungal_mono_plot <- predictors_plot(fungal_par_mono) + ggtitle("Monoculture") + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5),axis.title.y = element_blank())

fungal_poly_plot <- predictors_plot(saprotroph_poly) + ggtitle("Polyculture") + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5),axis.title.y = element_blank())

legend <- get_legend(predictors_plot(fungal_parasite))

fungal_par_plots<- cowplot::plot_grid(plot_grid(fungal_par_plot, fungal_mono_plot, fungal_poly_plot, nrow=1), 
                                       legend, rel_widths = c(3, .4) )



########################################################################
## FTBL plots
########################################################################

mono_f_plot <- predictors_plot(mono_f_model) + ggtitle("Monoculture F") + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))


mono_n_plot <- predictors_plot(mono_n_model) + ggtitle("Monoculture N") + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

poly_f_plot <- predictors_plot(poly_f_model) + ggtitle("Polyculture F") + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

poly_n_plot <- predictors_plot(poly_n_model) + ggtitle("Polyculture N") + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))


mono_nvf <- cowplot::plot_grid(plot_grid(mono_n_plot, mono_f_plot, nrow=1), 
                                       legend, rel_widths = c(3, .4) )


poly_nvf <- cowplot::plot_grid(plot_grid(poly_n_plot, poly_f_plot, nrow=1), 
                               legend, rel_widths = c(3, .4) )

polyvmono_f <- cowplot::plot_grid(plot_grid(poly_f_plot, mono_f_plot, nrow=1), 
                                  legend, rel_widths = c(3, .4) )

polyvmono_n <- cowplot::plot_grid(plot_grid(poly_n_plot,mono_n_plot, nrow=1), 
                                  legend, rel_widths = c(3, .4) )


########################################################################
## FTBL AMF plots
########################################################################

mono_f_amf_plot <- predictors_plot(mono_f_amf_model) + ggtitle("Monoculture F") + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))


mono_n_amf_plot <- predictors_plot(mono_n_amf_model) + ggtitle("Monoculture N") + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

poly_f_amf_plot <- predictors_plot(poly_f_amf_model) + ggtitle("Polyculture F") + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

poly_n_amf_plot <- predictors_plot(poly_n_amf_model) + ggtitle("Polyculture N") + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))


mono_amf_nvf <- cowplot::plot_grid(plot_grid(mono_n_amf_plot, mono_f_amf_plot, nrow=1), 
                               legend, rel_widths = c(3, .4) )


poly_amf_nvf <- cowplot::plot_grid(plot_grid(poly_n_amf_plot, poly_f_amf_plot, nrow=1), 
                               legend, rel_widths = c(3, .4) )

polyvmono_amf_f <- cowplot::plot_grid(plot_grid(poly_f_amf_plot, mono_f_amf_plot, nrow=1), 
                                  legend, rel_widths = c(3, .4) )

polyvmono_amf_n <- cowplot::plot_grid(plot_grid(poly_n_amf_plot,mono_n_amf_plot, nrow=1), 
                                  legend, rel_widths = c(3, .4) )


########################################################################
## box plots
########################################################################




#NP ratio

np_all <- boxplot_variable(all_fungi, "NP_ratio", "All farms")

np_mono <- boxplot_variable(monocultures, "NP_ratio", "Monocultures")

np_poly <- boxplot_variable(polycultures, "NP_ratio", "Polycultures")

np_ratio <- cowplot::plot_grid(plot_grid(np_mono, np_poly, np_all, nrow=3),
                               rel_widths = c(3, .4)) 


#pH

pH_all <- boxplot_variable(all_fungi, "pH", "All farms")

pH_mono <- boxplot_variable(monocultures, "pH", "Monocultures")

pH_poly <- boxplot_variable(polycultures, "pH", "Polycultures")

pH <- cowplot::plot_grid(plot_grid(pH_mono, pH_poly, pH_all, nrow=3), 
                         rel_widths = c(3, .4) )

#P

P_all <- boxplot_variable(all_fungi, "P", "All farms")

P_mono <- boxplot_variable(monocultures, "P", "Monocultures")

P_poly <- boxplot_variable(polycultures, "P", "Polycultures")

P <- cowplot::plot_grid(plot_grid(P_mono, P_poly, P_all, nrow=3), 
                         rel_widths = c(3, .4) )


#TOC

TOC_all <- boxplot_variable(all_fungi, "TOC", "All farms")

TOC_mono <- boxplot_variable(monocultures, "TOC", "Monocultures")

TOC_poly <- boxplot_variable(polycultures, "TOC", "Polycultures")

TOC <- cowplot::plot_grid(plot_grid(TOC_mono, TOC_poly, TOC_all, nrow=3), 
                        rel_widths = c(3, .4) )


#N

N_all <- boxplot_variable(all_fungi, "N", "All farms")

N_mono <- boxplot_variable(monocultures, "N", "Monocultures")

N_poly <- boxplot_variable(polycultures, "N", "Polycultures")

N <- cowplot::plot_grid(plot_grid(N_mono, N_poly, N_all, nrow=3), 
                        rel_widths = c(3, .4) )

########################################################################
## alpha diversity plots for functional  groups
########################################################################

divIndices <- c("obs_all","obs_amf", "obs_path","obs_sap","obs_par", "div_all","div_amf", "div_path","div_sap","div_par")
  
alphaPlots <- alpha_plot(colNames=divIndices,expVar = "FarmType", data = alphaDF)
alphaBlockPlots <-  alpha_plot(colNames=divIndices,expVar = "Block", data = alphaDF)
alphaFTBLPlots <-  alpha_plot(colNames=divIndices,expVar = "FTBL", data = alphaDF)

richPlots <- plot_grid(alphaPlots[[1]], alphaPlots[[2]], alphaPlots[[3]], alphaPlots[[4]], alphaPlots[[5]], ncol=3)
richBlockPlots <- plot_grid(alphaBlockPlots[[1]], alphaBlockPlots[[2]], alphaBlockPlots[[3]], alphaBlockPlots[[4]], alphaBlockPlots[[5]], ncol=3)
richFTBLPlots <- plot_grid(alphaFTBLPlots[[1]], alphaFTBLPlots[[2]], alphaFTBLPlots[[3]], alphaFTBLPlots[[4]], alphaFTBLPlots[[5]], ncol=3)

divPlots <- plot_grid(alphaPlots[[6]], alphaPlots[[7]], alphaPlots[[8]], alphaPlots[[9]], alphaPlots[[10]], ncol=3)

alpha_env_Plots <- alpha_env(colNames=divIndices,expVar = "pH", color="FarmType",data = alphaDF)


