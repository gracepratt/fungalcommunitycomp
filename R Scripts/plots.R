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

comp_plot(all_farms_model)

# all AMF
all_amf_comp <- data.frame(all_amf_model$predicted, all_amf_model$observed)

all_amf_comp_plot <- all_amf_comp %>% ggplot(aes(x = all_amf_model.predicted, y = all_amf_model.observed)) +
  geom_point(size=0.2, alpha=0.5) +
  geom_smooth(method = lm)+
  xlab("Predicted Community Dissimilarity") +
  ylab("Observed Community Dissimilarity") +
  theme_classic()

# ggsave("all_amf_comp_plot.pdf", plot=all_amf_comp_plot, path=fig.path, width = 6, height=4, useDingbats=FALSE)


# monoculture
mono_amf_comp <- data.frame(mono_model_amf$predicted, mono_model_amf$observed)

mono_amf_comp_plot <- mono_amf_comp %>% ggplot(aes(x = mono_model_amf.predicted, y = mono_model_amf.observed)) +
  geom_point(size=1, alpha=0.5) +
  geom_smooth(method = lm)+
  xlab("Predicted Community Dissimilarity") +
  ylab("Observed Community Dissimilarity") +
  theme_classic()

# ggsave("mono_amf_comp_plot.pdf", plot=mono_amf_comp_plot, path=fig.path, width = 6, height=4, useDingbats=FALSE)


# polyculture
poly_amf_comp <- data.frame(poly_model_amf$predicted, poly_model_amf$observed)

poly_amf_comp_plot <- poly_amf_comp %>% ggplot(aes(x = poly_model_amf.predicted, y = poly_model_amf.observed)) +
  geom_point(size=1, alpha=0.5) +
  geom_smooth(method = lm)+
  xlab("Predicted Community Dissimilarity") +
  ylab("Observed Community Dissimilarity") +
  theme_classic()

# ggsave("poly_amf_comp_plot.pdf", plot=poly_amf_comp_plot, path=fig.path, width = 6, height=4, useDingbats=FALSE)


########################################################################
## pred ecological dist vs observed compositional dissimilarity
########################################################################

ecodist_plot(all_farms_model)


########################################################################
## functional diveristy group plots
########################################################################

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



