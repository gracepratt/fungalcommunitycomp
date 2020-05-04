########################################################################
## plotting
########################################################################

########################################################################
## predictor coefficient plots
########################################################################

#all fungi
all_farms_plots <- predictors_plot(all_farms_model)

# all AMF
all_amf_plots <- predictors_plot(all_amf_model)
ggsave("all_amf_plots.pdf", plot=all_amf_plots, path=fig.path, width = 5, height=4, useDingbats=FALSE)

# monoculture
mono_amf_plots <- predictors_plot(mono_model_amf)
ggsave("mono_amf_plots.pdf", plot=mono_amf_plots, path=fig.path,  width = 5, height=4, useDingbats=FALSE)


# polyculture
poly_amf_plots <- predictors_plot(poly_model_amf)
ggsave("poly_amf_plots.pdf", plot=poly_amf_plots, path=fig.path,  width = 5, height=4, useDingbats=FALSE)

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

ggsave("all_amf_comp_plot.pdf", plot=all_amf_comp_plot, path=fig.path, width = 6, height=4, useDingbats=FALSE)


# monoculture
mono_amf_comp <- data.frame(mono_model_amf$predicted, mono_model_amf$observed)

mono_amf_comp_plot <- mono_amf_comp %>% ggplot(aes(x = mono_model_amf.predicted, y = mono_model_amf.observed)) +
  geom_point(size=1, alpha=0.5) +
  geom_smooth(method = lm)+ 
  xlab("Predicted Community Dissimilarity") +
  ylab("Observed Community Dissimilarity") + 
  theme_classic()

ggsave("mono_amf_comp_plot.pdf", plot=mono_amf_comp_plot, path=fig.path, width = 6, height=4, useDingbats=FALSE)


# polyculture
poly_amf_comp <- data.frame(poly_model_amf$predicted, poly_model_amf$observed)

poly_amf_comp_plot <- poly_amf_comp %>% ggplot(aes(x = poly_model_amf.predicted, y = poly_model_amf.observed)) +
  geom_point(size=1, alpha=0.5) +
  geom_smooth(method = lm)+ 
  xlab("Predicted Community Dissimilarity") +
  ylab("Observed Community Dissimilarity") + 
  theme_classic()

ggsave("poly_amf_comp_plot.pdf", plot=poly_amf_comp_plot, path=fig.path, width = 6, height=4, useDingbats=FALSE)


########################################################################
## pred ecological dist vs observed compositional dissimilarity
########################################################################

ecodist_plot(all_farms_model)



