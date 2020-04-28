########################################################################
## plotting
########################################################################

########################################################################
## predictor coefficient plots
########################################################################

predictors_plot <- function(model){
  isplines <- isplineExtract(model)
  
  x_values <- data.frame(isplines$x) %>%
    lapply(function(x) scale(x, center = TRUE)) %>% 
    as.data.frame() %>% 
    add_column(Key = row.names(data.frame(isplines$x))) %>%
    dplyr::select("Key", X = 1)
  
  y_values <- data.frame(isplines$y) %>% 
    add_column(Key = row.names(data.frame(isplines$y))) %>%
    gather(key = "Factor", value = "Y", -"Key" )
  
  predictors <- y_values %>%
    full_join(x_values, by = "Key")
  
  
  predictors %>% ggplot(aes(x = X, y = Y, color = Factor)) +
    geom_line() +
    xlab("Standardized Variables") +
    ylab("Partial Ecological Distance") + 
    theme_classic()
  
}



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

comp_df <- data.frame(all_farms_model$predicted, all_farms_model$observed)

comp_df %>% ggplot(aes(x = all_farms_model.predicted, y = all_farms_model.observed)) +
  geom_point() +
  geom_smooth(method = lm)



# all AMF
all_amf_comp <- data.frame(all_amf_model$predicted, all_amf_model$observed)

all_amf_comp_plot <- all_amf_comp %>% ggplot(aes(x = all_amf_model.predicted, y = all_amf_model.observed)) +
  geom_point(size=1, alpha=0.5) +
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



#ecological dist vs observed compositional dissimilarity

# dist_df <- data.frame(gdmModel$ecological, gdmModel$observed)
# 
# dist_df %>% ggplot(aes(x = gdmModel.ecological, y = gdmModel.observed)) +
#   geom_point(color = 'lightblue') +
#   geom_smooth(method = lm)
