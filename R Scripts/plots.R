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
    xlab("Predictor Dissimilarity") +
    ylab("Partial Ecological Distance")
  
}


########################################################################
## predicted vs observed compositional dissimilarity
########################################################################

comp_df <- data.frame(all_farms_model$predicted, all_farms_model$observed)

comp_df %>% ggplot(aes(x = gdmModel.predicted, y = gdmModel.observed)) +
  geom_point() +
  geom_smooth(method = lm)

#ecological dist vs observed compositional dissimilarity

dist_df <- data.frame(gdmModel$ecological, gdmModel$observed)

dist_df %>% ggplot(aes(x = gdmModel.ecological, y = gdmModel.observed)) +
  geom_point(color = 'lightblue') +
  geom_smooth(method = lm)
