########################################################################
## plotting
########################################################################

#custom plots

#4 predictors 
isplines <- isplineExtract(gdmModel)

x_values <- data.frame(isplines$x) %>%
  lapply(function(x) scale(x, center = TRUE)) %>% 
  as.data.frame() %>% 
  add_column(number = c(1:200)) 

y_values <- data.frame(isplines$y) %>% 
  add_column(number = c(1:200)) %>%
  rename(Geographic_y = Geographic, pH_y = pH, OM_y = OM, P_y = P)

four_predictors <- x_values %>%
  join(y_values)

four_predictors %>% ggplot() +
  geom_line(aes(x = Geographic, y = Geographic_y)) +
  geom_line(aes(x = pH, y = pH_y)) +
  geom_line(aes(x = OM, y = OM_y)) +
  geom_line(aes(x = P, y = P_y)) +
  xlab("Predictor Dissimilarity") +
  ylab("Partial Ecological Distance")

#predicted vs observed compositional dissimilarity

comp_df <- data.frame(gdmModel$predicted, gdmModel$observed)

comp_df %>% ggplot(aes(x = gdmModel.predicted, y = gdmModel.observed)) +
  geom_point(color = 'lightblue') +
  geom_smooth(method = lm)

#ecological dist vs observed compositional dissimilarity

dist_df <- data.frame(gdmModel$ecological, gdmModel$observed)

dist_df %>% ggplot(aes(x = gdmModel.ecological, y = gdmModel.observed)) +
  geom_point(color = 'lightblue') +
  geom_smooth(method = lm)
