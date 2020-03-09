########################################################################
## gdm analysis
########################################################################

########################################################################
## all farms
########################################################################

# prepare site-pair tables

test_gdm_table <- formatsitepair(species_table, bioFormat=1, XColumn="Long_point", YColumn="Lat_point",
                                 siteColumn="Key", predData=envi_table)

#create model
gdmModel <- gdm(test_gdm_table, geo= TRUE)
summary(gdmModel)

#gdm.varImp(test_gdm_table, geo = TRUE, nPerm = 10) #very slow


#gdm plot
plot(gdmModel, plot.layout = c(1,2))

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



########################################################################
## monocultures
########################################################################

mono_gdm_table <- formatsitepair(mono_species_table, bioFormat=1, XColumn="Long_point", YColumn="Lat_point",
                            siteColumn="Key", predData=mono_envi_table)

#create model
gdm_mono_model <- gdm(mono_gdm_table, geo= TRUE)
summary(gdm_mono_model)

#plot
plot(gdm_mono_model, plot.layout = c(2,2))

########################################################################
## polycultures
########################################################################

poly_gdm_table <- formatsitepair(poly_species_table, bioFormat=1, XColumn="Long_point", YColumn="Lat_point",
                                 siteColumn="Key", predData=poly_envi_table)

#create model
gdm_poly_model <- gdm(poly_gdm_table, geo= TRUE)
summary(gdm_poly_model)

#plot
plot(gdm_poly_model, plot.layout = c(2,2))



########################################################################
## mantel tests
########################################################################


# transformed OTU table 
h <- decostand(species.rr_df, method = "hellinger")
pa <- decostand(species.rr_df, method = "pa")

# CREATE DISSIMILARITY MATRIX
dist.h <- as.matrix((vegdist(h, "bray")))

dist.pa <- as.matrix((vegdist(pa, "bray")))

#dist.envi <- as.matrix((vegdist(envi_table, "bray")))

dist.envi <- as.matrix(dist(envi_table, method = "euclidean"))

#geo table
geo <- wo_mock %>% dplyr::select("Long_point", "Lat_point")

dist.geo <- distm(geo, fun = distHaversine) #not sure if I should be using this


#abundace vs. environment

mantel_envi_h <- mantel(dist.h, dist.envi, method = "spearman")

mantel_envi_pa <- mantel(dist.pa, dist.envi)


#abundance vs. geography

mantel(dist.h, dist.geo, method = "spearman")

mantel(dist.pa, dist.geo)






