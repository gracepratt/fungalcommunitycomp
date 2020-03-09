########################################################################
## gdm analysis
########################################################################

##all farms

# prepare site-pair tables

test_gdm_table <- formatsitepair(species_table, bioFormat=1, XColumn="Long_point", YColumn="Lat_point",
                                 siteColumn="Key", predData=envi_table)

table(is.na(test_gdm_table))

#create model
gdmModel <- gdm(test_gdm_table, geo= TRUE)
summary(gdmModel)

envi_data <- envi_table[,-1]

gdm.transform(gdmModel, envi_data)

#plot
plot(gdmModel, plot.layout = c(2,2))

#customplot

isplines <- isplineExtract(gdmModel)

x_values <- data.frame(isplines$x) %>%
  lapply(function(x) scale(x, center = TRUE)) %>% 
  as.data.frame() %>% 
  add_column(number = c(1:200)) 

y_values <- data.frame(isplines$y) %>% add_column(number = c(1:200)) %>%
  rename(Geographic_y = Geographic, pH_y = pH, OM_y = OM, P_y = P)

#Custom plots

geography <- x_values %>%
  join(y_values)

geography %>% ggplot() +
  geom_line(aes(x = Geographic, y = Geographic_y)) +
  geom_line(aes(x = pH, y = pH_y)) +
  geom_line(aes(x = OM, y = OM_y)) +
  geom_line(aes(x = P, y = P_y)) +
  xlab("Predictor Dissimilarity") +
  ylab("Partial Ecological Distance")




##monocultures

mono_gdm_table <- formatsitepair(mono_species_table, bioFormat=1, XColumn="Long_point", YColumn="Lat_point",
                            siteColumn="Key", predData=mono_envi_table)

#create model
gdm_mono_model <- gdm(mono_gdm_table, geo= TRUE)
summary(gdm_mono_model)

#plot
plot(gdm_mono_model, plot.layout = c(2,2))

##polycultures

poly_gdm_table <- formatsitepair(poly_species_table, bioFormat=1, XColumn="Long_point", YColumn="Lat_point",
                                 siteColumn="Key", predData=poly_envi_table)

#create model
gdm_poly_model <- gdm(poly_gdm_table, geo= TRUE)
summary(gdm_poly_model)

#plot
plot(gdm_poly_model, plot.layout = c(2,2))


#gdm.varImp(test_gdm_table, geo = TRUE, nPerm = 10) #very slow



########################################################################
## mantel tests
########################################################################


# transformed OTU table 
h <- decostand(species.rr_df, method = "hellinger")
pa <- decostand(species.rr_df, method = "pa")

# CREATE DISSIMILARITY MATRIX
dist.h <- as.matrix((vegdist(h, "bray")))

dist.pa <- as.matrix((vegdist(pa, "bray")))

dist.envi <- as.matrix((vegdist(envi_table, "bray")))

#geo table
dist.geo <- wo_mock %>% dplyr::select("Lat_point", "Long_point")

dist.geo <- as.matrix(dist(dist.geo, method = "euclidean")) #not sure if I should be using this



#abundace vs. environment

mantel(dist.h, dist.envi)

mantel(dist.pa, dist.envi)


#abundance vs. geography

mantel(dist.h, dist.geo)

mantel(dist.pa, dist.geo)






