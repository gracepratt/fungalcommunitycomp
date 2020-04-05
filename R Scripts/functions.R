########################################################################
## FUNCTIONS
########################################################################

########################################################################
## 1. create input dataframes for GDM model 
########################################################################

input_tables <- function(complete_table, envi_variables){
  species_table <- complete_table %>% 
    dplyr::select("Key", "Long_point", "Lat_point", contains("OTU"))
  
  envi_table <- complete_table %>% 
    dplyr::select("Key", "Long_point", "Lat_point", envi_factors)
  
  return(list(species_table, envi_table))
}

########################################################################
## 2. create GDM models
########################################################################

gdmModel <- function(inputs, geo = TRUE) {
  formated_tables <- formatsitepair(inputs[[1]], bioFormat=1, XColumn="Long_point", YColumn="Lat_point",
                                    siteColumn="Key", predData= inputs[[2]], abundance = FALSE)
  
  model <- gdm(formated_tables, geo = geo)
  return(model)
}

########################################################################
## 3. create nice tables with total Coeffs and % explain
########################################################################


table <- function(model){
  Predictors <- c(model$predictors, "Percent Deviance Explained")
  
  coef <- model$coefficients
  
  i <- 1
  coeffs <- c()
  while(i < length(coef)){
    coeffs <- c(coeffs, sum(coef[i:(i+2)]))
    i <- i+3
  }
  
  Coefficients <- c(coeffs, model$explained)
  
  nice_table <- data.frame(Predictors, Coefficients)
  return(nice_table)
}

########################################################################
## 3. mantel functions with nice table
########################################################################

mantel_func <- function(input_table, transform_method = "hellinger", mantel_method = "spearman"){
  
  species <- input_table[[1]] %>% dplyr::select(contains("OTU"))
  envi <- input_table[[2]] %>% dplyr::select(-"Key", -"Lat_point", -"Long_point")
  geo <- input_table[[1]] %>% dplyr::select("Long_point", "Lat_point")
  
  # transformed OTU table 
  trans <- decostand(species, method = transform_method)
  
  # CREATE DISSIMILARITY MATRIX
  dist.sp <- as.matrix((vegdist(trans, "bray")))
  dist.envi <- as.matrix(dist(envi, method = "euclidean"))
  dist.geo <- distm(geo, fun = distHaversine) 
  
  #mantel test
  mantel_envi <- mantel(dist.sp, dist.envi, method = mantel_method)
  mantel_geo <- mantel(dist.sp, dist.geo, method = mantel_method)
  
  #make a table 
  Factor <- c("Environment", "Geography")
  Statistic <- c(mantel_envi$statistic, mantel_geo$statistic)
  Significance <- c(mantel_envi$signif, mantel_geo$signif)
  
  table <- data.frame(Factor, Statistic, Significance)
  
  
  return(table)
}






