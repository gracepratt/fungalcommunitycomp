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
                                    siteColumn="Key", predData= inputs[[2]])
  
  model <- gdm(formated_tables, geo = geo)
  return(model)
}

########################################################################
## 3. create nice tables with total Coeffs and % explain
########################################################################


table <- function(model){
  Predictors <- c(poly_model$predictors, "Percent Deviance Explained")
  
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