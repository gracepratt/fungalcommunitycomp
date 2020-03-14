########################################################################
## FUNCTIONS
########################################################################

########################################################################
## 1. create input dataframes for GDM model 
########################################################################

input_tables <- function(complete_table, envi_variables){
  species_table <- complete_table %>% 
    dplyr::select("Key", "Lat_point", "Long_point", contains("OTU"))
  
  envi_table <- wo_extras %>% 
    dplyr::select("Key", "Lat_point", "Long_point", envi_factors)
  
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