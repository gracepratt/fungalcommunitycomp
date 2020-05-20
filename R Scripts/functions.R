########################################################################
## FUNCTIONS
########################################################################

########################################################################
## 1. species table inputs for gdm
########################################################################

input_tables <- function(complete_table, envi_variables){
  species_table <- complete_table %>% 
    dplyr::select("Key", "Long_point", "Lat_point", contains("OTU"))
  
  envi_table <- complete_table %>% 
    dplyr::select("Key", "Long_point", "Lat_point", envi_variables)
  
  formated_tables <- formatsitepair(species_table, bioFormat=1, XColumn="Long_point", YColumn="Lat_point",
                                    siteColumn="Key", predData= envi_table, abundance = FALSE)
  
  return(formated_tables)
}

########################################################################
## 2. dissimilarity matrix inputs for gdm
########################################################################

input_diss <- function(complete_table, envi_variables){
  species_table <- complete_table%>%
    dplyr::select(contains("OTU"))
  
  
  dist.sp <- as.matrix(vegdist(species_table, "bray"))
  
  species_matrix <- cbind(complete_table$Key, dist.sp) 
  
  colnames(species_matrix)[1] <- "Key"
  
  envi_table <- complete_table %>%
    dplyr::select("Key", "Long_point", "Lat_point", envi_variables)
  
  formated_tables <- formatsitepair(species_matrix, bioFormat=3, XColumn="Long_point", YColumn="Lat_point",
                                     siteColumn="Key", predData= envi_table, abundance = TRUE)
  
  return(formated_tables)
}


########################################################################
## 3. subsetting fungal guilds
########################################################################

guild_filter <- function(complete_table, guild){
  
  #pull out OTU columns and transpose to make 1 row per Key and OTU
  OTURows<- complete_table %>% 
    dplyr::select(Key, contains("OTU")) %>%
    pivot_longer(cols = starts_with("OTU"), names_to = "OTU")
  
  #pull out names of guilds
  rawguilds <- tax %>%
    dplyr::select("OTU" = "X.OTU.ID", "Guild")
  
  #join guild names with OTU in each key 
  wGuild <- OTURows %>%
    left_join(rawguilds) %>%
    drop_na()
  
  #subsetting OTUs to only guild of interest
  fdGroup <- wGuild %>% 
    filter(str_detect(Guild, pattern = guild)) 
  
  #re-pivot back to wide format 
  wide <- fdGroup %>%
    dplyr::select("Key", "OTU", "value") %>%
    pivot_wider(names_from = OTU, values_from = value)
  
  wide[is.na(wide)] <- 0
  
  #rejoin with full table 
  fd_complete <- complete_table %>%
    dplyr::select(-contains("OTU")) %>%
    join(wide)
  
  #taking out samples with none of the guild
  fd_complete$rowsum <- rowSums(fd_complete%>% dplyr::select(contains("OTU")))
  
  fd_complete <- fd_complete %>% filter(rowsum > 0)
  
  return(fd_complete)
}


########################################################################
## 4. nice output tables for gdm models
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
## 5. mantel functions with nice table
########################################################################

mantel_func <- function(complete_table, envi_variables, transform_method = "hellinger", mantel_method = "spearman"){
  
  species <- complete_table %>% dplyr::select(contains("OTU"))
  envi <- complete_table %>% dplyr::select(envi_factors)
  geo <- complete_table %>% dplyr::select("Long_point", "Lat_point")
  
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

########################################################################
## 6. plot variable isplines
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
    # ylim(0, 2.5) +
    theme_classic()
  
}


########################################################################
## 7. plot obs vs predicted comp dissimilarity
########################################################################

comp_plot <- function(model){
  
  x_values <- model$predicted
  y_values <- model$observed
  
  df <- data.frame(x_values, y_values)
  
  
  df %>% ggplot(aes(x = x_values, y = y_values)) +
    geom_point(size=0.2, alpha=0.5) +
    geom_smooth(method = lm)+ 
    xlab("Predicted Community Dissimilarity") +
    ylab("Observed Community Dissimilarity") + 
    theme_classic()
  
}


########################################################################
## 8. plot pred ecological distance vs obs comp dissimilarity
########################################################################

ecodist_plot <- function(model){
  
  x_values <- model$ecological
  y_values <- model$observed
  
  df <- data.frame(x_values, y_values)
  
  
  df %>% ggplot(aes(x = x_values, y = y_values)) +
    geom_point(size=0.2, alpha=0.5) +
    geom_smooth(method = lm)+ 
    xlab("Predicted Ecological Distance") +
    ylab("Observed Community Dissimilarity") + 
    theme_classic()
  
}

########################################################################
## 8. plot alpha diversity
########################################################################

alpha_plot <- plotFunction <- function(colNames, expVar, data){
  plotList <- list()
  for (i in colNames) {
    min <- (min/0.03)*.90
    min <- round(min ,0)
    plotList[[i]] <- ggplot(	df, aes(x = expVar, y = y, color=expVar, fill=expVar))  + 
      geom_boxplot()+
      scale_y_continuous(expand = c(0.01, 0.01), limits=c(min, max),breaks=seq(min, max,length.out=6)) +
      scale_fill_manual(values=c('#f2e6cb',  '#99cec6','#f2e6cb','#99cec6'))+
      scale_color_manual(values=c('#dfc27d',  '#018571','#dfc27d','#018571'))+
      ylab(paste(i)) +	
      xlab("") +	
      theme_simple() + theme(legend.position="none")
  }
  return(plotList)
}
