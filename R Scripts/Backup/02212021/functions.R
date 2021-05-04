## *****************************************************************************
## FUNCTIONS ###################################################################
## *****************************************************************************

## *****************************************************************************
## 1. species table inputs for gdm #############################################
## *****************************************************************************

# complete_table <- all_fungi
# envi_variables <- envi_factors

input_tables <- function(complete_table, envi_variables){
  species_table <- complete_table %>% 
    dplyr::select("Key", "Long_point", "Lat_point", contains("OTU"))
  
  envi_table <- complete_table %>% 
    dplyr::select("Key", "Long_point", "Lat_point", envi_variables)
  
  pd <- as.data.frame(as.matrix(dist(complete_table$FarmBi))) %>%
    rownames_to_column("Key")
  
  formated_tables <- formatsitepair(species_table, bioFormat=1,  
                                    XColumn="Long_point", YColumn="Lat_point",
                                    siteColumn="Key", predData= envi_table, abundance = FALSE)
  
  formated_tables2 <- formatsitepair(formated_tables, bioFormat=4,  
                                     siteColumn="Key", predData= envi_table,
                                     distPreds = list(as.matrix(pd)))
  
  
  return(formated_tables)
}

## *****************************************************************************
## 2. dissimilarity matrix inputs for gdm ######################################
## *****************************************************************************

input_diss <- function(complete_table, envi_variables){
  species_table <- complete_table%>%
    dplyr::select(contains("OTU"))
  
  
  dist.sp <- as.matrix(vegdist(species_table, "bray"))
  
  species_matrix <- cbind(complete_table$Key, dist.sp) 
  
  colnames(species_matrix)[1] <- "Key"
  
  envi_table <- complete_table %>%
    dplyr::select("Key", "Long_point", "Lat_point", envi_variables)
  
  
  pd <- as.data.frame(as.matrix(dist(complete_table$FarmBi))) %>%
    rownames_to_column("Key")
  
  formated_tables <- formatsitepair(species_matrix, bioFormat=3, XColumn="Long_point", YColumn="Lat_point",
                                     siteColumn="Key", predData= envi_table, abundance = TRUE)
  
  
  # formated_tables <- formatsitepair(formated_tables, bioFormat=4,  
                                     # siteColumn="Key", predData= envi_table,
                                     # distPreds = list(as.matrix(pd)))
  
  return(formated_tables)
}


## *****************************************************************************
## 3. subsetting fungal guilds #################################################
## *****************************************************************************

# 
# complete_table = all_fungi
# guild= "Arbuscular Mycorrhizal"
# family="NA"
#   


guild_filter <- function(complete_table, guild, family){
  

  
  #pull out OTU columns and transpose to make 1 row per Key and OTU
  OTURows<- complete_table %>% 
    dplyr::select(Key, contains("OTU")) %>%
    pivot_longer(cols = starts_with("OTU"), names_to = "OTU") %>%
    mutate(value = value + 0.000)
  
  rawguilds <- tax %>%
    dplyr::select("OTU" = "OTU", "Guild", "Family")
  
  #join guild names with OTU in each key 
  wGuild <- OTURows %>%
    left_join(rawguilds) #%>%
    # drop_na()
  
  if(family == "NA"){
    #subsetting OTUs to only guild of interest
    fdGroup <- wGuild %>% 
      filter(str_detect(Guild, pattern = guild)) 
    
  }
  else {
    #subsetting OTUs to only guild of interest
    fdGroup <- wGuild %>% 
      filter(str_detect(Guild, pattern= guild)) %>%
      filter(Family %in% family)
  }
  
  
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


## *****************************************************************************
## 4. nice output tables for gdm models ########################################
## *****************************************************************************


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

## *****************************************************************************
## 5. mantel functions with nice table #########################################
## *****************************************************************************

# complete_table=all
# envi_variables=edaphic_variables 
# type = "turnover"

# mantel <- mantel_func(complete_table=all, envi_variables=edaphic_variables, type = "composition")

mantel_func <- function(complete_table, envi_variables, type, transform_method = "hellinger", mantel_method = "spearman"){
  
  species <- complete_table %>% dplyr::select(contains("OTU"))
  envi <- complete_table %>% dplyr::select(envi_variables)
  geo <- complete_table %>% dplyr::select("Long_point", "Lat_point")
  cropDiv <- complete_table %>% dplyr::select("cropDiversity")
  pH <- complete_table %>% dplyr::select("pH")
  P <- complete_table %>% dplyr::select("P")
  N <- complete_table %>% dplyr::select("N")
  TOC <- complete_table %>% dplyr::select("TOC")
  NP <- complete_table %>% dplyr::select("NP_ratio")

  # transformed OTU table 
  trans <- decostand(species, method = transform_method)
  
  # CREATE DISSIMILARITY MATRIX FOR OTUs
  if(type == "nestedness"){
    dist.sp<- beta.pair(decostand(species, "pa"), index.family = "jaccard")$beta.jne
    
  } else if( type == "turnover"){
    
    dist.sp<- beta.pair(decostand(species, "pa"), index.family = "jaccard")$beta.jtu
    
  } else {
    
    dist.sp <- vegdist(trans, "bray")
  }
  
  
  # CREATE DISSIMILARITY MATRIX FOR EXPLANATORY VARIABLES
  dist.envi <- dist(envi, method = "euclidean")
  dist.geo <- as.dist(distm(geo, fun = distHaversine) )
  dist.cropDiv <-dist(cropDiv, method = "euclidean")
  dist.pH <- dist(pH, method = "euclidean")
  dist.P <- dist(P, method = "euclidean") 
  dist.N <-dist(N, method = "euclidean")
  dist.NP <- dist(NP, method = "euclidean")
  dist.TOC <- dist(TOC, method = "euclidean") 

  # MANTEL TEST
  mantel_envi <- vegan::mantel(dist.sp, dist.envi, method = mantel_method, permutations = 999)
  mantel_geo <- vegan::mantel(dist.sp, dist.geo, method = mantel_method, permutations = 999)
  mantel_envivgeo <- vegan::mantel(dist.envi, dist.geo, method = mantel_method, permutations = 999)
  mantel_envivCD <- vegan::mantel(dist.envi, dist.cropDiv, method = mantel_method, permutations = 999)
  mantel_cropDiv <- vegan::mantel(dist.sp, dist.cropDiv, method = mantel_method, permutations = 999)
  mantel_pH <- vegan::mantel(dist.sp, dist.pH, method = mantel_method, permutations = 999)
  mantel_P <- vegan::mantel(dist.sp, dist.P, method = mantel_method, permutations = 999)
  mantel_N <- vegan::mantel(dist.envi, dist.N, method = mantel_method, permutations = 999)
  mantel_NP <- vegan::mantel(dist.sp, dist.NP, method = mantel_method, permutations = 999)
  mantel_TOC <- vegan::mantel(dist.sp, dist.TOC, method = mantel_method, permutations = 999)
  
  
  # MAKE A TABLE
  Factor <- c("Species v Crop Diversity","Species v Environment", "Species v Geography", "Environment v Geography",  "Environment v Crop diversity",
              "Species v pH", "Species v P", "Species v N", "Species v NP", "Species v TOC")
  Statistic <- c(mantel_cropDiv$statistic, mantel_envi$statistic, mantel_geo$statistic, mantel_envivgeo$statistic, mantel_envivCD$statistic,
                 mantel_pH$statistic, mantel_P$statistic, mantel_N$statistic, mantel_NP$statistic, mantel_TOC$statistic)
  Significance <- c(mantel_cropDiv$signif, mantel_envi$signif, mantel_geo$signif, mantel_envivgeo$signif, mantel_envivCD$signif,
                    mantel_pH$signif, mantel_P$signif, mantel_N$signif, mantel_NP$signif, mantel_TOC$signif)
  
  table <- data.frame(Factor, Statistic, Significance)
  
  
  # DENSITY PLOT
  densityPlot = plot(density(dist.sp))
  
  # FINAL OUTPUT
  list(table = table, data.frame = dist.sp, plot = densityPlot )
}



## *****************************************************************************
## 6. plot variable isplines ###################################################
## *****************************************************************************

# model <- all_wa$gdmModels

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
    full_join(x_values, by = "Key") %>%
    mutate(Factor = as.factor(Factor))
  
  
  # create color palette (need to fix)
  palette <- data.frame(Predictors = c("cropDiversity", "FarmBi","Geographic","N", 
                                       "P", "pH", "TOC", "PolyYears", "matrix_1"), 
                        colors = c("#E69F00", "#A6230C", "#009E73", "#8EB042",
                                   "#0072B2", "#CC79A7", "#8E6345", "#000000", "#000000"))  %>%
    arrange(Predictors)
  
  colors <- palette %>%
    filter(Predictors %in% levels(predictors$Factor)) %>%
    pull(colors)
  
  
  # across_table <- across$tables$`item:3`
  
  predictors %>% ggplot(aes(x = X, y = Y, color = Factor)) +
    geom_line(size=1) +
    xlab("Standardized Variables") +
    ylab("Partial Ecological Distance") + 
    scale_color_manual(values=colors) + 
    scale_y_continuous(expand = c(0.01, 0.01), breaks=round(seq(min(predictors$Y), max(predictors$Y), length.out = 6),1)) +
    # ylim(0, 2.5) +
    themeBorder
  
}

## *****************************************************************************
## 6b. plot variable isplines ##################################################
## *****************************************************************************

# predictors_variable_plot(landscape_f$predictors,"Geographic") + xlab("Geographic (km)")

predictors_variable_plot <- function(predictors, variable){
  

  predictors <- predictors %>%
    filter(Factor %in% variable)
  
  
  # create color palette (need to fix)
  # order = "across_F" "across_N" "within_F" "within_N"
colors <- c("#B08ED3","#C8BE6A","#7C43B7","#A49307")
  
  
  
  # across_table <- across$tables$`item:3`
  
  predictors %>% ggplot(aes(x = X_unscaled, y = Y)) + #, color = scaleLevel
    geom_line(size=1) +
    xlab(paste(variable)) +
    ylab("Partial Ecological Distance") + 
    scale_color_manual(values=colors) + 
    # scale_y_continuous(expand = c(0.01, 0.01), breaks=round(seq(min(predictors$Y), max(predictors$Y), length.out = 6),1)) +
    # ylim(0, 2.5) +
    themeBorder 
  
}

# predictors_variable_plot(predictorsTable, variable = "N")

## *****************************************************************************
## 6c. plot variable isplines -  dataframe #####################################
## *****************************************************************************


predictorsDF <- function(model){
  
  
  isplines <- isplineExtract(model)
  
  x_values <- data.frame(isplines$x) %>%
    lapply(function(x) scale(x, center = TRUE)) %>% 
    as.data.frame() %>% 
    add_column(Key = row.names(data.frame(isplines$x))) %>%
    dplyr::select("Key", X = 1)
  
  x_values_unscaled <- data.frame(isplines$x) %>%
    # lapply(function(x) scale(x, center = TRUE)) %>% 
    as.data.frame() %>% 
    add_column(Key = row.names(data.frame(isplines$x))) %>%
    # dplyr::select("Key", X_unscaled = 1)
    gather(key = "Factor", value = "X_unscaled", -"Key" )
  
  
  y_values <- data.frame(isplines$y) %>% 
    add_column(Key = row.names(data.frame(isplines$y))) %>%
    gather(key = "Factor", value = "Y", -"Key" )
  
  predictors <- y_values %>%
    full_join(x_values, by = "Key") %>%
    full_join(x_values_unscaled, by = c("Key","Factor")) %>%
    mutate(Factor = as.factor(Factor), X_unscaled = X_unscaled*100)
  
  return(predictors)
  
}


## *****************************************************************************
## 7. plot obs vs predicted comp dissimilarity #################################
## *****************************************************************************

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


## *****************************************************************************
## 8. plot pred ecological distance vs obs comp dissimilarity ##################
## *****************************************************************************

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

## *****************************************************************************
## 8. plot alpha diversity #####################################################
## *****************************************************************************



alpha_plot <- plotFunction <- function(colNames, expVar, data){
  # library(scales)
  
  plotList <- list()
  for (i in colNames) {
    # create dataset
    df <- data[c(expVar,i)]
    
   
    # rename dataframe
    colnames(df) <- c("expVar","y")

    plotList[[i]] <- ggplot(	df, aes(x = expVar, y = y, color=expVar, fill=expVar))  + 
      geom_boxplot()+
      scale_y_continuous(expand = c(0.01, 0.01), breaks=round(seq(min(df$y), max(df$y), length.out = 6),1)) +
      scale_fill_manual(values=c('#f2e6cb',  '#99cec6','#f2e6cb','#99cec6'))+
      scale_color_manual(values=c('#dfc27d',  '#018571','#dfc27d','#018571'))+
      ylab(paste(i)) +	
      xlab("") +	
      themeBorder + theme(legend.position="none")
  }
  return(plotList)
}


# test <- alpha_env("observed", "pH", "FarmType", all_wa$df)

alpha_env <- plotFunction <- function(colNames, expVar, color, data){
  # library(scales)
  
  plotList <- list()
  for (i in colNames) {
    # create dataset
    df <- data[c(expVar,i, color)]
    
    
    # rename dataframe
    colnames(df) <- c("expVar","y", "color")
    
    plotList[[i]] <- ggplot(	df, aes(x = expVar, y = y))  + 
      geom_point(aes(color=color, fill=color, group=color), alpha = 0.6) +
      geom_smooth(aes(color=color, fill=color, group=color), method = "glm", fill = NA, fullrange=TRUE) +
      geom_smooth(method = "glm", fill = NA, color="black", fullrange=TRUE) +
      scale_y_continuous(expand = c(0.01, 0.01), breaks=round(seq(min(df$y), max(df$y), length.out = 6),1)) +
      # scale_x_continuous(expand = c(0.01, 0.01), breaks=round(seq(min(df$expVar), max(df$expVar), length.out = 6),1)) +
      scale_fill_manual(values=c('#f2e6cb',  '#99cec6','#f2e6cb','#99cec6'))+
      scale_color_manual(values=c('#dfc27d',  '#018571','#dfc27d','#018571'))+
      ylab(paste(i)) +	
      xlab(paste(expVar)) +	
      themeBorder + theme(legend.position="none")
  }
  return(plotList)
}



alpha_allVar <- function(model, exp.var){
  
  # create empty list for dataframes
  dfList <- list()
  
  for (i in exp.var) {
    
    # extract dataframe from model
    model_df <- model  @frame %>%
      dplyr::select(contains(i))
    
    # rename column to variable name
    colnames(model_df)[1] <- i
    
    # extract scaled values
    scaled.values <-  model_df %>% dplyr::select(i) %>% pull()
    
    # unscale the variable values
    unscaled.values <- scaled.values * attr(scaled.values, 'scaled:scale') + attr(scaled.values, 'scaled:center')
    
    # create rules for updating the ref_grid for emmeans
    rules <- list(var = seq(min(unscaled.values),max(unscaled.values), length.out = 25))
    names(rules) <- i # rename list to variable name
    
    # update reference grid
    em <- update(ref_grid(model, at=rules))
    
    # create estimated means data.frame
    em_df <- as.data.frame(summary(emmeans(em,as.formula(paste("~", i, sep=" "))), type='response'))
    
    # create final dataframe to add to empty list
    dfList[[i]] <- em_df %>%
      rename("value" = i) %>%
      mutate(exp.var = i, std.value = scale(value))
    
    
  }
  
  # bind all data frames in list
  finalDF <- do.call(rbind, dfList)  %>% filter(exp.var != "NP_ratio")
    
  
  # color palette for polots
  palette <- data.frame(Predictors = c("cropDiversity", "FarmBi","Geographic","N", 
                                       "P", "pH", "TOC", "PolyYears", "matrix_1"), 
                        colors = c("#E69F00", "#A6230C", "#009E73", "#8EB042",
                                   "#0072B2", "#CC79A7", "#8E6345", "#000000", "#000000"))  %>%
    arrange(Predictors)
  
  # extract colors to use based on explanatory variables in thed dataframe in the data frame
  colors <- palette %>%
    filter(Predictors %in% levels(factor(finalDF$exp.var))) %>%
    pull(colors)
  
  
# final plot 
 ggplot(	finalDF, aes(x = std.value, y = response, color = exp.var, group=exp.var))  + 
    geom_line(size = 1)+
    geom_ribbon(colour=NA,alpha=0.2, aes( ymin= response-SE, ymax= response+SE), 
                show.legend=FALSE) +
    # scale_y_continuous(expand = c(0.01, 0.01), breaks=round(seq(min(df_wide$response), max(df_wide$response), length.out = 6),1)) +
    scale_color_manual(values= colors)+
    # ylab(paste(i)) +	
    xlab(paste("standardized variables")) +
    themeBorder #+ theme(legend.position="none")
}

# model = all_wa_shannon_ft
# exp.var = "N"
# color = "FarmType"
# i = "pH"

alpha_predictor <- function(model, exp.var, color, raw.data = FALSE, length.out = 200){
  
  # create empty list for dataframes
  plotList <- list()
  
  for (i in exp.var) {
    
    # extract dataframe from model
    model_df <- model@frame %>%
      dplyr::select(contains(paste("scale(",i,")", sep="")))
    
    # rename column to variable name
    
    colnames(model_df)[1] <- i
    
    
    
    # extract scaled values
    scaled.values <-  model_df %>% dplyr::select(i) %>% pull()
    
    # unscale the variable values
    unscaled.values <- scaled.values * attr(scaled.values, 'scaled:scale') + attr(scaled.values, 'scaled:center')
    
    # response dataframe
    responseDF <- data.frame( response = model@frame[1], value = unscaled.values, exp.var = model@frame[color])
    colnames(responseDF)[1] <- "response"
    colnames(responseDF)[3] <- "exp.var"
    
    # create rules for updating the ref_grid for emmeans
    rules <- list(var = seq(min(unscaled.values),max(unscaled.values), length.out = length.out))
    names(rules) <- i # rename list to variable name
    
    # update reference grid
    em <- update(ref_grid(model, at=rules))
    
    # create estimated means data.frame
    em_df <- as.data.frame(summary(emmeans(em,as.formula(paste("~", i, sep=" "))), type='response')) %>%
      mutate(exp.var = "All farms") %>%
      rename("value" = i) %>%
      dplyr::select(exp.var, value, response, SE )
    
    em_df_interaction <- as.data.frame(summary(emmeans(em,as.formula(paste("~", i, "*", color, sep=" "))), type='response')) %>%
      rename("value" = i, "exp.var" = color) %>%
      dplyr::select(exp.var, value, response, SE )
    
    # create final dataframe to add to empty list
    finalDF <- rbind(em_df, em_df_interaction)
    
    
    if(raw.data == TRUE){
  
  # final plot 
  plotList[[i]] <- ggplot(	finalDF, aes(x = value, y = response, color = exp.var, group=exp.var))  + 
    geom_point(data= responseDF, mapping= aes( x = value, y=response, color=exp.var), alpha=0.5, size = 2, stroke=0) +
    geom_line(size = 0.75)+
    geom_ribbon(colour=NA,alpha=0.2, aes( ymin= response-SE, ymax= response+SE, fill = exp.var), 
                show.legend=FALSE) +
    # scale_y_continuous(expand = c(0.01, 0.01), breaks=round(seq(min(df_wide$response), max(df_wide$response), length.out = 6),1)) +
    scale_color_manual(values=c('#000000', '#dfc27d',  '#018571'))+
    scale_fill_manual(values=c('#000000', '#dfc27d',  '#018571'))+
    # ylab(paste(i)) +	
    xlab(paste(i)) +
    themeBorder } else{
     
       plotList[[i]] <- ggplot(	finalDF, aes(x = value, y = response, color = exp.var, group=exp.var))  + 
        geom_line(size = 0.75)+
        geom_ribbon(colour=NA,alpha=0.2, aes( ymin= response-SE, ymax= response+SE, fill = exp.var), 
                    show.legend=FALSE) +
        # scale_y_continuous(expand = c(0.01, 0.01), breaks=round(seq(min(df_wide$response), max(df_wide$response), length.out = 6),1)) +
        scale_color_manual(values=c('#000000', '#dfc27d',  '#018571'))+
        scale_fill_manual(values=c('#000000', '#dfc27d',  '#018571'))+
        # ylab(paste(i)) +	
        xlab(paste(i)) +
        themeBorder
      
    }
    
    
  } 
  
return(plotList)  
  
  }


# alpha_predictor(all_wa_shannon_ft, edaphic_variables, "FarmType")


effectSizes <- function(model){
  summary <- summary(model)
  table <- as.data.frame(summary$coefficients) %>%
    rownames_to_column("covariate") %>%
    filter(covariate !="(Intercept)") %>%
    mutate(covariate = factor(gsub("scale\\(|\\)","", covariate)),
           interaction = ifelse(str_detect(covariate, ":"), "yes", "no"))
  
  model.type = match('FarmTypePolyculture',table$covariate)
  
  if(is.na(model.type)){
  table <- table %>%
    mutate(covariate = factor(covariate, levels = rev(c("cropDiversity", "N", 
                                                        "P", "pH", "TOC", "cropDiversity:N","cropDiversity:P", "cropDiversity:pH","cropDiversity:TOC"))))
  
  palette <-c( "#8E6345","#CC79A7","#8EB042", "#0072B2",
               "#8E6345","#CC79A7","#8EB042", "#0072B2", "#E69F00")
  
  lim <- c(-0.67, 0.67)
  brks <- c(-0.50,  0.0, 0.50)
  
  } else{
  
    table <- table %>%
      mutate(covariate = factor(covariate, levels = rev(c("FarmTypePolyculture", "N", 
                                                          "P", "pH", "TOC", "FarmTypePolyculture:N","FarmTypePolyculture:P", "FarmTypePolyculture:pH","FarmTypePolyculture:TOC"))))
    
    palette <-c( "#A6230C","#CC79A7","#8EB042", "#0072B2",
                 "#8E6345","#CC79A7","#8EB042", "#0072B2", "#000000")
    
    lim <- c(-1.0, 1.0)
    brks <- c(-1.0, -0.50,  0.0, 0.50, 1.0)
}
  
  
  
  plot <- ggplot(table, aes(x = covariate, y = Estimate, 
                            color = covariate, group=covariate, linetype=interaction)) +
    geom_hline(yintercept=0, linetype = "dashed") +
    geom_point(size = 2, position=position_dodge(width = 0.8)) +
    geom_errorbar( aes(ymin= Estimate-`Std. Error`, ymax= Estimate + `Std. Error`),
                   width = 0.0, position=position_dodge(width = 0.8), show.legend=FALSE) +
    scale_color_manual(values =palette) +
    scale_y_continuous(breaks= brks, limits = lim) +
    themeBorder + theme(legend.position = "none", axis.title.y=element_blank()) +
    coord_flip() 
  
  return(list(plot = plot, df = table))
    
}


## *****************************************************************************
## 9. box plot of environmental variable range #################################
## *****************************************************************************

boxplot_variable <- function(data, variable){
  
  ggplot(data, aes_string(x= variable)) + 
    geom_boxplot() +
    themeBorder + 
    theme(axis.ticks.y = element_blank(), 
          axis.text.y = element_blank(),
          axis.line.y = element_blank())
  
}

env_boxplot <- function(data, variable){
  means <- data %>%
  group_by(FarmType, Block, FTBL) %>%
  summarise_at(variable, list(mean = mean, SE = std.error), na.rm=TRUE) 
  
  df <- data %>%
    mutate(variable = eval(as.symbol(variable)))

ggplot(df, aes(x=FTBL, y= variable, color = FarmType, fill = FarmType)) +
  geom_boxplot(alpha = 0.5, width= 0.4, position=position_dodge(0.6))+
  scale_color_manual(values=c('#dfc27d',  '#018571'))+
  scale_fill_manual(values=c('#dfc27d',  '#018571'))+
  scale_x_discrete(expand=c(0.0,0.5)) +
  ylab(paste(variable))+
  themeBorder + theme(axis.title.y=element_blank()) +
  coord_flip() 
}


env_lineplot <- function(data, variable){
 
  df <- data %>%
    rename("var" = variable) %>%
    mutate(all = "all")
  
  
  palette <- data.frame(Predictors = c("cropDiversity", "FarmBi","Geographic","N", 
                                       "P", "pH", "TOC", "PolyYears", "matrix_1"), 
                        colors = c("#E69F00", "#A6230C", "#009E73", "#8EB042",
                                   "#0072B2", "#CC79A7", "#8E6345", "#000000", "#000000"))  %>%
    arrange(Predictors)
  
  # extract colors to use based on explanatory variables in thed dataframe in the data frame
  color <- palette %>%
    filter(Predictors %in% variable) %>%
    pull(colors)
  
  ggplot() +
    geom_smooth(df, mapping= aes(x=cropDiversity, y= var, linetype = all), 
                method="glm",fullrange=TRUE, color = color, fill = color, alpha = 0.25,  size = 0.70) +
    geom_smooth(df, mapping= aes(x=cropDiversity, y= var, linetype = Block), 
                method="glm",fullrange=TRUE, color = color, fill = color, alpha = 0.25, size = 0.7) +
    scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
    ylab(paste(variable))+
    themeBorder 
}


# env_lineplot(all, "pH")


## *****************************************************************************
## 9. envi variable tables #####################################################
## *****************************************************************************


enviRange <- function(complete_table){
  
  mean <- complete_table %>%
    dplyr::select(envi_factors) %>%
    colMeans()
  
  
  min <- complete_table %>%
    dplyr::select(envi_factors) %>%
    as.matrix() %>%
    apply(2,min)
  
  
  max <- complete_table %>%
    dplyr::select(envi_factors) %>%
    as.matrix() %>%
    apply(2,max)
  
  
  sd <- complete_table %>%
    dplyr::select(envi_factors) %>%
    as.matrix() %>%
    apply(2,sd)
  
  table <- data.frame(mean, sd, min, max)
  
  return (table)
}










## *****************************************************************************
## 10. Backwards selection #####################################################
## *****************************************************************************


backwardsSelection <- function(df,guild, family, block, focalcrop, farmtype, year, env_factors, geo, maxDist){
  # to supress warnings
  oldw <- getOption("warn")
  options(warn = -1)
  
  # filter dataset
  if(guild == "all fungi"){
    
    df <- df %>%
      filter(Block %in% block, FocalCrop %in% focalcrop, FarmType %in% farmtype, Year %in% year)}
  
  else{
    df <- guild_filter(df, guild,family) %>%
      filter(Block %in% block, FocalCrop %in% focalcrop, FarmType %in% farmtype, Year %in% Year)}
  
  # calculate diversity indices 
  diversity <- df %>%
    dplyr::select(contains("OTU")) %>%
    t() %>%
    richness() %>%
    mutate(Key = df$Key,
           shannon = vegan::diversity(df %>%
                                        dplyr::select(contains("OTU"))))
  
  df <- df %>%
    left_join(diversity, by="Key")
  
  # create table list
  tableList <- list()
  
  # create gdm list
  gdmList <- list()
  
  # create plot list
  plotList <- list()
  
  compPlotList <- list()
  
  predictorsDFList <- list()
  
  spTableList <- list()
  
  # new predictors
  new_predictors <- env_factors
  
  # get length of predictos
  i <- length(new_predictors)
  
  repeat {
    
    # create name for list based on the model number order
    modelNum <- paste('item:',(length(env_factors)+1)-i,sep='') 
    
    # create inputs
    inputs <- input_diss(df, new_predictors)
    
    # calculate distance
    inputs$distanceM <-distHaversine(inputs[,3:4],inputs[,5:6])
    
    # remove distance
    
    if(maxDist == "local") { # for within type models
      finalDF <- inputs%>% 
        filter(distanceM < 60) %>%
        dplyr::select(-"distanceM")}
    
    else if(maxDist == "both") { # for within type models
      finalDF <- inputs%>% 
        # filter(distanceM < 60) %>%
        dplyr::select(-"distanceM")}
    
    else{                   # for across type models
      finalDF <- inputs%>% 
        filter(distanceM > 60) %>%
        dplyr::select(-"distanceM")
    }
    
    #spTable
    
    spTableList[[modelNum]]<- finalDF
    
    # finalDF <- within_AMF_f$spTable$`item:1`
    # geo = FALSE
    
    # run GDM model
    model <- gdm(finalDF, geo=geo)
    gdmList[[modelNum]] <- model
    
    # create plotList
    plotList[[modelNum]] <- predictors_plot(model) + ggtitle(paste(maxDist)) + theme( plot.title = element_text(hjust = 0.5)) 
    # + theme(legend.position="none")
    
    compPlotList[[modelNum]] <- comp_plot(model)
    
    predictorsDFList[[modelNum]] <- predictorsDF(model)
    
    # get DIC value
    DIC <- c("DIC", model$gdmdeviance)
    
    # add to list
    tableList[[modelNum]] <- table(model)
    
    # add DIC value
    tableList[[modelNum]]  <- rbind(tableList[[modelNum]], DIC)
    
    # round numeric values
    tableList[[modelNum]]$Coefficients <- round(as.numeric(tableList[[modelNum]]$Coefficients),3) 
    
    # find predictor with lowest coefficient
    predictor <- tableList[[modelNum]] %>% 
      filter(Coefficients == min(Coefficients)) %>% 
      pull(Predictors)
    
    
    # get list of new predictors
    new_predictors <- new_predictors[! new_predictors %in% predictor[1]]
    
    # get new length of predictors
    i <-  length(new_predictors)  
    
    if (predictor == "Geographic") { # If geographic is in the list, set geo=FALSE
      
      geo = FALSE
      
    } # If geographic is NOT in the list, set geo=TRUE
    
    else{
      geo = TRUE }
    
    if(i < 1) { # break when there is only 1 predictor left
      break
    }
    
  }
  options(warn = oldw)
  
  return(list(df= list(df), spTable = spTableList, tables=tableList,gdmModels= gdmList, plotList = plotList, predictors=predictorsDFList, compPlots = compPlotList))
}




## *****************************************************************************
## 11. simple selection ########################################################
## *****************************************************************************

# df<- all_fungi 
# guild= "all fungi", family="NA", block= c("F","N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture","Polyculture"), year = c("2017")
# 
# guild= "Saprotroph", family="NA", block= c("F","N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture","Polyculture"), year = c("2017")

simpleSelection <- function(df,guild, family, block, focalcrop, farmtype, year, env_factors, geo, maxDist){
  # to supress warnings
  oldw <- getOption("warn")
  options(warn = -1)
  
  # filter dataset
  if(guild == "all fungi"){
    
    df <- df %>%
      filter(Block %in% block, FocalCrop %in% focalcrop, FarmType %in% farmtype, Year %in% year)}
  
  else{
    df <- guild_filter(df, guild, family) %>%
      filter(Block %in% block, FocalCrop %in% focalcrop, FarmType %in% farmtype, Year %in% year)
    }
  
  # calculate diversity indices 
  diversity <- df %>%
    dplyr::select(contains("OTU")) %>%
    t() %>%
    richness() %>%
    mutate(Key = df$Key,
           shannon = vegan::diversity(df %>%
                                        dplyr::select(contains("OTU"))))
  
  df <- df %>%
    left_join(diversity, by="Key")
  
    
    # create inputs
    inputs <- input_diss(df, env_factors)
    
    # calculate distance
    inputs$distanceM <-distHaversine(inputs[,3:4],inputs[,5:6])
    
    # remove distance
    
    if(maxDist == "local") { # for within type models
      finalDF <- inputs%>% 
        filter(distanceM < 60) %>%
        dplyr::select(-"distanceM")}
    
    else if(maxDist == "both") { # for within type models
      finalDF <- inputs%>% 
        # filter(distanceM < 60) %>%
        dplyr::select(-"distanceM")}
    
    else{                   # for across type models
      finalDF <- inputs%>% 
        filter(distanceM > 60) %>%
        dplyr::select(-"distanceM")
    }
    
    #spTable
    
    spTableList <- finalDF
    
    # finalDF <- within_AMF_f$spTable$`item:1`
    # geo = FALSE
    
    # run GDM model
    model <- gdm(finalDF, geo=geo)
    gdmList <- model
    
    # create plotList
    plotList <- predictors_plot(model) + ggtitle(paste(maxDist)) + theme( plot.title = element_text(hjust = 0.5)) 
    # + theme(legend.position="none")
    
    compPlotList <- comp_plot(model)
    
    predictorsDFList <- predictorsDF(model)
    
    # get DIC value
    DIC <- c("DIC", model$gdmdeviance)
    
    # add to list
    tableList <- table(model)
    
    # add DIC value
    tableList  <- rbind(tableList, DIC)
    
    # round numeric values
    tableList$Coefficients <- round(as.numeric(tableList$Coefficients),3) 
    
  
  options(warn = oldw)
  
  return(list(df= df, spTable = spTableList, tables=tableList,gdmModels= gdmList, plotList = plotList, predictors=predictorsDFList, compPlots = compPlotList))
}

## *****************************************************************************
## 12. taxonColor ##############################################################
## *****************************************************************************


taxonColor <- function(taxons){
  
  Taxons <- c("Acaulosporaceae", "Acaulospora",  "Archaeospora", "Cetraspora", "Claroideoglomus",
              "Diversisporaceae","Diversisporales", "Diversispora", "Funneliformis", "Glomeromycota", 
              "Glomerales", "Glomarales" ,"Glomeraceae", "Paraglomerales", "Paraglomeraceae","Rhizophagus")
  
  Colors <- c( "#6f6c70", "#009E73", "#8569D5", "#FFA500", "#E7298A", 
               "#A6761D", "#CBD588", "#66A61E", "#A84100", "#1C3A85", 
               "#5BAABD", "#5BAABD", "#652926", "#BD544E", "#437EBB", "#9969C8")
  
  
  
  colorTable <- data.frame(Taxons = Taxons, Colors = Colors)
  
  colorTable %>% filter(Taxons %in% levels(factor(taxons))) %>%
    pull(Colors)
}


## *****************************************************************************
## 12. relative abundance plots ################################################
## *****************************************************************************
# 
# relAbunPlot(all_sap$df,"FarmType")
# 
# library(RColorBrewer)
# n <- 146
# qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
# col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# pie(rep(1,n), col=sample(col_vector, n))
# color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
# col=sample(color, n)
# 
# data <- all_sap$df
# variable = "FarmType"

relAbunPlot <- function(data, variable, taxanomy = tax){
  
  # make otu table with presence-abensence values
  # otu <- decostand(data %>% dplyr::select(contains("OTU")), "pa")  
  otu <- data %>% dplyr::select(contains("OTU"))
  
  # make data frame of relative abundances
  relAbunDF <- otu %>%
    mutate(totalFreq = c(rowSums(.)),
           Key = data$Key) %>%
    pivot_longer(cols=contains("OTU"), names_to = "OTU", values_to = "freq") %>%
    filter(freq != 0) %>%
    mutate(relAbun = freq/totalFreq) %>%
    left_join(taxanomy , by=c("OTU")) %>%
    left_join(data, by="Key") %>%
    rename("var" = variable)
  
  
  # create rounding factor
  roundFactor <- (max(relAbunDF$var) - min(relAbunDF$var))/5
  
  # create rounding rules
  roundRule <-  case_when(
    roundFactor < 1 ~ 2,
    roundFactor > 1 ~ 0)
    
  # update rounding factor to a "whole-ish" number
  roundFactor <- round(roundFactor, roundRule)
  
  # round data
  relAbunDF <- relAbunDF %>%
    mutate(var = factor(round_any(var,roundFactor)))
  
  set.seed(5)
  
  # plot
  relAbunPlot <-  ggplot(relAbunDF, aes(fill=Order, y=freq, x=FarmKey)) +
    geom_bar(position="fill", stat="identity") +
    ylab("relative abundance") +
    xlab(variable) +
    scale_fill_manual(values= taxonColor(relAbunDF$Taxon))+
    # scale_fill_manual(values= sample(color, 51))+
    theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust=1)) + themeBorder + facet_wrap(~var, scales="free")
  
    return(relAbunPlot)
  
}


# relAbunPlot(all_wa$df, "cropDiversity")


## *****************************************************************************
## 13. run titan ############################################################
## *****************************************************************************

# test <- run.titan(all_wa$df, "cropDiversity", aggregate = FALSE)
# plot_taxa(test)

run.titan <- function(data, variable, cpus = 8, aggregate = FALSE, taxanomy = tax) {
  
  if(aggregate == FALSE){
  # select taxa for titan analysis
  taxa <- data %>% dplyr::select(contains("OTU"))
  } else{
    
    taxa <- data %>% dplyr::select(Key, contains("OTU")) %>%
      pivot_longer(!Key, names_to = "OTU", values_to = "reads") %>%
      left_join(taxanomy %>% dplyr::select("OTU","Taxon"), by = "OTU") %>%
      group_by(Key, Taxon) %>%
      summarise_at("reads", list(sum)) %>%
      pivot_wider(names_from = Taxon, values_from = reads) %>%
      column_to_rownames("Key")
  }
  
  # convert taxa matrix to presence-absence only
  taxa.pa <- decostand(taxa, "pa")
  
  # select taxa that is present more than 3x
  keepTaxa <- data.frame(sums=colSums(taxa.pa)) %>%
    rownames_to_column("OTU") %>%
    filter(sums > 3) %>% pull(OTU)
  
  # remove taxa that are present 0x
  taxa <- taxa[, colSums(taxa != 0) > 0]
  
  # keep taxa present more than 3x
  taxa <- taxa[c(keepTaxa)]
  
  # select variables
  varDF <- data %>% dplyr::select(variable)
  
  # TITAN analysis
  titan <- titan(varDF, taxa, ncpus = cpus)
  
  return(titan)
  
}

## *****************************************************************************
## 13. TITAN plot ############################################################
## *****************************************************************************

titanPlot <- function(titan, rel = 1 ) {
  
  # create data frame from titan output
  df <- titan$sppmax  %>%
    as.data.frame() %>% 
    rownames_to_column("OTU") %>%
    left_join(tax, by = "OTU") %>%
    filter(filter >0, reliability >= rel) %>%
    mutate(lowSE = zenv.cp-`5%`, highSE = abs( zenv.cp-`95%`), filter = factor(filter), organization = ifelse(filter == 1, zenv.cp*1, zenv.cp*-1), zscore = ifelse(filter == 1, "z+", "z-")) %>%
    group_by(filter) %>%
    mutate(rank = order(order(organization, decreasing=TRUE)), 
           rank2 = ifelse(filter != 1, rank+0.5, rank)) %>%
    ungroup() %>%
    arrange( rank2) %>%
    mutate(OTU = factor(OTU, levels=OTU), zscore = factor(zscore, levels = c("z+","z-")))
  
  # make plot from titan data
  plot <- ggplot(df, aes(y= zenv.cp, x= OTU, color=Taxon)) +
    geom_errorbar(aes(ymin=zenv.cp-lowSE, ymax=zenv.cp+highSE, linetype = zscore), width=0, size=0.6, position=position_dodge(0.05)) +
    # scale_linetype_manual(values=c("dashed", "solid"))+
    geom_point(aes(shape=zscore, size=freq), fill = "white", stroke=1.25) +
    scale_shape_manual(values = c(16,21) ) + 
    # scale_x_discrete(breaks = seq(1,nrow(df), by=3), labels= seq(1,nrow(df), by=3)) +
    xlab("OTUs") +
    scale_color_manual(values = taxonColor(taxons=df$Taxon)) + 
    scale_x_discrete(expand=c(0.05,0.0)) +
    coord_flip() + themeBorder  +  #+ facet_wrap(~filter)
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title = element_blank())
  
  output <- list(df=df, plot=plot)
  
  return(output)
  
  
}

## *****************************************************************************
## 13. TITAN Output ############################################################
## *****************************************************************************


titanOutput <- function(data, variable, taxanomy = tax, cpus = 8){
  
  # 1. run titan
  titan <- run.titan(data, variable, cpus)
  
  # 2. create plot from titan output
  titanPlot <- titanPlot(titan)
  
  # 3. create relative abundance plot
  relAbunPlot <- relAbunPlot(data, variable, taxanomy)
  
}

## *****************************************************************************
## 14. model summary ###########################################################
## *****************************************************************************

# get response variable
getResponse <- function(formula) {
  tt <- terms(formula)
  vars <- as.character(attr(tt, "variables"))[-1] ## [1] is the list call
  response <- attr(tt, "response") # index of response var
  vars[response] 
}

# make table of the model summary and Anova
modelSummary <- function(model){
  summary <- summary(model)
  coef <- as.data.frame(summary$coefficients)
  type <- class(model)[1]
  
  if(type == "glmerMod"){
  
  summaryTable <- coef %>%
    rownames_to_column("covariate") %>% 
    filter(!str_detect(covariate, 'Intercept')) %>%
    mutate_if(is.numeric, ~round(., 3)) %>%
    rename(`P` = `Pr(>|z|)`, `SE` = `Std. Error`) %>%
    mutate(sig = case_when(
      P <= 0.001 ~ "***",
      P <= 0.01 &  P > 0.001 ~ "**",
      P <= 0.05 &  P > 0.01 ~ "*",
      P > 0.05  ~ "ns"),
      response = getResponse(model)) %>%
    dplyr::select(response, covariate, Estimate, SE, P, sig)
  
  anovaTable <- as.data.frame(Anova(model , type = 3)) %>%
    rownames_to_column("covariate") %>% 
    filter(!str_detect(covariate, 'Intercept')) %>%
    mutate_if(is.numeric, ~round(., 3)) %>%
    rename(`P` = `Pr(>Chisq)`) %>%
    mutate(sig = case_when(
      P <= 0.001 ~ "***",
      P <= 0.01 &  P > 0.001 ~ "**",
      P <= 0.05 &  P > 0.01 ~ "*",
      P > 0.05  ~ "ns"),
      response = getResponse(model)) %>%
    dplyr::select(response, covariate, Chisq, P, sig)
  
  } else if(type == "lmerModLmerTest"){
   
     summaryTable <- coef %>%
      rownames_to_column("covariate") %>% 
      filter(!str_detect(covariate, 'Intercept')) %>%
      mutate_if(is.numeric, ~round(., 3)) %>%
      rename(`P` = `Pr(>|t|)`, `SE` = `Std. Error`) %>%
      mutate(sig = case_when(
        P <= 0.001 ~ "***",
        P <= 0.01 &  P > 0.001 ~ "**",
        P <= 0.05 &  P > 0.01 ~ "*",
        P > 0.05  ~ "ns"),
        response = getResponse(model)) %>%
      dplyr::select(response, covariate, Estimate, SE, P, sig)
    
    anovaTable <- as.data.frame(Anova(model , type = 3)) %>%
      rownames_to_column("covariate") %>% 
      filter(!str_detect(covariate, 'Intercept')) %>%
      mutate_if(is.numeric, ~round(., 3)) %>%
      rename(`P` = `Pr(>Chisq)`) %>%
      mutate(sig = case_when(
        P <= 0.001 ~ "***",
        P <= 0.01 &  P > 0.001 ~ "**",
        P <= 0.05 &  P > 0.01 ~ "*",
        P > 0.05  ~ "ns"),
        response = getResponse(model)) %>%
      dplyr::select(response, covariate, Chisq, P, sig)
  }
 
  list(model = summary$call, AIC = AIC(model), summary = summaryTable, Anova = anovaTable) 
}



## *****************************************************************************
## 15. pcoa + plot #####################################################
## *****************************************************************************

pcoaFun <- function(data, shape, color, formula = NULL, strata= NULL, plot.means = FALSE){
  
  # extract OTUs
  otus <- data %>% dplyr::select(contains("OTU"))
  
  # create distance matrix
  dist <- vegdist(decostand(otus, "pa"), "bray")
  
  # crate PCoA
  PCoA <- ape::pcoa(dist)
  axes <- cbind(as.data.frame(PCoA$vectors[,1:2]), Key=as.character(data$Key)) 

  # join dataframes
  if(plot.means == FALSE){
  df <- data %>%
    mutate(Key = as.character(Key)) %>%
    left_join(axes, by = "Key") %>%
    mutate(clr = eval(as.symbol(color)),
           shp = eval(as.symbol(shape)))
  
  ## ************ Plot ************ ##
  plot <- ggplot(data=df, aes(x=Axis.1,y=Axis.2, colour= clr)) + 
    geom_point(aes(x=Axis.1,y=Axis.2), size=4, stroke=0, alpha=0.8)+ 
    # scale_color_manual(values=c('#dfc27d', '#018571')) +
    scale_shape_manual(values = c(16, 1))+
    ylab("PCoA 2") +
    xlab("PCoA 1") +
    themeBorder # +
  # legend_topRight()
  
  }else{
    
    df <- data %>%
      mutate(Key = as.character(Key)) %>%
      left_join(axes, by = "Key") %>%
      mutate(clr = eval(as.symbol(color)),
             shp = eval(as.symbol(shape))) %>%
      group_by( clr, shp, farmCode, Year) %>%
      summarise_at(vars("Axis.1", "Axis.2"), list(mean = mean, SE = std.error), na.rm=TRUE)
    
    ## ************ Plot ************ ##
    plot <- ggplot(data=df, aes(x=Axis.1_mean,y=Axis.2_mean, colour= clr)) + 
      geom_errorbar(aes(ymin = Axis.2_mean - Axis.2_SE, ymax = Axis.2_mean + Axis.2_SE), width = 0) +
      geom_errorbar(aes(xmin = Axis.1_mean - Axis.1_SE, xmax = Axis.1_mean + Axis.1_SE), width = 0) +
      geom_point(aes(x=Axis.1_mean,y=Axis.2_mean), size=1.5, stroke=1, alpha=1, fill = "white")+ 
      # scale_color_manual(values=c('#dfc27d', '#018571')) +
      # scale_color_gradient(low = "#CF3700", high = "#E3CB17" ) +
      scale_shape_manual(values = c(16, 21))+
      ylab("PCoA 2") +
      xlab("PCoA 1") +
      themeBorder # +
    # legend_topRight()
  }
  
  
  
 
  
  
  
  if(!is.null(formula)){
    
    permanovaDF <- data %>%
      dplyr::rename("strata" = strata)
    
    finalFormula <- as.formula(paste("dist ~", formula, sep=" "))
    permanova <- adonis(finalFormula, strata=permanovaDF$strata,  data = permanovaDF)
    
    output <- list(commMatrix = otus, dist = dist, df = df, plot=plot, permanova = permanova)
    
  } else {
    output <- list(commMatrix = otus, dist = dist, df = df, plot=plot)
  }

  
return(output)
  
}

# test <- pcoaFun(all_wa$df, color = "farmCode", shape = "FarmType", formula = "FarmType*Block", strata = "farmCode")

# test$plot

## *****************************************************************************
## 16. dbRDAs ######################################################
## *****************************************************************************

# # add a small amount to each species if sites have 0 present: https://archetypalecology.wordpress.com/2018/02/21/distance-based-redundancy-analysis-db-rda-in-r/
# ## https://github.com/pbuttigieg/marmicStats/blob/master/statsII/dbrda.R
# 
# # extract OTUs
# otus <- data %>% dplyr::select(contains("OTU"))
# 
# # create distance matrix
# dist <- vegdist(decostand(otus, "pa"), "bray")
# 
# # crate PCoA
# PCoA <- ape::pcoa(dist)
# axes <- cbind(as.data.frame(PCoA$vectors[,1:2]), Key=as.character(data$Key)) 
# 
# 
# 
# # partial dbRDA model
# dbRDAmodel <- capscale(otus ~  cropDiversity + pH + P + TOC + N +  Condition(farmCode)  , data=data, distance="bray", add = TRUE)
# 
# # automatically select variables of "env" matrix that best explain "spe" matrix
# # finalmodel <- ordistep(dbRDAmodel, scope=formula(dbRDAmodel), direction = "forward")
# 
# 
# # dataframe
# 
# # dbRDA_df <- as.data.frame(finalmodel$CCA$u)
# dbRDA_df <- as.data.frame(finalmodel$CCA$wa)
# dbRDA_df$Key <- as.integer(c(row.names(dbRDA_df)))
# dbRDA_df <- dbRDA_df %>%
#   left_join(ss)
# dbRDA_df_env <- as.data.frame(finalmodel$CCA$biplot)
# 
# # Testing the significance of the CCA model:
# # if our whole CCA model, the CCA terms (environmental varibles), and CCA axes explain more variance of "spe" (observations) matrix than expected by chance
# rdaModelSig <- anova.cca(finalmodel)
# 
# # Testing the significance of terms (environmental variables):
# #rdaTermsSig <- anova(finalmodel, by="terms", permutations = 999)
# rdaTermsSig <- anova.cca(dbRDAmodel, by="terms", model = c("direct"), strata = data$farmCode)
# 
# # rdaTermsSig$R2 <- rdaTermsSig$'SumOfSqs'/sum(rdaTermsSig$'SumOfSqs'[1:13])
# 
# # Testing the significance of CCA axes (at least the first two or three should present a significant p value):
# rdaAxesSig <- anova(finalmodel, by="axis")


## *****************************************************************************
## 16. PCA + plot ######################################################
## *****************************************************************************

pcaFun <- function(data, vars, shape, color, center = TRUE, scale = TRUE, testGroup = NULL){
  
  # extract matrix of variables
  varsDF <- data %>% dplyr::select(vars)
  
  # run PCA
  pca <- rda(varsDF, center = center, scale=scale)
  
  # proportion explained by each component
  propExplained <- summary(pca)$cont$importance[,1:2]
  
  # extract "loadings" of the PCA
  loadings <- data.frame(pca$CA$v)[1:2] %>% rownames_to_column("predictor")
  
  # extract values of the PCA
  pcaValues <- data.frame(pca$CA$u)[1:2] %>% rownames_to_column("Key")
  
  # add PCA values to data frame
  df <- data %>%
    mutate(Key = as.character(Key)) %>%
    left_join(pcaValues, by = "Key") %>%
    dplyr::rename("clr" = color ,
                  "shp" = shape)
  
  
  ## ************ Plot ************ ##
  pcaPlot <- ggplot(df) + 
    geom_point(aes(x = PC1, y = PC2, color = clr, shape = shp, group = clr), 
               size=5, alpha=0.75, position = position_jitter()) +
    scale_colour_manual(values=c('#d0a540', '#018571')) +
    scale_shape_manual(values = c(16, 1)) +
    geom_text(data = loadings, aes(x = (PC1/2.5)-0.01, y = (PC2/2.5)-0.01, label = predictor), col = 'black') +
    geom_segment(data = loadings,
                 aes(x = 0, y = 0, xend = PC1/2.5, yend = PC2/2.5), 
                 arrow=arrow(length=unit(0.2,"cm")),alpha = 0.75, color = 'black') +
    # scale_y_continuous(expand = c(0.01, 0.01), limits=c(-0.2, 0.2), breaks=round(seq(-0.2, 0.2,length.out=7),2)) +
    # scale_x_continuous(expand = c(0.01, 0.01), limits=c(-0.15, 0.15), breaks=round(seq(-0.15, 0.15,length.out=7),2)) +
    themeBorder # +
  # legend_topRight()
  
  if(!is.null(testGroup)){
    
    dispersionDF <- data %>%
      dplyr::rename("testGroup" = testGroup)
    
    dispersion <- betadisper(dist(varsDF), dispersionDF$testGroup)
    dispersionTest <-  permutest(dispersion)
    
    
    # create new dataframe
    dispersionDF <-  data.frame(dispersion.distances = dispersion$distances, Key = as.character(data$Key)) %>%
      # rownames_to_column("Key") %>%
      left_join(data %>% mutate(Key = as.character(Key)), by="Key") %>%
      dplyr::rename("clr" = color, "testGroup" = testGroup)
    
    # plot
    dispersionPlot <- dispersionDF %>% filter(!is.na(clr)) %>% ggplot( aes(x = testGroup, y = dispersion.distances, color = testGroup, group = testGroup, fill = testGroup)) +
      geom_boxplot() + 
      scale_colour_manual(values=c('#d0a540', '#018571')) +
      scale_fill_manual(values = c('#e8d2a2','#99cec6')) +
      xlab(color) + 
      themeBorder
    
    # final output
    output <- list(pca = list(pca = pca, propExplained = propExplained, loadings = loadings, df = df, 
                              varsDF = varsDF, pcaPlot = pcaPlot),
                   dispersion = list(dispersionTest = dispersionTest, dispersionDF = dispersionDF, 
                                     dispersionPlot = dispersionPlot))
    
    
    } else {
      
      # final output
      output <- list(pca = pca, propExplained = propExplained, loadings = loadings, df = df, varsDF = varsDF, plot = plot)
  }
  
  return(output)
}



## *****************************************************************************
## 17. theme settings ##########################################################
## *****************************************************************************

themeBorder <- theme_bw() + theme(legend.title=element_blank(), 
                                  panel.background = element_blank(),
                                  panel.grid.major = element_blank(), 
                                  panel.grid.minor = element_blank(),
                                  axis.line =element_blank(),
                                  panel.border = element_rect(colour = "black", fill=NA, size=0.75))

axis.x.blank <- theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank())

axis.y.blank <- theme(axis.title.y=element_blank(),
                      axis.text.y=element_blank(),
                      axis.ticks.y=element_blank())



legend_topRight <-  function(base_size=14, base_family=""){
  theme(legend.position =c(1,1.03),
        legend.justification = c(1,1),
        legend.background = element_blank())}

legend_topLeft <- function(base_size=14, base_family="")
{theme(legend.position =c(0,0.97),
       legend.justification = c(0,1),
       legend.background = element_blank())}


legend_bottomRight <-  function(base_size=14, base_family=""){
  theme(legend.position =c(1,0.03),
        legend.justification = c(1,0),
        legend.background = element_blank())}

legend_bottomLeft <- function(base_size=14, base_family="")
{theme(legend.position =c(0,0.03),
       legend.justification = c(0,0),
       legend.background = element_blank())}		

legend_blank <- function(base_size=14, base_family="")
{theme(legend.position = "none")}		





## *****************************************************************************
## 18. MRM with randomization ##################################################
## *****************************************************************************

# MMRR performs Multiple Matrix Regression with Randomization analysis
# Y is a dependent distance matrix
# X is a list of independent distance matrices (with optional names)


MMRR<-function(Y,X,nperm=999){
  #compute regression coefficients and test statistics
  nrowsY<-nrow(Y)
  y<-unfold(Y)
  if(is.null(names(X)))names(X)<-paste("X",1:length(X),sep="")
  Xmats<-sapply(X,unfold)
  fit<-lm(y~Xmats)
  coeffs<-fit$coefficients
  summ<-summary(fit)
  r.squared<-summ$r.squared
  tstat<-summ$coefficients[,"t value"]
  Fstat<-summ$fstatistic[1]
  tprob<-rep(1,length(tstat))
  Fprob<-1
  
  if(length(X) >=2){
  # calculate relative importance
  relimp <- calc.relimp(fit, type = c("lmg"), rela = TRUE) }
  else{
    relimp <- "insufficent variables to calculate variable importance"
  }
  
  
  #perform permutations
  for(i in 1:nperm){
    rand<-sample(1:nrowsY)
    Yperm<-Y[rand,rand]
    yperm<-unfold(Yperm)
    fit<-lm(yperm~Xmats)
    summ<-summary(fit)
    Fprob<-Fprob+as.numeric(summ$fstatistic[1]>=Fstat)
    tprob<-tprob+as.numeric(abs(summ$coefficients[,"t value"])>=abs(tstat))
  }
  
  #return values
  tp<-tprob/(nperm+1)
  Fp<-Fprob/(nperm+1)
  names(r.squared)<-"r.squared"
  names(coeffs)<-c("Intercept",names(X))
  names(tstat)<-paste(c("Intercept",names(X)),"(t)",sep="")
  names(tp)<-paste(c("Intercept",names(X)),"(p)",sep="")
  names(Fstat)<-"F-statistic"
  names(Fp)<-"F p-value"
  return(list(r.squared=r.squared,
              coefficients=coeffs,
              tstatistic=tstat,
              tpvalue=tp,
              Fstatistic=Fstat,
              Fpvalue=Fp,
              relimp = relimp))
}

# unfold converts the lower diagonal elements of a matrix into a vector
# unfold is called by MMRR

unfold<-function(X){
  x<-vector()
  for(i in 2:nrow(X)) x<-c(x,X[i,1:i-1])
  x<-scale(x, center=TRUE, scale=TRUE)  # Comment this line out if you wish to perform the analysis without standardizing the distance matrices! 
  return(x)
}



run.MMMR <- function(data, vars=FALSE, trans=NULL, type="bray", contains=c("OTU")){
  
  df <- data
  
  if(contains == "OTU"){
  spec <- df %>% dplyr::select(contains("OTU"))}
  else{
    spec <- df %>% dplyr::select(contains)
    spec <- as.data.frame(lapply(spec, function(x) scale(x)))
  }
  
  if(! is.null(trans)){
    specDF <- decostand(spec,trans)
  }else {
    specDF <- spec
  }
  
  # CREATE DISSIMILARITY MATRIX FOR OTUs
    dist.spec <- vegdist(spec, type)
  
  # Make a list of the explanatory (X) matrices.
  # Names are optional.  Order doesn't matter.
  # Can include more than two matrices, if desired.
  
  Xmats <- list()
  
  if(vars != FALSE){
  for(i in vars){
    
    Xmats[[i]] <- as.matrix(dist(scale(df %>% dplyr::select(i))))
    
  } } else {
    Xmats <- list()
  }
  
  # add geographic distance matrix
  geo <- df %>% dplyr::select("Long_point", "Lat_point")
  dist.geo <- as.matrix(as.dist(distm(geo, fun = distHaversine) ))
  
  Xmats$geography <- dist.geo
  
  # run MMRR
  # Run MMRR function using genMat as the response variable and Xmats as the explanatory variables.
  # nperm does not need to be specified, default is nperm=999)
  MMRRoutput <- MMRR(as.matrix(dist.spec),Xmats,nperm=999)

  # create MMRR table with rounded values
  MMRRtable <- cbind(coefficent=MMRRoutput$coefficients, t=MMRRoutput$tstatistic, p=MMRRoutput$tpvalue)
  MMRRtable <- round(MMRRtable,3)
  MMRRtable <- as.data.frame(MMRRtable) %>%
    rownames_to_column("variable")
  
  if(vars != FALSE){
    for(i in vars){
      
      # create relative importance table with rounded values
      relImp <- round(data.frame(lmg=MMRRoutput$relimp@lmg),3)
      row.names(relImp) <- names(Xmats)
      relImp <- as.data.frame(relImp) %>%
        rownames_to_column("variable")
      
      # model information
      varExplained <- paste("Proportion of variance explained by model:", round(MMRRoutput$relimp@R2,3), sep=" ")
      modelPvalue <- paste("Model p-value:", round(MMRRoutput$Fpvalue,3), sep=" ")
      modelFstat <- paste("Model F-statistic:", round(MMRRoutput$Fstatistic,3), sep=" ")
      
      
    } } else {
      
      relImp <- MMRRoutput$relimp
      
      # model information
      varExplained <- paste("Proportion of variance explained by model:", round(MMRRoutput$r.squared,3), sep=" ")
      modelPvalue <- paste("Model p-value:", round(MMRRoutput$Fpvalue,3), sep=" ")
      modelFstat <- paste("Model F-statistic:", round(MMRRoutput$Fstatistic,3), sep=" ")
      
    }
  
  
  return(list(variance=varExplained,`p-value`=modelPvalue, `F-statistic`=modelFstat, MMRR=MMRRtable, relative_importance = relImp))
  
}


# run.MMMR(all_wa$df, c("pH","P", "N", "TOC","FarmBi"), trans="hellinger")

## *****************************************************************************
## 19. variation partioning ####################################################
## *****************************************************************************

# data <- poly_wa$df

run.varpart <- function(data, env = TRUE, cd = TRUE, geo=TRUE){
  
  df <- data
  
  # prepare environmental variables  
  env <- df %>% dplyr::select(c("pH","P","N","TOC"))
  cd <- df %>% dplyr::select(c("FarmBi"))
  
  # geographic distances
  geo <- df %>% dplyr::select("Long_point", "Lat_point")
  dbmem.tmp <- dbmem(dist(geo), silent = FALSE)
  dbmemDF <- as.data.frame(dbmem.tmp)
  
  # community matrix
  otu <- df %>% dplyr::select(contains("OTU"))
  dist.otu <- vegdist(otu, "bray")
  dist.otu <- vegdist(decostand(otu, "hellinger"), "bray")
  
  
  if(cd==TRUE){
    
    parts <- c("geo","soil","cd", "geo&cd","soil&cd","soil&geo","geo&cd&soil","residuals")
    
    # run variation paritioning
    vp <- varpart(dist.otu,dbmemDF,env, cd)
    
    # extract individual fractions
    vpDF <- vp$part$indfract %>%
      mutate(parts = parts, 
             Adj.R.square = round(ifelse(Adj.R.square<0,0, Adj.R.square), 2)) %>%
      dplyr::select("parts", "Adj.R.square")
    
    
    # create combination and weights
    vpNames.tmp <- setNames(round(vpDF$Adj.R.square,3), as.character(vpDF$parts))
    
  } else {
    parts <- c("geo","soil","soil&geo","residuals")
    vp <- varpart(dist.otu,dbmemDF,env)
    
    # extract individual fractions
    vpDF <- vp$part$indfract %>%
      mutate(parts = parts, 
             Adj.R.squared = round(ifelse(Adj.R.squared<0,0, Adj.R.squared), 2)) %>%
      dplyr::select("parts", "Adj.R.squared")
    
    
    # create combination and weights
    vpNames.tmp <- setNames(round(vpDF$Adj.R.squared,3), as.character(vpDF$parts))
  }
  
  
  vpNames<- vpNames.tmp[names(vpNames.tmp) != "residuals"]
  
  # create venn diagram
  e <- euler(combinations=vpNames)
  
  # plot
  vpPlot <- plot(e, labels=FALSE, quantities=TRUE, legend=TRUE, main=paste("residuals:", vpNames.tmp[names(vpNames.tmp) == "residuals"]))
  
  
  colnames(vpDF)[2] <- "Adj.R"
  
  return(list(varpart = vpDF, varpart.plot=vpPlot))
  
}







run.varpart2 <- function(data){
  
  df <- data
  
  # prepare environmental variables  
  env <- df %>% dplyr::select(c("pH","P","N","TOC"))
  cd <- df %>% dplyr::select(c("FarmBi"))
  
  # geographic distances
  geo <- df %>% dplyr::select("Long_point", "Lat_point")
  dbmem.tmp <- dbmem(dist(geo), silent = FALSE)
  dbmemDF <- as.data.frame(dbmem.tmp)
  
  # community matrix
  otu <- df %>% dplyr::select(contains("OTU"))
  dist.otu <- vegdist(otu, "bray")
  dist.otu <- vegdist(decostand(otu, "hellinger"), "bray")
  
  
    parts <- c("geo","soil","soil&geo","residuals")
    vp <- varpart(dist.otu,dbmemDF,env)
    
    # extract individual fractions
    vpDF <- vp$part$indfract %>%
      mutate(parts = parts, 
             Adj.R.squared = round(ifelse(Adj.R.squared<0,0, Adj.R.squared), 2)) %>%
      dplyr::select("parts", "Adj.R.squared")
    
    
    # create combination and weights
    vpNames.tmp <- setNames(round(vpDF$Adj.R.squared,3), as.character(vpDF$parts))
  
  
  vpNames<- vpNames.tmp[names(vpNames.tmp) != "residuals"]
  
  # create venn diagram
  e <- euler(combinations=vpNames)
  
  # plot
  vpPlot <- plot(e, labels=FALSE, quantities=TRUE, legend=TRUE, main=paste("residuals:", vpNames.tmp[names(vpNames.tmp) == "residuals"]))
  
  colnames(vpDF)[2] <- "Adj.R"
  
  return(list(varpart = vpDF, varpart.plot=vpPlot))
  
}


# 


# cool code
# parts <- c("geo","soil","cd")
# t(combn(c("geo","soil","cd"),2))
# 
# 
# partsNames <- expand.grid(parts,parts, parts) %>%
#   mutate(parts = paste(Var1, Var2, Var3, sep="&"))
# 
# partsNames.temp <-  unname(sapply(partsNames$parts, function(x) {
#   paste(sort(trimws(strsplit(x[1], '&')[[1]])), collapse='&')} ))
# 
# unique(sapply(strsplit(partsNames.temp, "&", fixed = TRUE), function(x) 
#   paste(unique(x), collapse = "&")))