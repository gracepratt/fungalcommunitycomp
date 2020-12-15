## *****************************************************************************
## FUNCTIONS ###################################################################
## *****************************************************************************

## *****************************************************************************
## 1. species table inputs for gdm #############################################
## *****************************************************************************

input_tables <- function(complete_table, envi_variables){
  species_table <- complete_table %>% 
    dplyr::select("Key", "Long_point", "Lat_point", contains("OTU"))
  
  envi_table <- complete_table %>% 
    dplyr::select("Key", "Long_point", "Lat_point", envi_variables)
  
  formated_tables <- formatsitepair(species_table, bioFormat=1, XColumn="Long_point", YColumn="Lat_point",
                                    siteColumn="Key", predData= envi_table, abundance = FALSE)
  
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
  
  formated_tables <- formatsitepair(species_matrix, bioFormat=3, XColumn="Long_point", YColumn="Lat_point",
                                     siteColumn="Key", predData= envi_table, abundance = TRUE)
  
  return(formated_tables)
}


## *****************************************************************************
## 3. subsetting fungal guilds #################################################
## *****************************************************************************

guild_filter <- function(complete_table, guild, family){
  
  complete_table <- all_fungi
  
  #pull out OTU columns and transpose to make 1 row per Key and OTU
  OTURows<- complete_table %>% 
    dplyr::select(Key, contains("OTU")) %>%
    pivot_longer(cols = starts_with("OTU"), names_to = "OTU")
  

  
  rawguilds <- tax %>%
    dplyr::select("OTU" = "OTU", "Guild", "Family")
  
  #join guild names with OTU in each key 
  wGuild <- OTURows %>%
    left_join(rawguilds) %>%
    drop_na()
  
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



mantel_func <- function(complete_table, envi_variables, type , transform_method = "hellinger", mantel_method = "spearman", mantelType){
  
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

  if(mantelType == "vegan"){
    #mantel test
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
  
  
  #make a table 
  Factor <- c("Species v Crop Diversity","Species v Environment", "Species v Geography", "Environment v Geography",  "Environment v Crop diversity",
              "Species v pH", "Species v P", "Species v N", "Species v NP", "Species v TOC")
  Statistic <- c(mantel_cropDiv$statistic, mantel_envi$statistic, mantel_geo$statistic, mantel_envivgeo$statistic, mantel_envivCD$statistic,
                 mantel_pH$statistic, mantel_P$statistic, mantel_N$statistic, mantel_NP$statistic, mantel_TOC$statistic)
  Significance <- c(mantel_cropDiv$signif, mantel_envi$signif, mantel_geo$signif, mantel_envivgeo$signif, mantel_envivCD$signif,
                    mantel_pH$signif, mantel_P$signif, mantel_N$signif, mantel_NP$signif, mantel_TOC$signif)
  
  table <- data.frame(Factor, Statistic, Significance)}
  
  
  else{
  # NOTE ABOUT ECODIST: The mantel() function returns the Mantel r statistic, and three p values from a randomization procedure described below. For most circumstances, pval1, assessing the significance of the null hypothesis that r <= 0, is the appropriate choice. For this artificial example, pval1 provides the desired hypothesis test.The test results are non-significant, leading to the conclusion that closer samples are not more similar. The null hypotheses for the other two tests, that r >= 0 (pval2) and that r = 0 (pval3) are only relevant in special cases, most commonly when model matrices or transformations of the dissimilarities are used
    # https://www.jstatsoft.org/article/view/v022i07/v22i07.pdf
    
    
  mantel_envi <- ecodist::mantel(dist.sp ~ dist.envi)
  mantel_geo <- ecodist::mantel(dist.sp ~ dist.geo)
  mantel_envivgeo <- ecodist::mantel(dist.envi ~ dist.geo)
  mantel_cropDiv <- ecodist::mantel(dist.sp ~ dist.cropDiv)
  mantel_pH <- ecodist::mantel(dist.sp ~ dist.pH)
  mantel_P <- ecodist::mantel(dist.sp ~ dist.P)
  mantel_N <- ecodist::mantel(dist.envi~ dist.N)
  mantel_NP <- ecodist::mantel(dist.sp~ dist.NP)
  mantel_TOC <- ecodist::mantel(dist.sp~ dist.TOC)
  
  
  #make a table 
  Factor <- c("Species v Crop Diversity","Species v Environment", "Species v Geography", "Environment v Geography",
              "Species v pH", "Species v P", "Species v N", "Species v NP", "Species v TOC")
  Statistic <- c(as.data.frame(mantel_cropDiv)[1,1], as.data.frame(mantel_envi)[1,1], as.data.frame(mantel_geo)[1,1], as.data.frame(mantel_envivgeo)[1,1], as.data.frame(mantel_pH)[1,1],as.data.frame(mantel_P)[1,1], as.data.frame(mantel_N)[1,1], as.data.frame(mantel_NP)[1,1], as.data.frame(mantel_TOC)[1,1])
  
  Pval1 <- c(as.data.frame(mantel_cropDiv)[2,1], as.data.frame(mantel_envi)[2,1], as.data.frame(mantel_geo)[2,1], as.data.frame(mantel_envivgeo)[2,1], as.data.frame(mantel_pH)[2,1],as.data.frame(mantel_P)[2,1], as.data.frame(mantel_N)[2,1], as.data.frame(mantel_NP)[2,1], as.data.frame(mantel_TOC)[2,1])
  
  
  Pval2 <- c(as.data.frame(mantel_cropDiv)[3,1], as.data.frame(mantel_envi)[3,1], as.data.frame(mantel_geo)[3,1], as.data.frame(mantel_envivgeo)[3,1], as.data.frame(mantel_pH)[3,1],as.data.frame(mantel_P)[3,1], as.data.frame(mantel_N)[3,1], as.data.frame(mantel_NP)[3,1], as.data.frame(mantel_TOC)[3,1])
  
  
  Pval3 <- c(as.data.frame(mantel_cropDiv)[4,1], as.data.frame(mantel_envi)[4,1], as.data.frame(mantel_geo)[4,1], as.data.frame(mantel_envivgeo)[4,1], as.data.frame(mantel_pH)[4,1],as.data.frame(mantel_P)[4,1], as.data.frame(mantel_N)[4,1], as.data.frame(mantel_NP)[4,1], as.data.frame(mantel_TOC)[4,1])
  
  
  table <- data.frame(Factor, Statistic, Pval1,Pval2, Pval3)
  }
  
  
  
  
  
  densityPlot = plot(density(dist.sp))
  
  list(table = table, data.frame = dist.sp, plot = densityPlot )
}



## *****************************************************************************
## 6. plot variable isplines ###################################################
## *****************************************************************************

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
  palette <- data.frame(Predictors = c("FarmBi","Geographic","N","NP_ratio","P", "pH", "TOC", "CropBi","cropDiversity","cropRichness"), 
                        colors = c("#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7", "#E817DA","#6FC46C","#471E02"))
  
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
    theme_classic() 
  
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
    theme_classic() 
  
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
      theme_classic() + theme(legend.position="none")
  }
  return(plotList)
}

alpha_env <- plotFunction <- function(colNames, expVar, color, data){
  # library(scales)
  
  plotList <- list()
  for (i in colNames) {
    # create dataset
    df <- data[c(expVar,i, color)]
    
    
    # rename dataframe
    colnames(df) <- c("expVar","y", "color")
    
    plotList[[i]] <- ggplot(	df, aes(x = expVar, y = y))  + 
      geom_point(aes(color=color, fill=color, group=color)) +
      geom_smooth(aes(color=color, fill=color, group=color), method = "glm", fill = NA) +
      geom_smooth(method = "glm", fill = NA, color="black") +
      scale_y_continuous(expand = c(0.01, 0.01), breaks=round(seq(min(df$y), max(df$y), length.out = 6),1)) +
      # scale_x_continuous(expand = c(0.01, 0.01), breaks=round(seq(min(df$expVar), max(df$expVar), length.out = 6),1)) +
      scale_fill_manual(values=c('#f2e6cb',  '#99cec6','#f2e6cb','#99cec6'))+
      scale_color_manual(values=c('#dfc27d',  '#018571','#dfc27d','#018571'))+
      ylab(paste(i)) +	
      xlab(paste(expVar)) +	
      theme_classic() + theme(legend.position="none")
  }
  return(plotList)
}



## *****************************************************************************
## 9. box plot of environmental variable range #################################
## *****************************************************************************

boxplot_variable <- function(complete_table, variable, title){
  
  ggplot(complete_table, aes_string(x= variable)) + 
    geom_boxplot() + ggtitle(title) + theme_classic() + 
    theme(axis.ticks.y = element_blank(), 
          axis.text.y = element_blank(),
          axis.line.y = element_blank())
  
}


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
  
  return(list(df= list(df), spTable = spTableList, tables=tableList,gdmModels= gdmList, plotList = plotList, predictors=predictorsDFList, compPlots = compPlotList))
}

## *****************************************************************************
## 12. taxonColor ##############################################################
## *****************************************************************************



taxonColor <- function(taxons){
  
  Taxons <- c("Acaulospora", "Acaulosporaceae", "Archaeospora", "Cetraspora", 
              "Claroideoglomus", "Diversispora","Diversisporaceae","Diversisporales",
              "Funneliformis","Glomeraceae","Paraglomeraceae","Rhizophagus")
  
  Colors <- c(   "#009E73","#6f6c70" , "#8569D5", "#FFA500", 
               "#E7298A", "#66A61E", "#A6761D", "#CBD588",
               "#A84100", "#1C3A85", "#5BAABD", "#652926"
  )
  
  
  
  colorTable <- data.frame(Taxons = Taxons, Colors = Colors)
  
  colorTable %>% filter(Taxons %in% levels(factor(taxons))) %>%
    pull(Colors)
}

## *****************************************************************************
## 13. TITAN Output ############################################################
## *****************************************************************************

titanOutput <- function(titan) {
  
  
  
  # taxa <- data %>% dplyr::select(contains("OTU"))
  # 
  # taxa.pa <- decostand(taxa, "pa")
  # 
  # plus3 <- data.frame(sums=colSums(taxa.pa)) %>%
  #   rownames_to_column("OTU") %>%
  #   filter(sums > 3) %>% pull(OTU)
  # 
  # 
  # taxa <- taxa[, colSums(taxa != 0) > 0]
  # taxa <- taxa[c(plus3)]
  # 
  # 
  # 
  # # select variables
  # variableDF <- data %>% dplyr::select(variable)
  # 
  # # run titan
  # titan <- titan(variableDF, taxa, ncpus = 8)
  
  
  
  
  df <- titan$sppmax  %>%
    as.data.frame() %>% 
    rownames_to_column("OTU") %>%
    left_join(tax, by = "OTU") %>%
    filter(filter >0) %>%
    mutate(lowSE = zenv.cp-`5%`, highSE = abs( zenv.cp-`95%`), filter = factor(filter), organization = ifelse(filter == 1, zenv.cp*1, zenv.cp*-1), zscore = ifelse(filter == 1, "z+", "z-")) %>%
    group_by(filter) %>%
    mutate(rank = order(order(organization, decreasing=TRUE)), 
           rank2 = ifelse(filter != 1, rank+0.5, rank)) %>%
    ungroup() %>%
    arrange( rank2) %>%
    mutate(OTU = factor(OTU, levels=OTU))
  
  plot <- ggplot(df , aes(y= zenv.cp, x= OTU, color=Taxon )) +
    geom_errorbar(aes(ymin=zenv.cp-lowSE, ymax=zenv.cp+highSE), width=0, size=0.75, position=position_dodge(0.05)) +
    geom_point(aes(shape=factor(filter), size=zscore), size=2, fill = "white", stroke=0.75) +
    scale_shape_manual(values = c(16,21) ) + 
    # scale_x_discrete(breaks = seq(1,nrow(df), by=3), labels= seq(1,nrow(df), by=3)) +
    xlab("OTUs") +
    scale_color_manual(values = taxonColor(taxons=df$Taxon)) + 
    coord_flip() + theme_classic() + # +  #+ facet_wrap(~filter)
    theme(axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x = element_blank())
    
  list(df=df, plot=plot)
  
  
}


