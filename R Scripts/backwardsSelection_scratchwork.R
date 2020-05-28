# Backwards selection


backwardsSelection <- function(df,guild, block, focalcrop, farmtype, env_factors, geo, maxDist){
  # to supress warnings
  oldw <- getOption("warn")
  options(warn = -1)
  
  # filter dataset
  
  df <- guild_filter(df, guild) %>%
    filter(Block %in% block, FocalCrop %in% focalcrop, FarmType %in% farmtype)
  
  # create table list
  tableList <- list()
  
  # create gdm list
  gdmList <- list()
  
  # create plot list
  plotList <- list()
  
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
    
    if(maxDist == "within") { # for within type models
    finalDF <- inputs%>% 
      filter(distanceM < 60) %>%
      dplyr::select(-"distanceM")}
    
    else{                   # for across type models
      finalDF <- inputs%>% 
        # filter(distanceM < 60) %>%
        dplyr::select(-"distanceM")
    }
    
    # run GDM model
    model <- gdm(finalDF, geo=geo)
    gdmList[[modelNum]] <- model

    # create plotList
    plotList[[modelNum]] <- predictors_plot(model) + ggtitle(paste(maxDist)) + theme( plot.title = element_text(hjust = 0.5)) 
    # + theme(legend.position="none")
    
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
  
  return(list(tables=tableList,gdmModels= gdmList, plotList = plotList))
}





