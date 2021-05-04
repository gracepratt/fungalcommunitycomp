## *****************************************************************************
## plots #######################################################################
## *****************************************************************************

predictorColors <-  c("#E69F00", "#009E73", "#8EB042","#0072B2", "#CC79A7", "#8E6345")
predictorColors_ft <-  c("#A6230C", "#009E73", "#8EB042","#0072B2", "#CC79A7", "#8E6345")

ftColors = c('#b58648', '#017763')
ftFill = c('#fccd47','#22b573')

## *****************************************************************************
## 1.   GAMMA ####################################################################
## *****************************************************************************


saFun <- function(data, var){
  
  saDF <- list()
  
  for( i in levels(factor(data$FarmType))){
    
    df <- data %>% filter(FarmType == i)
    otu <- df %>% dplyr::select(contains("OTU"))
    sa <- specaccum(otu, "random")
    saDF[[i]] <- data.frame( var = df[var], sample=seq(1:length(sa$richness)), richness=sa$richness, sd=sa$sd)
    
  }
  
  return(saDF)
}

sa_ft <- do.call(rbind,saFun(data=all_wa$df, var = "FarmType"))
sa_ftbl <- do.call(rbind,saFun(data=all_wa$df, var = "FTBL"))




sa_ft_Plot <- ggplot(data=sa_ft, aes(x=sample,y=richness, color=FarmType)) + 
  geom_line() + 
  # geom_ribbon(aes(ymin=richness-sd,ymax=richness+sd), alpha=0.3)+
  scale_color_manual(values=ftColors) +
  scale_x_continuous(expand = c(0.01, 0.01),limits=c(1, 110),breaks=seq(1, 110,length.out=6))  + themeBorder + legend_bottomRight()

ggsave("sa_ft_Plot.pdf", plot=sa_ft_Plot, path=fig.path, width = 3.5, height=3, useDingbats=FALSE, dpi=300)

sa_ftbl_Plot <- ggplot(data=sa_ftbl, aes(x=sample,y=richness, color=FTBL, linetype=FTBL)) + 
  geom_line() + 
  # geom_ribbon(aes(ymin=richness-sd,ymax=richness+sd), alpha=0.3)+
  scale_color_manual(values=c('#b58648','#b58648', '#017763',  '#017763')) +
  scale_x_continuous(expand = c(0.01, 0.01),limits=c(1, 110),breaks=seq(1, 110,length.out=6))  + themeBorder

## *****************************************************************************
## 2. GDM ispline plots  #######################################################
## *****************************************************************************

final_ispline <- ggplot(predictorsDF_cd %>% mutate(Factor = factor(Factor, levels = c("cropDiversity","Geographic","N","P","pH","TOC"))) %>% filter(! location == "whole farm", !dataset == "all farms"), aes(x = X, y = Y, color = Factor)) +
  geom_line(size=1) +
  xlab("Standardized Variables") +
  ylab("Partial Ecological Distance") + 
  scale_color_manual(values=predictorColors) + 
  scale_y_continuous(expand = c(0.01, 0.01), breaks=round(seq(min(predictorsDF_cd$Y), max(predictorsDF_cd$Y), length.out = 6),1)) +
  # ylim(0, 2.5) +
  themeBorder + facet_grid(dataset ~ location) + theme(legend.position="bottom")


final_ispline_wa <- ggplot(predictorsDF_cd %>% mutate(Factor = factor(Factor, levels = c("cropDiversity","Geographic","N","P","pH","TOC"))) %>% filter(location == "whole farm", dataset == "all farms"), aes(x = X, y = Y, color = Factor)) +
  geom_line(size=1) +
  xlab("Standardized Variables") +
  ylab("Partial Ecological Distance") + 
  scale_color_manual(values=predictorColors) + 
  scale_y_continuous(expand = c(0.01, 0.01), breaks=round(seq(min(predictorsDF_cd$Y), max(predictorsDF_cd$Y), length.out = 6),1)) +
  # ylim(0, 2.5) +
  themeBorder + facet_grid(dataset ~ location) + theme(legend.position="none")

#SAVE
ggsave("final_ispline_wa.pdf", plot=final_ispline_wa, path=fig.path, width = 3.5, height=3, useDingbats=FALSE, dpi=300)
ggsave("final_ispline.pdf", plot=final_ispline, path=fig.path, width = 5, height=5, useDingbats=FALSE, dpi=300)
ggsave("Figure2.pdf", plot=final_ispline, path=fig.path, width = 6.5, height=6.5, useDingbats=FALSE, dpi=300)





final_ispline_ft <- ggplot(predictorsDF_ft, aes(x = X, y = Y, color = Factor)) +
  geom_line(size=1) +
  xlab("Standardized Variables") +
  ylab("Partial Ecological Distance") + 
  scale_color_manual(values=predictorColors_ft) + 
  scale_y_continuous(expand = c(0.01, 0.01), breaks=round(seq(min(predictorsDF_ft$Y), max(predictorsDF_ft$Y), length.out = 6),1)) +
  # ylim(0, 2.5) +
  themeBorder + facet_grid(dataset ~ location) + theme(legend.position="bottom")

#SAVE
ggsave("final_ispline_ft.pdf", plot=final_ispline_ft, path=fig.path, width = 6.5, height=6.5, useDingbats=FALSE, dpi=300)
ggsave("Figure2_v2.pdf", plot=final_ispline_ft, path=fig.path, width = 6.5, height=6.5, useDingbats=FALSE, dpi=300)




## *****************************************************************************
## predicted vs observed compositional dissimilarity ###########################
## *****************************************************************************

# landscape_f_comp <- landscape_f$compPlots
# 
# landscape_n_comp <- landscape_n$compPlots
# 
# 
# local_f_comp <- local_f$compPlots
# 
# local_n_comp <- local_n$compPlots
# 


## *****************************************************************************
## 3. variance importance by GDM ###############################################
## *****************************************************************************

final_varImp_plot <- ggplot(varImp_cd %>% mutate(location = factor(location, levels = c("whole farm","within row","across row"))), aes(fill=Predictors, y=variance, x=location)) + 
  geom_bar(position="stack", stat="identity") +
  ylab("% variance explained") +
  xlab("spatial scale") + 
  scale_fill_manual(values = predictorColors) +
  scale_y_continuous(expand = c(0.01, 0.01), 
                     limits= c(0,60), breaks=round(seq(0, 60, length.out = 6),1)) +
  themeBorder + facet_grid(~dataset)

#SAVE
ggsave("final_varImp_plot.pdf", plot=final_varImp_plot, path=fig.path, width = 7, height=3, useDingbats=FALSE)
ggsave("Figure3.pdf", plot=final_varImp_plot, path=fig.path, width = 7, height=3, useDingbats=FALSE)


varImp_GDM_plot_cd <- ggplot(varImp_cd, aes(y=variance/100, x=Predictors, alpha=location, group=location, fill=Predictors)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_alpha_manual(values = c(1, 0.7, 0.4)) + 
  scale_fill_manual(values=predictorColors)+
  ylab("variance importance") + xlab("Predictors")+
  ggtitle("variance importance from GDM")+
  facet_wrap(~dataset) + themeBorder

ggsave("varImp_GDM_plot_cd.pdf", plot=varImp_GDM_plot_cd, path=fig.path, width = 8, height=3, useDingbats=FALSE)
ggsave("Figure3_v1.pdf", plot=varImp_GDM_plot_cd, path=fig.path, width = 8, height=3, useDingbats=FALSE)


varImp_GDM_plot_ft <- ggplot(varImp_ft, aes(y=variance/100, x=Predictors, alpha=location, group=location, fill=Predictors)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_alpha_manual(values = c(1, 0.7, 0.4)) + 
  scale_fill_manual(values=predictorColors_ft)+
  ylab("variance importance") + xlab("Predictors")+
  ggtitle("variance importance from GDM")+
  facet_wrap(~dataset) + themeBorder

ggsave("varImp_GDM_plot_ft.pdf", plot=varImp_GDM_plot_ft, path=fig.path, width = 8, height=3, useDingbats=FALSE)
ggsave("Figure3_v2.pdf", plot=varImp_GDM_plot_ft, path=fig.path, width = 8, height=3, useDingbats=FALSE)


## *****************************************************************************
## 3. variance importance by MRM ###############################################
## *****************************************************************************

varImp_MRM_plot_cd <- ggplot(relImpTable, aes(y=lmg, x=variable, alpha=location, group=location, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_alpha_manual(values = c(1, 0.7, 0.4)) +
  scale_fill_manual(values = predictorColors) +
  ylab("variance importance") + xlab("predictor")+
  ggtitle("variance importance from MRM")+
  facet_wrap(~dataset) + themeBorder

ggsave("varImp_MRM_plot_cd.pdf", plot=varImp_MRM_plot_cd, path=fig.path, width = 8, height=3, useDingbats=FALSE)
ggsave("Figure3b.pdf", plot=varImp_MRM_plot_cd, path=fig.path, width = 8, height=3, useDingbats=FALSE)



varImp_MRM_plot_ft <- ggplot(relImpTable_ft, aes(y=lmg, x=variable, alpha=location, group=location, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_alpha_manual(values = c(1, 0.7, 0.4)) +
  scale_fill_manual(values = predictorColors_ft) +
  ylab("variance importance") + xlab("predictor")+
  ggtitle("variance importance from MRM")+
  facet_wrap(~dataset) + themeBorder


ggsave("varImp_MRM_plot_ft.pdf", plot=varImp_MRM_plot_ft, path=fig.path, width = 8, height=3, useDingbats=FALSE)
ggsave("Figure3b_v2.pdf", plot=varImp_MRM_plot_ft, path=fig.path, width = 8, height=3, useDingbats=FALSE)




## *****************************************************************************
## 4. variation partitioning ###########################################
## *****************************************************************************

all_varPart_plot <- plot_grid(all_wa_varpart$varpart.plot, all_w_varpart$varpart.plot, all_a_varpart$varpart.plot, nrow=1)

ggsave("all_varPart_plot.pdf", plot=all_varPart_plot, path=fig.path, width = 8, height=3, useDingbats=FALSE)

mono_varPart_plot <- plot_grid(mono_wa_varpart$varpart.plot, mono_w_varpart$varpart.plot, mono_a_varpart$varpart.plot, 
          nrow=1)

ggsave("mono_varPart_plot.pdf", plot=mono_varPart_plot, path=fig.path, width = 8, height=3, useDingbats=FALSE)

poly_varPart_plot <- plot_grid(poly_wa_varpart$varpart.plot, poly_w_varpart$varpart.plot, poly_a_varpart$varpart.plot, nrow=1)

ggsave("poly_varPart_plot.pdf", plot=poly_varPart_plot, path=fig.path, width = 8, height=3, useDingbats=FALSE)



varPart_plot <- ggplot(varpart_table %>% filter(!parts == "residuals"), 
       aes(x=location, y=Adj.R*100, fill=parts, alpha=parts)) +
  geom_bar(stat="identity") + 
  scale_fill_manual(values = rev(c("#D81B60","#1E88E5","#FFC107","#7B52A3","#a7776f","#EC6E34"))) + 
  ylab("% variance explained") +
  themeBorder+
  facet_wrap(~dataset) 

ggsave("varPart_plot.pdf", plot=varPart_plot, path=fig.path, width = 8, height=2.75, useDingbats=FALSE)


## *****************************************************************************
## 4. Alpha ~ crop diversity plots ###########################################
## *****************************************************************************

## *****************************************************************************
## 4a. individual plots -- Effect sizes ########################################
## *****************************************************************************

## RICHNESS
# whole farm
all_wa_effectSizes_richness_plot <- effectSizes(all_wa_richness_cd)$plot + xlab("Effect sizes on observed AMF richness")
#SAVE
ggsave("all_wa_effectSizes_richness_plot.pdf", plot=all_wa_effectSizes_richness_plot, path=fig.path, width = 3, height=3, useDingbats=FALSE)

# whole farm
all_w_effectSizes_richness_plot <- effectSizes(all_w_richness_cd)$plot 
#SAVE
ggsave("all_w_effectSizes_richness_plot.pdf", plot=all_w_effectSizes_richness_plot, path=fig.path, width = 3, height=3, useDingbats=FALSE)

# whole farm
all_a_effectSizes_richness_plot <- effectSizes(all_a_richness_cd)$plot 
#SAVE
ggsave("all_a_effectSizes_richness_plot.pdf", plot=all_a_effectSizes_richness_plot, path=fig.path, width = 3, height=3, useDingbats=FALSE)




# Shannon's diversity
# whole farm
all_wa_effectSizes_shannon_plot <- effectSizes(all_wa_shannon_cd)$plot 
#SAVE
ggsave("all_wa_effectSizes_shannon_plot.pdf", plot=all_wa_effectSizes_shannon_plot, path=fig.path, width = 3, height=3, useDingbats=FALSE)

# whole farm
all_w_effectSizes_shannon_plot <- effectSizes(all_w_shannon_cd)$plot 
#SAVE
ggsave("all_w_effectSizes_shannon_plot.pdf", plot=all_w_effectSizes_shannon_plot, path=fig.path, width = 3, height=3, useDingbats=FALSE)

# whole farm
all_a_effectSizes_shannon_plot <- effectSizes(all_a_shannon_cd)$plot 
#SAVE
ggsave("all_a_effectSizes_shannon_plot.pdf", plot=all_a_effectSizes_shannon_plot, path=fig.path, width = 3, height=3, useDingbats=FALSE)

## *****************************************************************************
## 4b. Final Effect sizes ########################################
## *****************************************************************************

# make dataframe of all models
effectSizes_richness <- rbind(effectSizes(all_wa_richness_cd)$df %>% mutate(location = "whole farm"),
                              effectSizes(all_w_richness_cd)$df %>% mutate(location = "within rows"),
                              effectSizes(all_a_richness_cd)$df %>% mutate(location = "across rows")) %>%
  mutate(location = factor(location, levels = c("whole farm", "within rows", "across rows")))


# plot the dataset
effectSizes_richness_plot <- ggplot(effectSizes_richness, aes(x = covariate, y = Estimate, 
                          color = covariate, group=covariate, linetype=interaction)) +
  geom_hline(yintercept=0, linetype = "dashed") +
  geom_point(size = 2, position=position_dodge(width = 0.8)) +
  geom_errorbar( aes(ymin= Estimate-`Std. Error`, ymax= Estimate + `Std. Error`),
                 width = 0.0, position=position_dodge(width = 0.8), show.legend=FALSE) +
  scale_color_manual(values =c( "#8E6345","#CC79A7","#8EB042", "#0072B2",
                                "#8E6345","#CC79A7","#8EB042", "#0072B2", "#E69F00")) +
  scale_y_continuous(breaks= c(-0.50,  0.0, 0.50), limits = c(-0.67, 0.67)) +
  ylab("Effect sizes on observed AMF richness") +
  themeBorder + theme(legend.position = "none", axis.title.y=element_blank()) +
  coord_flip() + facet_grid(~location)  

#SAVE
ggsave("effectSizes_richness_plot.pdf", plot=effectSizes_richness_plot, path=fig.path, width = 6, height=2.75, useDingbats=FALSE)
ggsave("Figure4.pdf", plot=effectSizes_richness_plot, path=fig.path, width = 6, height=2.75, useDingbats=FALSE)


# make dataframe of all models
effectSizes_shannon <- rbind(effectSizes(all_wa_shannon_cd)$df %>% mutate(location = "whole farm"),
                              effectSizes(all_w_shannon_cd)$df %>% mutate(location = "within rows"),
                              effectSizes(all_a_shannon_cd)$df %>% mutate(location = "across rows")) %>%
  mutate(location = factor(location, levels = c("whole farm", "within rows", "across rows")))


# plot the dataset
effectSizes_shannon_plot <- ggplot(effectSizes_shannon, aes(x = covariate, y = Estimate, 
                                                              color = covariate, group=covariate, linetype=interaction)) +
  geom_hline(yintercept=0, linetype = "dashed") +
  geom_point(size = 2, position=position_dodge(width = 0.8)) +
  geom_errorbar( aes(ymin= Estimate-`Std. Error`, ymax= Estimate + `Std. Error`),
                 width = 0.0, position=position_dodge(width = 0.8), show.legend=FALSE) +
  scale_color_manual(values =c( "#8E6345","#CC79A7","#8EB042", "#0072B2",
                                "#8E6345","#CC79A7","#8EB042", "#0072B2", "#E69F00")) +
  scale_y_continuous(breaks= c(-0.50,  0.0, 0.50), limits = c(-0.67, 0.67)) +
  ylab("Effect sizes on AMF Shannon's diversity") +
  themeBorder + theme(legend.position = "none", axis.title.y=element_blank()) +
  coord_flip() + facet_grid(~location)

#SAVE
ggsave("effectSizes_shannon_plot.pdf", plot=effectSizes_shannon_plot, path=fig.path, width = 6, height=2.75, useDingbats=FALSE)
ggsave("Figure4b.pdf", plot=effectSizes_shannon_plot, path=fig.path, width = 6, height=2.75, useDingbats=FALSE)


## *****************************************************************************
## 5. Alpha ~ farm type plots ###########################################
## *****************************************************************************

# make dataframe of all models
effectSizes_richness_ft <- rbind(effectSizes(all_wa_richness_ft)$df %>% mutate(location = "whole farm"),
                              effectSizes(all_w_richness_ft)$df %>% mutate(location = "within rows"),
                              effectSizes(all_a_richness_ft)$df %>% mutate(location = "across rows")) %>%
  mutate(location = factor(location, levels = c("whole farm", "within rows", "across rows")))


# plot the dataset
effectSizes_richness_plot_ft <- ggplot(effectSizes_richness_ft, aes(x = covariate, y = Estimate, 
                                                              color = covariate, group=covariate, linetype=interaction)) +
  geom_hline(yintercept=0, linetype = "dashed") +
  geom_point(size = 2, position=position_dodge(width = 0.8)) +
  geom_errorbar( aes(ymin= Estimate-`Std. Error`, ymax= Estimate + `Std. Error`),
                 width = 0.0, position=position_dodge(width = 0.8), show.legend=FALSE) +
  scale_color_manual(values =c( "#8E6345","#CC79A7","#8EB042", "#0072B2",
                                "#8E6345","#CC79A7","#8EB042", "#0072B2", "#E69F00")) +
  scale_y_continuous(breaks= c(-1,  0.0, 1), limits = c(-1.2, 1.2)) +
  ylab("Effect sizes on observed AMF richness") +
  themeBorder + theme(legend.position = "none", axis.title.y=element_blank()) +
  coord_flip() + facet_grid(~location)  

#SAVE
ggsave("effectSizes_richness_plot_ft.pdf", plot=effectSizes_richness_plot_ft, path=fig.path, width = 6, height=2.75, useDingbats=FALSE)


# make dataframe of all models
effectSizes_shannon_ft <- rbind(effectSizes(all_wa_shannon_ft)$df %>% mutate(location = "whole farm"),
                             effectSizes(all_w_shannon_ft)$df %>% mutate(location = "within rows"),
                             effectSizes(all_a_shannon_ft)$df %>% mutate(location = "across rows")) %>%
  mutate(location = factor(location, levels = c("whole farm", "within rows", "across rows")))


# plot the dataset
effectSizes_shannon_plot_ft <- ggplot(effectSizes_shannon_ft, aes(x = covariate, y = Estimate, 
                                                            color = covariate, group=covariate, linetype=interaction)) +
  geom_hline(yintercept=0, linetype = "dashed") +
  geom_point(size = 2, position=position_dodge(width = 0.8)) +
  geom_errorbar( aes(ymin= Estimate-`Std. Error`, ymax= Estimate + `Std. Error`),
                 width = 0.0, position=position_dodge(width = 0.8), show.legend=FALSE) +
  scale_color_manual(values =c( "#8E6345","#CC79A7","#8EB042", "#0072B2",
                                "#8E6345","#CC79A7","#8EB042", "#0072B2", "#E69F00")) +
  scale_y_continuous(breaks= c(-0.50,  0.0, 0.50), limits = c(-0.67, 0.67)) +
  ylab("Effect sizes on AMF Shannon's diversity") +
  themeBorder + theme(legend.position = "none", axis.title.y=element_blank()) +
  coord_flip() + facet_grid(~location)

#SAVE
ggsave("effectSizes_shannon_plot_ft.pdf", plot=effectSizes_shannon_plot_ft, path=fig.path, width = 6, height=2.75, useDingbats=FALSE)

## *****************************************************************************
## 5a. individual shannon  ########################################
## *****************************************************************************

richness_ft_plotList <- alpha_predictor(all_a_richness_ft, c("N", "P","pH","TOC"), "FarmType")
richness_N_ft_plot <- richness_ft_plotList$N + ylab("observed") 
richness_P_ft_plot <- richness_ft_plotList$P + ylab("observed")
richness_pH_ft_plot <- richness_ft_plotList$pH + ylab("observed")
richness_TOC_ft_plot <- richness_ft_plotList$TOC + ylab("observed")

ggsave("richness_N_ft_plot.pdf", plot= richness_N_ft_plot, path=fig.path, width = 3.5, height=2, useDingbats=FALSE)
ggsave("richness_P_ft_plot.pdf", plot= richness_P_ft_plot, path=fig.path, width = 3.5, height=2, useDingbats=FALSE)
ggsave("richness_pH_ft_plot.pdf", plot= richness_pH_ft_plot, path=fig.path, width = 3.5, height=2, useDingbats=FALSE)
ggsave("richness_TOC_ft_plot.pdf", plot= richness_TOC_ft_plot, path=fig.path, width = 3.5, height=2, useDingbats=FALSE)

## *****************************************************************************
## 5a. final richness ########################################
## *****************************************************************************


richness_ft_plots <- plot_grid(richness_N_ft_plot + theme(legend.position = "none"),
                               richness_P_ft_plot + legend_topRight() + axis.y.blank,
                               richness_pH_ft_plot + theme(legend.position = "none"),
                               richness_TOC_ft_plot + theme(legend.position = "none") + axis.y.blank,
                               nrow=2,
                               align="hv",
                               labels = "AUTO")

ggsave("richness_ft_a__plots.pdf", plot= richness_ft_plots, path=fig.path, width = 5.25, height=5, useDingbats=FALSE)
ggsave("Figure5.pdf", plot= richness_ft_plots, path=fig.path, width = 5.25, height=5, useDingbats=FALSE)



## *****************************************************************************
## 5c. individual plots -- shannon ########################################
## *****************************************************************************

shannon_ft_plotList <- alpha_predictor(all_wa_shannon_ft, c("N", "P","pH","TOC"), "FarmType")
shannon_N_ft_plot <- shannon_ft_plotList$N + ylab("shannon")
shannon_P_ft_plot <- shannon_ft_plotList$P + ylab("shannon")
shannon_pH_ft_plot <- shannon_ft_plotList$pH + ylab("shannon")
shannon_TOC_ft_plot <- shannon_ft_plotList$TOC + ylab("shannon")

ggsave("shannon_N_ft_plot.pdf", plot= shannon_N_ft_plot, path=fig.path, width = 3.5, height=2, useDingbats=FALSE)
ggsave("shannon_P_ft_plot.pdf", plot= shannon_P_ft_plot, path=fig.path, width = 3.5, height=2, useDingbats=FALSE)
ggsave("shannon_pH_ft_plot.pdf", plot= shannon_pH_ft_plot, path=fig.path, width = 3.5, height=2, useDingbats=FALSE)
ggsave("shannon_TOC_ft_plot.pdf", plot= shannon_TOC_ft_plot, path=fig.path, width = 3.5, height=2, useDingbats=FALSE)

## *****************************************************************************
## 5d. final - shannon ########################################
## *****************************************************************************


shannon_ft_plots <- plot_grid(shannon_N_ft_plot + theme(legend.position = "none"),
                               shannon_P_ft_plot + legend_topRight() + theme(legend.key = element_rect(colour = NA, fill = NA)) + axis.y.blank,
                               shannon_pH_ft_plot + theme(legend.position = "none"),
                               shannon_TOC_ft_plot + theme(legend.position = "none") + axis.y.blank,
                               nrow=2,
                               align="hv",
                               labels = "AUTO")

ggsave("shannon_ft_plots.pdf", plot= shannon_ft_plots, path=fig.path, width = 5.25, height=5, useDingbats=FALSE)
ggsave("Figure5b.pdf", plot= shannon_ft_plots, path=fig.path, width = 5.25, height=5, useDingbats=FALSE)

## *****************************************************************************
## 6a. composition plots ########################################################
## *****************************************************************************

composition <- pcoaFun(all_wa$df, color = "FarmType", shape = "Block", formula = "FarmType*Block", strata = "farmCode", plot.means = FALSE)

composition_plot <- composition$plot

ggsave("composition_plot.pdf", plot= composition_plot + legend_topLeft(), path=fig.path, width = 3.5, height=3.25, useDingbats=FALSE)
ggsave("Figure6.pdf", plot= composition_plot + legend_topLeft(), path=fig.path, width = 3.5, height=3.25, useDingbats=FALSE)

## *****************************************************************************
## 6b. dbRDA plots #############################################################
## *****************************************************************************

# dataframe for dbRDA
site_scores <- scores(finalmodel, choices = c(1,2), display = "sites")
dbRDA_df <- as.data.frame(cbind(site_scores, Key = all_wa$df$Key)) %>%
  left_join(all_wa$df, by = "Key")
dbRDA_df_env <- as.data.frame(finalmodel$CCA$biplot) %>%
  rownames_to_column("Predictors") %>%
  filter(! Predictors %in% c("N","TOC","dbmemDF$MEM1","dbmemDF$MEM2","dbmemDF$MEM5","dbmemDF$MEM6")) %>%
  mutate(Predictors = c("FarmType","TransectType","P","pH","MEM3","MEM4","FarmType:TransectType"))


dbRDA.plot <- ggplot(dbRDA_df, aes(x = CAP1, y = CAP2)) + 
  stat_ellipse(geom = "polygon", alpha=0.3, aes(fill=FarmType))+
  geom_point(aes(x = CAP1, y = CAP2, col = FarmType, shape = Block), size=3, alpha=0.8) +
  scale_colour_manual(values=ftColors) +
  scale_fill_manual(values=ftFill) +
  scale_shape_manual(values = c(16, 1))+
  geom_text(data = dbRDA_df_env, aes(x = (CAP1*4)-0.08, y = (CAP2*4)-0.08, label = Predictors), col = 'black') +
  geom_segment(data = dbRDA_df_env, 
               aes(x = 0, y = 0, xend = CAP1*4, yend = CAP2*4), arrow=arrow(length=unit(0.2,"cm")),alpha = 0.75, color = 'black')+
  # scale_y_continuous(expand = c(0.01, 0.01), limits=c(-0.5, 0.2), breaks=round(seq(-0.2, 0.2,length.out=7),2)) +
  # scale_x_continuous(expand = c(0.01, 0.01), limits=c(-0.15, 0.15), breaks=round(seq(-0.15, 0.15,length.out=7),2)) +
  themeBorder +
  legend_bottomLeft()


ggsave("dbRDA.plot.pdf", plot= dbRDA.plot + legend_topLeft(), path=fig.path, width = 5, height=5, useDingbats=FALSE)

## *****************************************************************************
## 6. titan plots ##############################################################
## *****************************************************************************
# 
# titan_cd_plot <- titanPlot(titan_cd)$plot + facet_wrap(~"cropDiversity")
# titan_N_plot <- titanPlot(titan_N)$plot + facet_wrap(~"N")
# titan_P_plot <- titanPlot(titan_P)$plot + facet_wrap(~"P")
# titan_pH_plot <- titanPlot(titan_pH)$plot + facet_wrap(~"pH")
# titan_TOC_plot <- titanPlot(titan_TOC)$plot + facet_wrap(~"TOC")
# 
# titan_legend <- get_legend(titan_cd_plot + theme(legend.position = "bottom") + guides(colour = guide_legend(nrow = 4))
# )
# 
# titan_plot <- plot_grid(titan_cd_plot + legend_blank(), titan_N_plot + legend_blank() + axis.y.blank, titan_P_plot + legend_blank() + axis.y.blank, titan_pH_plot + legend_blank() + axis.y.blank, titan_TOC_plot + legend_blank() + axis.y.blank, nrow = 1, align="hv")
# 
# # relAbun_cd_plot <- relAbunPlot(all_wa$df, "cropDiversity")
# # relAbun_N_plot <- relAbunPlot(all_wa$df, "N")
# # relAbun_P_plot <- relAbunPlot(all_wa$df, "P")
# # relAbun_pH_plot <- relAbunPlot(all_wa$df, "pH")
# # relAbun_TOC_plot <- relAbunPlot(all_wa$df, "TOC")
# # 
# # 
# # relAbun_plot <- plot_grid(relAbun_cd_plot + legend_blank(), relAbun_N_plot + legend_blank() + axis.y.blank, relAbun_P_plot + legend_blank() + axis.y.blank, relAbun_pH_plot + legend_blank() + axis.y.blank, relAbun_TOC_plot + legend_blank() + axis.y.blank, nrow = 1, align="hv")
# 
# titan_plot_final <- plot_grid(titan_plot, titan_legend, nrow=2, rel_heights = c(2,1))
# 
# ggsave("titan_plot.pdf", plot= titan_plot_final + legend_topLeft(), path=fig.path, width = 9.25, height=3.25, useDingbats=FALSE)
# ggsave("Figure7.pdf", plot= titan_plot_final + legend_topLeft(), path=fig.pathh, width = 9.25, height=4, useDingbats=FALSE)

## *****************************************************************************
## 7. Environmental predictor box plots ########################################
## *****************************************************************************

N_boxplot <- env_boxplot(all, "N")
P_boxplot <- env_boxplot(all, "P")
pH_boxplot <- env_boxplot(all, "pH")
TOC_boxplot <- env_boxplot(all, "TOC")

env_plot_ft <- egg::ggarrange(N_boxplot + legend_blank(),
                         P_boxplot + legend_blank()  + axis.y.blank, 
                         pH_boxplot + legend_blank()  + axis.y.blank, 
                         TOC_boxplot + legend_blank()  + axis.y.blank, nrow=1)

ggsave("env_plot_ft.pdf", plot= env_plot_ft, path=fig.path, width = 7, height=1.75, useDingbats=FALSE)
# ggsave("Figure7.pdf", plot= titan_plot_final + legend_topLeft(), path=fig.path, width = 9.25, height=4, useDingbats=FALSE

N_lineplot <- env_lineplot(all, "N")
P_lineplot <- env_lineplot(all, "P")
pH_lineplot <- env_lineplot(all, "pH")
TOC_lineplot <- env_lineplot(all, "TOC")

env_plot_cd <- egg::ggarrange(N_lineplot + legend_bottomLeft() ,
                              P_lineplot +  legend_blank(), 
                              pH_lineplot +  legend_blank(), 
                              TOC_lineplot +  legend_blank(), nrow=1)

ggsave("env_plot_cd.pdf", plot= env_plot_cd, path=fig.path, width = 7, height=1.75, useDingbats=FALSE)
# ggsave("Figure7.pdf", plot= titan_plot_final + legend_topLeft(), path=fig.path, width = 9.25, height=4, useDingbats=FALSE
