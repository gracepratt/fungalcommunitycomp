## *****************************************************************************
## plotting ####################################################################
## *****************************************************************************

## *****************************************************************************
## predictor coefficient plots #################################################
## *****************************************************************************

predictorColors <- c("#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7", "#E817DA","#6FC46C","#471E02")



## *****************************************************************************
## No scale ####################################################################
## *****************************************************************************

# Monoculture + Polyculture
noscale_plot <- noscale$plotList + ggtitle(paste("all")) #+ theme(legend.position="none")

#SAVE
ggsave("noscale_plot.pdf", plot=noscale_plot, path=fig.path, width = 5, height=4, useDingbats=FALSE, dpi=300)

# Monoculture
noscale_mono_plot <- noscale_mono$plotList + ggtitle(paste("monoculture")) + theme(legend.position="none")

#SAVE
ggsave("noscale_mono_plot.pdf", plot=noscale_mono_plot, path=fig.path, width = 5, height=4, useDingbats=FALSE)


# Polyculture
noscale_poly_plot <- noscale_poly$plotList + ggtitle(paste("polyculture")) + theme(legend.position="none")

#SAVE
ggsave("noscale_poly_plot.pdf", plot=noscale_poly_plot, path=fig.path, width = 5, height=4, useDingbats=FALSE)

# Legend
legend <- get_legend( noscale$plotList)

# JOIN
all_noscale <- plot_grid(plot_grid(noscale_plot,noscale_mono_plot,noscale_poly_plot, row=1),legend,  rel_widths = c(2, .4), nrow=1 )

ggsave("all_noscale.pdf", plot=all_noscale, path=fig.path, width = 9, height=8, useDingbats=FALSE)


## *****************************************************************************
## all block ###################################################################
## *****************************************************************************


# Monoculture + Polyculture
landscape_plot <- landscape$plotList + theme(legend.position="none")
local_plot <- local$plotList + theme(legend.position="none")

# legend
legend <- get_legend(landscape$plotList)

# JOIN
all_blocks <- plot_grid(plot_grid(landscape_plot, local_plot, nrow=1), legend, nrow=1,  rel_widths = c(2, .4) )

#SAVE
ggsave("all_blocks.pdf", plot=all_blocks, path=fig.path, width = 12, height=4, useDingbats=FALSE)



# Monoculture
landscape_mono_plot <- landscape_mono$plotList + theme(legend.position="none")
local_mono_plot <- local_mono$plotList + theme(legend.position="none")

# legend
legend <- get_legend(landscape_mono$plotList)

# JOIN
all_blocks_mono <- plot_grid(plot_grid(landscape_mono_plot, local_mono_plot, nrow=1), legend, nrow=1,  rel_widths = c(2, .4) )

#SAVE
ggsave("all_blocks_mono.pdf", plot=all_blocks_mono, path=fig.path, width = 12, height=4, useDingbats=FALSE)


# Polyculture
landscape_poly_plot <- landscape_poly$plotList + theme(legend.position="none")
local_poly_plot <- local_poly$plotList + theme(legend.position="none")

# legend
legend <- get_legend(landscape_poly$plotList)

# JOIN
all_blocks_poly <- plot_grid(plot_grid(landscape_poly_plot, local_poly_plot, nrow=1), legend, nrow=1,  rel_widths = c(2, .4) )

#SAVE
ggsave("all_blocks_poly.pdf", plot=all_blocks_poly, path=fig.path, width = 12, height=4, useDingbats=FALSE)



## *****************************************************************************
## Focal block #################################################################
## *****************************************************************************

predictors_variable_plot(local_mono_f$predictors, "Geographic")

# Monoculture + Polyculture
landscape_f_plot <- landscape_f$plotList + theme(legend.position="none")
local_f_plot <- local_f$plotList + theme(legend.position="none")

# legend for all plots
legend <- get_legend(landscape_f$plotList)

# all farms plots
focal_plots <- plot_grid(plot_grid(landscape_f_plot, local_f_plot, nrow=1), legend, rel_widths = c(2, .4) )

#SAVE
ggsave("focal_plots.pdf", plot=focal_plots, path=fig.path, width = 12, height=4, useDingbats=FALSE)

# Monoculture
landscape_mono_f_plot <- landscape_mono_f$plotList + theme(legend.position="none")
local_mono_f_plot <- local_mono_f$plotList + theme(legend.position="none")

# legend for all plots
legend <- get_legend(landscape_mono_f$plotList)

# all farms plots
focal_mono_plots <- plot_grid(plot_grid(landscape_mono_f_plot, local_mono_f_plot, nrow=1), legend, rel_widths = c(2, .4) )

#SAVE
ggsave("focal_mono_plots.pdf", plot=focal_mono_plots, path=fig.path, width = 12, height=4, useDingbats=FALSE)


# Polyculture
landscape_poly_f_plot <- landscape_poly_f$plotList + theme(legend.position="none")
local_poly_f_plot <- local_poly_f$plotList + theme(legend.position="none")

# legend for all plots
legend <- get_legend(landscape_poly_f$plotList)

# all farms plots
focal_poly_plots <- plot_grid(plot_grid(landscape_poly_f_plot, local_poly_f_plot, nrow=1), legend, rel_widths = c(2, .4) )

#SAVE
ggsave("focal_poly_plots.pdf", plot=focal_poly_plots, path=fig.path, width = 12, height=4, useDingbats=FALSE)


all_focal_plots <- plot_grid(focal_plots, focal_mono_plots, focal_poly_plots, labels=c("All","Monoculture","Polyculture"), nrow=3, rel_widths = c(2, .4) )


## *****************************************************************************
## Non-focal block #############################################################
## *****************************************************************************

# Monoculture + Polyculture
landscape_n_plot <- landscape_f$plotList + theme(legend.position="none")
local_n_plot <- local_f$plotList + theme(legend.position="none")

# legend for all plots
legend <- get_legend(landscape_f$plotList)

# all farms plots
nonfocal_plots <- plot_grid(plot_grid(landscape_n_plot, local_n_plot, nrow=1), legend, rel_widths = c(2, .4) )

#SAVE
ggsave("nonfocal_plots.pdf", plot=nonfocal_plots, path=fig.path, width = 12, height=4, useDingbats=FALSE)

# Monoculture
landscape_mono_n_plot <- landscape_mono_f$plotList + theme(legend.position="none")
local_mono_n_plot <- local_mono_f$plotList + theme(legend.position="none")

# legend for all plots
legend <- get_legend(landscape_mono_f$plotList)

# all farms plots
nonfocal_mono_plots <- plot_grid(plot_grid(landscape_mono_n_plot, local_mono_n_plot, nrow=1), legend, rel_widths = c(2, .4) )

#SAVE
ggsave("nonfocal_mono_plots.pdf", plot=nonfocal_mono_plots, path=fig.path, width = 12, height=4, useDingbats=FALSE)


# Polyculture
landscape_poly_n_plot <- landscape_poly_f$plotList + theme(legend.position="none")
local_poly_n_plot <- local_poly_f$plotList + theme(legend.position="none")

# legend for all plots
legend <- get_legend(landscape_poly_f$plotList)

# all farms plots
nonfocal_poly_plots <- plot_grid(plot_grid(landscape_poly_n_plot, local_poly_n_plot, nrow=1), legend, rel_widths = c(2, .4) )

#SAVE
ggsave("nonfocal_poly_plots.pdf", plot=nonfocal_poly_plots, path=fig.path, width = 12, height=4, useDingbats=FALSE)



## *****************************************************************************
## Main plots ##################################################################
## *****************************************************************************

mainPlot <- plot_grid(focal_plots, focal_poly_plots, focal_mono_plots, nrow=3, labels=c("ALL","POLY","MONO"), rel_widths = c(2, .4) )

#SAVE
ggsave("mainPlot.pdf", plot=mainPlot, path=fig.path, width = 12, height=12, useDingbats=FALSE)


## *****************************************************************************
## predicted vs observed compositional dissimilarity ###########################
## *****************************************************************************

landscape_f_comp <- landscape_f$compPlots

landscape_n_comp <- landscape_n$compPlots


local_f_comp <- local_f$compPlots

local_n_comp <- local_n$compPlots



## *****************************************************************************
## variance importance #########################################################
## *****************************************************************************



## *****************************************************************************
## no scale ####################################################################
## *****************************************************************************

varImp_noscale_plot <- ggplot(varImp_noscale, aes(fill=predictor, y=variance, x=scaleLevel)) + 
  geom_bar(position="stack", stat="identity") +
  ylab("% variance explained") +
  xlab("spatial scale") + 
  scale_fill_manual(values = predictorColors) +
  scale_y_continuous(expand = c(0.01, 0.01), 
                     limits= c(0,100), breaks=round(seq(0, 100, length.out = 6),1)) +
  theme_classic()

#SAVE
ggsave("varImp_noscale_plot.pdf", plot=varImp_noscale_plot, path=fig.path, width = 8, height=6, useDingbats=FALSE)



## *****************************************************************************
## All blocks ##################################################################
## *****************************************************************************


varImp_allBlocks_plot <- ggplot(varImp_allBlocks, aes(fill=predictor, y=variance, x=scaleLevel)) + 
  geom_bar(position="stack", stat="identity") +
  ylab("% variance explained") +
  xlab("spatial scale") + 
  scale_fill_manual(values = predictorColors) +
  scale_y_continuous(expand = c(0.01, 0.01), 
                     limits= c(0,100), breaks=round(seq(0, 100, length.out = 6),1)) +
  theme_classic()


#SAVE
ggsave("varImp_allBlocks_plot.pdf", plot=varImp_allBlocks_plot, path=fig.path, width = 8, height=6, useDingbats=FALSE)




## *****************************************************************************
## Focal #######################################################################
## *****************************************************************************



varImp_Focal_plot <- ggplot(varImp_Focal, aes(fill=predictor, y=variance, x=scaleLevel)) + 
  geom_bar(position="stack", stat="identity") +
  ylab("% variance explained") +
  xlab("spatial scale") + 
  scale_fill_manual(values = predictorColors) +
  scale_y_continuous(expand = c(0.01, 0.01), 
                     limits= c(0,100), breaks=round(seq(0, 100, length.out = 6),1)) +
  theme_classic()


#SAVE
ggsave("varImp_Focal_plot.pdf", plot=varImp_Focal_plot, path=fig.path, width = 8, height=6, useDingbats=FALSE)


varImp_Focal_plot2 <- ggplot(varImp_Focal %>% filter(scaleLevel %in% c("local_f","landscape_f")), aes(fill=predictor, y=variance, x=scaleLevel)) + 
  geom_bar(position="stack", stat="identity") +
  ylab("% variance explained") +
  xlab("spatial scale") + 
  scale_fill_manual(values = predictorColors) +
  scale_y_continuous(expand = c(0.01, 0.01), 
                     limits= c(0,100), breaks=round(seq(0, 100, length.out = 6),1)) +
  theme_classic()


#SAVE
ggsave("varImp_Focal_plot2.pdf", plot=varImp_Focal_plot2, path=fig.path, width = 4, height=6, useDingbats=FALSE)


## *****************************************************************************
## Non-focal ###################################################################
## *****************************************************************************


varImp_nonfocal_plot <- ggplot(varImp_nonfocal, aes(fill=predictor, y=variance, x=scaleLevel)) + 
  geom_bar(position="stack", stat="identity") +
  ylab("% variance explained") +
  xlab("spatial scale") + 
  scale_fill_manual(values = predictorColors) +
  scale_y_continuous(expand = c(0.01, 0.01), 
                     limits= c(0,100), breaks=round(seq(0, 100, length.out = 6),1)) +
  theme_classic()

#SAVE
ggsave("varImp_nonfocal_plot.pdf", plot=varImp_nonfocal_plot, path=fig.path, width = 8, height=6, useDingbats=FALSE)


## *****************************************************************************
## Predictor box plots #########################################################
## *****************************************************************************




#NP ratio

np_all <- boxplot_variable(all_fungi %>% filter(Block == "F"), "NP_ratio", "All farms")

np_mono <- boxplot_variable(all_fungi %>% filter(FarmType == "Monoculture", Block == "F"), "NP_ratio", "Monocultures")

np_poly <- boxplot_variable(all_fungi %>% filter(FarmType == "Polyculture", Block == "F"), "NP_ratio", "Polycultures")

np_ratio <- plot_grid(plot_grid(np_mono, np_poly, np_all, nrow=3),
                               rel_widths = c(3, .4))


ggsave("np_ratio.pdf", plot=np_ratio, path=fig.path, width = 4, height=4, useDingbats=FALSE)



#pH

pH_all <- boxplot_variable(all_fungi %>% filter(Block == "F"), "pH", "All farms")

pH_mono <- boxplot_variable(all_fungi %>% filter(FarmType == "Monoculture", Block == "F"), "pH", "Monocultures")

pH_poly <- boxplot_variable(all_fungi %>% filter(FarmType == "Monoculture", Block == "F"), "pH", "Polycultures")

pH <- plot_grid(plot_grid(pH_mono, pH_poly, pH_all, nrow=3),
                         rel_widths = c(3, .4) )


ggsave("pH.pdf", plot=pH, path=fig.path, width = 4, height=4, useDingbats=FALSE)

#P

P_all <- boxplot_variable(all_fungi %>% filter(Block == "F"), "P", "All farms")

P_mono <- boxplot_variable(all_fungi %>% filter(FarmType == "Monoculture", Block == "F"), "P", "Monocultures")

P_poly <- boxplot_variable(all_fungi %>% filter(FarmType == "Monoculture", Block == "F"), "P", "Polycultures")

P <- plot_grid(plot_grid(P_mono, P_poly, P_all, nrow=3),
                         rel_widths = c(3, .4) )


ggsave("P.pdf", plot=P, path=fig.path, width = 4, height=4, useDingbats=FALSE)

#TOC

TOC_all <- boxplot_variable(all_fungi %>% filter(Block == "F"), "TOC", "All farms")

TOC_mono <- boxplot_variable(all_fungi %>% filter(FarmType == "Monoculture", Block == "F"), "TOC", "Monocultures")

TOC_poly <- boxplot_variable(all_fungi %>% filter(FarmType == "Monoculture", Block == "F"), "TOC", "Polycultures")

TOC <- plot_grid(plot_grid(TOC_mono, TOC_poly, TOC_all, nrow=3),
                        rel_widths = c(3, .4) )


ggsave("TOC.pdf", plot=TOC, path=fig.path, width = 4, height=4, useDingbats=FALSE)

#N

N_all <- boxplot_variable(all_fungi %>% filter(Block == "F"), "N", "All farms")

N_mono <- boxplot_variable(all_fungi %>% filter(FarmType == "Monoculture", Block == "F"), "N", "Monocultures")

N_poly <- boxplot_variable(all_fungi %>% filter(FarmType == "Monoculture", Block == "F"), "N", "Polycultures")

N <- plot_grid(plot_grid(N_mono, N_poly, N_all, nrow=3),
                        rel_widths = c(3, .4) )

ggsave("N.pdf", plot=N, path=fig.path, width = 4, height=4, useDingbats=FALSE)

# #N focal
# 
# #N_f_all <- boxplot_variable(all_fungi, "N", "All farms")
# 
# N_f_mono <- boxplot_variable(mono_f, "N", "Monocultures F")
# 
# N_f_poly <- boxplot_variable(poly_f, "N", "Polycultures F")
# 
# N_n_mono <- boxplot_variable(mono_n, "N", "Monocultures N")
# 
# N_n_poly <- boxplot_variable(poly_n, "N", "Polycultures N")
# 
# N_ftbl <- plot_grid(plot_grid(N_f_mono, N_n_mono, N_f_poly, N_n_poly, nrow=4),
#                         rel_widths = c(3, .4) )

## *****************************************************************************
## alpha diversity plots for functional  groups ################################
## *****************************************************************************
# 
# divIndices <- c("observed","chao1","shannon")
# 
# alphaPlots <- alpha_plot(colNames=divIndices,expVar = "FarmType", data = all)
# alphaBlockPlots <-  alpha_plot(colNames=divIndices,expVar = "Block", data = all)
# alphaFTBLPlots <-  alpha_plot(colNames=divIndices,expVar = "FTBL", data = all)
# 
# # richPlots <- plot_grid(alphaPlots[[1]], alphaPlots[[2]], ncol=2)
# # richBlockPlots <- plot_grid(alphaBlockPlots[[1]], alphaBlockPlots[[2]], alphaBlockPlots[[3]], alphaBlockPlots[[4]], alphaBlockPlots[[5]], ncol=3)
# richFTBLPlots <- plot_grid(alphaFTBLPlots[[1]], ncol=1)
# 
# divPlots <- plot_grid(alphaFTBLPlots[[3]],ncol=1)
# 
# 
# 
# alpha_pH_Plots <- alpha_env(colNames=divIndices,expVar = "pH", color="FarmType",data = all)
# alpha_TOC_Plots <- alpha_env(colNames=divIndices,expVar = "TOC", color="FarmType",data = all)
# alpha_NP_Plots <- alpha_env(colNames=divIndices,expVar = "NP_ratio", color="FarmType",data = all)
# alpha_N_Plots <- alpha_env(colNames=divIndices,expVar = "N", color="FarmType",data = all)
# alpha_P_Plots <- alpha_env(colNames=divIndices,expVar = "P", color="FarmType",data = all)
# 
# 
# alpha_pH_Plots <- plot_grid(alpha_pH_Plots[[1]],alpha_pH_Plots[[3]], ncol=2)
# alpha_TOC_Plots <- plot_grid(alpha_TOC_Plots[[1]],alpha_TOC_Plots[[3]], ncol=2)
# alpha_NP_Plots <- plot_grid(alpha_NP_Plots[[1]],alpha_NP_Plots[[3]], ncol=2)
# alpha_N_Plots <- plot_grid(alpha_N_Plots[[1]],alpha_N_Plots[[3]], ncol=2)
# alpha_P_Plots <- plot_grid(alpha_P_Plots[[1]],alpha_P_Plots[[3]], ncol=2)
# 
# ggsave("alpha_pH_Plots.pdf", plot=alpha_pH_Plots, path=fig.path, width = 8, height=4, useDingbats=FALSE)
# ggsave("alpha_TOC_Plots.pdf", plot=alpha_TOC_Plots, path=fig.path, width = 8, height=4, useDingbats=FALSE)
# ggsave("alpha_NP_Plots.pdf", plot=alpha_NP_Plots, path=fig.path, width = 8, height=4, useDingbats=FALSE)
# ggsave("alpha_N_Plots.pdf", plot=alpha_N_Plots, path=fig.path, width = 8, height=4, useDingbats=FALSE)
# ggsave("alpha_P_Plots.pdf", plot=alpha_P_Plots, path=fig.path, width = 8, height=4, useDingbats=FALSE)




# ## *****************************************************************************
# ## plotting envi predictors together ###########################################
# ## *****************************************************************************
# 
# landscape_fungal_model <- isplineExtract(landscape_fungal$gdmModels) %>% data.frame()
# 
# win_fungal_model <- isplineExtract(win_fungal$gdmModels) %>% data.frame()
# 
# landscape_TOC_x <- landscape_fungal_model$x.TOC
# 
# landscape_TOC_y <- landscape_fungal_model$y.TOC
# 
# landscape_TOC <- data.frame(landscape_TOC_x, landscape_TOC_y)
# 
# win_TOC_x <- win_fungal_model$x.TOC
# 
# win_TOC_y <- win_fungal_model$y.TOC
# 
# win_TOC <- data.frame(win_TOC_x, win_TOC_y)
# 
# ggplot() + 
#   geom_line(data= landscape_TOC, aes(x= landscape_TOC_x, y= landscape_TOC_y), color='green') + 
#   geom_line(data= win_TOC, aes(x= win_TOC_x, y= win_TOC_y), color='red')
# 
# landscape_plant_model <- isplineExtract(landscape_plant$gdmModels) %>% data.frame()
# landscape_plant_TOC <- data.frame(landscape_plant_model$x.TOC, landscape_plant_model$y.TOC)
# 
# 
# ggplot() + 
#   geom_line(data= landscape_TOC, aes(x= landscape_TOC_x, y= landscape_TOC_y), color='green') + 
#   geom_line(data= landscape_plant_TOC, aes(x= landscape_plant_model.x.TOC, y= landscape_plant_model.y.TOC), color='red')
# 
# 
# 
# 
# 
# 




