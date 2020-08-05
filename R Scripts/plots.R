########################################################################
## plotting
########################################################################

########################################################################
## predictor coefficient plots
########################################################################

predictorColors <- c("#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7", "#E817DA","#6FC46C","#471E02")



########################################################################
## AMF - no scale 
########################################################################

noscale_amf_plot <- noscale_amf$plotList$`item:1` + ggtitle(paste("all")) + theme(legend.position="none")

#SAVE
ggsave("noscale_amf_plot.pdf", plot=noscale_amf_plot, path=fig.path, width = 5, height=4, useDingbats=FALSE)



noscale_mono_amf_plot <- noscale_mono_amf$plotList + ggtitle(paste("monoculture")) + theme(legend.position="none")

#SAVE
ggsave("noscale_mono_amf_plot.pdf", plot=noscale_mono_amf_plot, path=fig.path, width = 5, height=4, useDingbats=FALSE)



noscale_poly_amf_plot <- noscale_poly_amf$plotList$`item:1` + ggtitle(paste("polyculture")) + theme(legend.position="none")

#SAVE
ggsave("noscale_poly_amf_plot.pdf", plot=noscale_poly_amf_plot, path=fig.path, width = 5, height=4, useDingbats=FALSE)

legend <- get_legend( noscale_amf$plotList$`item:1`)


all_amf_noscale <- cowplot::plot_grid(plot_grid(noscale_amf_plot,noscale_mono_amf_plot,noscale_poly_amf_plot, row=1),legend,  rel_widths = c(2, .4), nrow=1 )

ggsave("all_amf_noscale.pdf", plot=all_amf_noscale, path=fig.path, width = 9, height=8, useDingbats=FALSE)


########################################################################
## AMF - ALL
########################################################################



landscape_amf_plot <- landscape_amf$plotList$`item:1` + theme(legend.position="none")


local_amf_plot <- local_amf$plotList$`item:1` + theme(legend.position="none")

# legend for all plots
legend1 <- get_legend(landscape_amf$plotList$`item:1`)
legend2 <- get_legend(local_amf$plotList$`item:1`)

# all farms plots
amf_plots <- cowplot::plot_grid(plot_grid(landscape_amf_plot, local_amf_plot, nrow=1), legend2, nrow=1,  rel_widths = c(2, .4) )


#SAVE
ggsave("amf_plots.pdf", plot=amf_plots, path=fig.path, width = 12, height=4, useDingbats=FALSE)




########################################################################
## AMF - Focal
########################################################################


landscape_AMF_f_plot <- landscape_AMF_f$plotList$`item:1` + theme(legend.position="none")


local_AMF_f_plot <- local_AMF_f$plotList$`item:1` + theme(legend.position="none")

# legend for all plots
legend <- get_legend(landscape_AMF_f$plotList$`item:1`)

# all farms plots
amf_f_plots <- cowplot::plot_grid(plot_grid(landscape_AMF_f_plot, local_AMF_f_plot, nrow=1), legend, rel_widths = c(2, .4) )


#SAVE
ggsave("amf_f_plots.pdf", plot=amf_f_plots, path=fig.path, width = 12, height=4, useDingbats=FALSE)


########################################################################
## AMF - Non-Focal
########################################################################


landscape_AMF_n_plot <- landscape_AMF_n$plotList$`item:1` + theme(legend.position="none")


local_AMF_n_plot <- local_AMF_n$plotList$`item:1` + theme(legend.position="none")

# legend for all plots
legend <- get_legend(landscape_AMF_n$plotList$`item:1`)

# all farms plots
amf_n_plots <- cowplot::plot_grid(plot_grid(landscape_AMF_n_plot, local_AMF_n_plot, nrow=1), legend, rel_widths = c(2, .4) )

#SAVE
ggsave("amf_n_plots.pdf", plot=amf_n_plots, path=fig.path, width = 12, height=4, useDingbats=FALSE)


########################################################################
## AMF - all
########################################################################

amf_all_plots <- cowplot::plot_grid(amf_f_plots, amf_n_plots, nrow=2, rel_widths = c(2, .4) )

#SAVE
ggsave("amf_all_plots.pdf", plot=amf_all_plots, path=fig.path, width = 12, height=8, useDingbats=FALSE)


########################################################################
## predicted vs observed compositional dissimilarity
########################################################################

landscape_AMF_f_comp <- landscape_AMF_f$compPlots$`item:1`

landscape_AMF_n_comp <- landscape_AMF_n$compPlots$`item:1`


local_AMF_f_comp <- local_AMF_f$compPlots$`item:1`

local_AMF_n_comp <- local_AMF_n$compPlots$`item:1`



########################################################################
## variance importance
########################################################################


noscale_varImp_plot <- ggplot(noscale_amf_var, aes(fill=predictor, y=variance, x=scaleLevel)) + 
  geom_bar(position="stack", stat="identity") +
  ylab("% variance explained") +
  xlab("spatial scale") + 
  scale_fill_manual(values = predictorColors) +
  scale_y_continuous(expand = c(0.01, 0.01), 
                     limits= c(0,50), breaks=round(seq(0, 50, length.out = 6),1)) +
  theme_classic()


#SAVE
ggsave("noscale_varImp_plot.pdf", plot=noscale_varImp_plot, path=fig.path, width = 5, height=4, useDingbats=FALSE)



varImp_plot <- ggplot(varianceImportance, aes(fill=predictor, y=variance, x=scaleLevel)) + 
  geom_bar(position="stack", stat="identity") +
  ylab("% variance explained") +
  xlab("spatial scale") + 
  scale_fill_manual(values = predictorColors) +
  scale_y_continuous(expand = c(0.01, 0.01), 
                     limits= c(0,75), breaks=round(seq(0, 75, length.out = 6),1)) +
  theme_classic()


#SAVE
ggsave("varImp_plot.pdf", plot=varImp_plot, path=fig.path, width = 8, height=6, useDingbats=FALSE)






varImp_ALL_plot <- ggplot(varianceImportance_all, aes(fill=predictor, y=variance, x=scaleLevel)) + 
  geom_bar(position="stack", stat="identity") +
  ylab("% variance explained") +
  xlab("spatial scale") + 
  scale_fill_manual(values = predictorColors) +
  scale_y_continuous(expand = c(0.01, 0.01), 
                     limits= c(0,75), breaks=round(seq(0, 75, length.out = 6),1)) +
  theme_classic()


#SAVE
ggsave("varImp_ALL_plot.pdf", plot=varImp_ALL_plot, path=fig.path, width = 6, height=6, useDingbats=FALSE)


########################################################################
## box plots
########################################################################




#NP ratio

np_all <- boxplot_variable(all_fungi, "NP_ratio", "All farms")

np_mono <- boxplot_variable(all_fungi %>% filter(FarmType == "Monoculture"), "NP_ratio", "Monocultures")

np_poly <- boxplot_variable(all_fungi %>% filter(FarmType == "Polyculture"), "NP_ratio", "Polycultures")

np_ratio <- cowplot::plot_grid(plot_grid(np_mono, np_poly, np_all, nrow=3),
                               rel_widths = c(3, .4))


ggsave("np_ratio.pdf", plot=np_ratio, path=fig.path, width = 4, height=4, useDingbats=FALSE)



#pH

pH_all <- boxplot_variable(all_fungi, "pH", "All farms")

pH_mono <- boxplot_variable(all_fungi %>% filter(FarmType == "Monoculture"), "pH", "Monocultures")

pH_poly <- boxplot_variable(all_fungi %>% filter(FarmType == "Monoculture"), "pH", "Polycultures")

pH <- cowplot::plot_grid(plot_grid(pH_mono, pH_poly, pH_all, nrow=3),
                         rel_widths = c(3, .4) )


ggsave("pH.pdf", plot=pH, path=fig.path, width = 4, height=4, useDingbats=FALSE)

#P

P_all <- boxplot_variable(all_fungi, "P", "All farms")

P_mono <- boxplot_variable(all_fungi %>% filter(FarmType == "Monoculture"), "P", "Monocultures")

P_poly <- boxplot_variable(all_fungi %>% filter(FarmType == "Monoculture"), "P", "Polycultures")

P <- cowplot::plot_grid(plot_grid(P_mono, P_poly, P_all, nrow=3),
                         rel_widths = c(3, .4) )


ggsave("P.pdf", plot=P, path=fig.path, width = 4, height=4, useDingbats=FALSE)

#TOC

TOC_all <- boxplot_variable(all_fungi, "TOC", "All farms")

TOC_mono <- boxplot_variable(all_fungi %>% filter(FarmType == "Monoculture"), "TOC", "Monocultures")

TOC_poly <- boxplot_variable(all_fungi %>% filter(FarmType == "Monoculture"), "TOC", "Polycultures")

TOC <- cowplot::plot_grid(plot_grid(TOC_mono, TOC_poly, TOC_all, nrow=3),
                        rel_widths = c(3, .4) )


ggsave("TOC.pdf", plot=TOC, path=fig.path, width = 4, height=4, useDingbats=FALSE)

#N

N_all <- boxplot_variable(all_fungi, "N", "All farms")

N_mono <- boxplot_variable(all_fungi %>% filter(FarmType == "Monoculture"), "N", "Monocultures")

N_poly <- boxplot_variable(all_fungi %>% filter(FarmType == "Monoculture"), "N", "Polycultures")

N <- cowplot::plot_grid(plot_grid(N_mono, N_poly, N_all, nrow=3),
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
# N_ftbl <- cowplot::plot_grid(plot_grid(N_f_mono, N_n_mono, N_f_poly, N_n_poly, nrow=4),
#                         rel_widths = c(3, .4) )

########################################################################
## alpha diversity plots for functional  groups
########################################################################

divIndices <- c("observed","chao1","shannon")

alphaPlots <- alpha_plot(colNames=divIndices,expVar = "FarmType", data = all_amf)
alphaBlockPlots <-  alpha_plot(colNames=divIndices,expVar = "Block", data = all_amf)
alphaFTBLPlots <-  alpha_plot(colNames=divIndices,expVar = "FTBL", data = all_amf)

# richPlots <- plot_grid(alphaPlots[[1]], alphaPlots[[2]], ncol=2)
# richBlockPlots <- plot_grid(alphaBlockPlots[[1]], alphaBlockPlots[[2]], alphaBlockPlots[[3]], alphaBlockPlots[[4]], alphaBlockPlots[[5]], ncol=3)
richFTBLPlots <- plot_grid(alphaFTBLPlots[[1]], ncol=1)

divPlots <- plot_grid(alphaFTBLPlots[[3]],ncol=1)



alpha_pH_Plots <- alpha_env(colNames=divIndices,expVar = "pH", color="FarmType",data = all_amf)
alpha_TOC_Plots <- alpha_env(colNames=divIndices,expVar = "TOC", color="FarmType",data = all_amf)
alpha_NP_Plots <- alpha_env(colNames=divIndices,expVar = "NP_ratio", color="FarmType",data = all_amf)
alpha_N_Plots <- alpha_env(colNames=divIndices,expVar = "N", color="FarmType",data = all_amf)
alpha_P_Plots <- alpha_env(colNames=divIndices,expVar = "P", color="FarmType",data = all_amf)


alpha_pH_Plots <- plot_grid(alpha_pH_Plots[[1]],alpha_pH_Plots[[3]], ncol=2)
alpha_TOC_Plots <- plot_grid(alpha_TOC_Plots[[1]],alpha_TOC_Plots[[3]], ncol=2)
alpha_NP_Plots <- plot_grid(alpha_NP_Plots[[1]],alpha_NP_Plots[[3]], ncol=2)
alpha_N_Plots <- plot_grid(alpha_N_Plots[[1]],alpha_N_Plots[[3]], ncol=2)
alpha_P_Plots <- plot_grid(alpha_P_Plots[[1]],alpha_P_Plots[[3]], ncol=2)

ggsave("alpha_pH_Plots.pdf", plot=alpha_pH_Plots, path=fig.path, width = 8, height=4, useDingbats=FALSE)
ggsave("alpha_TOC_Plots.pdf", plot=alpha_TOC_Plots, path=fig.path, width = 8, height=4, useDingbats=FALSE)
ggsave("alpha_NP_Plots.pdf", plot=alpha_NP_Plots, path=fig.path, width = 8, height=4, useDingbats=FALSE)
ggsave("alpha_N_Plots.pdf", plot=alpha_N_Plots, path=fig.path, width = 8, height=4, useDingbats=FALSE)
ggsave("alpha_P_Plots.pdf", plot=alpha_P_Plots, path=fig.path, width = 8, height=4, useDingbats=FALSE)




# ########################################################################
# ## plotting envi predictors together
# ########################################################################
# 
# landscape_fungal_model <- isplineExtract(landscape_fungal$gdmModels$`item:1`) %>% data.frame()
# 
# win_fungal_model <- isplineExtract(win_fungal$gdmModels$`item:1`) %>% data.frame()
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
# landscape_plant_model <- isplineExtract(landscape_plant$gdmModels$`item:1`) %>% data.frame()
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




