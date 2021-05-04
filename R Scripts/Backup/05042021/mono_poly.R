

## both

poly_both <- backwardsSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA",block= c("F","N") ,focalcrop= c("Eggplant"), farmtype=c("Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "both")


mono_both <- backwardsSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA",block= c("F","N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "both")



## landcape

poly_landscape <- backwardsSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA",block= c("F","N") ,focalcrop= c("Eggplant"), farmtype=c("Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "landscape")


mono_landscape <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA",block= c("F","N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "landscape")

## local

poly_local <- backwardsSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA",block= c("F","N") ,focalcrop= c("Eggplant"), farmtype=c("Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "local")


mono_local <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA",block= c("F","N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "local")





########################################################################
## AMF -  landscape - polyculture
########################################################################



## all farms  -  Focal - landscape

landscape_POLY_f <- backwardsSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA",block= c("F") ,focalcrop= c("Eggplant"), farmtype=c("Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "landscape")

## all farms  -  Non-focal - landscape

landscape_POLY_n <- backwardsSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA",block= c("N") ,focalcrop= c("Eggplant"), farmtype=c("Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "landscape")

########################################################################
## AMF -  local
########################################################################

## all farms  -  Focal - local

local_POLY_f <- backwardsSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA",block= c("F") ,focalcrop= c("Eggplant"), farmtype=c("Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "local")

## all farms  -  Non-focal - local

local_POLY_n <- backwardsSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA",block= c("N") ,focalcrop= c("Eggplant"), farmtype=c("Polyculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "local")




########################################################################
## AMF -  landscape - monoculture
########################################################################



## all farms  -  Focal - landscape

landscape_MONO_f <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA",block= c("F") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "landscape")

## all farms  -  Non-focal - landscape

landscape_MONO_n <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA",block= c("N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "landscape")

df <- guild_filter(all_fungi, guild= "Arbuscular Mycorrhizal") %>%
  filter(Block %in% c("N","F"), FocalCrop %in% c("Eggplant"), FarmType %in% c("Monoculture"), Year %in% c("2018","2017"))

inputs <- input_diss(df, envi_factors)
inputs$distanceM <-distHaversine(inputs[,3:4],inputs[,5:6])

finalDF <- inputs%>% 
  filter(distanceM > 60) %>%
  dplyr::select(-"distanceM")

model <- gdm(finalDF, geo=TRUE)

plot <- predictors_plot(model) + ggtitle(paste("landscape")) + theme( plot.title = element_text(hjust = 0.5)) 

landscape_MONO_n <- list(   gdmModels= model, tables= table(model), plotList=plot)
########################################################################
## AMF -  local
########################################################################

## all farms  -  Focal - local

local_MONO_f <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA",block= c("F") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "local")

## all farms  -  Non-focal - local

local_MONO_n <- simpleSelection(df=all_fungi, guild= "Arbuscular Mycorrhizal", family = "NA",block= c("N") ,focalcrop= c("Eggplant"), farmtype=c("Monoculture"), year = c("2018","2017"), env_factors=envi_factors, geo=TRUE, maxDist = "local")





















########################################################################
## AMF - Focal
########################################################################


landscape_MONO_f_plot <- landscape_MONO_f$plotList$`item:1` + theme(legend.position="none")


local_MONO_f_plot <- local_MONO_f$plotList$`item:1` + theme(legend.position="none")

# legend for all plots
legend <- get_legend(landscape_MONO_f$plotList$`item:1`)

# all farms plots
mono_f_plots <- cowplot::plot_grid(plot_grid(landscape_MONO_f_plot, local_MONO_f_plot, nrow=1), legend, rel_widths = c(2, .4) )


#SAVE
ggsave("mono_f_plots.pdf", plot=mono_f_plots, path=fig.path, width = 12, height=4, useDingbats=FALSE)


########################################################################
## AMF - Non-Focal
########################################################################


landscape_MONO_n_plot <- landscape_MONO_n$plotList + theme(legend.position="none")


local_MONO_n_plot <- local_MONO_n$plotList$`item:1`+ theme(legend.position="none")

# legend for all plots
legend <- get_legend(landscape_MONO_n$plotList)

# all farms plots
mono_n_plots <- cowplot::plot_grid(plot_grid(landscape_MONO_n_plot, local_MONO_n_plot, nrow=1), legend, rel_widths = c(2, .4) )

#SAVE
ggsave("mono_n_plots.pdf", plot=mono_n_plots, path=fig.path, width = 12, height=4, useDingbats=FALSE)


########################################################################
## AMF - all
########################################################################

mono_all_plots <- cowplot::plot_grid(mono_f_plots, mono_n_plots, nrow=2, rel_widths = c(2, .4) )

#SAVE
ggsave("mono_all_plots.pdf", plot=mono_all_plots, path=fig.path, width = 12, height=8, useDingbats=FALSE)







########################################################################
## AMF - Focal
########################################################################


landscape_POLY_f_plot <- landscape_POLY_f$plotList$`item:1` + theme(legend.position="none")


local_POLY_f_plot <- local_POLY_f$plotList$`item:1` + theme(legend.position="none")

# legend for all plots
legend <- get_legend(landscape_POLY_f$plotList$`item:1`)

# all farms plots
poly_f_plots <- cowplot::plot_grid(plot_grid(landscape_POLY_f_plot, local_POLY_f_plot, nrow=1), legend, rel_widths = c(2, .4) )


#SAVE
ggsave("poly_f_plots.pdf", plot=poly_f_plots, path=fig.path, width = 12, height=4, useDingbats=FALSE)


########################################################################
## AMF - Non-Focal
########################################################################


landscape_POLY_n_plot <- landscape_POLY_n$plotList$`item:1` + theme(legend.position="none")


local_POLY_n_plot <- local_POLY_n$plotList$`item:1` + theme(legend.position="none")

# legend for all plots
legend <- get_legend(landscape_POLY_n$plotList$`item:1`)

# all farms plots
poly_n_plots <- cowplot::plot_grid(plot_grid(landscape_POLY_n_plot, local_POLY_n_plot, nrow=1), legend, rel_widths = c(2, .4) )

#SAVE
ggsave("poly_n_plots.pdf", plot=poly_n_plots, path=fig.path, width = 12, height=4, useDingbats=FALSE)


########################################################################
## AMF - all
########################################################################

poly_all_plots <- cowplot::plot_grid(poly_f_plots, poly_n_plots, nrow=2, rel_widths = c(2, .4) )

#SAVE
ggsave("poly_all_plots.pdf", plot=poly_all_plots, path=fig.path, width = 12, height=8, useDingbats=FALSE)




