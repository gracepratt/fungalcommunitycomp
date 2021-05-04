
########################################################################
## alpha summary
########################################################################

divIndices <- c("observed","shannon")

summaryFT <-  all[, names(all) %in% c("FarmType", divIndices, envi_factors)] %>%
  gather(key = "variable", value = "value", -c(FarmType)) %>%
  group_by(FarmType, variable) %>%
  summarize_at("value", list(mean = mean, SE=std.error, min = min, max = max), na.rm=TRUE) %>%
  ungroup() %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(Block = "-") %>%
  arrange(variable) %>%
  dplyr::select(FarmType, Block, variable, mean, SE, min, max)


summaryBlock <-  all[, names(all) %in% c("Block", divIndices, envi_factors)] %>%
  gather(key = "variable", value = "value", -c(Block)) %>%
  group_by(Block, variable) %>%
  summarize_at("value", list(mean = mean, SE=std.error, min = min, max = max), na.rm=TRUE) %>%
  ungroup() %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(FarmType = "-") %>%
  arrange(variable) %>%
  dplyr::select(FarmType, Block, variable, mean, SE, min, max)


summaryFTBL <-  all[, names(all) %in% c("FarmType","Block", divIndices,envi_factors)] %>%
  gather(key = "variable", value = "value", -c(FarmType, Block)) %>%
  group_by(FarmType, Block, variable) %>%
  summarize_at("value", list(mean = mean, SE=std.error, min = min, max = max), na.rm=TRUE) %>%
  ungroup() %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, 3) %>%
  arrange(variable)


summaryDF <- rbind(summaryFT,summaryBlock,summaryFTBL) %>%
  filter(! variable == "FarmBi")



########################################################################
## envi variables
########################################################################


all_wa_envi_range <- enviRange(all_wa$df)
all_w_envi_range <- enviRange(all_w$df)
all_a_envi_range <- enviRange(all_a$df)



mono_wa_envi_range <- enviRange(mono_wa$df)
mono_w_envi_range <- enviRange(mono_w$df)
mono_a_envi_range <- enviRange(mono_a$df)



poly_wa_envi_range <- enviRange(poly_wa$df)
poly_w_envi_range <- enviRange(poly_w$df)
poly_a_envi_range <- enviRange(poly_a$df)

