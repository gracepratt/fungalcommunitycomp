
########################################################################
## alpha summary
########################################################################

divIndices <- c("observed","shannon")

summaryFT <-  all_amf[, names(all_amf) %in% c("FarmType", divIndices, envi_factors)] %>%
  gather(key = "variable", value = "value", -c(FarmType)) %>%
  group_by(FarmType, variable) %>%
  summarize_at("value", list(mean = mean, SE=std.error, min = min, max = max), na.rm=TRUE) %>%
  ungroup() %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(Block = "-") %>%
  arrange(variable) %>%
  dplyr::select(FarmType, Block, variable, mean, SE, min, max)


summaryBlock <-  all_amf[, names(all_amf) %in% c("Block", divIndices, envi_factors)] %>%
  gather(key = "variable", value = "value", -c(Block)) %>%
  group_by(Block, variable) %>%
  summarize_at("value", list(mean = mean, SE=std.error, min = min, max = max), na.rm=TRUE) %>%
  ungroup() %>%
  as.data.frame() %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(FarmType = "-") %>%
  arrange(variable) %>%
  dplyr::select(FarmType, Block, variable, mean, SE, min, max)


summaryFTBL <-  all_amf[, names(all_amf) %in% c("FarmType","Block", divIndices,envi_factors)] %>%
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


all_envi <- enviRange(all_fungi)

mono_envi <- enviRange(all_fungi %>% filter(FarmType == "Monoculture"))

poly_envi <- enviRange(all_fungi %>% filter(FarmType == "Polyculture"))



