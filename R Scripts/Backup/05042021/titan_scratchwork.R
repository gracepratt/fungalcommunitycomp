# TITAN CODE


pH.titan_table <- pH.titan$sppmax %>%
  as.data.frame() %>% 
  rownames_to_column("OTU") %>%
  left_join(tax, by = "OTU") %>%
  filter(filter >0) %>%
  mutate(lowSE = zenv.cp-`5%`, highSE = abs( zenv.cp-`95%`), filter = factor(filter), organization = ifelse(filter == 1, zenv.cp*1, zenv.cp*-1)) %>%
  group_by(filter) %>%
  mutate(rank = order(order(organization, decreasing=TRUE)), 
         rank2 = ifelse(filter != 1, rank+0.5, rank)) %>%
  ungroup() %>%
  arrange( rank2) %>%
  mutate(OTU = factor(OTU, levels=OTU))

ggplot(pH.titan_table , aes(y= zenv.cp, x= OTU, color=Taxon )) +
  geom_errorbar(aes(ymin=zenv.cp-lowSE, ymax=zenv.cp+highSE), width=0, size=0.75, position=position_dodge(0.05)) +
  geom_point(aes(shape=factor(filter), size=zscore), fill = "white", stroke=0.75) +
  scale_shape_manual(values = c(16,21) ) + 
  scale_color_manual(values = taxonColor(taxons=pH.titan_table$Taxon)) + 
  coord_flip() + theme_classic() #+ facet_wrap(~filter)


# ggplot(pH.titan_table , aes(y= zenv.cp, x= rank2, color=Taxon )) +
#   geom_errorbar(data=pH.titan_table %>% filter(filter == 1) , aes(ymin=zenv.cp-lowSE, ymax=zenv.cp+highSE), width=0, size=0.75, position=position_dodge(0.05)) +
#   geom_point(data=pH.titan_table %>% filter(filter == 1) ,aes(shape=factor(filter), size=zscore), fill = "white", stroke=0.75) +
#   geom_errorbar(data=pH.titan_table %>% filter(filter != 1) , aes(ymin=zenv.cp-lowSE, ymax=zenv.cp+highSE), width=0, size=0.75, position=position_dodge(0.05)) +
#   geom_point(data=pH.titan_table %>% filter(filter != 1) ,aes(shape=factor(filter), size=zscore), fill = "white", stroke=0.75) +
#   scale_shape_manual(values = c(16,21) ) + 
#   scale_color_brewer(palette = "Dark2") +
#   scale_x_continuous(sec.axis = sec_axis(~./1, breaks = c(1:31), labels=pH.titan_table$OTU), breaks = c(1:31), labels=pH.titan_table$OTU) +
#   coord_flip() + theme_classic() #+ facet_wrap(~filter)


P.titan_table <- P.titan$sppmax %>%
  as.data.frame() %>% 
  rownames_to_column("OTU") %>%
  left_join(tax, by = "OTU") %>%
  filter(filter >0) %>%
  mutate(lowSE = zenv.cp-`5%`, highSE = abs( zenv.cp-`95%`), filter = factor(filter), organization = ifelse(filter == 1, zenv.cp*-1, zenv.cp)) %>%
  arrange( organization, filter) %>%
  mutate(OTU = factor(OTU, levels=OTU))

ggplot(P.titan_table , aes(y= zenv.cp, x= OTU, color=Taxon )) +
  geom_errorbar(aes(ymin=zenv.cp-lowSE, ymax=zenv.cp+highSE, linetype=factor(filter)), width=0, size=0.75, position=position_dodge(0.05)) +
  geom_point(aes(shape=factor(filter), size=zscore), fill = "white", stroke=0.75) +
  scale_shape_manual(values = c(16,21) ) + 
  scale_color_manual(values = taxonColor(taxons=P.titan_table$Taxon)) + 
  coord_flip() + theme_classic() #+ facet_wrap(~filter,scales = "free")




CD.titan_table <- CD.titan$sppmax %>%
  as.data.frame() %>% 
  rownames_to_column("OTU") %>%
  left_join(tax, by = "OTU") %>%
  filter(filter >0) %>%
  mutate(lowSE = zenv.cp-`5%`, highSE = abs( zenv.cp-`95%`), filter = factor(filter), organization = ifelse(filter == 1, zenv.cp*-1, zenv.cp)) %>%
  arrange( organization, filter) %>%
  mutate(OTU = factor(OTU, levels=OTU))

ggplot(CD.titan_table , aes(y= zenv.cp, x= OTU, color=Taxon )) +
  geom_errorbar(aes(ymin=zenv.cp-lowSE, ymax=zenv.cp+highSE, linetype=factor(filter)), width=0, size=0.75, position=position_dodge(0.05)) +
  geom_point(aes(shape=factor(filter), size=zscore), fill = "white", stroke=0.75) +
  scale_shape_manual(values = c(16,21) ) + 
  # scale_color_brewer(palette = "Dark2") +
  coord_flip() + theme_classic() #+ facet_wrap(~filter,scales = "free")




NP.titan_table <- NP.titan$sppmax %>%
  as.data.frame() %>% 
  rownames_to_column("OTU") %>%
  left_join(tax, by = "OTU") %>%
  filter(filter >0) %>%
  mutate(lowSE = zenv.cp-`5%`, highSE = abs( zenv.cp-`95%`), filter = factor(filter), organization = ifelse(filter == 1, zenv.cp*-1, zenv.cp)) %>%
  arrange( organization, filter) %>%
  mutate(OTU = factor(OTU, levels=OTU))

ggplot(NP.titan_table , aes(y= zenv.cp, x= OTU, color=Taxon )) +
  geom_errorbar(aes(ymin=zenv.cp-lowSE, ymax=zenv.cp+highSE, linetype=factor(filter)), width=0, size=0.75, position=position_dodge(0.05)) +
  geom_point(aes(shape=factor(filter), size=zscore), fill = "white", stroke=0.75) +
  scale_shape_manual(values = c(16,21) ) + 
  # scale_color_brewer(palette = "Dark2") +
  coord_flip() + theme_classic() #+ facet_wrap(~filter,scales = "free")



TOC.titan_table <- TOC.titan$sppmax %>%
  as.data.frame() %>% 
  rownames_to_column("OTU") %>%
  left_join(tax, by = "OTU") %>%
  filter(filter >0) %>%
  mutate(lowSE = zenv.cp-`5%`, highSE = abs( zenv.cp-`95%`), filter = factor(filter), organization = ifelse(filter == 1, zenv.cp*-1, zenv.cp)) %>%
  arrange( organization, filter) %>%
  mutate(OTU = factor(OTU, levels=OTU))

ggplot(TOC.titan_table , aes(y= zenv.cp, x= OTU, color=Taxon )) +
  geom_errorbar(aes(ymin=zenv.cp-lowSE, ymax=zenv.cp+highSE, linetype=factor(filter)), width=0, size=0.75, position=position_dodge(0.05)) +
  geom_point(aes(shape=factor(filter), size=zscore), fill = "white", stroke=0.75) +
  scale_shape_manual(values = c(16,21) ) + 
  # scale_color_brewer(palette = "Dark2") +
  coord_flip() + theme_classic() #+ facet_wrap(~filter,scales = "free")



# ggplot(pH.titan_table , aes(x= zenv.cp, y= OTU, color=Taxon )) +
#   geom_point(data=pH.titan_table %>% filter(filter == 1) , aes(x= zenv.cp, y= OTU, color=Taxon , shape=factor(filter), size=zscore), color = "black") +
#   geom_errorbar(data=pH.titan_table %>% filter(filter == 1) , aes(xmin=zenv.cp-lowSE, xmax=zenv.cp+highSE), width=.2, position=position_dodge(0.05), color = "black") +
#   geom_point(data=pH.titan_table %>% filter(filter == 2) , aes(x= zenv.cp, y= OTU, color=Taxon , shape=factor(filter), size=zscore), color = "black") +
#   geom_errorbar(data=pH.titan_table %>% filter(filter == 2) , aes(xmin=zenv.cp-lowSE, xmax=zenv.cp+highSE), width=.2, position=position_dodge(0.05), color = "black") +
#   scale_shape_manual(values = c(16,1) ) +
#   # scale_y_continuous(sec.axis = sec_axis(~./1)) +
#   # coord_flip() + 
#   theme_classic()