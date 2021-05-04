library(dplyr)
library(tidyverse)

titanTaxa <- function(titan, variable){
  
  as.data.frame(titan$sppmax) %>% rownames_to_column("OTU") %>% 
    filter(filter >0) %>% mutate(z = ifelse(filter == 1, "z-", "z+"), variable = variable) %>% left_join(tax, by="OTU") %>%
    # dplyr::select(variable, OTU, freq, zenv.cp, IndVal, zscore, obsiv.prob, Phylum, Class, Order, Family, Genus, Species, Taxon) %>% 
    arrange(zscore)
  
}

N.taxa <- titanTaxa(N.titan, variable = "N")
NP.taxa <- titanTaxa(NP.titan, variable = "NP")
TOC.taxa <- titanTaxa(TOC.titan, variable = "TOC")
pH.taxa <- titanTaxa(pH.titan, variable = "pH")
P.taxa <- titanTaxa(P.titan, variable = "P")
CD.taxa <- titanTaxa(CD.titan, variable = "CD")


titanTaxa_all <- rbind(N.taxa, NP.taxa, TOC.taxa, pH.taxa, P.taxa, CD.taxa)

write.csv(titanTaxa_all,"Outputs/Tables/titanTaxa.csv", row.names = FALSE)


