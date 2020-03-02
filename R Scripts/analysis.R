########################################################################
## gdm analysis
########################################################################


########################################################################
## 1. prepare site-pair tables
########################################################################

test_gdm_table <- formatsitepair(species_table, bioFormat=1, XColumn="Long_point", YColumn="Lat_point",
                                 siteColumn="Code", predData=mock_envi_table)

test_gdm_table
