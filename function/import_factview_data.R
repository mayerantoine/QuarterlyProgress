
library(tidyverse)
library(formattable)
library(knitr)
library(scales)




################# IMPORT STE By IM Factview ########################################

import_factview_site_im <- function() {

    site_im <- read_tsv("data/ICPI_FactView_Site_IM_Haiti_20171115_v1_1.txt")
    names(site_im) <- tolower(names(site_im))
    site_im
}

