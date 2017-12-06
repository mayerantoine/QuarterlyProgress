
library(tidyverse)
library(formattable)
library(knitr)
library(scales)



## Initial parameters ----------------------------------------------------------------

params <- list(country = c("Haiti"),
               source_date = c("20171115"),
               dataset = c("Site X IM"),
               filename = c("ICPI_FactView_Site_IM_Malawi_20171115_v1_1"))

## IMPORT STE By IM Factview ----------------------------------------------------------

import_factview_site_im <- function(filename = "ICPI_FactView_Site_IM_Haiti_20171115_v1_1") {
    filepath <- paste0("./data/",filename,".txt")
    site_im <- read_tsv(filepath)
    names(site_im) <- tolower(names(site_im))
    site_im
}

