
library(tidyverse)
library(formattable)
library(knitr)
library(scales)



# Initial parameters ----------------------------------------------------------------

params <- list(country = c("Haiti"),
               source_date = c("20171115"),
               dataset = c("Site X IM"),
               filename = c("ICPI_FactView_Site_IM_Haiti_20171222_v2_2"))

# Import FactView File ----------------------------------------------------------

import_factview_site_im <- function(filename = "ICPI_FactView_Site_IM_Haiti_20171222_v2_2") {
    filepath <- paste0("./data/",filename,".txt")
    
    # if file exist
    site_im <- read_tsv(filepath)
    names(site_im) <- tolower(names(site_im))
    site_im
    
   # if not exist   
}


get_country_name <- function(site_im){
    
    # if site_im not null
    name <- site_im$countryname[1]
    
    # if not empty
    name
    
}