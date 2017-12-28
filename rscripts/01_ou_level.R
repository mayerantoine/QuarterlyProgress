######################################################################################
## Quaterly Progress Report  
## APR17                                                                          
## Mayer  Antoine , CDC Haiti                                                               
## Purpose : Create OU Level Report Table    
## Updated : 12/1/2017                                                             
## https://github.com/mayerantoine/QuarterlyProgress                                                                               
######################################################################################


library(tidyverse)
library(formattable)
library(knitr)
library(scales)

rm(list = ls())

#? How to get the IMPPAT data ? is it complete ?
# why use html, r markdown not excel tool ? (distribution and maintenace)
# example of application :
    # Developed data pipeline and standard report
    # retro feed the facility on what is on DATIM using standard report 
    # Site visit report
    # Quarterly data review report
    # 
 
# can we run the code for past quarter
# Import FactView File ----------------------------------------------------------

source("./rscripts/00_import_factview_data.R")

site_im <- import_factview_site_im("ICPI_FactView_Site_IM_Malawi_20171115_v1_1")

site_im$fy2015q4 <- as.numeric(site_im$fy2015q4)
site_im$fy2015q2 <- as.numeric(site_im$fy2015q2)
site_im$fy2015q3 <- as.numeric(site_im$fy2015q3)
site_im$fy2016_targets <- as.integer(site_im$fy2016_targets)
site_im$fy2016q1 <- as.integer(site_im$fy2016q1)
site_im$fy2016q2 <- as.integer(site_im$fy2016q2)

country_name <- get_country_name(site_im)

caption <- c("Data source:ICPI FactView SitexIM")

key_indicators <- c("HTS_TST","HTS_TST_POS","TX_NEW","PMTCT_ART","PMTCT_EID","PMTCT_STAT",
                    "PMTCT_STAT_POS","TB_ART","TB_STAT","TB_STAT_POS","PMTCT_EID_POS")

#key_indicators <- c("HTS_TST","HTS_TST_POS","TX_NEW","KP_PREV","PMTCT_ART","PMTCT_EID","PMTCT_STAT",
#                    "PMTCT_STAT_POS","TB_ART","TB_STAT","TB_STAT_POS","PMTCT_EID_POS",
#                      "PMTCT_EID_Less_Equal_Two_Months",
#                    "PMTCT_EID_Two_Twelve_Months","PMTCT_STAT_KnownatEntry_POSITIVE",
#                    "PMTCT_STAT_NewlyIdentified_POSITIVE")

key_cummulative_indicator <- c("TX_CURR", "OVC_SERV","TX_PVLS","TX_RET","OVC_HIVSTAT")

# OU Level Results dataset --------------------------------------------------------------------
# we do not use fy2017apr because we need the code to be use for any quarter.

get_ou_level_data <- function(site_im) {
    
# OU Level Results for non-cummalative 
ou_level_non_cummulative <- site_im %>%
    filter(indicator %in% key_indicators) %>%
    filter(disaggregate == "Total Numerator") %>%
    filter(numeratordenom == "N") %>% 
    select(indicator,fy2015apr,fy2016apr,fy2017q1,fy2017q2,fy2017q3,fy2017q4,fy2017_targets) %>%
    group_by(indicator) %>%
    summarise(fy2015apr = sum(fy2015apr, na.rm = T),
              fy2016apr = sum(fy2016apr,na.rm = T),
              fy2017q1 = sum(fy2017q1, na.rm = T),
              fy2017q2 = sum(fy2017q2, na.rm = T),
              fy2017q3 = sum(fy2017q3,na.rm = T),
              fy2017q4 = sum(fy2017q4,na.rm = T),
              fy2017_targets= sum(fy2017_targets, na.rm = T)) %>%
    mutate(fy2017Cum  = fy2017q1+fy2017q2+fy2017q3+fy2017q4,
           fy2017Perf = ifelse(fy2017_targets > 0,round((fy2017Cum/fy2017_targets)*100,1), 0))
    
# OU Level Results for cummulative    
ou_level_cummulative <- site_im %>%
    filter(indicator %in% key_cummulative_indicator) %>%
    filter(disaggregate == "Total Numerator") %>%
    filter(numeratordenom == "N") %>%
    select(indicator,fy2015apr,fy2016apr,fy2017q1,fy2017q2,fy2017q3,fy2017q4,fy2017_targets) %>%
    group_by(indicator) %>%
    summarise(fy2015apr = sum(fy2015apr, na.rm = T),
              fy2016apr = sum(fy2016apr,na.rm = T),
              fy2017q1 = sum(fy2017q1, na.rm = T),
              fy2017q2 = sum(fy2017q2, na.rm = T),
              fy2017q3 = sum(fy2017q3,na.rm = T),
              fy2017q4 = sum(fy2017q4,na.rm = T),
              fy2017_targets= sum(fy2017_targets, na.rm = T)) %>%
    mutate(fy2017Cum  = ifelse(fy2017q4 == 0, fy2017q3,fy2017q4),
           fy2017Perf = ifelse(fy2017_targets > 0,round((fy2017Cum/fy2017_targets)*100,1), 0))  
    

ou_level <-  rbind(ou_level_cummulative,ou_level_non_cummulative)  
    
    
# summarise tx_curr to calculate tx_net_new
tx_curr <- site_im %>%
    filter(indicator == "TX_CURR") %>%
    filter(disaggregate == "Total Numerator") %>%
    filter(numeratordenom == "N") %>%
    select(indicator,fy2015apr,fy2016apr,fy2017q1,fy2017q2,fy2017q3,fy2017q4,fy2017_targets) %>%
    group_by(indicator) %>%
    summarise(fy2015apr = sum(fy2015apr, na.rm = T),
              fy2016apr = sum(fy2016apr,na.rm = T),
              fy2017q1 = sum(fy2017q1, na.rm = T),
              fy2017q2 = sum(fy2017q2, na.rm = T),
              fy2017q3 = sum(fy2017q3,na.rm = T),
              fy2017q4 = sum(fy2017q4,na.rm = T),
              fy2017_targets = sum(fy2017_targets,na.rm = T))

# calculate net_new by quarter
tx_net_new <- data_frame (indicator = c("TX_NET_NEW"),
                          fy2015apr = c(0),
                          fy2016apr = c(tx_curr$fy2016apr - tx_curr$fy2015apr),
                          fy2017q1 = c(tx_curr$fy2017q1 - tx_curr$fy2016apr),
                          fy2017q2 = c(tx_curr$fy2017q2 - tx_curr$fy2017q1),
                          fy2017q3 = c(tx_curr$fy2017q3 - tx_curr$fy2017q2),
                          fy2017q4 = c(tx_curr$fy2017q4 - tx_curr$fy2017q3),
                          fy2017Cum = c(fy2017q1+fy2017q2+fy2017q3+fy2017q4),
                          fy2017_targets = c(tx_curr$fy2017_targets - tx_curr$fy2016apr),
                          fy2017Perf = ifelse(fy2017_targets > 0,round((fy2017Cum/fy2017_targets)*100,1), 0))


# OU Level Results bind non-cummul , cummul and tx_net_new
ou_level <- rbind(ou_level,tx_net_new)
ou_level
}

ou_level <- get_ou_level_data(site_im = site_im)


## Render Markdown ------------------------------------------------------------------------------------

# html output
output_country_dir <- paste0("./report/",country_name)
dir.create(file.path(output_country_dir),showWarnings = FALSE)
rmarkdown::render("./rmds/ou_level_report.Rmd",output_format = "html_document",output_dir = output_country_dir)





