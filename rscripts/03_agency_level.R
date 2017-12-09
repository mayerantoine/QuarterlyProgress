######################################################################################
## Quaterly Progress Report  
## APR17                                                                          
## Mayer  Antoine , CDC Haiti                                                               
## Purpose : Create OU Level Report Table and Analysis by Agency   
## Updated : 12/9/2017                                                             
## https://github.com/mayerantoine/QuarterlyProgress                                                                               
######################################################################################


library(tidyverse)
library(formattable)
library(knitr)
library(scales)

rm(list = ls())


## IMPORT STE By IM Factview ----------------------------------------------------------

source("./rscripts/00_import_factview_data.R")
site_im <- import_factview_site_im()

caption <- c("Data source:ICPI FactView SitexIM")

key_indicators <- c("HTS_TST","HTS_TST_POS","TX_NEW","PMTCT_ART","PMTCT_EID","PMTCT_STAT",
                    "PMTCT_STAT_POS","TB_ART","TB_STAT","TB_STAT_POS","PMTCT_EID_POS")

#key_indicators <- c("HTS_TST","HTS_TST_POS","TX_NEW","KP_PREV","PMTCT_ART","PMTCT_EID","PMTCT_STAT",
#                    "PMTCT_STAT_POS","TB_ART","TB_STAT","TB_STAT_POS","PMTCT_EID_POS",
#                      "PMTCT_EID_Less_Equal_Two_Months",
#                    "PMTCT_EID_Two_Twelve_Months","PMTCT_STAT_KnownatEntry_POSITIVE",
#                    "PMTCT_STAT_NewlyIdentified_POSITIVE")

key_cummulative_indicator <- c("TX_CURR", "OVC_SERV","TX_PVLS","TX_RET","OVC_HIVSTAT")

## OU Level Results --------------------------------------------------------------------

# we do not use fy2017apr because we need the code the use for any quarter.

# OU Level Results for non-cummalative 
ou_level_non_cummulative <- site_im %>%
    #filter(snu1 != "_Military Haiti") %>%
    filter(indicator %in% key_indicators) %>%
    filter(fundingagency == "HHS/CDC") %>%
    filter(disaggregate == "Total Numerator") %>%
    #filter(indicatortype == "DSD") %>% 
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
    filter(snu1 != "_Military Haiti") %>%
    filter(indicator %in% key_cummulative_indicator) %>%
    filter(fundingagency == "HHS/CDC") %>%
    filter(disaggregate == "Total Numerator") %>%
    # filter(indicatortype == "DSD") %>% 
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
    # filter(snu1 != "_Military Haiti") %>%
    filter(indicator == "TX_CURR") %>%
    filter(disaggregate == "Total Numerator") %>%
    filter(fundingagency == "HHS/CDC") %>%
    # filter(indicatortype == "DSD") %>% 
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

#write_csv(ou_level,"processed_data/ou_level_cdc.csv")


##################################################################################################

tx_indicator <- c("HTS_TST","HTS_TST_POS","TX_NEW","TX_CURR","TX_PVLS","TX_RET")

ou_agency <- site_im %>%
    filter(snu1 != "_Military Haiti") %>%
    filter(indicator %in% tx_indicator) %>%
    filter(disaggregate == "Total Numerator") %>%
    filter(indicatortype == "DSD") %>% 
    filter(numeratordenom == "N") %>% 
    select(indicator,fundingagency,fy2017apr) %>%
    group_by(indicator,fundingagency) %>%
    summarise(fy2017apr = sum(fy2017apr, na.rm = T)) %>%
    spread(fundingagency,fy2017apr) %>%
    select(indicator,`HHS/CDC`,USAID) %>%
    as.data.frame()



# summarise tx_curr to calculate tx_net_new
tx_curr <- site_im %>%
    filter(snu1 != "_Military Haiti") %>%
    filter(indicator == "TX_CURR") %>%
    filter(disaggregate == "Total Numerator") %>%
    filter(indicatortype == "DSD") %>% 
    filter(numeratordenom == "N") %>%
    filter(!(fundingagency %in% c("Dedup"))) %>%
    select(indicator,fundingagency,fy2016apr,fy2017apr) %>%
    group_by(indicator,fundingagency) %>%
    summarise(fy2016apr = sum(fy2016apr,na.rm = T),
              fy2017apr = sum(fy2017apr, na.rm = T)) %>%
    mutate(tx_net_new = fy2017apr - fy2016apr) %>%
    select(indicator,fundingagency,tx_net_new) %>%
    spread(fundingagency,tx_net_new) %>%
    as.data.frame()

# calculate net_new by quarter
tx_curr$indicator <- c("TX_NET_NEW")
                            


# OU Level Results bind non-cummul , cummul and tx_net_new
ou_agency <- rbind(ou_agency,tx_curr)


#write_csv(ou_agency,"processed_data/ou_agency.csv")



#####


partner_3_90 <- site_im %>%
    filter(snu1 != "_Military Haiti") %>%
    filter(indicator %in% c("TX_RET","TX_PVLS")) %>%
    filter(standardizeddisaggregate %in% c("Total Numerator","Total Denominator")) %>%
    filter(indicatortype == "DSD") %>% 
    #filter(numeratordenom == "N") %>%
    select(implementingmechanismname,fundingagency,psnu,facility,community,
           indicator,standardizeddisaggregate,fy2017q4,fy2017_targets) %>%
    group_by(implementingmechanismname,fundingagency,psnu,facility,community,
             indicator,standardizeddisaggregate) %>%
    summarise(   fy2017q4 = sum(fy2017q4,na.rm = T),
              fy2017_targets= sum(fy2017_targets, na.rm = T)) 


write_csv(partner_3_90,"processed_data/partner_3_90.csv")