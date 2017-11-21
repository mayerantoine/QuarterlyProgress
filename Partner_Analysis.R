library(tidyverse)
library(formattable)
library(knitr)


rm(list = ls())

############ IMPORT STE By IM Factview ################################

site_im <- read_tsv("data/ICPI_FactView_Site_IM_Haiti_20170922_v2_1.txt")
names(site_im) <- tolower(names(site_im))

old_mechanism <- c("FOSREF","PIH","CDS","TBD2","Catholic Medical Mission Board","University of Maryland","Health Service Delivery","GHESKIO 0545","POZ","GHESKIO 541","ITECH 1331","University of Miami","Dedup","ITECH 549")


key_indicators <- c("HTS_TST","HTS_TST_POS","TX_NEW","PMTCT_ART","PMTCT_EID","PMTCT_STAT",
                    "PMTCT_STAT_POS","TB_ART","TB_STAT","TB_STAT_POS","PMTCT_EID_POS")

#key_indicators <- c("HTS_TST","HTS_TST_POS","TX_NEW","KP_PREV","PMTCT_ART","PMTCT_EID","PMTCT_STAT",
#                    "PMTCT_STAT_POS","TB_ART","TB_STAT","TB_STAT_POS","PMTCT_EID_POS","PMTCT_EID_Less_Equal_Two_Months",
#                    "PMTCT_EID_Two_Twelve_Months","PMTCT_STAT_KnownatEntry_POSITIVE",
#                    "PMTCT_STAT_NewlyIdentified_POSITIVE")

key_cummulative_indicator <- c("TX_CURR", "OVC_SERV","TX_PVLS","TX_RET")


#################### OU Level Results ########################################

# OU Level Results for non-cummalative indicators
ou_level_non_cummul <- site_im %>%
                filter(snu1 != "_Military Haiti") %>%
                filter(indicator %in% key_indicators) %>%
                filter(!(implementingmechanismname %in% old_mechanism)) %>%
                filter(disaggregate == "Total Numerator") %>%
                filter(indicatortype == "DSD") %>% 
                filter(typefacility == "Y") %>%
                filter(numeratordenom == "N") %>%
                select(indicator,fy2016apr,fy2017q1,fy2017q2,fy2017q3,fy2017_targets) %>%
                group_by(indicator) %>%
                summarise(fy2016apr = sum(fy2016apr,na.rm = T),
                          fy2017q1 = sum(fy2017q1, na.rm = T),
                          fy2017q2 = sum(fy2017q2, na.rm = T),
                          fy2017q3 = sum(fy2017q3,na.rm = T),
                          fy2017_targets= sum(fy2017_targets, na.rm = T)) %>%
                mutate(fy2017Cum  = fy2017q1+fy2017q2+fy2017q3,
                       fy2017Perf = ifelse(fy2017_targets > 0,round((fy2017Cum/fy2017_targets)*100,1), 0))


# OU Level Results for cummulative/annual/semi-annual indicators
ou_level_cumul <- site_im %>%
    filter(snu1 != "_Military Haiti") %>%
    filter(indicator %in% key_cummulative_indicator) %>%
    filter(!(implementingmechanismname %in% old_mechanism)) %>%
    filter(disaggregate == "Total Numerator") %>%
    filter(indicatortype == "DSD") %>% 
    filter(typefacility == "Y") %>%
    filter(numeratordenom == "N") %>%
    select(indicator,fy2016apr,fy2017q1,fy2017q2,fy2017q3,fy2017_targets) %>%
    group_by(indicator) %>%
    summarise(fy2016apr = sum(fy2016apr,na.rm = T),
              fy2017q1 = sum(fy2017q1, na.rm = T),
              fy2017q2 = sum(fy2017q2, na.rm = T),
              fy2017q3 = sum(fy2017q3,na.rm = T),
              fy2017_targets= sum(fy2017_targets, na.rm = T)) %>%
    mutate(fy2017Cum  = ifelse(fy2017q3 == 0, fy2017q2,fy2017q3),
           fy2017Perf = ifelse(fy2017_targets > 0,round((fy2017Cum/fy2017_targets)*100,1), 0))

# summarise tx_curr to calculate tx_net_new
tx_curr <- site_im %>%
    filter(snu1 != "_Military Haiti") %>%
    filter(indicator=="TX_CURR") %>%
    filter(!(implementingmechanismname %in% old_mechanism)) %>%
    filter(disaggregate == "Total Numerator") %>%
    filter(indicatortype == "DSD") %>% 
    filter(typefacility == "Y") %>%
    filter(numeratordenom == "N") %>%
    select(indicator,fy2016apr,fy2017q1,fy2017q2,fy2017q3,fy2017_targets) %>%
    group_by(indicator) %>%
    summarise(fy2016apr = sum(fy2016apr,na.rm = T),
              fy2017q1 = sum(fy2017q1, na.rm = T),
              fy2017q2 = sum(fy2017q2, na.rm = T),
              fy2017q3 = sum(fy2017q3,na.rm = T),
              fy2017_targets = sum(fy2017_targets,na.rm = T))

# calculate net_new by quarter
tx_net_new <- data_frame (indicator = c("TX_NET_NEW"),
                            fy2016apr = c(0),
                            fy2017q1 = c(tx_curr$fy2017q1 - tx_curr$fy2016apr),
                            fy2017q2 = c(tx_curr$fy2017q2 - tx_curr$fy2017q1),
                            fy2017q3 = c(tx_curr$fy2017q3 - tx_curr$fy2017q2),
                            fy2017Cum = c(fy2017q1+fy2017q2+fy2017q3),
                            fy2017_targets = c(tx_curr$fy2017_targets - tx_curr$fy2016apr),
                            fy2017Perf = ifelse(fy2017_targets > 0,round((fy2017Cum/fy2017_targets)*100,1), 0))


# OU Level Results bind non-cummul , cummul and tx_net_new
ou_level <- rbind(ou_level_non_cummul,ou_level_cumul,tx_net_new)
#ou_level



################## Partner Performance Results #############################################


# Partner Performance for cummulative indicators

partner_performance_cum <- site_im %>%
    filter(snu1 != "_Military Haiti") %>%
    filter(indicator %in% key_cummulative_indicator) %>%
    filter(!(implementingmechanismname %in% old_mechanism)) %>%
    filter(disaggregate == "Total Numerator") %>%
    filter(indicatortype == "DSD") %>% 
    filter(typefacility == "Y") %>%
    filter(numeratordenom == "N") %>%
    select(implementingmechanismname,indicator,fy2016apr,fy2017q1,fy2017q2,fy2017q3,fy2017_targets) %>%
    group_by(implementingmechanismname,indicator) %>%
    summarise(fy2016apr = sum(fy2016apr,na.rm = T),
              fy2017q1 = sum(fy2017q1, na.rm = T),
              fy2017q2 = sum(fy2017q2, na.rm = T),
              fy2017q3 = sum(fy2017q3,na.rm = T),
              fy2017_targets= sum(fy2017_targets, na.rm = T)) %>%
    mutate(fy2017Cum  = ifelse(fy2017q3 == 0, fy2017q2,fy2017q3),
           fy2017Perf = ifelse(fy2017_targets > 0,round((fy2017Cum/fy2017_targets)*100,1), 0)) %>%
     select(implementingmechanismname,indicator,fy2017Perf) %>%
    as.data.frame()


# Partner Performance for key indicators
 partner_performance_key <- site_im %>%
    filter(snu1 != "_Military Haiti") %>%
    filter(indicator %in% key_indicators) %>%
    filter(!(implementingmechanismname %in% old_mechanism)) %>%
    filter(disaggregate == "Total Numerator") %>%
    filter(indicatortype == "DSD") %>% 
    filter(typefacility == "Y") %>%
    filter(numeratordenom == "N") %>%
    select(implementingmechanismname,indicator,fy2016apr,fy2017q1,fy2017q2,fy2017q3,fy2017_targets) %>%
    group_by(implementingmechanismname,indicator) %>%
    summarise(fy2016apr = sum(fy2016apr,na.rm = T),
              fy2017q1 = sum(fy2017q1, na.rm = T),
              fy2017q2 = sum(fy2017q2, na.rm = T),
              fy2017q3 = sum(fy2017q3,na.rm = T),
              fy2017_targets= sum(fy2017_targets, na.rm = T)) %>%
    mutate(fy2017Cum  = fy2017q1+fy2017q2+fy2017q3,
           fy2017Perf = ifelse(fy2017_targets > 0,round((fy2017Cum/fy2017_targets)*100,1), 0)) %>% 
    select(implementingmechanismname,indicator,fy2017Perf) %>%
    as.data.frame()

# summarise tx_curr by partner to calculate tx_net_new
tx_curr_partner <- site_im %>%
    filter(snu1 != "_Military Haiti") %>%
    filter(indicator=="TX_CURR") %>%
    filter(!(implementingmechanismname %in% old_mechanism)) %>%
    filter(disaggregate == "Total Numerator") %>%
    filter(indicatortype == "DSD") %>% 
    filter(typefacility == "Y") %>%
    filter(numeratordenom == "N") %>%
    select(implementingmechanismname,indicator,fy2016apr,fy2017q1,fy2017q2,fy2017q3,fy2017_targets) %>%
    group_by(implementingmechanismname,indicator) %>%
    summarise(fy2016apr = sum(fy2016apr,na.rm = T),
              fy2017q1 = sum(fy2017q1, na.rm = T),
              fy2017q2 = sum(fy2017q2, na.rm = T),
              fy2017q3 = sum(fy2017q3,na.rm = T),
              fy2017_targets = sum(fy2017_targets,na.rm = T))

tx_net_new_partner <- data_frame(implementingmechanismname = tx_curr_partner$implementingmechanismname,
                                 indicator = c("TX_NET_NEW"),
                                 fy2016apr = c(0),
                                 fy2017q1 = c(tx_curr_partner$fy2017q1 - tx_curr_partner$fy2016apr),
                                 fy2017q2 = c(tx_curr_partner$fy2017q2 - tx_curr_partner$fy2017q1),
                                 fy2017q3 = c(tx_curr_partner$fy2017q3 - tx_curr_partner$fy2017q2),
                                 fy2017Cum = c(fy2017q1+fy2017q2+fy2017q3),
                                 fy2017_targets = c(tx_curr_partner$fy2017_targets - tx_curr_partner$fy2016apr),
                                 fy2017Perf = ifelse(fy2017_targets > 0,round((fy2017Cum/fy2017_targets)*100,1), 0))

tx_net_new_partner <- tx_net_new_partner %>% 
                        select(implementingmechanismname,indicator,fy2017Perf) %>%
                        as.data.frame()

partner_performance <- rbind(partner_performance_key,partner_performance_cum,tx_net_new_partner)

partner_performance_sp <- partner_performance %>%
    spread(implementingmechanismname,round(fy2017Perf,1))

#partner_performance_sp

partner_performance %>%
    filter(indicator %in% c("HTS_TST_POS")) %>%
    ggplot(mapping = aes(x=reorder(implementingmechanismname,fy2017Perf),y=fy2017Perf))+
        geom_bar(stat = "identity")+
        coord_flip()

partner_performance %>%
    filter(indicator %in% c("TX_NET_NEW")) %>%
    ggplot(mapping = aes(x=reorder(implementingmechanismname,fy2017Perf),y=fy2017Perf))+
    geom_bar(stat = "identity")+
    coord_flip()
 
        

