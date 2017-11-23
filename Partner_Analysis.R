#################################################################################
# Partner Performance Report Script                                             #
# APR17                                                                         # 
# Mayer  Antoine                                                                # 
# Purpose : Import FactView Site by IM data and create visual for Partner       # 
#           Performance Moniting                                                #
# Date : 11/22/2017                                                             #
#                                                                               #
#                                                                               #
#                                                                               #
#################################################################################


library(tidyverse)
library(formattable)
library(knitr)


rm(list = ls())

############ IMPORT STE By IM Factview ################################

site_im <- read_tsv("data/ICPI_FactView_Site_IM_Haiti_20171115_v1_1.txt")
names(site_im) <- tolower(names(site_im))

old_mechanism <- c("FOSREF","PIH","CDS","TBD2","Catholic Medical Mission Board","University of Maryland","Health Service Delivery","GHESKIO 0545","POZ","GHESKIO 541","ITECH 1331","University of Miami","Dedup","ITECH 549")


key_indicators <- c("HTS_TST","HTS_TST_POS","TX_NEW","PMTCT_ART","PMTCT_EID","PMTCT_STAT",
                    "PMTCT_STAT_POS","TB_ART","TB_STAT","TB_STAT_POS","PMTCT_EID_POS")

#key_indicators <- c("HTS_TST","HTS_TST_POS","TX_NEW","KP_PREV","PMTCT_ART","PMTCT_EID","PMTCT_STAT",
#                    "PMTCT_STAT_POS","TB_ART","TB_STAT","TB_STAT_POS","PMTCT_EID_POS","PMTCT_EID_Less_Equal_Two_Months",
#                    "PMTCT_EID_Two_Twelve_Months","PMTCT_STAT_KnownatEntry_POSITIVE",
#                    "PMTCT_STAT_NewlyIdentified_POSITIVE")

key_cummulative_indicator <- c("TX_CURR", "OVC_SERV","TX_PVLS","TX_RET")
#################################################################################

partner_data <- site_im %>%
    filter(snu1 != "_Military Haiti") %>%
    filter(indicator %in% key_indicators) %>%
    filter(disaggregate == "Total Numerator") %>%
    filter(indicatortype == "DSD") %>% 
    filter(numeratordenom == "N") %>%
    select(implementingmechanismname,fundingagency,psnu,facility,community,indicator,fy2015apr,fy2016apr,fy2017q1,
           fy2017q2,fy2017q3,fy2017q4,fy2017_targets) %>%
    group_by(implementingmechanismname,fundingagency,psnu,facility,community,indicator) %>%
    summarise(fy2015apr = sum(fy2015apr, na.rm = T),
              fy2016apr = sum(fy2016apr,na.rm = T),
              fy2017q1 = sum(fy2017q1, na.rm = T),
              fy2017q2 = sum(fy2017q2, na.rm = T),
              fy2017q3 = sum(fy2017q3,na.rm = T),
              fy2017q4 = sum(fy2017q4,na.rm = T),
              fy2017_targets= sum(fy2017_targets, na.rm = T)) %>%
    mutate(fy2017Cum  = fy2017q1+fy2017q2+fy2017q3+fy2017q4)


## Partnet dataset for cummulative, need to calculate APR results
partner_data_cum <- site_im %>%
    filter(snu1 != "_Military Haiti") %>%
    filter(indicator %in% key_cummulative_indicator) %>%
    filter(disaggregate == "Total Numerator") %>%
    filter(indicatortype == "DSD") %>% 
    filter(numeratordenom == "N") %>%
    select(implementingmechanismname,fundingagency,psnu,facility,community,indicator,fy2015apr,fy2016apr,fy2017q1,
           fy2017q2,fy2017q3,fy2017q4,fy2017_targets) %>%
    group_by(implementingmechanismname,fundingagency,psnu,community,facility,indicator) %>%
    summarise(fy2015apr = sum(fy2015apr, na.rm = T),
              fy2016apr = sum(fy2016apr,na.rm = T),
              fy2017q1 = sum(fy2017q1, na.rm = T),
              fy2017q2 = sum(fy2017q2, na.rm = T),
              fy2017q3 = sum(fy2017q3,na.rm = T),
              fy2017q4 = sum(fy2017q4,na.rm = T),
              fy2017_targets= sum(fy2017_targets, na.rm = T)) %>%
    mutate(fy2017Cum  = ifelse(fy2017q4 == 0, fy2017q3,fy2017q4))

partner_data_final <- rbind(partner_data,partner_data_cum) %>% as.data.frame()

## We need to assgin new mechanism name to fy2016, fy2015 data, because mechanism
## has changed in 2017 for CDC Haiti, this is useful to have yearly trend for a specific partner
site_by_mechanism_2017 <- partner_data_final %>%
    filter(indicator == "HTS_TST") %>%
    filter(!(implementingmechanismname %in% old_mechanism)) %>%
    group_by(implementingmechanismname,facility) %>%
    summarise(fy2017Cum = sum(fy2017Cum)) %>%
    select(implementingmechanismname,facility) %>% as.data.frame()

site_by_mechanism_2017 <- site_by_mechanism_2017[!is.na(site_by_mechanism_2017$facility),]


#################### OU Level Results ##############################################################

# OU Level Results for non-cummalative and cummul indicators
ou_level_data <- partner_data_final %>%
                select(indicator,fy2015apr,fy2016apr,fy2017q1,fy2017q2,fy2017q3,fy2017q4,fy2017Cum,fy2017_targets) %>%
                group_by(indicator) %>%
                summarise(fy2015apr = sum(fy2015apr, na.rm = T),
                          fy2016apr = sum(fy2016apr,na.rm = T),
                          fy2017q1 = sum(fy2017q1, na.rm = T),
                          fy2017q2 = sum(fy2017q2, na.rm = T),
                          fy2017q3 = sum(fy2017q3,na.rm = T),
                          fy2017q4 = sum(fy2017q4,na.rm = T),
                          fy2017Cum  = sum(fy2017Cum,na.rm = T),
                          fy2017_targets= sum(fy2017_targets, na.rm = T)) %>%
                mutate(fy2017Perf = ifelse(fy2017_targets > 0,round((fy2017Cum/fy2017_targets)*100,1), 0))



# summarise tx_curr to calculate tx_net_new
tx_curr <- partner_data_final %>%
    filter(indicator=="TX_CURR") %>%
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
ou_level <- rbind(ou_level_data,tx_net_new)
#ou_level



################## Partner Performance Results #############################################


# Partner Performance: Results vs Targets = Performance

partner_performance <- partner_data_final %>%
    filter(!(implementingmechanismname %in% old_mechanism)) %>%
     select(implementingmechanismname,indicator,fy2017Cum,fy2017_targets) %>%
    group_by(implementingmechanismname,indicator) %>%
    summarise(fy2017Cum = sum(fy2017Cum, na.rm = T),
              fy2017_targets= sum(fy2017_targets, na.rm = T)) %>%
    mutate( fy2017Perf = ifelse(fy2017_targets > 0,round((fy2017Cum/fy2017_targets)*100,0), 0)) %>%
    as.data.frame()



# summarise tx_curr by partner to calculate tx_net_new
tx_curr_partner <- partner_data_final %>%
    filter(indicator=="TX_CURR") %>%
    select(implementingmechanismname,indicator,fy2016apr,fy2017Cum,fy2017_targets) %>%
    group_by(implementingmechanismname,indicator) %>%
    summarise(fy2016apr = sum(fy2016apr, na.rm = T),
              fy2017Cum = sum(fy2017Cum,na.rm = T),
              fy2017_targets = sum(fy2017_targets,na.rm = T))

## Calculate net_new  ?? note net_new target to be fix because partners names have changed
tx_net_new_partner <- data_frame(implementingmechanismname = tx_curr_partner$implementingmechanismname,
                                 indicator = c("TX_NET_NEW"),
                                 fy2017Cum = c(tx_curr_partner$fy2017Cum - tx_curr_partner$fy2016apr),
                                 fy2017_targets = c(tx_curr_partner$fy2017_targets - tx_curr_partner$fy2016apr),
                                 fy2017Perf = ifelse(fy2017_targets > 0,round((fy2017Cum/fy2017_targets)*100,1), 0))

tx_net_new_partner <- tx_net_new_partner %>% 
                        select(implementingmechanismname,indicator,fy2017Perf) %>%
                        as.data.frame()

#partner_performance <- rbind(partner_performance_key,partner_performance_cum,tx_net_new_partner)

partner_performance_sp <- partner_performance %>%
        select(implementingmechanismname,indicator,fy2017Perf) %>%
        spread(implementingmechanismname,round(fy2017Perf,1))

names(partner_performance_sp) <- c("Indicator","BEST","CDS 1528","FOSREF 1925","GHESKIO 1969","Linkages",
                                   "PIH 1926","SSQH Nord","CMMB 1970","GHESKIO 1924",
                                   "HTW","MSPP","SSQH Sud")
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
 


