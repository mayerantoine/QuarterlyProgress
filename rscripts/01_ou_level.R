#################################################################################
## Quaterly Progress Report  

## APR17                                                                          

## Mayer  Antoine , CDC Haiti                                                               

## Purpose : Create OU Level Report Table    

## Updated : 11/22/2017                                                             

## https://github.com/mayerantoine/QuarterlyProgress                                                                               
#################################################################################


library(tidyverse)
library(formattable)
library(knitr)
library(scales)

rm(list = ls())


## IMPORT STE By IM Factview ----------------------------------------------------------

source("./function/import_factview_data.R")
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

# OU Level Results for non-cummalative 
ou_level_non_cummulative <- site_im %>%
    #filter(snu1 != "_Military Haiti") %>%
    filter(indicator %in% key_indicators) %>%
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
    #filter(snu1 != "_Military Haiti") %>%
    filter(indicator %in% key_cummulative_indicator) %>%
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

#write_csv(ou_level,"processed_data/ou_level.csv")

## Overall achievement bar -------------------------------------------------------------------------

ou_level %>%
    filter(!(indicator %in% c("TB_STAT_POS","OVC_HIVSTAT"))) %>%
    ggplot(aes(x = reorder(indicator, fy2017Perf), y = fy2017Perf)) +
    geom_bar(stat = "identity",fill="#0072B2") +
    geom_hline(yintercept = 85)+
    geom_text(aes(x = reorder(indicator, fy2017Perf), y = fy2017Perf,
                  label=paste0(sprintf("%.0f",round(fy2017Perf,0)),"%")),size = 4, hjust =-0.05) +
    coord_flip()+
    labs(y="% of achievement", 
         x="",
         fill="",
         title="APR17 Overall Performance",
         subtitle="",
         caption="Data source:ICPI FactView SitexIM Haiti")+
    #scale_y_continuous(limits=c(0,180))+
    #scale_y_continuous(labels = percent_format())+
    guides(fill = FALSE) +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12), 
          panel.background = element_blank(),
          axis.line= element_line(),
          axis.title.x = element_text(size = 10),
          plot.title = element_text(size = 16) )
        

## Cascade FY17 ---------------------------------------------------------------------------------


ou_level %>%
    filter(indicator %in% c("HTS_TST_POS","TX_NEW","TX_NET_NEW")) %>%
    select(indicator,fy2017Cum) %>%
    ggplot(aes(x = reorder(indicator,-fy2017Cum),y= fy2017Cum))+
    geom_bar(stat = "identity", width = 0.7) +
    geom_text(aes(label= comma(fy2017Cum)),size=4.5) +
    labs(y="", 
         x="",
         fill="",
         title="FY17 Cascade",
         subtitle="",
         caption= "Data source:ICPI FactView SitexIM Haiti")+
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 13,face= "bold"), 
          panel.background = element_blank(),
          axis.line=element_line(),
          axis.title.x = element_text(size = 10),
          plot.title = element_text(size = 18),
          plot.subtitle = element_text(size = 12),
          axis.ticks.y = element_blank())  

#################### Results vs Targets #######################################################################


#################### Trend


####################