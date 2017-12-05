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
    filter(snu1 != "_Military Haiti") %>%
    filter(indicator %in% key_cummulative_indicator) %>%
    filter(disaggregate == "Total Numerator") %>%
    filter(indicatortype == "DSD") %>% 
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

## Overall achievement bar charts-------------------------------------------------------------------------

 g_ou_level <- ou_level %>%
    filter(!(indicator %in% c("TB_STAT_POS","OVC_HIVSTAT","OVC_SERV"))) %>%
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
         caption="Data source:ICPI FactView SitexIM")+
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


g_cascade <- ou_level %>%
    filter(indicator %in% c("HTS_TST_POS","TX_NEW","TX_NET_NEW")) %>%
    select(indicator,fy2017Cum,fy2017_targets) %>%
    ggplot(aes(x = reorder(indicator,-fy2017Cum),y= fy2017Cum))+
    geom_bar(stat = "identity", width = 0.7) +
    geom_text(aes(label= comma(fy2017Cum)),size=4.5,vjust = 1.5,colour = "white") +
    geom_errorbar(aes(ymin=fy2017_targets,ymax=fy2017_targets),width=0.5, size=1.5, color="#FF7F00")+
    labs(y="", 
         x="",
         fill="",
         title="FY17 Cascade",
         subtitle="",
         caption= "Data source:ICPI FactView SitexIM")+
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 13,face= "bold"), 
          panel.background = element_blank(),
          axis.line=element_line(),
          axis.title.x = element_text(size = 10),
          plot.title = element_text(size = 18),
          plot.subtitle = element_text(size = 12),
          axis.ticks.y = element_blank())  


## TX_NEW Trend --------------------------------------------------------------------------------------------

site_im %>%
    filter(snu1 != "_Military Haiti") %>%
    filter(indicator == "TX_NEW") %>%
    filter(disaggregate == "Total Numerator") %>%
    filter(indicatortype == "DSD") %>% 
    filter(numeratordenom == "N") %>%
    select(indicator,fy2015q3,fy2015q4,fy2016q1,fy2016q2,fy2016q3,fy2016q4,
           fy2017q1,fy2017q2,fy2017q3,fy2017q4,fy2017_targets) %>%
    group_by(indicator) %>%
    summarise(fy2015q3 = sum(fy2015q3, na.rm = T),
              fy2015q4 = sum(fy2015q4, na.rm = T),
              fy2016q1 = sum(fy2016q1, na.rm = T),
              fy2016q2 = sum(fy2016q2, na.rm = T),
              fy2016q3 = sum(fy2016q3, na.rm = T),
              fy2016q4 = sum(fy2016q4, na.rm = T),
              fy2017q1 = sum(fy2017q1, na.rm = T),
              fy2017q2 = sum(fy2017q2, na.rm = T),
              fy2017q3 = sum(fy2017q3,na.rm = T),
              fy2017q4 = sum(fy2017q4,na.rm = T)) %>%
    gather("fiscal_year","results",2:11) %>%
    ggplot(aes(fiscal_year,results))+
    geom_bar(stat = "identity", fill = "#0072B2",width  = 0.7)+
    geom_text(aes( y = results,
                   label=paste0(sprintf("%.0f",round(results,0)))),size = 4,vjust =-1.1 )+
    labs(y="# new people enroled on ART", 
         x="",
         fill="",
         title="TX_NEW Trend from FY15 to FY17",
         subtitle="",
         caption="Data source: ICPI FactView SitexIM")+
    scale_y_continuous(breaks = seq(0,10000,1000),limits =c(0,10000),labels =comma,expand = c(0, 0))+
    expand_limits(x = 0, y = 0)+
    theme(axis.text.x = element_text(size = 10,face="bold"),
          axis.text.y = element_text(size = 13,face= "bold"), 
          panel.background = element_blank(),
          axis.line=element_line(),
          axis.title.x = element_text(size = 8),
          plot.title = element_text(size = 18),
          plot.subtitle  = element_text(size = 12))   

## TX_CURR Trend -----------------------------------------------------------------------------------------

site_im %>%
    filter(snu1 != "_Military Haiti") %>%
    filter(indicator == "TX_CURR") %>%
    filter(disaggregate == "Total Numerator") %>%
    filter(indicatortype == "DSD") %>% 
    filter(numeratordenom == "N") %>%
    select(indicator,fy2015q4,fy2016q2,fy2016q4,
           fy2017q1,fy2017q2,fy2017q3,fy2017q4) %>%
    group_by(indicator) %>%
    summarise(fy2015q4 = sum(fy2015q4, na.rm = T),
              fy2016q2 = sum(fy2016q2, na.rm = T),
              fy2016q4 = sum(fy2016q4, na.rm = T),
              fy2017q1 = sum(fy2017q1, na.rm = T),
              fy2017q2 = sum(fy2017q2, na.rm = T),
              fy2017q3 = sum(fy2017q3,na.rm = T),
              fy2017q4 = sum(fy2017q4,na.rm = T)) %>%
    gather("quarter","results",2:8) %>%
    ggplot(aes(x=factor(quarter),y=results, group =1))+
        geom_line()+
        geom_point()+
    geom_text(aes( y = results,
                   label=paste0(sprintf("%.0f",round(results,0)))),size = 4,vjust =-1.1 )+
    labs(y="# patient on ART", 
         x="",
         fill="",
         title="TX_CURR Trend from FY15 to FY17",
         subtitle="",
         caption="Data source: ICPI FactView SitexIM")+
    scale_y_continuous(breaks = seq(0,100000,10000),limits =c(0,100000),labels =comma,expand = c(0, 0))+
    expand_limits(x = 0, y = 0)+
    theme(axis.text.x = element_text(size = 10,face="bold"),
          axis.text.y = element_text(size = 13,face= "bold"), 
          panel.background = element_blank(),
          axis.line=element_line(),
          axis.title.x = element_text(size = 8),
          plot.title = element_text(size = 18),
          plot.subtitle  = element_text(size = 12))

## HTS and PMTCT_STAT yield --------------------------------------------------------------------------

site_im %>%
    filter(snu1 != "_Military Haiti") %>%
    filter(indicator %in% c("HTS_TST","HTS_TST_POS","PMTCT_STAT","PMTCT_STAT_POS")) %>%
    filter(disaggregate == "Total Numerator") %>%
    filter(indicatortype == "DSD") %>% 
    filter(numeratordenom == "N") %>%
    select(indicator,fy2015q4,fy2016q2,fy2016q4,
           fy2017q1,fy2017q2,fy2017q3,fy2017q4) %>%
    group_by(indicator) %>%
    summarise(fy2015q4 = sum(fy2015q4, na.rm = T),
              fy2016q2 = sum(fy2016q2, na.rm = T),
              fy2016q4 = sum(fy2016q4, na.rm = T),
              fy2017q1 = sum(fy2017q1, na.rm = T),
              fy2017q2 = sum(fy2017q2, na.rm = T),
              fy2017q3 = sum(fy2017q3,na.rm = T),
              fy2017q4 = sum(fy2017q4,na.rm = T)) %>%
    gather("quarter","results",2:8) %>%
    spread(indicator,results) %>%
    mutate(HTS_YIELD = round(HTS_TST_POS/HTS_TST,3),
           PMTCT_STAT_YIELD = round(PMTCT_STAT_POS/ PMTCT_STAT,3)) %>%
    select(quarter,HTS_YIELD,PMTCT_STAT_YIELD) %>%
    gather("indicator","results",2:3) %>%
    ggplot(aes(x=factor(quarter),y=results, group =1))+
    geom_line()+
    geom_point()+
    geom_text(aes( y = results,
                   label=paste0(results*100,"%")),size = 4,vjust =-1.1 )+
    facet_wrap(~indicator)+
    labs(y="% Yield", 
         x="",
         fill="",
         title="HTS_TST Yield Trend from FY15 to FY17",
         subtitle="",
         caption="Data source: ICPI FactView SitexIM")+
    scale_y_continuous(limits =c(0,0.05),labels = percent_format(),expand = c(0, 0))+
    expand_limits(x = 0, y = 0)+
    theme(axis.text.x = element_text(size = 10,face="bold"),
          axis.text.y = element_text(size = 13,face= "bold"), 
          panel.background = element_blank(),
          axis.line=element_line(),
          axis.title.x = element_text(size = 8),
          plot.title = element_text(size = 18),
          plot.subtitle  = element_text(size = 12))




## Linkage --------------------------------------------------------------------------------------------

## Tx_Net_New -----------------------------------------------------------------------------------------

## Coverage Indicator ---------------------------------------------------------------------------------
    # PMTCT_ART
    # TX_RET
    # TX_PVLS

## Render Markdown ------------------------------------------------------------------------------------

rmarkdown::render("./rmds/ou_level_report.Rmd",output_format = "github_document",output_dir="./rmds")
