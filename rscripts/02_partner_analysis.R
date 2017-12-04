#########################################################################################################
#Quaterly Progress Report                                            
# APR17                                                                          
# Mayer  Antoine, CDC Haiti                                                                 
# Purpose : Create visual for  partner performance monitoring                                            
# Update: 12/2/2017                                                             
# https://github.com/mayerantoine/QuarterlyProgress                                                                                 
#                                                                               
#                                                                               
#########################################################################################################


library(tidyverse)
library(formattable)
library(knitr)
library(scales)

rm(list = ls())
## IMPORT STE By IM Factview ------------------------------------------------------------------------------

source("./rscripts/00_import_factview_data.R")
site_im <- import_factview_site_im()



old_mechanism <- c("FOSREF","PIH","CDS","TBD2","Catholic Medical Mission Board","University of Maryland","Health Service Delivery","GHESKIO 0545","POZ","GHESKIO 541","ITECH 1331","University of Miami","Dedup","ITECH 549")


key_indicators <- c("HTS_TST","HTS_TST_POS","TX_NEW","PMTCT_ART","PMTCT_EID","PMTCT_STAT",
                    "PMTCT_STAT_POS","TB_ART","TB_STAT","TB_STAT_POS","PMTCT_EID_POS")

#key_indicators <- c("HTS_TST","HTS_TST_POS","TX_NEW","KP_PREV","PMTCT_ART","PMTCT_EID","PMTCT_STAT",
#                    "PMTCT_STAT_POS","TB_ART","TB_STAT","TB_STAT_POS","PMTCT_EID_POS",
#                      "PMTCT_EID_Less_Equal_Two_Months",
#                    "PMTCT_EID_Two_Twelve_Months","PMTCT_STAT_KnownatEntry_POSITIVE",
#                    "PMTCT_STAT_NewlyIdentified_POSITIVE")

key_cummulative_indicator <- c("TX_CURR", "OVC_SERV","TX_PVLS","TX_RET","OVC_HIVSTAT")

## Partner Data ----------------------------------------------------------------------------------------------------

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


## Partner dataset for cummulative, need to calculate APR results
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

## shorten the name of the mechanism for better display this only apply for Haiti
partner_data_final$mechanism <- NA
partner_data_final$mechanism <- partner_data_final$implementingmechanismname
partner_data_final[partner_data_final$implementingmechanismname ==
                       "Catholic Medical Mission Board",]$mechanism <- "CMMB"
partner_data_final[partner_data_final$implementingmechanismname == 
                    "BEST (Byen en ak Sante Timoun)",]$mechanism <- "BEST"
partner_data_final[partner_data_final$implementingmechanismname == 
                       "SSQH Nord (Services de Sant? de Qualit? pour Ha?ti)",]$mechanism  <- "SSQH"
partner_data_final[partner_data_final$implementingmechanismname == 
                       "SSQH Centre/Sud (Services de Sant? de Qualit? pour Ha?ti)",]$mechanism  <- "SSQH"
partner_data_final[partner_data_final$implementingmechanismname == 
                       "HTW (Health Through Walls)",]$mechanism  <- "HTW"
partner_data_final[partner_data_final$implementingmechanismname == 
                       "MSPP/UGP (National AIDS Strategic Plan)",]$mechanism  <- "MSPP/UGP"
#partner_data_final[partner_data_final$implementingmechanismname == 
#                       "GHESKIO 1924",]$mechanism  <- "GHESKIO"
#partner_data_final[partner_data_final$implementingmechanismname == 
#                       "GHESKIO 1969",]$mechanism  <- "GHESKIO"

#write_csv(partner_data_final,"./processed_data/partner_final_data.csv")


## Patient Initiated not Linked by IM --------------------------------------------------------------------------

linkage <- partner_data_final %>%
    filter(!(implementingmechanismname %in% old_mechanism)) %>%
    filter(indicator %in% c("HTS_TST_POS","TX_NEW")) %>%
    select(mechanism,indicator,fy2017Cum) %>%
    group_by(mechanism,indicator) %>%
    summarise(fy2017Cum = sum(fy2017Cum, na.rm = T)) %>%
    spread(indicator,fy2017Cum) %>%
    mutate(Patient_Not_Intiated = ifelse(HTS_TST_POS - TX_NEW < 0, 0,HTS_TST_POS - TX_NEW),
           Linkage = ifelse(TX_NEW > 0, TX_NEW/HTS_TST_POS, 0 ))

write_csv(linkage, "./processed_data/linkage.csv")
 
# Patient not initiated 
 linkage %>%
    filter(!is.na(Patient_Not_Intiated)) %>%
    ggplot(mapping = aes(x=reorder(mechanism,Patient_Not_Intiated),y=Patient_Not_Intiated))+
        geom_bar(stat = "identity",fill= "#FF6633", width = 0.8)+
        geom_text(aes(label=comma(Patient_Not_Intiated)),size=4.5, hjust = 1.5, colour = "white") +
        labs(y="", 
             x="",
             fill="",
             title= "Patient identified but not initiated",
             subtitle="",
             caption= "Data source:ICPI FactView SitexIM Haiti")+
        coord_flip()+
      scale_y_continuous( limits = c(0, 1100),expand = c(0, 0))+
        expand_limits(x = 0, y = 0)+
        scale_x_discrete(expand = c(0, 0))+
        theme(axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 13,face= "bold"), 
              panel.background = element_blank(),
              axis.line=element_line(),
              axis.title.x = element_text(size = 10),
              plot.title = element_text(size = 18),
              plot.subtitle = element_text(size = 12),
              axis.ticks.y = element_blank())  


### Patient  Linkage by IM 
linkage %>%
    filter(!is.na(Patient_Not_Intiated)) %>%
    ggplot(mapping = aes(x=reorder(mechanism,Linkage*100),y=Linkage))+
    geom_bar(stat = "identity",fill= "#FF6633", width = 0.8)+
    geom_text(aes(label=paste0(sprintf("%.0f", Linkage*100),"%")),size=4.5, hjust = 1.5, colour = "white") +
    labs(y="", 
         x="",
         fill="",
         title= "Linkage to Enrollement by Partner",
         subtitle="",
         caption= "Data source:ICPI FactView SitexIM Haiti")+
    coord_flip()+
   # scale_y_continuous(limits = c(0,100), labels = percent_format(), expand = c(0, 0))+
    expand_limits(x = 0, y = 0)+
    scale_x_discrete(expand = c(0, 0))+
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 13,face= "bold"), 
          panel.background = element_blank(),
          axis.line=element_line(),
          axis.title.x = element_text(size = 10),
          plot.title = element_text(size = 18),
          plot.subtitle = element_text(size = 12),
          axis.ticks.y = element_blank())

#Patient linkage disagg
tx_new_disagg <- site_im %>%
    filter(snu1 != "_Military Haiti") %>%
    filter(indicator == "TX_NEW") %>%
    filter(standardizeddisaggregate == "MostCompleteAgeDisagg") %>%
    filter(indicatortype == "DSD") %>% 
    filter(numeratordenom == "N") %>%
    group_by(age,sex) %>%
    summarise(TX_NEW = sum(fy2017apr,na.rm = T)) %>%
    unite(category,age,sex,sep = " ")


hts_tst_pos <- site_im %>%
    filter(snu1 != "_Military Haiti") %>%
    filter(indicator == "HTS_TST_POS") %>%
    filter(standardizeddisaggregate == "Modality/MostCompleteAgeDisagg") %>%
    filter(indicatortype == "DSD") %>% 
    filter(numeratordenom == "N") %>%
    group_by(age,sex) %>%
    summarise(HTS_TST_POS = sum(fy2017apr,na.rm = T)) %>%
    unite(category,age,sex, sep =" ")


net_new_fy2017q4 <- site_im %>%
    filter(snu1 != "_Military Haiti") %>%
    filter(indicator == "TX_CURR") %>%
    filter(standardizeddisaggregate == "MostCompleteAgeDisagg") %>%
    filter(indicatortype == "DSD") %>% 
    filter(numeratordenom == "N") %>%
    group_by(age,sex) %>%
    summarise(fy2016q4 = sum(fy2016q4,na.rm = T),
              fy2017q4 = sum(fy2017q4,na.rm = T)) %>%
    mutate( net_new_fy2017q4 = fy2017q4 - fy2016q4) %>%
    unite(category,age, sex,sep =" ")


not_intiated <- left_join(hts_tst_pos,tx_new_disagg, by = c("category" ="category"))
not_intiated <- left_join(not_intiated,net_new_fy2017q4, by = c("category" ="category"))
not_intiated <- not_intiated %>%
    mutate(not_intiated = HTS_TST_POS - TX_NEW,
           Attrition =TX_NEW -net_new_fy2017q4 )

write_csv(not_intiated,"not_initiated.csv")


## Partner Performance Results ---------------------------------------------------------------------------------


# Partner Performance: Results vs Targets = Performance
partner_performance <- partner_data_final %>%
    filter(!(implementingmechanismname %in% old_mechanism)) %>%
     select(mechanism,indicator,fy2017Cum,fy2017_targets) %>%
    group_by(mechanism,indicator) %>%
    summarise(fy2017Cum = sum(fy2017Cum, na.rm = T),
              fy2017_targets= sum(fy2017_targets, na.rm = T)) %>%
    mutate( fy2017Perf = ifelse(fy2017_targets > 0,round((fy2017Cum/fy2017_targets),2), 0)) %>%
    as.data.frame()



# summarise tx_curr by partner to calculate tx_net_new
tx_curr_partner <- partner_data_final %>%
    filter(indicator=="TX_CURR") %>%
    select(mechanism,indicator,fy2016apr,fy2017Cum,fy2017_targets) %>%
    group_by(mechanism,indicator) %>%
    summarise(fy2016apr = sum(fy2016apr, na.rm = T),
              fy2017Cum = sum(fy2017Cum,na.rm = T),
              fy2017_targets = sum(fy2017_targets,na.rm = T))

## Calculate net_new  ?? note net_new target to be fix because partners names have changed
tx_net_new_partner <- data_frame(mechanism = tx_curr_partner$mechanism,
                                 indicator = c("TX_NET_NEW"),
                                 fy2017Cum = c(tx_curr_partner$fy2017Cum - tx_curr_partner$fy2016apr),
                                 fy2017_targets = c(tx_curr_partner$fy2017_targets - tx_curr_partner$fy2016apr),
                                 fy2017Perf = ifelse(fy2017_targets > 0,round((fy2017Cum/fy2017_targets)*100,1), 0))

tx_net_new_partner <- tx_net_new_partner %>% 
                        select(mechanism,indicator,fy2017Perf) %>%
                        as.data.frame()

#partner_performance <- rbind(partner_performance_key,partner_performance_cum,tx_net_new_partner)

partner_performance_sp <- partner_performance %>%
        select(mechanism,indicator,fy2017Perf) %>%
        spread(mechanism,round(fy2017Perf,1))

write_csv(partner_performance_sp,"./processed_data/partner_performance_sp_2.csv")

## partner_performance

partner_performance %>%
    filter(fy2017Perf > 0) %>%
    filter(mechanism == "GHESKIO 1969") %>%
    ggplot(mapping = aes(x=reorder(indicator,fy2017Perf),y=fy2017Perf))+
    geom_hline(yintercept = 0.85)+
    geom_bar(stat = "identity",fill= "#009999", width = 0.8)+
    geom_text(aes(label=paste0(sprintf("%.0f", fy2017Perf*100),"%")),size=4.8,
              position=position_stack(vjust=0.5), colour="white") +
    labs(y="", 
         x="",
         fill="",
         title=paste0("APR17 GHESKIO 1969"," ", "% achievement "),
         subtitle="",
         caption= "Data source:ICPI FactView SitexIM Haiti")+
    coord_flip()+
    scale_y_continuous(labels = percent_format(),expand = c(0, 0))+
    expand_limits(x = 0, y = 0)+
    scale_x_discrete(expand = c(0, 0))+
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 13,face= "bold"), 
          panel.background = element_blank(),
          axis.line=element_line(),
          axis.title.x = element_text(size = 10),
          plot.title = element_text(size = 18),
          plot.subtitle = element_text(size = 12),
          axis.ticks.y = element_blank()) 


## Generate Visuals from Partner Performance Data ---------------------------------------------------------------------------------


fill_pall <- c("BEST" = "#FFFF99","CDS 1528"="#B15928","CMMB 1970"="#6A3D9A",
               "FOSREF 1925"="#CAB2D6","GHESKIO 1924"="#FF7F00","GHESKIO 1969"="#999966","HTW"="#E31A1C",
               "Linkages"="#006666","MSPP/UGP"="#1F78B4","PIH 1926"="#33A02C","SSQH Nord"="#B2DF8A","SSQH Sud"="#A6CEE3")

partner_performance %>%
    filter(fy2017Perf > 0) %>%
    filter(indicator == "TX_CURR") %>%
    ggplot(mapping = aes(x=reorder(mechanism,fy2017Perf),y=fy2017Perf))+
    geom_bar(stat = "identity",fill= "#009999", width = 0.8)+
    geom_text(aes(label=paste0(sprintf("%.0f", fy2017Perf*100),"%")),size=4.8,
              position=position_stack(vjust=0.5), colour="white") +
    labs(y="", 
         x="",
         fill="",
         title=paste0("APR17 TX_CURR"," ", "Partner Performance"),
         subtitle="Rank from highest to lowest % of achievement",
         caption= "Data source:ICPI FactView SitexIM Haiti")+
    coord_flip()+
    scale_y_continuous(labels = percent_format(),expand = c(0, 0))+
    expand_limits(x = 0, y = 0)+
    scale_x_discrete(expand = c(0, 0))+
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 13,face= "bold"), 
          panel.background = element_blank(),
          axis.line=element_line(),
          axis.title.x = element_text(size = 10),
          plot.title = element_text(size = 18),
          plot.subtitle = element_text(size = 12),
          axis.ticks.y = element_blank())  

partner_performance %>%
    filter(indicator == "TX_NEW", fy2017Cum > 0) %>%
    ggplot(mapping = aes(x=reorder(mechanism,fy2017Cum),y=fy2017Cum))+
    geom_bar(stat = "identity",fill= "#1F78B4", width = 0.8)+
    geom_text(aes(label=paste0(comma(fy2017Cum))),size=4.5,
              position=position_stack(vjust=0.5), colour="white") +
    geom_errorbar(aes(ymin=fy2017_targets,ymax=fy2017_targets),width=1, size=1.5, color="#FF7F00")+
    labs(y="", 
         x="",
         fill="",
         title=paste0("APR17 TX_NEW"," "," Partner Results vs Targets"),
         subtitle="Rank from highest to lowest results",
         caption="Data source:ICPI FactView SitexIM Haiti")+
    coord_flip()+
    scale_y_continuous(expand = c(0, 0))+
    expand_limits(x = 0, y = 0)+
    scale_x_discrete(expand = c(0, 0))+
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 13,face= "bold"), 
          panel.background = element_blank(),
          axis.line=element_line(),
          axis.title.x = element_text(size = 10),
          plot.title = element_text(size = 18),
          plot.subtitle  = element_text(size = 12),
          axis.ticks.y = element_blank())  


## remove them because they have no target, no performance
partner_performance <- partner_performance %>%
                        filter(!(indicator %in% c("TB_STAT_POS","OVC_HIVSTAT")))
cascade_indicator <- unique(partner_performance$indicator)


generatePerformancePlot <- function(cascade_indicator,partner_performance) {
    
fill_pall <- c("BEST" = "#FFFF99","CDS 1528"="#B15928","CMMB 1970"="#6A3D9A",
                   "FOSREF 1925"="#CAB2D6","GHESKIO 1924"="#FF7F00","GHESKIO 1969"="#999966",
               "HTW"="#E31A1C","Linkages"="#006666","MSPP/UGP"="#1F78B4","PIH 1926"="#33A02C",
               "SSQH Nord"="#B2DF8A","SSQH Sud"="#A6CEE3")
    
    
   for (i in seq_along(cascade_indicator)) {
    
plot1  <- partner_performance %>%
           filter(fy2017Perf > 0) %>%
           filter(indicator == cascade_indicator[i]) %>%
           ggplot(mapping = aes(x=reorder(mechanism,fy2017Perf),y=fy2017Perf))+
           geom_bar(stat = "identity",fill= "#009999", width = 0.8)+
           geom_text(aes(label=paste0(sprintf("%.0f", fy2017Perf*100),"%")),size=4.8,
                     position=position_stack(vjust=0.5), colour="white") +
           labs(y="", 
                x="",
                fill="",
                title=paste0("APR17 ",cascade_indicator[i]," ", "Partner Performance"),
                subtitle="Rank from highest to lowest % of achievement",
                caption="Data source:ICPI FactView SitexIM Haiti")+
           coord_flip()+
           scale_y_continuous(labels = percent_format(),expand = c(0, 0))+
           expand_limits(x = 0, y = 0)+
           scale_x_discrete(expand = c(0, 0))+
           theme(axis.text.x = element_text(size = 12),
                 axis.text.y = element_text(size = 13,face= "bold"), 
                 panel.background = element_blank(),
                 axis.line=element_line(),
                 axis.title.x = element_text(size = 10),
                 plot.title = element_text(size = 18),
                 plot.subtitle = element_text(size = 12),
                 axis.ticks.y = element_blank())  

 plot2 <- partner_performance %>%
     filter(indicator == cascade_indicator[i], fy2017Cum > 0) %>%
     ggplot(mapping = aes(x=reorder(mechanism,fy2017Cum),y=fy2017Cum))+
     geom_bar(stat = "identity",fill= "#1F78B4", width = 0.8)+
     geom_text(aes(label=paste0(sprintf("%.0f", fy2017Cum))),size=4.8,
               position=position_stack(vjust=0.5), colour="white") +
     geom_errorbar(aes(ymin=fy2017_targets,ymax=fy2017_targets),width=1, size=1.5, color="#FF7F00")+
     labs(y="", 
          x="",
          fill="",
          title=paste0("APR17 ",cascade_indicator[i]," "," Partner Results vs Targets"),
          subtitle="Rank from highest to lowest Results",
          caption="Data source: ICPI FactView SitexIM Haiti")+
     coord_flip()+
     scale_y_continuous(expand = c(0, 0))+
     expand_limits(x = 0, y = 0)+
     scale_x_discrete(expand = c(0, 0))+
     theme(axis.text.x = element_text(size = 12),
           axis.text.y = element_text(size = 13,face= "bold"), 
           panel.background = element_blank(),
           axis.line=element_line(),
           axis.title.x = element_text(size = 10),
           plot.title = element_text(size = 18),
           plot.subtitle  = element_text(size = 12),
           axis.ticks.y = element_blank())   

dir.create(file.path("Figs/achievements"),showWarnings = FALSE)
dir.create(file.path("Figs/results"),showWarnings = FALSE)
 
 # save plots as .png
 ggsave(plot1, filename =paste("performance",cascade_indicator[i], ".png", sep=''), path = "Figs/achievements/", scale=2)
 ggsave(plot2, filename =paste("results",cascade_indicator[i], ".png", sep=''), path = "Figs/results/", scale=2)
 
 
 ## remove ticks
 ## place x axis up
 ## align y-axis label left
 ## Remove zero results IM
 ## Review font weight , and titles
 
 
## print(plot1)
## print(plot2)
    }
}


generatePerformancePlot(cascade_indicator,partner_performance)


## Bubble Charts Partner Performance ##################################################
## Bubble charts x = Target,y =Performance, size = Results, color = Partner

partner_performance %>%
    filter(indicator == "HTS_TST_POS") %>%
    ggplot(mapping = aes(x=fy2017_targets,y=round(fy2017Perf,0),size = fy2017Cum,fill= mechanism))+
  #  geom_jitter(aes(col=mechanism,size =fy2017Cum ))+
    geom_jitter(shape = 21)+
   geom_text(aes(label=mechanism,size=100),hjust = 0.2, vjust=-2.5) +
    labs(y="APR17 % achievements", 
         x="FY2017 Targets",
         size = "APR17 Results",
         fill="",
         title=paste0("HTS_TST_POS"," ", "Performance"),
         subtitle="",
         caption="Data source:ICPI FactView SitexIM Haiti")+
   scale_size_area(max_size = 22)+
    scale_fill_manual(values = fill_pall,guide = FALSE)+
    scale_y_continuous(labels = percent_format())+
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10), 
          panel.background = element_blank(),
          axis.line=element_line(),
          axis.title.x = element_text(size = 10),
          plot.title = element_text(size = 16),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.key.size = unit(1, "cm") )  



## HTS_TST  vs HTS_YIELD ##################################################    

hts_yield <- site_im %>%
    filter(snu1 != "_Military Haiti") %>%
    filter(indicator %in% c("HTS_TST","HTS_TST_POS")) %>%
    filter(standardizeddisaggregate == "Total Numerator") %>%
    filter(indicatortype == "DSD") %>% 
    filter(numeratordenom == "N") %>%
    select(implementingmechanismname,fundingagency,psnu,facility,community,indicator,fy2015q2,fy2015q3,fy2015q4,
           fy2016q1,fy2016q2,fy2016q3,fy2016q4,
           fy2017q1,fy2017q2,fy2017q3,fy2017q4,fy2017_targets) %>%
    group_by(indicator) %>%
    summarise(fy2015q2 = sum(fy2015q2, na.rm = T),
              fy2015q3 = sum(fy2015q3, na.rm = T),
              fy2015q4 = sum(fy2015q4, na.rm = T),
              fy2016q1 = sum(fy2016q1, na.rm = T),
              fy2016q2 = sum(fy2016q2, na.rm = T),
              fy2016q3 = sum(fy2016q3, na.rm = T),
              fy2016q4 = sum(fy2016q4, na.rm = T),
              fy2017q1 = sum(fy2017q1, na.rm = T),
              fy2017q2 = sum(fy2017q2, na.rm = T),
              fy2017q3 = sum(fy2017q3,na.rm = T),
              fy2017q4 = sum(fy2017q4,na.rm = T)) %>%
    gather("fiscal_year","results",2:12) %>%
    spread(indicator,results) %>%
    mutate(HTS_YIELD = round(HTS_TST_POS/HTS_TST,3)) %>%
    ggplot(aes(x= fiscal_year))+
    geom_bar(aes(fiscal_year,HTS_TST), stat = "identity", fill = "#0072B2",width  = 0.7)+
    geom_line(stat = "identity", aes(y=round(HTS_TST_POS/HTS_TST,3)),colour="#0072B2",size =1.5)+
    geom_text(aes( y = HTS_TST,
                   label=paste0(sprintf("%.0f",round(HTS_TST,0)))),size = 4,vjust =-1.1 )+
    labs(y="", 
         x="",
         fill="",
         title="HTS_TST Trend from FY15 to FY17",
         subtitle="",
         caption="Data source: ICPI FactView SitexIM Haiti")+
    scale_y_continuous(sec.axis = sec_axis(~.*HTS_TST_POS*100),breaks = seq(0,600000,100000),limits =c(0,600000),labels =comma,expand = c(0, 0))+
    expand_limits(x = 0, y = 0)+
    theme(axis.text.x = element_text(size = 10,face="bold"),
          axis.text.y = element_text(size = 13,face= "bold"), 
          panel.background = element_blank(),
          axis.line=element_line(),
          axis.title.x = element_text(size = 8),
          plot.title = element_text(size = 18),
          plot.subtitle  = element_text(size = 12)) 

write_csv(hts_yield,"hts_yield.csv")


## Yield, Linkage , Retention Rate by IM  ###################################



barriers <-  site_im %>%
    filter(snu1 != "_Military Haiti") %>%
    filter(indicator %in% c("HTS_TST","HTS_TST_POS","TX_NEW","TX_RET")) %>%
    filter(standardizeddisaggregate == "Total Numerator") %>%
    filter(indicatortype == "DSD") %>% 
    filter( numeratordenom == "N") %>%
    group_by(indicator) %>%
    summarise(fy2017q1 = sum(fy2017q1,na.rm = T) ,
              fy2017q2 = sum(fy2017q2,na.rm = T),
              fy2017q3 = sum(fy2017q3,na.rm = T),
              fy2017q4 = sum(fy2017q4,na.rm = T)) %>%
    mutate(fy2017Cum = fy2017q1+fy2017q2+fy2017q4+fy2017q3)


tx_ret_d <- site_im %>%
    filter(snu1 != "_Military Haiti") %>%
    filter(indicator %in% c("TX_RET")) %>%
    filter(standardizeddisaggregate == "Total Denominator") %>%
    filter(indicatortype == "DSD") %>% 
    filter( numeratordenom == "D") %>%
    group_by(indicator) %>%
    summarise(fy2017q1 = sum(fy2017q1,na.rm = T) ,
              fy2017q2 = sum(fy2017q2,na.rm = T),
              fy2017q3 = sum(fy2017q3,na.rm = T),
              fy2017q4 = sum(fy2017q4,na.rm = T)) %>%
    mutate(fy2017Cum = fy2017q1+fy2017q2+fy2017q4+fy2017q3)



tx_ret_d <- data_frame(indicator = c("TX_RET_D"),
                       fy2017q1 = tx_ret_d$fy2017q1,
                       fy2017q2 = tx_ret_d$fy2017q2,
                       fy2017q3 = tx_ret_d$fy2017q3,
                       fy2017q4 = tx_ret_d$fy2017q4,
                       fy2017Cum = tx_ret_d$fy2017Cum)

barriers <- rbind(barriers,tx_ret_d)

barriers %>%
    select(indicator,fy2017Cum) %>%
    spread(indicator,fy2017Cum) %>%
    mutate(HTS_YIELD = HTS_TST_POS/HTS_TST,
           Linkage = TX_NEW/ HTS_TST_POS,
           Retention = TX_RET/TX_RET_D) %>%
    ggplot(aes())


## Tx_net_NEW overall -----------------------------------------------------------------------------------------------

# summarise tx_curr to calculate tx_net_new trend
 site_im %>%
    filter(snu1 != "_Military Haiti") %>%
    filter(indicator == "TX_CURR") %>%
    filter(disaggregate == "Total Numerator") %>%
    filter(indicatortype == "DSD") %>% 
    filter(numeratordenom == "N") %>%
    select(indicator,fy2015q3,fy2015apr,fy2016q1,fy2016q2,fy2016q3,fy2016q4,fy2017q1,fy2017q2,fy2017q3,fy2017q4) %>%
    group_by(indicator) %>%
    summarise(fy2015q3 = sum(fy2015q3,na.rm = T),
              fy2015q4 = sum(fy2015apr,na.rm = T),
              fy2016q2 = sum(fy2016q2,na.rm = T),
              fy2016q4 = sum(fy2016q4,na.rm = T),
              fy2017q1 = sum(fy2017q1, na.rm = T),
              fy2017q2 = sum(fy2017q2, na.rm = T),
              fy2017q3 = sum(fy2017q3,na.rm = T),
              fy2017q4 = sum(fy2017q4,na.rm = T)) %>%
    mutate(net_new_fy2015q4 = fy2015q4 -  fy2015q3,
           net_new_fy2016q2 = fy2016q2 - fy2015q4,
           net_new_fy2016q4 = fy2016q4 - fy2016q2,
           net_new_fy2017q1 = fy2017q1 - fy2016q4,
           net_new_fy2017q2 = fy2017q2 - fy2017q1,
           net_new_fy2017q3 = fy2017q3 - fy2017q2,
           net_new_fy2017q4 = fy2017q4 - fy2017q3) %>%
    mutate(net_new_fy2017q1q2 =net_new_fy2017q1+net_new_fy2017q2,
           net_new_fy2017q3q4 = net_new_fy2017q3+net_new_fy2017q4) %>%
    select(net_new_fy2016q2,net_new_fy2016q4,net_new_fy2017q1q2,net_new_fy2017q3q4) %>%
    gather("quarter","tx_net_new",1:4) %>%
    ggplot(aes(quarter,tx_net_new))+
    geom_bar(stat = "identity", fill = "#0072B2",width  = 0.7)+
    geom_text(aes( y = tx_net_new,
                   label=paste0(sprintf("%.0f",round(tx_net_new,0)))),size = 4,vjust =-1.1 )+
    labs(y="# new people enroled on ART", 
         x="",
         fill="",
         title="TX_NET_NEW Trend from FY16 to FY17",
         subtitle="",
         caption="Data source: ICPI FactView SitexIM Haiti")+
    scale_y_continuous(breaks = seq(0,12000,1000),limits =c(0,12000),labels =comma,expand = c(0, 0))+
    scale_x_discrete(labels =c("FY16 Q1+Q2","FY16 Q3+Q4","FY17 Q1+Q2","FY17 Q3+Q4"))+
    expand_limits(x = 0, y = 0)+
    theme(axis.text.x = element_text(size = 10,face="bold"),
          axis.text.y = element_text(size = 13,face= "bold"), 
          panel.background = element_blank(),
          axis.line=element_line(),
          axis.title.x = element_text(size = 8),
          plot.title = element_text(size = 18),
          plot.subtitle  = element_text(size = 12))


## TX_net_NEW  by Partner ------------------------------------------------------------------------------------

partner_mapping <- read_csv("data/fy17_partner_mapping.csv")
names(partner_mapping) <- c("partner_facility","mechanism")

# summarise tx_curr to calculate tx_net_new By Partner
tx_curr_facility <- partner_data_final %>%
    filter(indicator=="TX_CURR") %>%
    #filter(!(implementingmechanismname %in% c("BEST (Byen en ak Sante Timoun)","Dedup",""))) %>%
    select(facility,fy2016apr,fy2017q1,fy2017q2,fy2017q3,fy2017q4,
           fy2017Cum,fy2017_targets) %>%
    group_by(facility) %>%
    summarise(fy2016apr = sum(fy2016apr,na.rm = T),
              fy2017q1 = sum(fy2017q1, na.rm = T),
              fy2017q2 = sum(fy2017q2, na.rm = T),
              fy2017q3 = sum(fy2017q3,na.rm = T),
              fy2017q4 = sum(fy2017q4,na.rm = T),
              fy2017Cum = sum(fy2017Cum, na.rm = T),
              fy2017_targets = sum(fy2017_targets,na.rm = T)) 

tx_curr_facility <- tx_curr_facility %>%
                     left_join(partner_mapping,by = c("facility" = "partner_facility"))



tx_curr_facility <- tx_curr_facility[!is.na(tx_curr_facility$mechanism),]
#tx_net_new_partner$mechanism_name <- NA
tx_curr_facility$mechanism_name <- tx_curr_facility$mechanism
tx_curr_facility[tx_curr_facility$mechanism == 
                       "SSQH Nord (Services de Sant? de Qualit? pour Ha?ti)",]$mechanism_name  <- "SSQH"
tx_curr_facility[tx_curr_facility$mechanism == 
                       "SSQH Centre/Sud (Services de Sant? de Qualit? pour Ha?ti)",]$mechanism_name  <- "SSQH"
tx_curr_facility[tx_curr_facility$mechanism == 
                       "HTW (Health Through Walls)",]$mechanism_name  <- "HTW"
tx_curr_facility[tx_curr_facility$mechanism == 
                       "MSPP/UGP (National AIDS Strategic Plan)",]$mechanism_name  <- "MSPP/UGP"
tx_curr_facility[tx_curr_facility$mechanism == 
                     "GHESKIO 1924",]$mechanism_name  <- "GHESKIO"
tx_curr_facility[tx_curr_facility$mechanism == 
                     "GHESKIO 1969",]$mechanism_name  <- "GHESKIO"

tx_net_new_partner <- tx_curr_facility %>%
    group_by(mechanism_name) %>%
    summarise(fy2016apr = sum(fy2016apr,na.rm = T),
              fy2017q1 = sum(fy2017q1, na.rm = T),
              fy2017q2 = sum(fy2017q2, na.rm = T),
              fy2017q3 = sum(fy2017q3,na.rm = T),
              fy2017q4 = sum(fy2017q4,na.rm = T),
              fy2017Cum = sum(fy2017Cum, na.rm = T),
              fy2017_targets = sum(fy2017_targets,na.rm = T)) %>%
    mutate(tx_net_new_q1 = fy2017q1 - fy2016apr,
           tx_net_new_q2 = fy2017q2 - fy2017q1,
           tx_net_new_q3 = fy2017q3 - fy2017q2,
           tx_net_new_q4 = fy2017q4 - fy2017q3,
           tx_net_new_cum = fy2017q4 - fy2016apr,
           tx_net_new_target = fy2017_targets - fy2016apr) %>%
    select(mechanism_name,tx_net_new_q1,tx_net_new_q2,tx_net_new_q3,tx_net_new_q4,tx_net_new_cum,tx_net_new_target) %>%
    as.data.frame()



# TX_NET_NEW Partner Performance
tx_net_new_partner %>%
    filter(!is.na(mechanism_name)) %>%
    ggplot(mapping = aes(x=reorder(mechanism_name,tx_net_new_cum),y=tx_net_new_cum))+
    geom_bar(stat = "identity",fill= "#009999", width = 0.8)+
    geom_text(aes(label=paste0(sprintf("%.0f", tx_net_new_cum))),size=4.8, colour="white", hjust = 1.3) +
   # geom_errorbar(aes(ymin=tx_net_new_target,ymax=tx_net_new_target),width=1, size=1.5, color="#FF7F00") +
    labs(y="", 
         x="",
         fill="",
         title=paste0("APR17 TX_NET_NEW"," ", "by Partner Results vs Targets "),
         subtitle="",
         caption= "Data source:ICPI FactView SitexIM Haiti")+
    coord_flip()+
    scale_y_continuous(expand = c(0, 0))+
    expand_limits(x = 0, y = 0)+
    scale_x_discrete(expand = c(0, 0))+
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 13,face= "bold"), 
          panel.background = element_blank(),
          axis.line=element_line(),
          axis.title.x = element_text(size = 10),
          plot.title = element_text(size = 18),
          plot.subtitle = element_text(size = 12),
          axis.ticks.y = element_blank()) 

write_csv(tx_net_new_partner,"./processed_data/tx_net_new_partner_2.csv")

###############


  

    
    
## net new by partner
## trend by partner ~ facet
## Why MSPP and PIH are low performer ? why they did not reach their target ?
    ## which sites are holding them back ?
    ## what district are they working in ?
    ## can you use maps ??
## if we merge SSQH ? GHESKIO ?? Present the Profile of GHESKIO, PIH, UGP, SSQH
## IMs Profile : District, Volume on Tx, nb Sites, achievements, trends, Barriers (YIELD , Enrollement, NET_NEW), spending,






