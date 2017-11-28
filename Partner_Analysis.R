#################################################################################
# Partner Performance Report Script                                             #
# APR17                                                                         # 
# Mayer  Antoine                                                                # 
# Purpose : Import FactView Site by IM data and create visual for Partner       # 
#           Performance Monitoring                                               #
# Date : 11/22/2017                                                             #
#                                                                               #
#                                                                               #
#                                                                               #
#################################################################################


library(tidyverse)
library(formattable)
library(knitr)
library(scales)

rm(list = ls())

################# IMPORT STE By IM Factview ########################################

site_im <- read_tsv("data/ICPI_FactView_Site_IM_Haiti_20171115_v1_1.txt")
names(site_im) <- tolower(names(site_im))
caption <- c("Data source:ICPI FactView SitexIM Haiti")

old_mechanism <- c("FOSREF","PIH","CDS","TBD2","Catholic Medical Mission Board","University of Maryland","Health Service Delivery","GHESKIO 0545","POZ","GHESKIO 541","ITECH 1331","University of Miami","Dedup","ITECH 549")


key_indicators <- c("HTS_TST","HTS_TST_POS","TX_NEW","PMTCT_ART","PMTCT_EID","PMTCT_STAT",
                    "PMTCT_STAT_POS","TB_ART","TB_STAT","TB_STAT_POS","PMTCT_EID_POS")

#key_indicators <- c("HTS_TST","HTS_TST_POS","TX_NEW","KP_PREV","PMTCT_ART","PMTCT_EID","PMTCT_STAT",
#                    "PMTCT_STAT_POS","TB_ART","TB_STAT","TB_STAT_POS","PMTCT_EID_POS","PMTCT_EID_Less_Equal_Two_Months",
#                    "PMTCT_EID_Two_Twelve_Months","PMTCT_STAT_KnownatEntry_POSITIVE",
#                    "PMTCT_STAT_NewlyIdentified_POSITIVE")

key_cummulative_indicator <- c("TX_CURR", "OVC_SERV","TX_PVLS","TX_RET","OVC_HIVSTAT")
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

## shorten the name of the mechanism for better display this only apply for Haiti
partner_data_final$mechanism <- NA
partner_data_final$mechanism <- partner_data_final$implementingmechanismname
partner_data_final[partner_data_final$implementingmechanismname ==
                       "Catholic Medical Mission Board",]$mechanism <- "CMMB"
partner_data_final[partner_data_final$implementingmechanismname == 
                    "BEST (Byen en ak Sante Timoun)",]$mechanism <- "BEST"
partner_data_final[partner_data_final$implementingmechanismname == 
                       "SSQH Nord (Services de Sant? de Qualit? pour Ha?ti)",]$mechanism  <- "SSQH Nord"
partner_data_final[partner_data_final$implementingmechanismname == 
                       "SSQH Centre/Sud (Services de Sant? de Qualit? pour Ha?ti)",]$mechanism  <- "SSQH Sud"
partner_data_final[partner_data_final$implementingmechanismname == 
                       "HTW (Health Through Walls)",]$mechanism  <- "HTW"
partner_data_final[partner_data_final$implementingmechanismname == 
                       "MSPP/UGP (National AIDS Strategic Plan)",]$mechanism  <- "MSPP/UGP"


## We need to assgin new mechanism name to fy2016, fy2015 data, because mechanism
## has changed in 2017 for CDC Haiti, this is useful to have yearly trend for a specific partner
site_by_mechanism_2017 <- partner_data_final %>%
    filter(indicator == "HTS_TST") %>%
    filter(!(implementingmechanismname %in% old_mechanism)) %>%
    group_by(implementingmechanismname,facility) %>%
    summarise(fy2017Cum = sum(fy2017Cum)) %>%
    select(implementingmechanismname,facility) %>% as.data.frame()

site_by_mechanism_2017 <- site_by_mechanism_2017[!is.na(site_by_mechanism_2017$facility),]


################# OU Level Results ##############################################################

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

write_csv(ou_level,"processed_data/ou_level.csv")
################# Cascade FY17 ###############################################


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
    
################## Patient Initiated not Linked by IM ################################################

linkage <- partner_data_final %>%
    filter(!(implementingmechanismname %in% old_mechanism)) %>%
    filter(indicator %in% c("HTS_TST_POS","TX_NEW")) %>%
    select(mechanism,indicator,fy2017Cum) %>%
    group_by(mechanism,indicator) %>%
    summarise(fy2017Cum = sum(fy2017Cum, na.rm = T)) %>%
    spread(indicator,fy2017Cum) %>%
    mutate(Patient_Not_Intiated = ifelse(HTS_TST_POS - TX_NEW < 0, 0,HTS_TST_POS - TX_NEW),
           Linkage = ifelse(TX_NEW > 0, TX_NEW/HTS_TST_POS, 0 ))

 write_csv(linkage, "linkage.csv")
 
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


################# Partner Performance Results ########################################################


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

write_csv(partner_performance_sp,"processed_data/partner_performance_sp.csv")

## partner_performance


################# Generate Visuals from Partner Performance Data ##########################################

## Generate Visuals from Partner Performance Data

fill_pall <- c("BEST" = "#FFFF99","CDS 1528"="#B15928","CMMB 1970"="#6A3D9A",
               "FOSREF 1925"="#CAB2D6","GHESKIO 1924"="#FF7F00","GHESKIO 1969"="#999966","HTW"="#E31A1C",
               "Linkages"="#006666","MSPP/UGP"="#1F78B4","PIH 1926"="#33A02C","SSQH Nord"="#B2DF8A","SSQH Sud"="#A6CEE3")

partner_performance %>%
    filter(fy2017Perf > 0) %>%
    filter(indicator == "TX_NEW") %>%
    ggplot(mapping = aes(x=reorder(mechanism,fy2017Perf),y=fy2017Perf))+
    geom_bar(stat = "identity",fill= "#009999", width = 0.8)+
    geom_text(aes(label=paste0(sprintf("%.0f", fy2017Perf*100),"%")),size=4.8,
              position=position_stack(vjust=0.5), colour="white") +
    labs(y="", 
         x="",
         fill="",
         title=paste0("APR17 TX_NEW"," ", "Partner Performance"),
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
    filter(indicator == "TX_CURR", fy2017Cum > 0) %>%
    ggplot(mapping = aes(x=reorder(mechanism,fy2017Cum),y=fy2017Cum))+
    geom_bar(stat = "identity",fill= "#1F78B4", width = 0.8)+
    geom_text(aes(label=paste0(comma(fy2017Cum))),size=4,
              position=position_stack(vjust=0.5), colour="white") +
    geom_errorbar(aes(ymin=fy2017_targets,ymax=fy2017_targets),width=1, size=1.5, color="#FF7F00")+
    labs(y="", 
         x="",
         fill="",
         title=paste0("APR17 TX_CURR"," "," Partner Results vs Targets"),
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

################# Overall achievement bar ###########################################################3

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
    scale_y_continuous(limits=c(0,180))+
    scale_y_continuous(labels = percent_format())+
    guides(fill = FALSE) +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12), 
          panel.background = element_blank(),
          axis.line= element_line(),
          axis.title.x = element_text(size = 10),
          plot.title = element_text(size = 16) )  



################# Bubble Charts Partner Performance ##################################################
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

############### Trend by Partner ##############################################################
# Looking at the trend by IM for APR15, APR16, APR17

partner_data_final %>%
    group_by(mechanism,indicator) %>%
    summarise(fy2015apr=sum(fy2015apr),fy2016apr=sum(fy2016apr), fy2017Cum=sum(fy2017Cum)) %>%
    gather("fiscal_year","total",3:5) %>%
    filter(indicator == "TX_NEW") %>%
    ggplot(aes(fiscal_year,total))+
        geom_bar(stat = "identity")+
        facet_wrap(~mechanism)

############################ TX_NEW Trend ###############################################################

site_im %>%
    filter(snu1 != "_Military Haiti") %>%
    filter(indicator == "TX_NEW") %>%
    filter(disaggregate == "Total Numerator") %>%
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
             caption="Data source: ICPI FactView SitexIM Haiti")+
    scale_y_continuous(breaks = seq(0,10000,1000),limits =c(0,10000),labels =comma,expand = c(0, 0))+
    expand_limits(x = 0, y = 0)+
        theme(axis.text.x = element_text(size = 10,face="bold"),
              axis.text.y = element_text(size = 13,face= "bold"), 
              panel.background = element_blank(),
              axis.line=element_line(),
              axis.title.x = element_text(size = 8),
              plot.title = element_text(size = 18),
              plot.subtitle  = element_text(size = 12))   


############### HTS_TST  vs HTS_YIELD ##################################################    

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


################## Yield, Linkage , Retention Rate by IM  ###################################



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


## net new by partner
## trend by partner ~ facet
## Why MSPP and PIH are low performer ? why they did not reach their target ?
    ## which sites are holding them back ?
    ## what district are they working in ?
    ## can you use maps ??
## if we merge SSQH ? GHESKIO ?? Present the Profile of GHESKIO, PIH, UGP, SSQH
## IMs Profile : District, Volume on Tx, nb Sites, achievements, trends, Barriers (YIELD , Enrollement, NET_NEW), spending,






