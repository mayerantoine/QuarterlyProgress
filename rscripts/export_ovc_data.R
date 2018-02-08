library(tidyverse)
library(formattable)
library(knitr)
library(scales)
library(xlsx)

rm(list = ls())

source("./rscripts/00_import_factview_data.R")
site_im <- import_factview_site_im("ICPI_FactView_Site_IM_Haiti_20171222_v2_2")
site_im$fy2015q4 <- as.numeric(site_im$fy2015q4)
site_im$fy2015q2 <- as.numeric(site_im$fy2015q2)
site_im$fy2015q3 <- as.numeric(site_im$fy2015q3)
site_im$fy2016_targets <- as.integer(site_im$fy2016_targets)
site_im$fy2016q1 <- as.integer(site_im$fy2016q1)
site_im$fy2016q2 <- as.integer(site_im$fy2016q2)

partner_mapping <- read_csv("data/fy17_partner_mapping.csv")
names(partner_mapping) <- c("facility","mechanism")


## OVC data for Target setting  -----------------------------------------------------------------------------------

 
# facility ovc
ovc_apr17_f <-  site_im %>%
    filter(indicator == "OVC_SERV") %>%
         filter(standardizeddisaggregate == "ProgramStatus") %>%
    filter(indicatortype == "DSD",typefacility == "Y") %>% 
    filter(numeratordenom == "N") %>%
      select(indicator,facility,fy2017apr) %>%
    group_by(facility) %>%
    summarise(fy2017apr = sum(fy2017apr,na.rm = T)) 

ovc_disagg_apr17_f <-  site_im %>%
    filter(indicator == "OVC_SERV") %>%
         filter(standardizeddisaggregate == "ProgramStatus") %>%
    filter(indicatortype == "DSD",typefacility == "Y") %>% 
    filter(numeratordenom == "N") %>%
      select(indicator,facility,otherdisaggregate,fy2017apr) %>%
    group_by(facility,otherdisaggregate) %>%
    summarise(fy2017apr = sum(fy2017apr,na.rm = T))  %>%
        spread(otherdisaggregate, fy2017apr)
 
ovc_apr17_f <- left_join(ovc_apr17_f,ovc_disagg_apr17_f, by  =c("facility" = "facility"))
ovc_apr17_f <- left_join(ovc_apr17_f,partner_mapping , by = c("facility" = "facility")) %>%
    as.data.frame()

# community ovc
ovc_apr17_c <-  site_im %>%
    filter(indicator == "OVC_SERV") %>%
         filter(standardizeddisaggregate == "ProgramStatus") %>%
    filter(indicatortype == "DSD",typecommunity == "Y") %>% 
    filter(numeratordenom == "N") %>%
      select(indicator,community,implementingmechanismname,fy2017apr) %>%
    group_by(community,implementingmechanismname) %>%
    summarise(fy2017apr = sum(fy2017apr,na.rm = T)) 

ovc_disagg_apr17_c <-  site_im %>%
    filter(indicator == "OVC_SERV") %>%
         filter(standardizeddisaggregate == "ProgramStatus") %>%
    filter(indicatortype == "DSD",typecommunity == "Y") %>% 
    filter(numeratordenom == "N") %>%
      select(indicator,community,otherdisaggregate,fy2017apr) %>%
    group_by(community,otherdisaggregate) %>%
    summarise(fy2017apr = sum(fy2017apr,na.rm = T))  %>%
        spread(otherdisaggregate, fy2017apr)
 
ovc_apr17_c <- left_join(ovc_apr17_c,ovc_disagg_apr17_c, by  =c("community" = "community")) %>%
    as.data.frame()


# ovc target fy2018 ------------------------------------------

#facility
ovc_target2018_f <-  site_im %>%
    filter(indicator == "OVC_SERV") %>%
         filter(standardizeddisaggregate == "ProgramStatus") %>%
    filter(indicatortype == "DSD",typefacility == "Y") %>% 
    filter(numeratordenom == "N") %>%
      select(indicator,facility,fy2018_targets) %>%
    group_by(facility) %>%
    summarise(fy2018_targets = sum(fy2018_targets,na.rm = T)) 

ovc_disagg_target_f <-  site_im %>%
    filter(indicator == "OVC_SERV") %>%
         filter(standardizeddisaggregate == "ProgramStatus") %>%
    filter(indicatortype == "DSD",typefacility == "Y") %>% 
    filter(numeratordenom == "N") %>%
      select(indicator,facility,otherdisaggregate,fy2018_targets) %>%
    group_by(facility,otherdisaggregate) %>%
    summarise(fy2018_targets = sum(fy2018_targets,na.rm = T))  %>%
        spread(otherdisaggregate, fy2018_targets)

ovc_target2018_f <- left_join(ovc_target2018_f,ovc_disagg_target_f, by = c("facility" = "facility"))
ovc_target2018_f <- left_join(ovc_target2018_f,partner_mapping , by = c("facility" = "facility")) %>%
    as.data.frame()


# community
ovc_target2018_c <-  site_im %>%
    filter(indicator == "OVC_SERV") %>%
         filter(standardizeddisaggregate == "ProgramStatus") %>%
    filter(indicatortype == "DSD",typecommunity == "Y") %>% 
    filter(numeratordenom == "N") %>%
      select(indicator,community,implementingmechanismname,fy2018_targets) %>%
    group_by(community,implementingmechanismname) %>%
    summarise(fy2018_targets = sum(fy2018_targets,na.rm = T)) 


ovc_disagg_target2018_c <-  site_im %>%
    filter(indicator == "OVC_SERV") %>%
         filter(standardizeddisaggregate == "ProgramStatus") %>%
    filter(indicatortype == "DSD",typecommunity == "Y") %>% 
    filter(numeratordenom == "N") %>%
      select(indicator,community,otherdisaggregate,fy2018_targets) %>%
    group_by(community,otherdisaggregate) %>%
    summarise(fy2018_targets = sum(fy2018_targets,na.rm = T))  %>%
        spread(otherdisaggregate, fy2018_targets)

ovc_target2018_c <- left_join(ovc_target2018_c,ovc_disagg_target2018_c, by = c("community" = "community")) %>%
    as.data.frame()



# ovc age_Sex disagg facility
ovc_age_sex_f <-  site_im %>%
    filter(indicator == "OVC_SERV") %>%
    filter(standardizeddisaggregate %in% c("AgeAboveTen/Sex","AgeLessThanTen")) %>%
    filter(indicatortype == "DSD",typefacility == "Y") %>% 
    filter(numeratordenom == "N") %>%
      select(facility,fy2017apr,age,sex) %>%
    group_by(facility,age,sex) %>%
    summarise(fy2017apr = sum(fy2017apr,na.rm = T)) %>%
     unite("age_sex",age,sex) %>%
     spread(age_sex, fy2017apr)
 
ovc_age_sex_f <- left_join(ovc_age_sex_f,partner_mapping , by = c("facility" = "facility")) %>%
    as.data.frame()

 ovc_age_sex_c <-  site_im %>%
    filter(indicator == "OVC_SERV") %>%
    filter(standardizeddisaggregate %in% c("AgeAboveTen/Sex","AgeLessThanTen")) %>%
    filter(indicatortype == "DSD",typecommunity == "Y") %>% 
    filter(numeratordenom == "N") %>%
      select(community,fy2017apr,implementingmechanismname,age,sex) %>%
    group_by(community,implementingmechanismname,age,sex) %>%
    summarise(fy2017apr = sum(fy2017apr,na.rm = T)) %>%
     unite("age_sex",age,sex) %>%
     spread(age_sex, fy2017apr) %>%
     as.data.frame()


 
# ovc under 18
ovc_18_apr17 <-  site_im %>%
    filter(indicator %in%  c("OVC_SERV_UNDER_18","OVC_SERV_OVER_18")) %>%
         filter(standardizeddisaggregate == "Total Numerator") %>%
    filter(indicatortype == "DSD",typefacility == "Y") %>% 
    filter(numeratordenom == "N") %>%
      select(indicator,facility,fy2017apr) %>%
    group_by(facility,indicator) %>%
    summarise(fy2017apr = sum(fy2017apr,na.rm = T)) %>%
    spread(indicator, fy2017apr)
 
 
 cop18_ovc_data <- createWorkbook()
 
 ovc_apr17_facility <- createSheet(cop18_ovc_data, sheetName = "ovc_apr17_facility" )
 ovc_apr17_community <- createSheet(cop18_ovc_data, sheetName = "ovc_apr17_community" )
 
 ovc_target2018_facility <- createSheet(cop18_ovc_data, sheetName = "ovc_target2018_facility" )
 ovc_target2018_community <- createSheet(cop18_ovc_data, sheetName = "ovc_target2018_community" )
 
 ovc_age_sex_facility <- createSheet(cop18_ovc_data, sheetName = "ovc_age_sex_facility" )
 ovc_age_sex_community <- createSheet(cop18_ovc_data, sheetName = "ovc_age_sex_community" )
  
 addDataFrame(ovc_apr17_f, sheet=ovc_apr17_facility, startColumn=1, row.names=T)
 addDataFrame(ovc_apr17_c, sheet=ovc_apr17_community, startColumn=1, row.names=T)

 addDataFrame(ovc_target2018_f, sheet=ovc_target2018_facility, startColumn=1, row.names=T)
 addDataFrame(ovc_target2018_c, sheet=ovc_target2018_community, startColumn=1, row.names=T)

 addDataFrame(ovc_age_sex_f, sheet=ovc_age_sex_facility, startColumn=1, row.names=T)
 addDataFrame(ovc_age_sex_c, sheet=ovc_age_sex_community, startColumn=1, row.names=T)
 
  saveWorkbook(cop18_ovc_data,"cop18_ovc_data.xlsx")
 

 
 
 
 