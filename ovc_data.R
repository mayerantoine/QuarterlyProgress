


ovc_hivstat_totalnum <- site_im %>%
    filter(snu1 != "_Military Haiti") %>%
    filter(indicator == "OVC_HIVSTAT") %>%
    filter(standardizeddisaggregate == "Total Numerator") %>%
    filter(indicatortype == "DSD") %>% 
    filter(numeratordenom == "N") %>%
    select(implementingmechanismname,fundingagency,psnu,facility,community,indicator,standardizeddisaggregate,fy2015apr,fy2016apr,fy2017q1, fy2017q2,fy2017q3,fy2017q4,fy2017_targets) %>%
    group_by(implementingmechanismname,fundingagency,psnu,community,facility,indicator,standardizeddisaggregate) %>%
    summarise(fy2015apr = sum(fy2015apr, na.rm = T),
              fy2016apr = sum(fy2016apr,na.rm = T),
              fy2017q1 = sum(fy2017q1, na.rm = T),
              fy2017q2 = sum(fy2017q2, na.rm = T),
              fy2017q3 = sum(fy2017q3,na.rm = T),
              fy2017q4 = sum(fy2017q4,na.rm = T),
              fy2017_targets= sum(fy2017_targets, na.rm = T)) %>%
    mutate(fy2017Cum  = ifelse(fy2017q4 == 0, fy2017q3,fy2017q4))


ovc_hivstat_disagg <- site_im %>%
    filter(snu1 != "_Military Haiti") %>%
    filter(indicator == "OVC_HIVSTAT") %>%
    filter(indicatortype == "DSD") %>% 
    filter(numeratordenom == "N") %>%
    select(implementingmechanismname,fundingagency,psnu,facility,community,indicator,standardizeddisaggregate,
           categoryoptioncomboname,fy2015apr,fy2016apr,fy2017q1, fy2017q2,fy2017q3,fy2017q4,fy2017_targets) %>%
    group_by(implementingmechanismname,fundingagency,psnu,community,facility,indicator,
             standardizeddisaggregate,categoryoptioncomboname) %>%
    summarise(fy2015apr = sum(fy2015apr, na.rm = T),
              fy2016apr = sum(fy2016apr,na.rm = T),
              fy2017q1 = sum(fy2017q1, na.rm = T),
              fy2017q2 = sum(fy2017q2, na.rm = T),
              fy2017q3 = sum(fy2017q3,na.rm = T),
              fy2017q4 = sum(fy2017q4,na.rm = T),
              fy2017_targets= sum(fy2017_targets, na.rm = T)) %>%
    mutate(fy2017Cum  = ifelse(fy2017q4 == 0, fy2017q3,fy2017q4))
