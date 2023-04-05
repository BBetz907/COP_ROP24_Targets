
# dplyr::all_equal(mer, tast_targets)	
janitor::compare_df_cols(mer, tast_targets)	
# vetr::alike()	
# diffdf::diffdf()

countries_with_targets <- tast_targets %>% select(country_name) %>% group_by_all() %>% summarise()
countries <- c(countries_with_targets$country_name)
countries
# mer %>% filter(ou == "Western Hemisphere Region") %>% select(country_name) %>% group_by_all() %>% summarise()


class(tast_targets$modality)
# append targets with MSD -------------------------------------------------
targets_and_msd <- mer %>% 
  filter(country_name %in% countries,
         !indicator %in% c("HIV_PREV", "POP_EST", "TX_VL_COVERAGE")) %>%
  bind_rows(tast_targets) %>% mutate(current_quarter = "2023 Q1",
         nd = if_else(indicator == "TX_PVLS", numerator_denom, ""),
         ind = if_else(indicator == "TX_PVLS", indicator, ""),
         ind1 = if_else(indicator == "TX_PVLS", "(", ""),
         ind2 = if_else(indicator == "TX_PVLS", ")", ""),
         key_pops = recode(key_pops, "People in prisons and other enclosed settings" = "People in prisons")) %>%
  unite("indn", ind1, nd, ind2, sep = "") %>%
  unite("ind_vl", ind, indn, sep = " ") %>%
  mutate(indicator = recode(indicator, "TX_PVLS" = ind_vl),
         funding_agency = if_else(str_detect(funding_agency, "CDC"), "CDC", funding_agency),
         age_coarse = if_else(str_detect(age, "Months|<|0[0-9]-0[0-9]|10-14"), "<15", if_else(age!="Unknown Age", "15+", age)),
         # modality = str_replace(modality, "Mod", " Community"),
         # modality = recode(modality,
         #                   "Emergency Ward" = "Emergency",
         #                   "Index" = "Index Facility",
         #                   "Inpat" = "Inpatient",
         #                   "Mobile Community" = "Community Mobile",
         #                   "OtherPITC" = "Other PITC",
         #                   "TBClinic" = "TB Clinic"),
         day = today(tzone = "UTC")) %>% select(-ind_vl) %>%
  left_join(datapack, by = c("country_name")) %>% 
  relocate(age_coarse, .after = datapack) %>%
  glimpse()


# targets_and_msd <- targets_and_msd[ , order(names(targets_and_msd))] %>% arrange(country_name)  #order names


targets_and_msd %>% group_by(country_name, source_name) %>% summarise() %>% filter(!str_detect(source_name, "^DATIM|Derived")) %>% print(n=length(unique(country_name)))



rm(mer, targets, tasts, tasts_im, tast_targets, tasts_neg, tasts_index, tasts_sd, tasts_sd_merge)
gc()
write_csv(targets_and_msd, "Data Out/targets_and_msd.csv", na = "")
rm(targets_and_msd)
gc()



