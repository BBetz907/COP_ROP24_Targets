
# dplyr::all_equal(mer_ug, tast_targets)	
janitor::compare_df_cols(mer_ug, tast_targets)	
# vetr::alike()	
# diffdf::diffdf()

countries_with_targets <- tast_targets %>% select(country_name) %>% group_by_all() %>% summarise()
countries <- c(countries_with_targets$country_name)

# mer_ug %>% filter(ou == "Western Hemisphere Region") %>% select(country_name) %>% group_by_all() %>% summarise()


class(tast_targets$modality)
# append targets with msd_ug -------------------------------------------------
targets_and_msd_ug <- bind_rows(mer_ug, tast_targets) %>%  
  filter(country_name %in% c("Uganda")) %>%
  glimpse()
  
targets_and_msd_ug <- targets_and_msd_ug %>% mutate(current_quarter = "2023 Q1",
         nd = if_else(indicator == "TX_PVLS", numerator_denom, ""),
         ind = if_else(indicator == "TX_PVLS", indicator, ""),
         ind1 = if_else(indicator == "TX_PVLS", "(", ""),
         ind2 = if_else(indicator == "TX_PVLS", ")", ""),
         key_pops = recode(key_pops, "People in prisons and other enclosed settings" = "People in prisons")) %>%
  unite("indn", ind1, nd, ind2, sep = "") %>%
  unite("ind_vl", ind, indn, sep = " ") %>%
  mutate(indicator = recode(indicator, "TX_PVLS" = ind_vl),
         funding_agency = if_else(str_detect(funding_agency, "CDC"), "CDC", funding_agency),
         modality = str_replace(modality, "Mod", " Community"),
         modality = recode(modality,
                           "Emer_uggency Ward" = "Emer_uggency",
                           "Index" = "Index Facility",
                           "Inpat" = "Inpatient",
                           "Mobile Community" = "Community Mobile",
                           "OtherPITC" = "Other PITC",
                           "TBClinic" = "TB Clinic"),
         day = today(tzone = "UTC")) %>% select(-ind_vl) %>%
  left_join(datapack, by = c("country_name")) %>%
  glimpse()

targets_and_msd_ug %>% group_by(country_name, datapack) %>% summarise() %>% print(n=6)


write_csv(targets_and_msd_ug, "Data Out/targets_and_msd_ug.csv", na = "")
glimpse(targets_and_msd_ug)
unique(targets_and_msd_ug$datapack)

