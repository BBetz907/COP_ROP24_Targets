


filename <- "Data/MER/MSD_PREV_FY.csv" 



# Read Flatpack and (for now) manipulate ----------------------------------
# somehow the change from readr to data.table and read.csv is doubling values
mer <- readr::read_csv(filename, col_names = TRUE) %>% clean_names() %>% 
  rename(age = age_as_entered,
         ou = operating_unit,
         ou_id = operating_unit_uid,
         disaggregate = standardized_disaggregate) %>%
# somehow the change from readr to data.table and read.csv is doubling values
  select(-age_2018, -age_2019, -award_number, -hiv_treatment_status, -status_tb, -status_cx, -category_option_combo_name)  %>%
  mutate(psnu = if_else(country_name == "Uganda", snu, psnu),
         psnuuid = if_else(country_name == "Uganda", snu_uid, psnuuid),
         modality = str_replace(modality, "\\(", ""),
         modality = str_replace(modality, "\\)$", ""),
         mech_code = as.numeric(mech_code),
         modality=recode(modality,
                         "SNS Facility" = "SNS",
                         "SNS Community" = "SNS",
                         "Index Facility" = "Index",
                         "Index Community" = "Index",
                         "Other PITC" = "ActiveOtherMod",
                         "Community VCT" = "ActiveOtherMod",
                         "Community Mobile" = "ActiveOtherMod",
                         "Other Community" = "ActiveOtherMod"),
         modality = if_else(modality %in% c("Pediatric", "Emergency", "Inpatient", "Malnutrition", "PITC", "VCT"), "Other", modality)) %>% #remove ()s
  glimpse()

mer |> count(results_or_targets)

glimpse(mer)

# table(mer$modality)
# table(tast_targets$modality)
# 
# table(mer$indicator)
mer <- mer[ , order(names(mer))] %>% arrange(country_name) %>% glimpse() #order names
# table(mer$country_name)
glimpse(mer)

mer |> count(country_name)

# # create list of countries ------------------------------------------------
# 
# any_country <- mer %>% select(country_name) %>% group_by_all() %>% summarise() %>% ungroup() %>%
#   pull() %>% paste(collapse = "|") %>% print()
# class(country_name)
# # append targets with MSD
# bind_rows(mer, COPtargets)
# 
# 
# 
# 
# 
# 
# 
# 
# FHI <- mer %>% filter(funding_agency == "USAID", 
#                       fiscal_year == 2022,
#                       prime_partner == "Family Health International" | prime_partner == "FHI Development 360 LLC") %>% 
#   select(country_name, mech_name, mech_code, prime_partner) %>%
#   group_by_all() %>%
#   summarise() %>%
#   ungroup() %>%
#   glimpse()
# 
# FHI
