


filename <- "Data/MER/MSD_PREV_FY_UG.csv"


# Read Flatpack and (for now) manipulate ----------------------------------
# somehow the change from readr to data.table and read.csv is doubling values
mer_ug <- readr::read_csv(filename, col_names = TRUE) %>% clean_names() %>% 
  rename(age = age_as_entered,
         ou = operating_unit,
         ou_id = operating_unit_uid) %>%
# somehow the change from readr to data.table and read.csv is doubling values
  select(-age_2018, -age_2019, -award_number, -hiv_treatment_status, -standardized_disaggregate, -status_tb, -status_cx, -category_option_combo_name, -exclude_due_to_known_issue, -g2g) %>%
  mutate(modality = str_replace(modality, "\\(", ""),
         modality = str_replace(modality, "\\)$", "")) %>% #remove ()s
  glimpse()

table(mer_ug$indicator)
mer_ug <- mer_ug[ , order(names(mer_ug))] %>% arrange(country_name) %>% glimpse() #order names
table(mer_ug$country_name)


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
pmtct <- mer_ug %>% filter(str_detect(indicator, "PMTCT"))
ovc <- mer_ug %>% filter(str_detect(indicator, "OVC"))
tb <- mer_ug %>% filter(str_detect(indicator, "TB"))

table(pmtct$indicator)
table(ovc$indicator)
tb %>% select(indicator, other_disaggregate, numerator_denom, status_hiv) %>% group_by(indicator, numerator_denom, status_hiv) %>% summarise(.groups = "drop")


tast_targets %>% filter(country_name == "Uganda") %>%  group_by(indicator, otherdisaggregate, numerator_denom, status_hiv) %>% summarise(.groups = "drop") %>% 
  filter(str_detect(indicator, "TB")) %>% print(n=60)

# CREATE THE FOLLOWING FROM TARGETS by filtering and binding

# TX_TB   ,   TX_TB_D_NEG + TX_TB_D_POS
# TB_STAT_POS , TB_STAT_POS_KnownAtEntry_Positive + TB_STAT_POS_NewlyIdentified_Positive