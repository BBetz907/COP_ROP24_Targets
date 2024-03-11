flatpath <- "Data/Flatpack/"
flatpack <- list.files(path = flatpath, pattern = "\\.xlsx") %>% print()

# Read Flatpack and (for now) manipulate -----------------------------------------------------------

library(readxl)

flat <- read_xlsx(path = str_c(flatpath, flatpack), sheet = "CSO export") %>% glimpse()
  select(-country_uid, -partner_id, -dataelement_id, -categoryoptioncombo_id, -top_level) %>%
  clean_names() %>%
  group_by_all() %>%
  summarise() %>%
  mutate(fiscal_year = fiscal_year + 1,
         target_value = if_else(indicator == "TX_PVLS", round(target_value*1.4, 0), target_value)) %>% #temporarily boost FY by 1 and increase PVLS to trigger error to treat this as COP22/FY23 data
  ungroup() %>% glimpse()



targets <- targets %>%
  filter(fiscal_year == 2023, 
         !str_detect(targets$indicator, "SUBNAT$|EST|^PRIORITY|HIV$")) %>% #Keep COP targets, EXCLUDE non-cascade indicators
  mutate(FY = "FY",
         fy = str_extract(as.character(fiscal_year), "(?<=^20)2[23]"), #create fy
         mod1 = str_extract(hts_modality, "(?<=\\s-\\s).+(?=\\sFY)"),
         mod1 = str_replace(mod1, "\\s+Services", ""),
         mod2 = str_extract(hts_modality, "^[A-Za-z]+(?=\\s-\\s)"),
         mod2 = if_else(mod1 != "Index" & mod2 == "Facility", "", mod2)) %>% #recreate modality parts to match
  unite("fy", FY:fy, sep = "") %>% #create FY, relics auto-removed
  unite("modality", mod1:mod2, sep = " ") %>%
  rename(category_option_combo_name = categoryoptioncombo_name,
         disaggregate = disagg_type,
         indicator_type = support_type,
         key_pops = key_population,
         prime_partner = partner_desc,
         status_hiv = -resultstatus_inclusive,
         snu = snu1,
         snu_uid = snu1_id,
         values = target_value) %>% #rename to match MSD
  separate(mechanism_desc, c("a","b","mech_name"), sep = "\\-\\s") %>% #create mech_name from end of the string
  mutate(targets = values,
         mech_code = as.numeric(mechanism_code),
         # mech_name = str_extract(str_extract(mechanism_desc, "(?<=\\s-\\s).+$"), "(?<=\\s-\\s).+$"), #extract the text after the second dash
         modality = na_if(modality, "NA NA"),
         numerator_denom = str_extract(dataelement_name, "(?<=\\s\\()[ND]"),
         results_or_targets = "Targets",
         source_name = "Datapack DRAFT") %>%
  select(-dataelement_name, -fiscal_year, -hts_modality, -mechanism_code, -a, -b, -numerator_denominator, -prioritization)


targets <- targets[ , order(names(targets))] %>%
  glimpse()



# Create TST_STAT indictors -----------------------------------------------


test <- c("HTS_INDEX", "HTS_TST")
hts <- targets %>%
  filter(indicator %in% test) %>%
  separate(indicator_code, c("ind","ind2","status","t"), sep="\\.") %>%
  select(-ind, -t) %>%
  glimpse()
  
  
hts_tst <- hts %>%
  filter(indicator == "HTS_TST") %>%
  mutate(status = toupper(status)) %>%
  select(-ind2) %>%
  unite("indicator", indicator:status, sep = "_") %>%
  glimpse()
table(hts_tst$indicator)


hts_index <- hts %>%
  filter(indicator == "HTS_INDEX") %>%
  mutate(status = toupper(status),
         ind2 = toupper(ind2)) %>%
  unite("ind2", ind2:status, sep = "") %>%
  unite("indicator", indicator:ind2, sep = "_") %>%
  glimpse()
table(hts_index$indicator)

#append hts_index, hts_tst with targets
testing_targets <- bind_rows(hts_index, hts_tst) %>% glimpse()

# maybe an issue with the above results_status: known and new positives ------------------------
table(testing_targets$resultstatus_specific, testing_targets$indicator)
#nope all good


# appends test_status indicators to other indicators to recreate t --------

COPtargets <- bind_rows(targets, testing_targets) %>% 
  select(-indicator_code, -resultstatus_specific) %>%
  glimpse()






# join with partner type to add partner type field ------------------------


# join with standardized_disaggregate to add partner type field ------------------------

