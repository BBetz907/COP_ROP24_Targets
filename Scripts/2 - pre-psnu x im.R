tasts <- filepaths %>% map_dfr(tame_dp, map_names = FALSE,
                               # psnu_lvl = TRUE,
                               # type = "PSNUxIM"
                               ) %>% filter(!is.na(targets), !is.na(operatingunit)) %>% 
  # get_names(datim_user = datim_user, datim_password = datim_password)  %>% 
  glimpse()



# manipulate --------------------------------------------------------------
KP <- c("FSW", "MSM", "PWID", "TG", "People in prisons and other enclosed settings")

tasts_sd <- tasts %>% 
  mutate(values = targets,
         results_or_targets = "Targets",
         fy = paste0("FY", str_extract(as.character(fiscal_year), "2[0-9]$")),
         # mech_code = as.numeric(mech_code),
         key_pops = case_when(otherdisaggregate %in% KP ~ otherdisaggregate),
         otherdisaggregate = case_when(!otherdisaggregate %in% KP  ~ otherdisaggregate),
         country_name = recode(country, "Democratic Republic of the Congo" = "DRC")) %>% 
  rename(age = ageasentered,
         # indicator_type = indicatortype,
         numerator_denom = numeratordenom,
         ou = operatingunit,
         # prime_partner = prime_partner_name,
         psnu_uid = psnuuid,
         status_hiv = statushiv,
         disaggregate = standardizeddisaggregate,
         # snu = snu1
         ) %>%
  select(-cumulative, -snuprioritization, -country) %>% glimpse()


# index testing -----------------------------------------------------------
testing <- c("KnownPos","NewNeg","NewPos")

tasts_index <- tasts_sd %>%
  filter(indicator == "HTS_INDEX") %>% 
  mutate(indicator = paste0(indicator, "_", toupper(otherdisaggregate))) 
# test_neg ----------------------------------------------------------------
tasts_neg <- tasts_sd %>%
  filter(indicator == "HTS_TST" & status_hiv == "Negative") %>% 
  mutate(indicator = paste0(indicator, "_", toupper(str_extract(status_hiv, "^Neg"))))

# rejoin tamedp sections --------------------------------------------------
# all.equal(tasts_sd, tasts_index, tasts_neg)	
janitor::compare_df_cols(tasts_sd, tasts_index, tasts_neg)	


tasts_sd_merge <- bind_rows(tasts_sd, tasts_index, tasts_neg) %>%
  mutate(indicator = str_replace(indicator, "^PREP", "PrEP")) %>%
  select(-otherdisaggregate) 

tast_targets <- tasts_sd_merge %>% left_join(source_files) %>%
  mutate(ou = case_when(country_name == "Caribbean Region" ~ "Western Hemisphere Region",
                        TRUE ~ ou),
         country_name = case_when(country_name == "Caribbean Region" ~ psnu,
                                  TRUE ~ country_name),
         psnu = case_when(ou == "Western Hemisphere Region" ~ "",
                          TRUE ~ psnu)) %>%
  mutate(country_name = recode(country_name, "Papua New Guinea" = "PNG"),
         ou = recode(ou, "Democratic Republic of the Congo" = "DRC"))



tast_targets <- tast_targets[ , order(names(tast_targets))] %>% arrange(country_name)  #order names

tast_targets %>% group_by(country_name, source_name) %>% summarise() %>% print(n=length(unique(country_name)))



