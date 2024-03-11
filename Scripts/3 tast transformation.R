
# combine PSNUxIM and TaST files ------------------------------------------

targets <- bind_rows(tasts, tastsXim)
#output country names duplicated
tasts |> semi_join(tastsXim, by = "country") |> count(country)
# manipulate --------------------------------------------------------------
KP <- c("FSW", "MSM", "PWID", "TG", "People in prisons and other enclosed settings")

tasts_sd <- targets %>% 
  filter(fiscal_year == 2025,
         !(indicator == "PrEP_CT" & standardizeddisaggregate =="Age/Sex/HIVStatus")) %>%
  mutate(values = targets,
         results_or_targets = "Targets",
         modality = recode(modality, "TBClinic" = "TB Clinic"),
         fy = paste0("FY", str_extract(as.character(fiscal_year), "2[0-9]$")),
         mech_code = as.numeric(mech_code),
         key_pops = case_when(otherdisaggregate %in% KP ~ otherdisaggregate),
         otherdisaggregate = case_when(!otherdisaggregate %in% KP  ~ otherdisaggregate),
         country_name = recode(country, "Democratic Republic of the Congo" = "DRC"),
         indicator = if_else(indicator %in% c("TB_STAT", "PMTCT_STAT") & statushiv == "Positive", str_c(indicator, "POS", sep = "_"), indicator),
         indicator = if_else(indicator == "OVC_SERV" & ! is.na(otherdisaggregate), str_c(indicator, str_to_upper(otherdisaggregate), sep = "_"), indicator),) %>% 
  filter(!(indicator %in% c("TB_STAT", "PMTCT_STAT") & statushiv == "Negative")) %>%
  rename(age = ageasentered,
         # indicator_type = indicatortype,
         numerator_denom = numeratordenom,
         ou = operatingunit,
         prime_partner = prime_partner_name,
         status_hiv = statushiv,
         disaggregate = standardizeddisaggregate,
         age_coarse = trendscoarse
         # snu = snu1
  ) %>%
  select(-cumulative, -snuprioritization, -country) %>% glimpse()

table(tasts_sd$source_name)

# ug <- tasts_sd %>% filter(country_name=="Uganda", str_detect(psnu, "Kampala")) %>% glimpse()
# table(ug$indicator, ug$psnu)


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


tast_targets_pre <- bind_rows(tasts_sd, tasts_index, tasts_neg) %>%
  mutate(indicator = str_replace(indicator, "^PREP", "PrEP"),
         country_name = recode(country_name, "Papua New Guinea" = "PNG"),
         ou = recode(ou, "Democratic Republic of the Congo" = "DRC")) %>%
  select(-otherdisaggregate)

tast_targets <- tast_targets_pre[ , order(names(tast_targets_pre))] %>% arrange(country_name) 
# |> 
  # filter(country_name == "Uganda") #order names

datapack <- tast_targets %>% group_by(country_name, source_name) %>% summarise() %>% rename("datapack" = "source_name") %>% print(n=55)

table(tast_targets$fiscal_year, tast_targets$results_or_targets)



# unique(tast_targets$age)
# mer %>% select(age) %>% group_by_all() %>% summarise() %>%
#   mutate(age_coarse = if_else(str_detect(age, "Months|<|0[0-9]-0[0-9]|10-14"), "<15", if_else(age!="Unknown Age", "15+", age))) %>% print(n=25)
