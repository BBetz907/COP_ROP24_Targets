# read in targets ---------------------------------------------------------

flatfile <- return_latest(folderpath = "Data/MER/", pattern = "COP23_TST_[0-9]{2}_SUMMARY.xlsx")
uganda_targets_psnu_im <- readxl::read_xlsx(flatfile, sheet = "DISTRICTxIM") %>% 
  select(-contains("FSW"), -contains("MSM"), -contains("PWID"), -contains("prisons"), -contains("TG")) %>% 
  mutate(fiscal_year = 2024, cop = "COP23") %>% 
  rename( "HTS_INDEX.Neg.T" = "HTS.Index.Neg.T",
          "HTS_INDEX.Pos.T" = "HTS.Index.Pos.T",
          "funding_agency" = "Agency",
          "im_name" = "IMName"
  ) %>% 
  # mutate(results_or_targets = "Targets") %>%
  select(-contains("HEI")) %>%
  relocate(fiscal_year:cop, .after = funding_agency) %>% 
  select(-VMMC_CIRC.Neg.T, -VMMC_CIRC.Pos.T) %>% 
  mutate_at(c(9:62) , ~replace_na(.,0)) %>% 
  glimpse()



columns <- str_extract(c(colnames(uganda_targets_psnu_im[9:62])), ".+(?=\\.[R,T]$)") %>% print()



# read in IMs by region ---------------------------------------------------------

file <- "Data/MER/IM codes and Names .xlsx"
sheet <- readxl::excel_sheets(file)
ims <- readxl::read_xlsx(file, sheet=sheet[1]) %>% arrange(IMCode2) %>% clean_names() %>% filter(!is.na(im_name)) %>%
  rename("funding_agency" = "funding_agency_group",
         "IMCode2" = "im_code2") %>% print(n=63)





# read in historic data ---------------------------------------------------------

ug_psnu_msd <- readRDS("Data/MER/uganda_psnu_msd_fy21-23.rds")%>% glimpse()

nds <- ug_psnu_msd %>% filter(str_detect(indicator, ("PMTCT_HEI"))) %>% glimpse()
table(nds$otherdisaggregate, nds$statushiv)
table(nds$categoryoptioncomboname)
table(nds$otherdisaggregate_sub)

uganda_mer_psnu_im <- ug_psnu_msd %>% 
  filter(country == "Uganda") %>%
  mutate(ind2 = recode(otherdisaggregate,
                       "Physical and/or Emotional Violence" = "PhysicalEmotionalViolence",
                        "Sexual Violence Post-Rape Care" = "SexualViolence"),
         indicator = if_else(fiscal_year==2023 & indicator == "GEND_GBV" & str_detect(otherdisaggregate, "Violence"), str_c(indicator, ind2, sep = "_"), indicator),
         disaggregate = if_else(fiscal_year==2023 & str_detect(indicator, "GEND_GBV_"), "Total Numerator", disaggregate)) %>% 
  filter(
         !indicator %in% c("HTS_TST_POS", "HTS_TST_NEG", "VMMC_CIRC_FollowUp"),
         !str_detect(indicator, "ML|ARV|HRH|RTT|SC\\_|NET|FPINT|EMR"),
         !str_detect(indicator, "KP|PrEP|OVC"), #ADD thes as code develops
         indicator %in% c("TB_STAT","PMTCT_STAT") & disaggregate!= "Age/Sex" | str_detect(disaggregate, "Total") | 
           indicator %in% c("HTS_INDEX", "HTS_TST", "GEND_GBV", "TB_PREV", "TB_ART", "PMTCT_ART", "PMTCT_EID", "TX_TB") & !str_detect(disaggregate, "Total") & !str_detect(disaggregate, "Key") ) %>% 
  select(-qtr1:-qtr4) %>% 
  pivot_longer(cumulative:targets, names_to = "resultsortargets", values_to = "values") %>% 
  mutate(part2 = if_else(indicator %in% c("AGYW_PREV", "TB_PREV", "TB_STAT", "TX_PVLS", "PMTCT_STAT", "PMTCT_EID", "TX_TB"), numeratordenom, ""),
         funding_agency = recode(                      funding_agency,
                                                       "HHS/CDC" = "CDC"),
         mod = if_else(indicator == "HTS_TST",
                       recode(modality,
                      # "IndexMod" = "Index",
                      "OtherMod" = "ActiveOther",
                      "SNSMod" = "SNS",
                      "STI Clinic" = "STI",
                      "Post ANC1" = "PostANC1"), ""),
         mod = if_else(!mod %in% c("ActiveOther","SNS","STI","PostANC1") & indicator == "HTS_TST", "Other", mod),
         mod = if_else(is.na(mod), "", mod),
         otherdisaggregate = if_else(indicator %in% c("TB_STAT", "PMTCT_STAT") & !is.na(otherdisaggregate), 
                                     recode(otherdisaggregate,
                                            "Newly Identified" = "New",
                                            "Known at Entry" = "KnownPos",
                                            "Recent" = "EXCLUDE"),
                                     otherdisaggregate),
         part3 = if_else(indicator %in% c("HTS_TST", "HTS_INDEX"), str_extract(statushiv, "^.{3}"), 
                         if_else(indicator == "TX_PVLS", "Routine", 
                                 if_else(indicator %in% c("TB_PREV", "TB_ART", "PMTCT_ART"), hiv_treatment_status, 
                                         if_else(indicator %in% c("TB_STAT", "PMTCT_STAT") & otherdisaggregate == "KnownPos", otherdisaggregate, 
                                                 if_else(indicator %in% c("TB_STAT", "PMTCT_STAT") & otherdisaggregate == "New", str_c(otherdisaggregate, 
                                                                                                                         str_extract(statushiv, "^[A-Z,a-z]{3}"),
                                                                                                                         sep = "."), ""))))),
         part3 = if_else(is.na(part3) & indicator %in% c("TB_STAT","PMTCT_STAT"), "", part3),
         part3 = if_else(indicator == "PMTCT_EID", str_extract(ageasentered, "12|(?<=\\=0)2"), part3),
         part3 = if_else(indicator == "TX_TB", str_c(hiv_treatment_status, str_extract(statustb, "[A-Z,a-z]{3}"), sep="."), part3),
         resultsortargets = if_else(resultsortargets=="cumulative", "R", "T"),
         # results_or_targets = if_else(resultsortargets=="cumulative", "Results", "Targets"),
         column = str_c(indicator,part2,mod,part3, resultsortargets, sep = "."),
         column = str_replace(column, "_PhysicalEmotionalViolence", "\\.PE"),
         column = str_replace(column, "_SexualViolence", "\\.S"),
         column = str_replace(column, "\\.\\.", "\\."),
         column = str_replace(column, "\\.\\.", "\\."),
         column = str_replace(column, "\\.\\.", "\\."),
         values = coalesce(values, 0),
         IMCode2 = str_c(mech_code, "DSD", sep = "_"),
         psnucode = str_c("[", psnuuid, "]"),
         PSNU = str_c(psnu, psnucode, sep = " "),
         cop= "") %>% 
  filter(
       !is.na(column),
         !(!is.na(otherdisaggregate) & otherdisaggregate=="EXCLUDE"),
         !str_detect(indicator, "INDEX_")) %>% 
  filter(!str_detect(column, "GEND_GBV(?=\\.R$|\\.T$)")) %>%   
 select(-source_name, -part2:-part3, -ageasentered:-age_2019, -trendscoarse, -sex, -categoryoptioncomboname, -award_number,  -indicatortype, -prime_partner_duns, -prime_partner_uei, 
         -mech_code, -indicator, -country, -snu1, -snu1uid, -psnu, -psnuuid, -psnucode, -typemilitary,
         -resultsortargets, -modality, -contains("status"), -contains("disaggregate"), -numeratordenom,-dreams,-contains("operating"),-contains("other"), -snuprioritization) %>%
  group_by(across(-c(values))) %>% summarise(values = sum(values), .groups = "drop") %>% 
  mutate(column2 = str_extract(column, ".+(?=\\.[R,T]$)")) %>% 
  filter(column2 %in% columns) %>% 
  pivot_wider(names_from = column, values_from = values) %>% 
  select(-column2) %>% 
  mutate_at(c(9:98) , ~replace_na(.,0)) %>% 
  left_join(ims, by = c("IMCode2", "funding_agency")) %>% relocate(im_name, .after = "IMCode2") %>%
  glimpse()
  
  columns

  table(uganda_mer_psnu_im$indicator, uganda_mer_psnu_im$column)

# table(uganda_mer_psnu_im$column)
# test <- uganda_mer_psnu_im %>% filter(str_detect(indicator, "PVLS"))
# glimpse(test)
# table(test$otherdisaggregate)

# bind results and COP targets ---------------------------------------------------------
  
uganda_results_targets_psnu_im <- uganda_mer_psnu_im %>% bind_rows(uganda_targets_psnu_im) %>% 
    mutate(source = str_extract(flatfile, ("(?<=\\/[A-Z,a-z]{3,4}\\/).+(?=.xlsx)"))) %>%
    relocate(source,
      # `DATIM Region` ,`DHIS2 Region` , 
      .after = "cop" ) %>%
    mutate_at(c(101:111) , ~replace_na(.,0)) %>%
  glimpse()

write_csv(uganda_results_targets_psnu_im, "Data Out/uganda_results_targets_psnu_im.csv")


#pivot long for export also
uganda_results_targets_psnu_im_long <- uganda_results_targets_psnu_im %>% 
  pivot_longer(cols = c(11:111), names_to = "indicator_plus", values_to = "values") %>% 
  mutate(
    resultsortargets = str_extract(indicator_plus, "(?<=\\.)[R,T]$"),
         resultsortargets = recode(resultsortargets,
                                   "R" = "Results",
                                   "T" = "Targets"),
         indicator = str_extract(indicator_plus, ".+(?=\\.R$|\\.T$)")) %>% clean_names() %>%
  relocate(values, .after = indicator) %>%
  glimpse()

write_csv(uganda_results_targets_psnu_im_long, "Data Out/uganda_results_targets_psnu_im_long.csv")

