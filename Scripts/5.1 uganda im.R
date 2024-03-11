uganda_targets_and_msd <- targets_and_msd %>% filter(country_name == "Uganda", fiscal_year == 2024) %>% 
  glimpse()

uganda_psnu_im <- readxl::read_xlsx(path = "Data/MER/COP23_TST_21_SUMMARY_NTO.xlsx", sheet = "DISTRICTxIM") %>% mutate(fiscal_year = 2024, cop = "COP23") %>% 
  rename( "HTS_INDEX.Neg.T" = "HTS.Index.Neg.T",
          "HTS_INDEX.Pos.T" = "HTS.Index.Pos.T",
          # "funding_agency" = "Agency"
  ) %>% select(c(1,6)) %>% 
  mutate(psnu = str_extract(PSNU, ".+(?=\\s\\[)"),
         psnu_uid = str_extract(PSNU, "(?<=\\[)[A-Z,0-9,a-z]+")) %>%
  group_by_all() %>% summarise(.groups = "drop") %>%
  glimpse()

uganda_targets_and_msd %>% left_join(uganda_psnu_im, by = c("psnu_uid","psnu"))
