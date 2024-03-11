filepaths[8]

kenya <- tame_dp(filepath = filepaths[8], map_names = FALSE) %>% 
  filter(indicator %in% c("CXCA_SCRN", "TX_CURR", "TX_NEW"), fiscal_year == 2024) %>% 
  # filter(targets >= 0) %>%
  group_by(indicator, standardizeddisaggregate) %>%
  summarise(targets = sum(targets), .groups = "drop") %>% print()

