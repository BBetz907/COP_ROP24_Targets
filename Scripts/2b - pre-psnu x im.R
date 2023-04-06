tasts <- tast_filepaths %>% map_dfr(tame_dp, map_names = FALSE,
                               # psnu_lvl = TRUE,
                               # type = "PSNUxIM"
                               ) %>% filter(!is.na(targets), !is.na(operatingunit)) %>% 
  mutate(source_type = "TaST") %>%
  glimpse()


