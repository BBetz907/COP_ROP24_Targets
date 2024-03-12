tasts <- tast_filepaths %>% map_dfr(tame_dp, map_names = FALSE,
                               # psnu_lvl = TRUE,
                               # type = "PSNUxIM"
                               ) %>% filter(!is.na(targets),
                                            # fiscal_year > 2024
                                            ) %>% 
  mutate(source_type = "TaST",
         operatingunit = if_else(country %in% c("Central America and Brazil", "Caribbean Region"), "Western Hemisphere Region", operatingunit),
         country = if_else(country %in% c("Central America and Brazil", "Caribbean Region") & str_detect(psnu, countrynames), str_extract(psnu, countrynames), 
                           if_else(country == "Central America and Brazil" & !str_detect(psnu, countrynames), "Brazil", country))) %>% glimpse()
table(tasts$operatingunit)
table(tasts$country)

tasts |> filter(fiscal_year==2023) |> count(operatingunit)
