# get country names from TaST PSNUxIM tabs -----------------------------------------------------------------
psnuXim_filepaths <- list.files(path = "Data/PSNUxIM/", 
                           pattern=str_to_lower("\\.xlsx"),
                           full.names = TRUE) %>% print()

load_secrets() #for DATIM login
source_psnu <- "PSNUxIM"

#save psnuXim inputs as DF
tastsXim <- psnuXim_filepaths %>% map_dfr(
                                     .f = ~tame_dp(.x), 
                                     type = source_psnu,
                                     # map_names = TRUE
                                     ) %>%
   mutate(operatingunit = if_else(is.na(operatingunit) & 
                                    country == "Central America and Brazil", "Central America Region", operatingunit)) |> 
   get_names(map_names = TRUE) %>%
   mutate(source_type = source_psnu) %>% 
  glimpse()

#generate list of countries with psnu x im tabs to exclude from earlier versions of TaSTs being read
exclude_due_to_psnuXim <- paste( c(unique(tastsXim$country)), collapse = "|") %>% print()

