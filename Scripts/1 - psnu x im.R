# get country names from TaST PSNUxIM tabs -----------------------------------------------------------------
psnuXim_filepaths <- list.files(path = "Data/PSNUxIM/", 
                           pattern=str_to_lower("\\.xlsx"),
                           full.names = TRUE) %>% print()

load_secrets() #for DATIM login
source_psnu <- "PSNUxIM"

#save psnuXim inputs as DF
tastsXim <- psnuXim_filepaths %>% map_dfr(tame_dp, 
                                     type = source_psnu,
                                     # map_names = TRUE
                                     ) %>%
   get_names(datim_user = datim_user(), datim_password = datim_pwd()) %>%
   mutate(source_type = source_psnu) %>% 
  glimpse()

#generate list of countries with psnu x im tabs to exclude from earlier versions of TaSTs being read
exclude_due_to_psnuXim <- paste( c(unique(tastsXim$country)), collapse = "|") %>% print()
