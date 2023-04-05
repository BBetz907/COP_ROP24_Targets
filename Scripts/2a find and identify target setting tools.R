

# get country_names -------------------------------------------------------
countrynames <- glamr::pepfar_country_list  %>%
  mutate(country_name = country) %>%
  distinct(country_name) %>% 
  arrange(country_name) %>% 
  pull() %>% 
  paste0(collapse = "|")
class(countrynames)
countrynames



# get country names from TaSTs -----------------------------------------------------------------
tast_files <- list.files(path = "Data/TaST/", 
                        pattern=str_to_lower("\\.xlsx"),
                        full.names = TRUE) %>% print()

tast_filepaths <- files[!grepl(exclude_due_to_psnuXim, files)]
tast_filepaths

filepaths <- c(psnuXim_filepaths, tast_filepaths)

source_name <- str_extract(filepaths, "(?<=./).+(?=.xlsx)") #look behind ./ and ahead of .xlsx
country_name <- if_else(str_detect(filepaths, countrynames), str_extract(filepaths, countrynames),
                        if_else(str_detect(filepaths, "HAITI"), "Haiti",
                                if_else(str_detect(filepaths, "CaribbeanRegion"), "Caribbean Region",              
                                        if_else(str_detect(filepaths, "DRC|DemocraticRepublic"), "DRC",
                                                if_else(str_detect(filepaths, "DR|DominicanRepublic"), "Dominican Republic",
                                                        if_else(str_detect(filepaths, "SierraLeone"), "Sierra Leone",
                                                        if_else(str_detect(filepaths, "CI|CotedIvoire"), "Cote d'Ivoire", 
                                                        if_else(str_detect(filepaths, "CI"), "Cote d'Ivoire", 
                                                                if_else(str_detect(filepaths, "PNG"), "Papua New Guinea",
                                                                        if_else(str_detect(filepaths, "Peru"), "Peru",
                                                                                if_else(str_detect(filepaths, "Colombia"), "Colombia",
                                                                                        if_else(str_detect(filepaths, "BurkinaFaso"), "Burkina Faso",
                                                                                                if_else(str_detect(filepaths, "Benin"), "Benin", "NA")))))))))))))

countries <- as.list(country_name)

source_files <- data.frame(country_name, source_name) %>% 
  mutate(country_name = recode(country_name, "Jamaica" = "Caribbean Region"),
         source_name = if_else(country_name == "Ethiopia", str_replace(source_name, "Group [1-4]", "All Groups"), source_name),
         source_name = if_else(country_name == "Ethiopia", str_replace(source_name, "All Groups with PSNUxIM 04122022", "All Groups with PSNUxIM 04142022"), source_name)) %>% arrange(country_name) %>%
  group_by(country_name, source_name) %>% summarise() %>% ungroup() %>% print(n=45)


# # # paths <- str_extract(filepaths, "(?<=ack_|ack\\s)[A-Z,a-z]+")
# # country_name <- if_else(str_detect(source_name,"Datapack_COP22"), str_extract(source_name, "(?<=COP22_)[A-Z,a-z]+"), #look behind COP22_
# #                         if_else(str_detect(source_name,"ROP22"), str_extract(source_name, "[A-Z,a-z]+"),
#                                 if_else(str_detect(source_name, "South"), str_extract(source_name, "South\\s[A-Z,a-z]+"), #look behind South and include it and any alphabetic chars after the space...
#                                 if_else(str_detect(source_name, "Burkina"), str_extract(source_name, "Burkina\\s[A-Z,a-z]+"), #look behind Burkina and include it and any alphabetic chars after the space...
#                                 if_else(str_detect(source_name, "Sierra"), str_extract(source_name, "Sierra\\s[A-Z,a-z]+"))))) #look behind Sierra and include it and any alphabetic chars after the space...
                  
