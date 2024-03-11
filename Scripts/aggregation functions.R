simplify_targets <- function(df) {
  df %>% group_by(across(-c("targets"))) %>% 
    summarise(targets = sum(targets), .groups = "drop") %>%
    filter(targets > 0)
}

targets_and_msd %>% 
  select(country_name, indicator, disaggregate, targets) %>% 
  mutate_at(c(4), ~replace_na(.,0)) %>% 
    simplify_targets() %>% glimpse()
