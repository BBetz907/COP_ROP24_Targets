indicators <- c("HTS_TST", "KP_PREV", "TX_NEW", "TX_CURR", "TX_PVLS", "PrEP_NEW")
mechanism_by_ind <- bind_rows(MER, alltargets) %>% filter(str_detect(results_or_targets, "^Targets|Cumulative"), funding_agency=="USAID", indicator %in% indicators) %>%
  mutate(kp = if_else(str_detect(disaggregate,"KeyPop"), "KP", "Total")) %>%
  group_by(fy, ou, country_name, mech_code, mech_name, prime_partner, partner_type, kp, indicator, results_or_targets) %>% 
  summarise(values = sum(values, na.rm = TRUE)) %>% pivot_wider(names_from = kp, values_from = values) %>%
  mutate(kpshare = round(if_else(!indicator=="KP_PREV", KP/Total, 1), 2),
         nonkp = if_else(!indicator=="KP_PREV", Total -KP, 0)) %>% glimpse()


mechanisms <- mechanism_by_ind %>% group_by(fy, ou, country_name, mech_code, mech_name, prime_partner, partner_type, results_or_targets) %>%
  summarise(kpshares = mean(kpshare, na.rm=TRUE)) %>% ungroup() %>%
  mutate(kpshares = round(kpshares,2)) %>% filter(kpshares != 0) %>% glimpse()


mechanism_by_ind2 <- mechanism_by_ind %>% select(!c("kpshare", "Total")) %>% pivot_longer(cols = KP:nonkp, names_to = "pop") %>% glimpse()

write_csv(mechanisms, "C:/Users/bbetz/projects/COP22/Data Out/Mechanisms.csv", na = "")
write_csv(mechanism_by_ind2, "C:/Users/bbetz/projects/COP22/Data Out/Mechanisms_by_indicator.csv", na = "")

help("summarise")
