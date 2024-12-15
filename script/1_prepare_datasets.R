# Script information ------------
#' Aim: Prepare datasets for the LLM benchmarking tool
#' Author: Laura Espinosa
#' Date created: 15 December 2024
#' Date updated: 15 December 2024

# Import datasets ---------------------
df_don <- read_csv("data/DON-db_mergedTexts.csv")

df_don_country_iso <- df_don %>% 
  filter(!is.na(ISO)) %>% 
  select(`texts-reduced`, ISO)

df_don_country_iso %>% write_csv("data/DON-db_merged_country_iso.csv")

df_don_cases_total <- df_don %>% 
  filter(!is.na(CasesTotal)) %>% 
  select(`texts-reduced`, CasesTotal)

df_don_cases_total %>% write_csv("data/DON-db_merged_cases_total.csv")
