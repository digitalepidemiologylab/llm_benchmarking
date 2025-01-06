# Script information ------------
#' Aim: Prepare datasets for the LLM benchmarking tool
#' Author: Laura Espinosa
#' Date created: 15 December 2024
#' Date updated: 30 December 2024

# Import datasets ---------------------
df_don <- read_csv("data/datasets/DON-db_mergedTexts.csv")

df_don_text <- df_don %>% 
  filter(!is.na(`texts-reduced`)) %>% 
  select(`texts-reduced`) %>% 
  mutate(id = 1:nrow(.)) %>% 
  filter(!is.na(`texts-reduced`))

df_don_text %>% write_csv("data/datasets/DON-db_merged_text.csv")

df_don_text %>% sample_n(100) %>% rename(`00_MSG_00_TEXT` = `texts-reduced`) %>% write_csv("data/datasets/DON-db_merged_text_100.csv")

df_don_country_iso <- df_don %>% 
  filter(!is.na(ISO)) %>% 
  select(`texts-reduced`, ISO)

df_don_country_iso %>% write_csv("data/datasets/DON-db_merged_country_iso.csv")

df_don_cases_total <- df_don %>% 
  filter(!is.na(CasesTotal)) %>% 
  select(`texts-reduced`, CasesTotal)

df_don_cases_total %>% write_csv("data/datasets/DON-db_merged_cases_total.csv")
