# Script information ------------
#' Aim: Prepare datasets for the LLM benchmarking tool
#' Author: Laura Espinosa
#' Date created: 15 December 2024
#' Date updated: 30 December 2024

# Clean datasets ---------------------
## WHO DONs ---------------
df_don <- read_csv("data/datasets/DON-db_mergedTexts.csv")

df_don_cleaned <- df_don %>% 
  filter(!is.na(`texts-reduced`)) %>% 
  filter(!is.na(CasesTotal)) %>% 
  filter(!is.na(ISO))  %>% 
  filter(!is.na(DiseaseLevel1)) %>% 
  mutate(id = 1:nrow(.)) 

df_don_cleaned_300 <- df_don_cleaned %>% 
  sample_n(300) %>% 
  rename(`00_MSG_00_TEXT` = `texts-reduced`)

df_don_cleaned_300 %>% write_csv("data/datasets/DON-db_merged_300_all_variables.csv")

df_don_cleaned_300 %>% select(id, `00_MSG_00_TEXT`) %>% write_csv("data/datasets/DON-db_merged_300_text.csv")

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

## Facebook posts ----------------
df_fb_anti <- read_csv("data/datasets/local/anti - anti.csv") %>% 
  filter(!is.na(CLASS_COMMENT)) %>% 
  sample_n(150) %>% 
  mutate(id = 0:149)

df_fb_pro <- read_csv("data/datasets/local/pro - pro.csv") %>% 
  filter(!is.na(CLASS_COMMENT)) %>% 
  sample_n(150) %>% 
  mutate(id = 0:149)

df_fb <- df_fb_anti %>% 
  rbind(df_fb_pro) %>% 
  mutate(id = 0:299) %>% 
  rename(`00_MSG_00_TEXT` = message)

df_fb %>% write_csv("data/datasets/local/facebook_anti_pro_300_comments.csv")

df_fb %>% select(id, post_id, CLASS_COMMENT) %>% write_csv("data/datasets/facebook_anti_pro_300_comments_anonymised.csv")
