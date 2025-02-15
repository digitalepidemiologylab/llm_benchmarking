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

#df_don_cleaned_300 %>% select(id, `00_MSG_00_TEXT`) %>% write_csv("data/datasets/DON-db_merged_300_text.csv")

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

## Deepseek LLM results -------------
### Facebook ----------
df_llm_facebook_deepseek_json <- jsonlite:::fromJSON("data/llm_results/llm_facebook_300_deepseek.json") 

df_llm_facebook_deepseek <- df_llm_facebook_deepseek_json$annotations %>% 
  unlist() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "id") %>% 
  rename(variables_raw = ".") %>% 
  mutate(stance_llm = str_extract(variables_raw, '"stance":\\s*"([^"]+)"') %>% 
           str_remove_all('"stance":\\s*"|"$'),
         across(everything(), ~ tolower(as.character(.))),
         id = as.numeric(id),
         across(everything(), ~ tolower(as.character(.))),
         id = as.numeric(id)) %>% 
  select(id, stance_llm) 

write_csv(df_llm_facebook_deepseek, "data/llm_results/llm_facebook_deepseek.csv")

### Tweets -----------------
df_llm_tweets_deepseek_json <- jsonlite:::fromJSON("data/llm_results/llm_tweets_en_epfl_deepseek.json") 

df_llm_tweets_deepseek <- df_llm_tweets_deepseek_json$annotations %>% 
  unlist() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "id") %>% 
  rename(variables_raw = ".") %>% 
  mutate(stance_llm = str_extract(variables_raw, '"stance":\\s*"([^"]+)"') %>% 
           str_remove_all('"stance":\\s*"|"$'),
         across(everything(), ~ tolower(as.character(.))),
         id = as.numeric(id)) #%>% 
  #select(id, stance_llm) 

write_csv(df_llm_tweets_deepseek, "data/llm_results/llm_tweets_en_epfl_deepseek.csv")

### WHO DONs -------------------
df_llm_don_deepseek_json <- jsonlite:::fromJSON("data/llm_results/llm_don_300_deepseek.json") 

df_llm_don_deepseek <- df_llm_don_deepseek_json$annotations %>% 
  unlist() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "id") %>% 
  rename(variables_raw = ".") %>% 
  mutate(
    virus = str_extract(variables_raw, '"virus":\\s*"([^"]+)"') %>% str_remove_all('"virus":\\s*"|"$'),
    country = str_extract(variables_raw, '"country":\\s*"([^"]+)"') %>% str_remove_all('"country":\\s*"|"$'),
    date = str_extract(variables_raw, '"date":\\s*"([^"]+)"') %>% str_remove_all('"date":\\s*"|"$'),
    cases = str_extract(variables_raw, '"cases":\\s*([0-9]+)') %>% str_remove_all('"cases":\\s*'),
    deaths = str_extract(variables_raw, '"deaths":\\s*([0-9]+)') %>% str_remove_all('"deaths":\\s*'),
    across(everything(), ~ tolower(as.character(.))),
    id = as.numeric(id)
  ) %>% 
  select(-variables_raw)

write_csv(df_llm_don_deepseek, "data/llm_results/llm_don_300_deepseek.csv")

### BabyCenter -----------------
df_llm_babycenter_deepseek_json <- jsonlite:::fromJSON("data/llm_results/llm_baby_center_deepseek.json") 

df_llm_babycenter_deepseek <- df_llm_babycenter_deepseek_json$annotations %>% 
  unlist() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "id") %>% 
  rename(variables_raw = ".") %>% 
  mutate(
    class_llm = str_extract(variables_raw, '"class":\\s*"([^"]+)"') %>% str_remove_all('"class":\\s*"|"$'),
    `adverse reaction` = str_extract(variables_raw, '"adverse reaction":\\s*"([^"]+)"') %>% str_remove_all('"adverse reaction":\\s*"|"$'),
    across(everything(), ~ tolower(as.character(.))),
    id = as.numeric(id)
  ) %>% 
  select(-variables_raw)

write_csv(df_llm_babycenter_deepseek, "data/llm_results/llm_baby_center_deepseek.csv")

## GPT o1 and o1 mini for tweets --------------
df_gpto1mini_tweets_1_json <- jsonlite:::fromJSON("data/llm_results/llm_tweets_en_epfl_gpto1mini.json")

df_gpto1mini_tweets_1 <- df_gpto1mini_tweets_1_json$annotations %>% 
  unlist() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "id") %>% 
  rename(variables_raw = ".") %>% 
  mutate(
    stance_llm = str_extract(variables_raw, '"stance":\\s*"([^"]+)"') %>% str_remove_all('"stance":\\s*"|"$'),
    across(everything(), ~ tolower(as.character(.))),
    id = as.numeric(id)
  ) %>% 
  select(-variables_raw)

df_gpto1mini_tweets_2_json <- jsonlite:::fromJSON("data/llm_results/llm_tweets_en_epfl_gpto1mini_2.json")

df_gpto1mini_tweets_2 <- df_gpto1mini_tweets_2_json$annotations %>% 
  unlist() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "id") %>% 
  rename(variables_raw = ".") %>% 
  mutate(
    stance_llm = str_extract(variables_raw, '"stance":\\s*"([^"]+)"') %>% str_remove_all('"stance":\\s*"|"$'),
    across(everything(), ~ tolower(as.character(.))),
    id = as.numeric(282:(282+501))
  ) %>% 
  select(-variables_raw)

df_llm_tweets_gpto1mini <- df_gpto1mini_tweets_1 %>% 
  rbind(df_gpto1mini_tweets_2) 

df_llm_tweets_gpto1mini %>% write_csv("data/llm_results/llm_tweets_en_epfl_gpto1mini_all.csv")

### GPT o1 ------------------
df_gpto1_tweets_1_json <- jsonlite:::fromJSON("data/llm_results/llm_tweets_en_epfl_gpto1.json")

df_gpto1_tweets_1 <- df_gpto1_tweets_1_json$annotations %>% 
  unlist() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "id") %>% 
  rename(variables_raw = ".") %>% 
  mutate(
    stance_llm = str_extract(variables_raw, '"stance":\\s*"([^"]+)"') %>% str_remove_all('"stance":\\s*"|"$'),
    across(everything(), ~ tolower(as.character(.))),
    id = as.numeric(id)
  ) %>% 
  select(-variables_raw)

df_gpto1_tweets_2_json <- jsonlite:::fromJSON("data/llm_results/llm_tweets_en_epfl_gpto1_2.json")

df_gpto1_tweets_2 <- df_gpto1_tweets_2_json$annotations %>% 
  unlist() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "id") %>% 
  rename(variables_raw = ".") %>% 
  mutate(
    stance_llm = str_extract(variables_raw, '"stance":\\s*"([^"]+)"') %>% str_remove_all('"stance":\\s*"|"$'),
    across(everything(), ~ tolower(as.character(.))),
    id = as.numeric(282:(282+716))
  ) %>% 
  select(-variables_raw)

df_llm_tweets_gpto1 <- df_gpto1_tweets_1 %>% 
  rbind(df_gpto1_tweets_2) 

df_llm_tweets_gpto1 %>% write_csv("data/llm_results/llm_tweets_en_epfl_gpto1_all.csv")
