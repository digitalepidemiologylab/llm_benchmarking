# Script information ------------
#' Aim: Evaluate results from the LLM benchmarking tool
#' Author: Laura Espinosa
#' Date created: 30 December 2024
#' Date updated: 30 December 2024

# Evaluation pipeline -----------
## Variables ----------
### Datasets (targets) ------------
dataset <- "data/datasets/epfl_1000_clean_data_no_text.csv"

### LLM results -------------
llm_results <- "data/llm_results/llm_tweets_en_epfl_gpt4o.json"

### Other -----------
llm <- "gpt4o"

target <- "stance_target"

llm_prediction <- "stance"

# experiment <- "tweets_epfl_en"
experiment <- "tweet_epfl_en"

## Import dataset -----------------
df_target <- read_csv(dataset)

## Import LLM results -----------
df_llm_json <- jsonlite:::fromJSON(llm_results) 

df_llm_results <- df_llm_json$annotations %>% 
  unlist() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "id") %>% 
  rename(variables = ".") %>% 
  mutate(id = str_extract(id, "\\d+"),
         variables = map(variables, ~ fromJSON(.x))) %>% 
  unnest_wider(variables) %>% 
  mutate(across(everything(), ~ tolower(as.character(.))),
         id = as.numeric(id)) 

## Join target and LLM results -----------------
df_target_llm <- df_target %>% 
  full_join(df_llm_results, by = "id") %>% 
  mutate(across(everything(), ~ tolower(as.character(.)))) 

## Evaluate LLM ------------------
### Categorical variables -----------
unique_predictions <- df_target_llm %>% pull(!!sym(llm_prediction)) %>% unique()
unique_targets <- df_target_llm %>% pull(!!sym(target)) %>% unique()

levels <- union(unique_predictions, unique_targets)

df_conf_matrix <- df_target_llm %>% 
  #filter(agree_stance == "partial" | agree_stance == "full") %>% 
  select(!!sym(target), 
         !!sym(llm_prediction))   %>%  # modify per experiment
  mutate(!!sym(llm_prediction) := factor(!!sym(llm_prediction), levels = levels),
         !!sym(target) := factor(!!sym(target), levels = levels)) 

conf_matrix <- confusionMatrix(df_conf_matrix %>% pull(!!sym(llm_prediction)),
                               df_conf_matrix %>% pull(!!sym(target)),
                                   mode = "everything")

stats_conf_matrix <- conf_matrix$byClass %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  rename(Class = rowname)

stats_conf_matrix %>% write_csv(paste("data/evaluation/eval_stats_", experiment, "_", llm, "_", target, ".csv", sep = ""))

accuracy <- conf_matrix$overall %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  rename(!!sym(llm) := ".",
         "accuracy_variable" = "rowname")
  
accuracy %>% write_csv(paste("data/evaluation/eval_accuracy_", experiment, "_", llm, "_", target, ".csv", sep = ""))

### Numerical variables -----------------





# OLD CODE --------------------
# Experiment with WHO DON random 100 samples ----------
## Import dataset with targets -----------
df_don_simplified <- read_csv("data/datasets/DON-db_mergedTexts.csv") %>% 
  select(`texts-reduced`, DiseaseLevel1, DiseaseLevel2, Country, ISO, 
         OutbreakStartYear, OutbreakStartMonth, OutbreakStartDay,
         CasesTotal, CasesConfirmed, Deaths)

df_don_text_100_raw <- read_csv('data/datasets/DON-db_merged_text_100_tool.csv') %>% 
  rename(`texts-reduced` = `00_MSG_00_TEXT`) %>%  
  left_join(df_don_simplified) %>% 
  rename(id_file = id,
         id = id_tool) %>% 
  mutate(id = as.numeric(id))

## Import JSON results -------------------

df_don_text_100_results_json <- jsonlite:::fromJSON("data/llm_results/llm_don_random_100.json") 

df_don_text_100_results <- df_don_text_100_results_json %>% 
  unlist() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "id") %>% 
  rename(variables = ".") %>% 
  mutate(id = str_extract(id, "\\d+"),
         variables = map(variables, ~ fromJSON(.x))) %>% 
  unnest_wider(variables) %>% 
  mutate(across(everything(), ~ tolower(as.character(.))),
         id = as.numeric(id))

## Join results and target ---------
df_don_text_all <- df_don_text_100_results %>% 
  left_join(df_don_text_100_raw, by = "id")

# Experiment with tweets annotated by EPFL ----------

## Import dataset with targets -----------
# df_tweet_epfl <- read_csv(dataset_epfl) %>% 
#   mutate(agree_stance = case_when(neu_stan ==75 | neg_stan == 75 | pos_stan == 75 ~ "partial",
#                                   neu_stan ==100 | neg_stan == 100 | pos_stan == 100 ~ "full",
#                                   .default = "none"),
#          stance_partial = case_when(
#     neu_stan == pmax(neu_stan, pos_stan, neg_stan) ~ "neutral",
#     pos_stan == pmax(neu_stan, pos_stan, neg_stan) ~ "positive",
#     neg_stan == pmax(neu_stan, pos_stan, neg_stan) ~ "negative"
#   ),
#   id = 0:999) %>%
#   rename(`00_MSG_00_TEXT` = text,
#          target = stance_partial) %>% 
#   select(id, `00_MSG_00_TEXT`, target, agree_stance) %>% 
#   write_csv("data/local/epfl_1000_clean_data_2.csv")
# 
# df_tweet_epfl %>% select(-`00_MSG_00_TEXT`) %>% write_csv("data/datasets/epfl_1000_clean_data_no_text.csv")

## Import JSON results -------------------

df_tweet_epfl_json <- jsonlite:::fromJSON("data/llm_results/llm_tweets_en_epfl.json") 

df_tweet_epfl_results <- df_tweet_epfl_json$annotations %>% 
  unlist() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "id") %>% 
  rename(variables = ".") %>% 
  mutate(id = str_extract(id, "\\d+"),
         variables = map(variables, ~ fromJSON(.x))) %>% 
  unnest_wider(variables) %>% 
  mutate(across(everything(), ~ tolower(as.character(.))),
         id = as.numeric(id)) 

## Join results and target ------------

df_tweet_epfl_all <- df_tweet_epfl_results %>% 
  left_join(df_tweet_epfl, by = 'id') 

## Evaluation ------------
df_con_matrix_tweet_epfl <- df_tweet_epfl_all %>% 
  select(stance_partial, stance) %>% 
  mutate(stance_partial = factor(stance_partial, ordered = TRUE,
                                 levels = c("positive","neutral", "negative")),
         stance = factor(stance, ordered = TRUE,
                         levels = c("positive", "neutral", "negative"))) 

conf_tweet_epfl <- confusionMatrix(df_con_matrix_tweet_epfl$stance,
                                   df_con_matrix_tweet_epfl$stance_partial,
                                   mode = "everything")

conf_tweet_epfl_gpt4o <- conf_tweet_epfl$overall %>% 
  as.data.frame() %>% 
  rename(gpt4o = ".")