# Script information ------------
#' Aim: Evaluate results from the LLM benchmarking tool
#' Author: Laura Espinosa
#' Date created: 30 December 2024
#' Date updated: 30 December 2024

# Evaluation from Shiny outputs --------------
## Accuracy -----------
accuracy_cat <- read_csv("data/evaluation/eval_all_accuracy.csv")

accuracy_cat_wider <- accuracy_cat %>% 
  pivot_wider(names_from = "Accuracy (variable)", values_from = "Accuracy (value)") %>% 
  mutate("Accuracy (IQR)" = paste(Accuracy, " (", AccuracyLower, "-", AccuracyUpper, ")"))

accuracy_num <- read_csv("data/evaluation/eval_all_metrics.csv")


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





