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
  mutate("Accuracy (IQR)" = paste(Accuracy, " (", AccuracyLower, "-", AccuracyUpper, ")"),
         `P-value` = case_when(AccuracyPValue <= 0.05 ~ "<= 0.05",
                               .default = "> 0.05"))

accuracy_num <- read_csv("data/evaluation/eval_all_metrics.csv")

accuracy_num_wider <- accuracy_num %>% 
  pivot_wider(names_from = "Metric", values_from = "Value")

accuracy_num_wider_plot <- accuracy_num_wider %>% 
  mutate(Experiment = case_when(
    Experiment == "don_300" ~ "WHO DONs",
    .default = "NA"),
    AccuracyLower = Accuracy,
    AccuracyUpper = Accuracy,
    `P-value` = "Non applicable",
    Accuracy = Accuracy/100,
    Target = case_when(Target == "CasesTotal" ~ "Reported cases",
                       Target == "Deaths" ~ "Reported deaths",
                       .default = "NA"))

### Overall accuracy with CI plot --------
accuracy_plot <- accuracy_cat_wider %>% 
  mutate(Experiment = case_when(Experiment == "baby_center" ~ "BabyCenter posts",
                                Experiment == "don_300" ~ "WHO DONs",
                                Experiment == "facebook_300" ~ "Facebook posts",
                                Experiment == "tweet_en_epfl" ~ "Tweets",
                                .default = "NA"),
         Target = case_when(Target == "adverse_reactions" ~ "Vaccine adverse reactions",
                            Target == "stance_target" ~ "Stance towards vaccination",
                            Target == "ISO" ~ "Affected countries (ISO)",
                            Target == "DiseaseLevel1" ~ "Reported disease",
                            .default = "NA")) %>%
  full_join(accuracy_num_wider_plot, by = c("Accuracy", "P-value", "Experiment", "LLM",
                                            "AccuracyLower", "AccuracyUpper", "Target")) %>% 
  mutate(LLM = case_when(LLM == "claude" ~ "Claude 3.5",
                         LLM == "gemini" ~ "Gemini 1.5",
                         LLM == "gemini2" ~ "Gemini 2.0",
                         LLM == "gpt4o" ~ "GPT-4o",
                         LLM == "gpt4omini" ~ "GPT-4o mini",
                         LLM == "gpto1" ~ "GPT o1",
                         LLM == "gpto1mini" ~ "GPT o1 mini",
                         LLM == "gpto3mini" ~ "GPT o3 mini",
                         LLM == "deepseek" ~ "Deepseek R1",
                         .default = "NA")) %>%
  ggplot(aes(x = LLM, y = Accuracy, group = Target)) +
  geom_point(aes(color = `P-value`, shape = Target, size = Target),
             #size = 3, 
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = AccuracyLower,
                    ymax = AccuracyUpper,
                    color = `P-value`),
                width = 0.2, linewidth = 0.5, 
                position = position_dodge(width = 0.5)) +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                     limits = c(0,1), 
                     expand = c(0,0.01)) +
  scale_size_manual(values = c(4, 4.5, 3.5, 4, 3, 3)) +
  scale_shape_manual(values = c(18, 10, 7, 19, 15, 17)) +
  scale_color_manual(values = c("red", "black", "blue")) +
  facet_grid(~Experiment, scales = "free_x") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2),
    panel.background = element_rect(fill = "grey95", color = NA), 
    panel.grid.major = element_line(color = "white"),  
    panel.grid.minor = element_line(color = "white"),  
    strip.background = element_rect(fill = "grey95", color = "black") 
  ) +
  labs(x = "Large Language Model")

accuracy_plot
ggsave("outputs/accuracy_all.png", height = 6, width = 8)

## Confusion matrix metrics ----------
df_stats <- read_csv("data/evaluation/eval_all_stats.csv") %>% 
  arrange(Experiment, Target, desc("Balanced Accuracy"))

df_stats_plot <- df_stats %>% 
  select(Experiment, LLM, Target, Class, Frequency, F1, Sensitivity, Specificity, Frequency) %>%
  pivot_longer(cols = c("F1", "Sensitivity", "Specificity"), names_to = "metric",
               values_to = "metric_value") %>% 
  filter(Target != "adverse_reaction") %>% 
  mutate(Experiment = case_when(Experiment == "baby_center" ~ "BabyCenter posts\n(vaccine adverse reactions)",
                                Experiment == "don_300" ~ "WHO DONs",
                                Experiment == "facebook_300" ~ "Facebook posts\n(stance towards vaccination)",
                                Experiment == "tweet_en_epfl" ~ "Tweets\n(stance towards vaccination)",
                                .default = "NA"),
         Experiment = case_when(Experiment == "WHO DONs" & Target == "ISO" ~ "WHO DONs (Affected\n countries (ISO))",
                                Experiment == "WHO DONs" & Target == "DiseaseLevel1" ~ "WHO DONs\n(Reported disease)",
                                .default = Experiment),
         Target = case_when(Target == "adverse_reactions" ~ "Vaccine adverse reactions",
                            Target == "stance_target" ~ "Stance towards vaccination",
                            Target == "ISO" ~ "Affected countries (ISO)",
                            Target == "DiseaseLevel1" ~ "Reported disease",
                            .default = "NA")) %>% 
  filter(Class != "unrelated")

stats_plot <- df_stats_plot %>% 
  select(-Frequency) %>% 
  filter(Experiment != "WHO DONs (Affected\n countries (ISO))" & Experiment != "WHO DONs\n(Reported disease)") %>% 
  mutate(LLM = case_when(LLM == "claude" ~ "Claude\n3.5",
                         LLM == "gemini" ~ "Gemini\n1.5",
                         LLM == "gemini2" ~ "Gemini\n2.0",
                         LLM == "gpt4o" ~ "GPT\n4o",
                         LLM == "gpt4omini" ~ "GPT\n4o\nmini",
                         LLM == "gpto1" ~ "GPT\no1",
                         LLM == "gpto1mini" ~ "GPT\no1\nmini",
                         LLM == "gpto3mini" ~ "GPT\no3\nmini",
                         LLM == "deepseek" ~ "Deep-\nseek\nR1",
                         .default = "NA")) %>% 
  ggplot(aes(x = Class)) +
  geom_point(aes(y = metric_value, color = metric,
                 shape = metric),
             size = 2)  +
  facet_nested(~ Experiment + LLM, scales = "free_x", space = "fixed") +
  scale_y_continuous(limits = c(0.3,1),
                     expand = c(0.01, 0),
                     breaks = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  scale_shape_manual(values = c(17, 19, 15)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 13),
        axis.text.x = element_text(vjust = 0.5, hjust = 1, angle = 90),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        axis.title = element_text(size = 14),
        #strip.text.y = element_text(size = 7),
        strip.text = element_text(size = 13),  
        legend.position = "bottom",
        plot.background = element_rect(color = "black", fill="white", size=1),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black", size = 0.5),
        legend.text = element_text(size = 14),
        panel.spacing.x = unit(0.05, "lines"),   # No space between LLMs within each experiment
        #strip.placement = "outside",         # Ensures facet headers align better
        legend.title = element_text(size = 10, face = "bold")) +
  labs(x = "Target classes",
       y = "Performance metric's value",
       colour = "Performance metric", 
       shape = "Performance metric",
       linetype = "F1 score for selecting \n majority class") 

stats_plot

ggsave("outputs/confusion_matrix_all.png", stats_plot, 
       width=17.45, height=9.25)

### WHO DONs ----------------
df_who_don <- read_csv("data/datasets/DON-db_merged_300_all_variables.csv")

df_who_don_ISO <- df_who_don %>% 
  select(ISO) %>% 
  unique() %>% 
  rename(Class = ISO)

df_who_don_disease <- df_who_don %>% 
  select(DiseaseLevel1) %>% 
  unique( ) %>% 
  rename(Class = DiseaseLevel1)

df_who_don_disease_iso <- df_who_don_disease %>% 
  rbind(df_who_don_ISO) %>% 
  mutate(Class = tolower(Class))


stats_plot_don <- df_stats_plot %>% 
  filter(Experiment == "WHO DONs (Affected\n countries (ISO))" 
         | Experiment == "WHO DONs\n(Reported disease)") %>%
  filter(!is.na(metric_value)) %>% 
  filter(Class %in% df_who_don_disease_iso$Class) %>% 
  filter(case_when(Target == "Reported disease" ~ Frequency >= 1.925,
                   Target == "Affected countries (ISO)" ~ Frequency >= 1.7,
                   .default =  FALSE)) %>% 
  mutate(Experiment = case_when(Experiment == "WHO DONs (Affected\n countries (ISO))" ~
                                  "WHO DONs (Affected countries (ISO))",
                                Experiment == "WHO DONs\n(Reported disease)" ~
                                  "WHO DONs (Reported disease)"),
         Class = case_when(Experiment == "WHO DONs (Affected countries (ISO))" ~ toupper(Class),
                           .default = Class),
         LLM = case_when(LLM == "claude" ~ "Claude\n3.5",
                         LLM == "gemini" ~ "Gemini\n1.5",
                         LLM == "gemini2" ~ "Gemini\n2.0",
                         LLM == "gpt4o" ~ "GPT\n4o",
                         LLM == "gpt4omini" ~ "GPT\n4o\nmini",
                         LLM == "gpto1" ~ "GPT\no1",
                         LLM == "gpto1mini" ~ "GPT\no1\nmini",
                         LLM == "gpto3mini" ~ "GPT\no3\nmini",
                         LLM == "deepseek" ~ "Deep-\nseek\nR1",
                         .default = "NA"),
         Class = case_when(Target == "Reported disease" ~ str_to_sentence(Class),
                           .default = Class),
         Class = case_when(Class == "Meningococcal disease" ~ "Meningococcal\ndisease",
                            Class == "Syndromic: neurological" ~ "Syndromic:\nneurological",
                            Class == "Influenza a" ~ "Influenza A",
                           .default = Class)) %>% 
  ggplot(aes(x = Class)) +
  geom_point(aes(y = metric_value, color = metric,
                 shape = metric),
             size = 2)  +
  facet_nested(LLM ~ Experiment, scales = "free_x", 
               #space = "fixed",
               labeller = labeller(Target = c(
                 "WHO DONs (Reported disease)" = "Disease Labels",
                 "WHO DONs (Affected countries (ISO))" = "Country Labels"))) +
  scale_y_continuous(limits = c(0,1),
                     expand = c(0.05, 0),
                     breaks = c(0, 0.5, 1)) +
  scale_shape_manual(values = c(17, 19, 15)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 13),
        axis.text.x = element_text(vjust = 1, hjust = 1, angle = 45,
                                   margin = margin(t = -3, r = 8),
                                   lineheight = 0.7),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        axis.title = element_text(size = 14),
        strip.placement = "outside",  # Moves facet labels to the outside
        #strip.text.y = element_text(size = 7),
        strip.text = element_text(size = 13),  
        legend.position = "right",
        plot.background = element_rect(color = "black", fill="white", size=1),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black", size = 0.5),
        legend.text = element_text(size = 14),
        panel.spacing.x = unit(0.05, "lines"),   # No space between LLMs within each experiment
        #strip.placement = "outside",         # Ensures facet headers align better
        legend.title = element_text(size = 10, face = "bold")) +
  labs(x = "Target classes",
       y = "Performance metric's value",
       colour = "Performance metric", 
       shape = "Performance metric",
       linetype = "F1 score for selecting \n majority class") 

stats_plot_don

ggsave("outputs/confusion_matrix_don.png", stats_plot_don, 
       width=17.45, height=9.25)


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

# Other relevant aspects for the evaluation -----------------
## Tweets and o1/o1 mini models ---------------
df_tweets <- read_csv("data/datasets/epfl_1000_clean_data_no_text.csv")

### o1 results -------------
df_tweets_o1 <- read_csv("data/llm_results/llm_tweets_en_epfl_gpto1_all.csv")

df_tweets_o1_partial <- df_tweets %>% 
  filter(agree_stance != "none") %>% 
  left_join(df_tweets_o1, by = "id")

### o1 mini results ----------------
df_tweets_o1mini <- read_csv("data/llm_results/llm_tweets_en_epfl_gpto1mini_all.csv")

df_tweets_o1mini_partial <- df_tweets %>% 
  filter(agree_stance != "none") %>% 
  left_join(df_tweets_o1mini, by = "id")

