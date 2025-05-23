# Script information ------------
#' Aim: EDA for the datasets included
#' Author: Laura Espinosa
#' Date created: 2 February 2025
#' Date updated: 2 February 2025

# Tweets in English -----------------
## Import data ------------
df_epfl <- read_csv("data/datasets/epfl_1000_clean_data_no_text_partial_agreement.csv")

## Frequency per class ----------------
df_epfl_class <- df_epfl %>% 
  count(stance_target) %>% 
  mutate(Experiment = "Stance_vaccination_tweets",
         Percent = round(n/sum(n) * 100, digits = 1)) %>% 
  rename(Class = stance_target,
         Frequency = n) %>% 
  select(Experiment, Class, Frequency, Percent)

## Plot of classes -----------
plot_epfl_class <- df_epfl_class %>% 
  select(Class, Percent) %>% 
  ggplot(aes(x = Class, y = Percent)) +
  geom_bar(stat = "identity", position = "stack")

plot_epfl_class

# WHO DONs ----------------------
## Import data ------------
df_don <- read_csv("data/datasets/DON-db_merged_300_all_variables.csv") %>% 
  mutate(ReportDate = as.Date(ReportDate, format = "%m/%d/%Y"))

## Frequency per class ----------------
df_don_class_country <- df_don %>% 
  count(ISO) %>% 
  mutate(Experiment = "WHO_DON",
         Target = "Country",
         Percent = round(n/sum(n) * 100, digits = 1)) %>% 
  rename(Class = ISO,
         Frequency = n) %>% 
  select(Experiment, Target, Class, Frequency, Percent)

df_don_class_disease <- df_don %>% 
  count(DiseaseLevel1) %>% 
  mutate(Experiment = "WHO_DON",
         Target = "Disease",
         Percent = round(n/sum(n) * 100, digits = 1)) %>% 
  rename(Class = DiseaseLevel1,
         Frequency = n) %>% 
  select(Experiment, Target, Class, Frequency, Percent)

df_don_class <- rbind(df_don_class_country, df_don_class_disease)

## Plot of classes -----------
plot_don_country <- df_don_class_country %>% 
  select(Class, Percent) %>% 
  ggplot(aes(x = reorder(Class, -Percent), y = Percent)) +
  geom_bar(stat = "identity", fill = "#377EB8", width = 0.6) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +  
  labs(x = "ISO countries", y = "Percent (%)",
       title = "(d)") +
  theme_classic() +
  theme(plot.margin = margin(10, 10, 40, 10),
        plot.background = element_rect(color = "black", 
                                       fill = "white", size = 1),
        axis.text.x = element_text(
          #angle = 45, 
          #hjust = 0, 
          size = 14,
          vjust = 0.5),
        axis.text = element_text(size = 16),
        title = element_text(size = 16),
        legend.text = element_text(size = 16))

plot_don_country

plot_don_disease <- df_don_class_disease %>% 
  select(Class, Percent) %>% 
  mutate(Class = case_when(Class == "Crimean-Congo haemorrhagic fever" ~ "CCHF",
                           Class == "Zika virus disease" ~ "Zika",
                           Class == "Syndromic: neurological" ~ "Neurological",
                           Class == "Marburg fever" ~ "Marburg",
                           Class == "Pseudomonas aeruginosa" ~ "Ps. aeruginosa",
                           Class == "Syndromic: diarrhoeal" ~ "Diarrhoeal",
                           Class == "Syndromic: respiratory" ~ "Respiratory",
                           Class == "Transmissible spongiform encephalopathy" ~ "TSE",
                           Class == "Human coronavirus OC43"~ "Coronavirus OC43",
                           Class == "Hand, foot, and mouth disease" ~ "HFM disease",
                           .default = Class)) %>% 
  mutate(Class = str_wrap(Class, width = 30)) %>%  
  ggplot(aes(x = reorder(Class, -Percent), y = Percent)) +
  geom_bar(stat = "identity", fill = "#5AAE61", width = 0.7) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +  # Staggers labels to prevent overlap
  labs(x = "Disease", y = "Percent (%)",
       title = "(e)") +
  theme_classic() +
  theme(plot.margin = margin(10, 10, 10, 10),
        plot.background = element_rect(color = "black", fill = "white", size = 1),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,
                                   lineheight = 0.6, size = 16),
        axis.text = element_text(size = 16),
        title = element_text(size = 16),
        legend.text = element_text(size = 16))

plot_don_disease

plot_don_cases <- df_don %>%
  mutate(CasesTotal = replace_na(as.numeric(CasesTotal), 0),
         Deaths = replace_na(as.numeric(Deaths), 0)) %>%
  rename(Cases = CasesTotal) %>% 
  pivot_longer(cols = c(Cases, Deaths),  
               names_to = "Variable",
               values_to = "Value") %>% 
  filter(Variable == "Cases") %>% 
  ggplot(aes(y = Variable, x = Value)) +
  geom_boxplot() +
  labs(title = "(b)",
       caption = "Note: Outlier at 616,034 cases is not shown.") +
  scale_x_continuous(
    labels = comma,
    expand = c(0.01, 0),
    limits = c(-1000, 220000),
    breaks = c(0, 100000, 200000),
    #limits = c(0, 650000)
    ) +
  # scale_x_break(c(210000, 600000), 
  #               #scales = c(0.2,0.8),
  #               space = 0.2) +
  # scale_x_break(c(120000, 190000), 
  #               #scales = c(0.2,0.8),
  #               space = 0.2) +
  theme_classic() +
  theme(plot.margin = margin(10, 10, 10, 10),
        plot.background = element_rect(color = "black", fill = "white", size = 1),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_text(size = 16),
        panel.border = element_blank(),
        title = element_text(size = 16),
        legend.text = element_text(size = 16),
        plot.caption = element_text(size = 16, hjust = 0.9, face = "italic"))

plot_don_cases

plot_don_deaths <- df_don %>%
  mutate(CasesTotal = replace_na(as.numeric(CasesTotal), 0),
         Deaths = replace_na(as.numeric(Deaths), 0)) %>%
  rename(Cases = CasesTotal) %>% 
  pivot_longer(cols = c(Cases, Deaths),  
               names_to = "Variable",
               values_to = "Value") %>% 
  filter(Variable == "Deaths") %>% 
  ggplot(aes(y = Variable, x = Value)) +
  geom_boxplot() +
  scale_x_continuous(labels = comma,
                     limits = c(0, 4500),
                     expand = c(0.01,0)) +
  labs(title = "(c)") +
  theme_classic() +
  theme(plot.margin = margin(10, 10, 10, 10),
        plot.background = element_rect(color = "black", fill = "white", size = 1),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_text(size = 16),
        title = element_text(size = 16),
        legend.text = element_text(size = 16))

plot_don_deaths

# BabyCenter forum posts ----------------------
## Import data ------------
df_baby <- read_csv("data/datasets/Babycenter_adverse_reactions_cleaned_anonymised.csv")

## Frequency per class ----------------
df_baby_class <- df_baby %>% 
  count(adverse_reactions) %>% 
  mutate(Experiment = "Vaccine_adverse_reaction",
         Percent = round(n/sum(n) * 100, digits = 1)) %>% 
  rename(Class = adverse_reactions,
         Frequency = n) %>% 
  select(Experiment, Class, Frequency, Percent)

# Facebook posts -----------------
## Import data ------------
df_facebook <- read_csv("data/datasets/facebook_anti_pro_300_comments_anonymised.csv")

## Frequency per class ----------------
df_facebook_class <- df_facebook %>% 
  count(stance_target) %>% 
  mutate(Experiment = "Stance_vaccination_facebook",
         Percent = round(n/sum(n) * 100, digits = 1)) %>% 
  rename(Class = stance_target,
         Frequency = n) %>% 
  select(Experiment, Class, Frequency, Percent)

# Combined datasets -------------
## Get datasets --------
df_epfl_baby_facebook_class <- rbind(df_epfl_class, df_baby_class, df_facebook_class)

## Plot of classes -----------
plot_epfl_baby_facebook <- df_epfl_baby_facebook_class %>% 
  mutate(Class = str_replace(Class, "ANTI", "negative"),
         Class = str_replace(Class, "PRO", "positive"),
         Class = str_replace(Class, "NON", "neutral"),
         Class = str_replace(Class, "no", " no"),
         Class = str_replace(Class, "yes", " yes"),
         Class = str_replace(Class, "unrelated", " unrelated"),
         Experiment = str_replace(Experiment, "Stance_vaccination_facebook", "Facebook"),
         Experiment = str_replace(Experiment, "Stance_vaccination_tweets", "Twitter"),
         Experiment = str_replace(Experiment, "Vaccine_adverse_reaction", "BabyCenter")) %>% 
  ggplot(aes(fill = Class, x = Experiment, y = Percent)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c(" no" = "#D32F2F",
                               " unrelated" = "#D2691E",
                               " yes" = "#DAA520",
                               "negative" = "#E41A1C",
                               "neutral" = "#377EB8",
                               "positive" = "#5AAE61")) +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Percent (%)", title = "(a)") +
  theme_classic() +
  theme(plot.margin = margin(10, 10, 10, 10),
        plot.background = element_rect(color = "black", fill = "white", size = 1),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text = element_text(size = 16),
        title = element_text(size = 16),
        legend.text = element_text(size = 16)) 

plot_epfl_baby_facebook

## Plot with all EDA of all use cases ----------------
left_column <- ggarrange(
  plot_epfl_baby_facebook, 
  plot_don_cases, 
  plot_don_deaths, 
  ncol = 3, 
  widths = c(1, 1.5, 1.5)  
)

right_column <- ggarrange(
  plot_don_country, 
  plot_don_disease, 
  ncol = 1, heights = c(0.6, 1)
)

plot_all <- ggarrange(
  left_column, right_column, 
  ncol = 1, 
  heights = c(0.9, 2)  
)

plot_all

ggsave("outputs/eda_plot_use_cases.png", plot_all,
       width = 20, height = 10)
