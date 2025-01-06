library(shiny)
library(tidyverse)
library(jsonlite)
library(caret)
library(DT) # For formatted tables

# Define UI for the application
ui <- fluidPage(
  titlePanel("Evaluation Pipeline"),
  sidebarLayout(
    sidebarPanel(
      tags$style(HTML("h4 { font-weight: bold; }")),
      h4("Setting up variables and paths"),
      p("Specify the required file paths, LLM details, target columns, and experiment name below."),
      textInput("dataset", "Dataset file path", "data/datasets/epfl_1000_clean_data_no_text.csv"),
      textInput("llm_results", "LLM results file path", "data/llm_results/llm_tweets_en_epfl.json"),
      textInput("evaluation_path", "Evaluation results path", "data/evaluation/"),  # New input for evaluation directory
      textInput("llm", "LLM", "gpt4o"),
      textInput("target", "Target column", "stance_target"),
      textInput("llm_prediction", "LLM prediction column", "stance"),
      textInput("experiment", "Experiment", "tweet_epfl_en"),
      
      h4("Pipeline steps"),
      fluidRow(
        column(6, 
               actionButton("import_dataset", "1. Import dataset", style = "display: block; width: 100%; margin-bottom: 10px;"),
               actionButton("join_data", "3. Join target & LLM results", style = "display: block; width: 100%; margin-bottom: 10px;"),
               actionButton("view_stats", "5. View confusion matrix stats", style = "display: block; width: 100%; margin-bottom: 10px;")
        ),
        column(6, 
               actionButton("import_llm", "2. Import LLM results", style = "display: block; width: 100%; margin-bottom: 10px;"),
               actionButton("calc_conf_matrix", "4. Calculate confusion matrix", style = "display: block; width: 100%; margin-bottom: 10px;"),
               actionButton("view_accuracy", "6. View accuracy", style = "display: block; width: 100%; margin-bottom: 10px;")
        )
      ),
      hr(),
      p("Execute each step sequentially. Results and messages will appear in the right panel.")
    ),
    mainPanel(
      h4("Pipeline Results"),
      uiOutput("results") # Dynamic output for either messages or tables
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store intermediate results
  values <- reactiveValues(
    df_target = NULL,
    df_llm_results = NULL,
    df_target_llm = NULL,
    conf_matrix = NULL,
    stats_conf_matrix = NULL,
    accuracy = NULL
  )
  
  # Import dataset
  observeEvent(input$import_dataset, {
    tryCatch({
      values$df_target <- read_csv(input$dataset)
      output$results <- renderUI({
        h5("Dataset successfully imported.")
      })
    }, error = function(e) {
      output$results <- renderUI({
        h5(paste("Error importing dataset:", as.character(e)))
      })
    })
  })
  
  # Import LLM results
  observeEvent(input$import_llm, {
    tryCatch({
      df_llm_json <- fromJSON(input$llm_results)
      values$df_llm_results <- df_llm_json$annotations %>%
        unlist() %>%
        as.data.frame() %>%
        rownames_to_column(var = "id") %>%
        rename(variables = ".") %>%
        mutate(id = str_extract(id, "\\d+"),
               variables = map(variables, ~ fromJSON(.x))) %>%
        unnest_wider(variables) %>%
        mutate(across(everything(), ~ tolower(as.character(.))),
               id = as.numeric(id))
      output$results <- renderUI({
        h5("LLM results successfully imported.")
      })
    }, error = function(e) {
      output$results <- renderUI({
        h5(paste("Error importing LLM results:", as.character(e)))
      })
    })
  })
  
  # Join target and LLM results
  observeEvent(input$join_data, {
    tryCatch({
      req(values$df_target, values$df_llm_results)
      values$df_target_llm <- values$df_target %>%
        full_join(values$df_llm_results, by = "id") %>%
        mutate(across(everything(), ~ tolower(as.character(.))))
      output$results <- renderUI({
        h5("Target and LLM results successfully joined.")
      })
    }, error = function(e) {
      output$results <- renderUI({
        h5(paste("Error joining target and LLM results:", as.character(e)))
      })
    })
  })
  
  # Calculate confusion matrix
  observeEvent(input$calc_conf_matrix, {
    tryCatch({
      req(values$df_target_llm)
      unique_predictions <- values$df_target_llm %>% pull(!!sym(input$llm_prediction)) %>% unique()
      unique_targets <- values$df_target_llm %>% pull(!!sym(input$target)) %>% unique()
      levels <- union(unique_predictions, unique_targets)
      
      df_conf_matrix <- values$df_target_llm %>%
        select(!!sym(input$target), !!sym(input$llm_prediction)) %>%
        mutate(!!sym(input$llm_prediction) := factor(!!sym(input$llm_prediction), levels = levels),
               !!sym(input$target) := factor(!!sym(input$target), levels = levels))
      
      values$conf_matrix <- confusionMatrix(df_conf_matrix %>% pull(!!sym(input$llm_prediction)),
                                            df_conf_matrix %>% pull(!!sym(input$target)),
                                            mode = "everything")
      output$results <- renderUI({
        h5("Confusion matrix successfully calculated.")
      })
    }, error = function(e) {
      output$results <- renderUI({
        h5(paste("Error calculating confusion matrix:", as.character(e)))
      })
    })
  })
  
  # View statistics of the confusion matrix
  observeEvent(input$view_stats, {
    tryCatch({
      req(values$conf_matrix)
      values$stats_conf_matrix <- values$conf_matrix$byClass %>%
        as.data.frame() %>%
        rownames_to_column() %>%
        rename(Class = rowname)
      
      write_csv(
        values$stats_conf_matrix, 
        paste0(input$evaluation_path, "eval_stats_", input$experiment, "_", input$llm, "_", input$target, ".csv")
      )
      
      output$results <- renderUI({
        DT::dataTableOutput("conf_matrix_stats")
      })
      output$conf_matrix_stats <- DT::renderDataTable(values$stats_conf_matrix, options = list(pageLength = 10, scrollX = TRUE))
    }, error = function(e) {
      output$results <- renderUI({
        h5(paste("Error generating statistics:", as.character(e)))
      })
    })
  })
  
  # View accuracy
  observeEvent(input$view_accuracy, {
    tryCatch({
      req(values$conf_matrix)
      values$accuracy <- values$conf_matrix$overall %>%
        as.data.frame() %>%
        rownames_to_column() %>%
        rename(!!sym(input$llm) := ".",
               "accuracy_variable" = "rowname")
      
      write_csv(
        values$accuracy, 
        paste0(input$evaluation_path, "eval_accuracy_", input$experiment, "_", input$llm, "_", input$target, ".csv")
      )
      
      output$results <- renderUI({
        DT::dataTableOutput("accuracy_table")
      })
      output$accuracy_table <- DT::renderDataTable(values$accuracy, options = list(pageLength = 10, scrollX = TRUE))
    }, error = function(e) {
      output$results <- renderUI({
        h5(paste("Error calculating accuracy:", as.character(e)))
      })
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
