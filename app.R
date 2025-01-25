library(shiny)
library(tidyverse)
library(jsonlite)
library(caret)
library(DT)
library(shinyBS)

# Define UI for the application
ui <- navbarPage(
  title = "LLM benchmarking tool",
  tabPanel(
    title = "Instructions",
    fluidPage(
      sidebarPanel(
      p = "This is the tab with instructions"
    )
    )
  ),
  tabPanel(
    title = "Evaluation of categorial variables",
  fluidPage(
  titlePanel("Evaluation Pipeline"),
  sidebarLayout(
    sidebarPanel(
      tags$style(HTML("h4 { font-weight: bold; }")),
      h4("Setting up variables and paths"),
      p("Specify the required file paths, LLM details, target columns, and experiment name below."),
      textInput("dataset", "Dataset file path", "data/datasets/epfl_1000_clean_data_no_text.csv"),
      div(
        style = "display: flex; align-items: center;",
        checkboxInput("apply_transform", "Data transformation for LLM results", value = FALSE),
        actionButton("help_transform", "?", class = "btn-sm")
      ),
      
      bsTooltip("help_transform", 
                title = "When selected, the JSON LLM results file will undergo transformations such as unnesting and column renaming.   ", 
                placement = "right", 
                trigger = "hover"),
      textInput("llm_results", "LLM results file path", "data/llm_results/llm_tweets_en_epfl_gpt4o.csv"),
      textInput("evaluation_path", "Evaluation results path", "data/evaluation/"),  # New input for evaluation directory
      textInput("llm", "LLM", "gpt4o"),
      textInput("target", "Target column", "stance_target"),
      textInput("llm_prediction", "LLM prediction column", "stance"),
      textInput("experiment", "Experiment", "tweet_epfl_en"),
      
      actionButton("refresh_code", "Refresh settings"),
      
      h4("Pipeline steps"),
      p("Execute each step sequentially. Results and messages will appear in the right panel."),
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
      fluidRow(
        column(12,  # Occupies the entire width
               actionButton("llm_bench_stats", "7. LLM benchmarking (import all confusion matrix stats results)", style = "display: block; width: 100%; margin-bottom: 10px; font-weight: bold;")
        )
      ),
      fluidRow(
        column(12,  # Occupies the entire width
               actionButton("llm_bench_accuracy", "8. LLM benchmarking (import all accuracy results)", style = "display: block; width: 100%; margin-bottom: 10px; font-weight: bold;")
        )
      ),
      hr()
    ),
    mainPanel(
      h4("Pipeline Results"),
      uiOutput("results") # Dynamic output for either messages or tables
    )
  )
)
),
tabPanel(
  title = "Evaluation of numerical variables",
  fluidPage(
    titlePanel("Evaluation Pipeline"),
    sidebarLayout(
      sidebarPanel(
        tags$style(HTML("h4 { font-weight: bold; }")),
        h4("Setting up variables and paths"),
        p("Specify the required file paths, LLM details, target columns, and experiment name below."),
        textInput("dataset_num", "Dataset file path", "data/datasets/epfl_1000_clean_data_no_text.csv"),
        textInput("llm_results_num", "LLM results file path", "data/llm_results/llm_tweets_en_epfl.json"),
        textInput("evaluation_path_num", "Evaluation results path", "data/evaluation/"),  # New input for evaluation directory
        textInput("llm_num", "LLM", "gpt4o"),
        textInput("target_num", "Target column", "stance_target"),
        textInput("llm_prediction_num", "LLM prediction column", "stance"),
        textInput("experiment_num", "Experiment", "tweet_epfl_en"),
        
        h4("Pipeline steps"),
        p("Execute each step sequentially. Results and messages will appear in the right panel."),
        fluidRow(
          column(6, 
                 actionButton("import_dataset_num", "1. Import dataset", style = "display: block; width: 100%; margin-bottom: 10px;"),
                 actionButton("join_data_num", "3. Join target & LLM results", style = "display: block; width: 100%; margin-bottom: 10px;"),
                 actionButton("view_stats_num", "5. View confusion matrix stats", style = "display: block; width: 100%; margin-bottom: 10px;")
          ),
          column(6, 
                 actionButton("import_llm_num", "2. Import LLM results", style = "display: block; width: 100%; margin-bottom: 10px;"),
                 actionButton("calc_conf_matrix_num", "4. Calculate confusion matrix", style = "display: block; width: 100%; margin-bottom: 10px;"),
                 actionButton("view_accuracy_num", "6. View accuracy", style = "display: block; width: 100%; margin-bottom: 10px;")
          )
        ),
        fluidRow(
          column(12,  # Occupies the entire width
                 actionButton("step_7", "7. LLM benchmarking (import all evaluation results)", style = "display: block; width: 100%; margin-bottom: 10px; background-color: #f0f0f0; font-weight: bold;")
          )
        ),
        hr()
      ),
      mainPanel(
        h4("Pipeline Results"),
        uiOutput("results_num") # Dynamic output for either messages or tables
      )
    )
  )
)
)

# Define server logic
server <- function(input, output, session) {
  dataset_path <- reactive({
    req(input$dataset)  # Ensure the input is not empty or NULL
    input$dataset       # Return the dataset path
  })
  
  llm_results_path <- reactive({
    req(input$llm_results)  # Ensure the input is not empty or NULL
    input$llm_results       # Return the LLM results path
  })

  # Reactive values to store intermediate results
  values <- reactiveValues(
    dataset = NULL,
    df_target = NULL,
    df_llm_results = NULL,
    df_target_llm = NULL,
    conf_matrix = NULL,
    stats_conf_matrix = NULL,
    accuracy = NULL
  )
  
  # Refresh settings
  observeEvent(input$refresh_code, {
    values$dataset <- NULL
    values$df_target <- NULL
    values$df_llm_results <- NULL
    values$df_target_llm <- NULL
    
    output$results <- renderUI({
      h5("Settings refreshed. Ready for new inputs.")
    })
  })
  
  # Import dataset
  observeEvent(input$import_dataset, {
    #dataset <- input$dataset
    tryCatch({
      values$df_target <- read_csv(dataset_path())
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
    #df_llm_results <- input$llm_results
    tryCatch({
      llm_file <- llm_results_path()
      file_ext <- tools::file_ext(llm_file)  # Get the file extension (e.g., "csv", "json")
      
      if (file_ext == "json") {
        # Handle JSON files
        df_llm <- fromJSON(llm_file)
        
        if (input$apply_transform) {
          # Apply transformation for JSON
          values$df_llm_results <- df_llm$annotations %>%
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
            h5("LLM results successfully imported and transformed (JSON).")
          })
        } else {
          # No transformation, just flatten JSON
          values$df_llm_results <- as.data.frame(df_llm)
          output$results <- renderUI({
            h5("LLM results successfully imported (JSON).")
          })
        }
      } else if (file_ext == "csv") {
        # Handle CSV files
        values$df_llm_results <- read_csv(llm_file)
        output$results <- renderUI({
          h5("LLM results successfully imported (CSV).")
        })
      } else {
        stop("Unsupported file type. Please provide a CSV or JSON file.")
      }
      
    }, error = function(e) {
      output$results <- renderUI({
        h5(paste("Error importing LLM results:", as.character(e)))
      })
    })
  })
  
  # Join target and LLM results
  observeEvent(input$join_data, {
    req(input$target, input$llm_prediction)
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
        rename(Class = rowname) %>% 
        mutate(LLM = input$llm,
               Experiment = input$experiment) %>% 
        select(Experiment, LLM, everything()) %>% 
        mutate(across(where(is.numeric), 
                      ~ round(., digits = 4)))
      
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
               "accuracy_variable" = "rowname") %>% 
        mutate(LLM = input$llm,
               Experiment = input$experiment) %>% 
        select(Experiment, LLM, everything()) %>% 
        mutate(across(where(is.numeric), 
                      ~ round(., digits = 4)))
      
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
