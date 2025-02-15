library(shiny)
library(tidyverse)
library(jsonlite)
library(caret)
library(DT)
library(shinyBS)
library(yardstick)

#options(warn = -1)  # Suppress all warnings globally

# Define UI for the application -----------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Style for buttons */
    .btn {
      white-space: normal !important; /* Allow text to wrap */
      word-wrap: break-word; /* Break long words */
      text-align: center; /* Center-align text */
      display: inline-block; /* Maintain inline-block for flexible size */
      min-height: 40px; /* Set a minimum height */
      padding: 10px; /* Add padding for better spacing */
    }

    /* Increase font size in Instructions tab */
    .instructions-text {
      font-size: 18px; /* Adjust as needed */
      font-weight: normal;
      color: #333333;
      line-height: 1.6;
    }
    "))
  ),
  navbarPage(
  ## General tab -------
  title = "LLM benchmarking tool",
  
  ## Instructions tab ------------
  tabPanel(
    title = "Instructions",
    fluidPage(
      div(class = "instructions-text",
          h1("How to Use This Tool"),
      fluidRow(
        column(
          width = 6,
          h2("Information on the tool"),
          p("• This tool is designed for benchmarking large language models (LLMs) for public health use cases."),
          p("• The pipeline supports both categorical and numerical variables."),
          p("• Data transformation is available for specific JSON-based results."),
          p("• Ensure all required inputs are correctly specified before running analyses."),
          p("• Confusion matrix, accuracy and other metrics are automatically generated and stored."),
          hr()
        ),
        column(
          width = 6,
          h2("Requirements"),
          fluidPage(
            p("• CSV file with the target dataset with at least two columns: ", 
              tags$code("id"), " (unique identifier for every entry), ", 
              tags$code("column_target"), " (gold standard of the categorical or numerical variable)."),
            
            p("• CSV file with the LLM annotations/extractions with at least two columns: ", 
              tags$code("id"), " (same unique identifier as the target dataset), and ", 
              tags$code("column_llm"), " (LLM annotations/extractions of the ", 
              tags$code("column_target"), ")."),
            
            p("• ", tags$code("column_target"), " and ", tags$code("column_llm"), 
              " cannot be named ", tags$code("class"), " or ", tags$code("Class"), ".")
          ),
          h2("Step-by-Step Instructions"),
          p("1. Choose the appropriate tab for categorical or numerical variables."),
          p("2. Specify the required file paths, LLM details, target columns, and experiment name."),
          p("3. Execute each step using the action buttons from 1-6 (categorical) or 1-4 (numerical) to evaluate LLM's performance."),
          p("4. Review results in the right panel."),
          p("5. Import existing evaluation results from your path to visualise them all."),
          p("6. If you encounter issues, refresh settings and retry."),
          hr()
        )
        
      ))
    )
  ),
  ## Categorical variables tab -------------
  tabPanel(
    title = "Evaluation of categorial variables",
  fluidPage(
  titlePanel("Evaluation Pipeline"),
  sidebarLayout(
    sidebarPanel(
      tags$style(HTML("h4 { font-weight: bold; }")),
      h4("Setting up variables and paths"),
      p("Specify the required file paths, LLM details, target columns, and experiment name below."),
      textInput("dataset", "Dataset file path", "data/datasets/epfl_1000_clean_data_no_text_partial_agreement.csv"),
      div(
        style = "display: flex; align-items: center;",
        checkboxInput("apply_transform", "Data transformation for LLM results", value = TRUE),
        actionButton("help_transform", "?", class = "btn-sm")
      ),
      
      bsTooltip("help_transform", 
                title = "When selected, the JSON LLM results file will undergo transformations such as unnesting and column renaming.   ", 
                placement = "right", 
                trigger = "hover"),
      textInput("llm_results", "LLM results file path", "data/llm_results/llm_tweets_en_epfl_gpt4o.json"),
      textInput("evaluation_path", "Evaluation results path", "data/evaluation/"),  
      textInput("llm", "LLM", "gpt4o"),
      textInput("target", "Target column", "stance_target"),
      textInput("llm_prediction", "LLM prediction column", "stance"),
      textInput("experiment", "Experiment", "tweet_en_epfl"),
      
      actionButton("refresh_code", "Refresh settings"),
      h4("Pipeline steps"),
      p("Execute each step sequentially. Results and messages will appear in the right panel."),
      fluidRow(
        column(6, 
               actionButton("import_dataset", "1. Import dataset", style = "display: block; width: 100%; margin-bottom: 10px;"),
               actionButton("join_data", "3. Join target & LLM results", style = "display: block; width: 100%; margin-bottom: 10px;"),
               actionButton("view_stats", "5. View/save confusion matrix stats", style = "display: block; width: 100%; margin-bottom: 10px;")
        ),
        column(6, 
               actionButton("import_llm", "2. Import LLM results", style = "display: block; width: 100%; margin-bottom: 10px;"),
               actionButton("calc_conf_matrix", "4. Calculate confusion matrix", style = "display: block; width: 100%; margin-bottom: 10px;"),
               actionButton("view_accuracy", "6. View/save accuracy", style = "display: block; width: 100%; margin-bottom: 10px;")
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
    style = "padding-top: 200px;",  
    h4("Pipeline Results"),
      uiOutput("results") # Dynamic output for either messages or tables
    )
  )
)
),
# Numerical variables tab -----------
tabPanel(
  title = "Evaluation of numerical variables",
  fluidPage(
    titlePanel("Evaluation Pipeline"),
    sidebarLayout(
      sidebarPanel(
        tags$style(HTML("h4 { font-weight: bold; }")),
        h4("Setting up variables and paths"),
        p("Specify the required file paths, LLM details, target columns, and experiment name below."),
        textInput("dataset_num", "Dataset file path", "data/datasets/DON-db_merged_300_all_variables.csv"),
        div(
          style = "display: flex; align-items: center;",
          checkboxInput("apply_transform_num", "Data transformation for LLM results", value = TRUE),
          actionButton("help_transform_num", "?", class = "btn-sm")
        ),
        
        bsTooltip("help_transform_num", 
                  title = "When selected, the JSON LLM results file will undergo transformations such as unnesting and column renaming.   ", 
                  placement = "right", 
                  trigger = "hover"),
        textInput("llm_results_num", "LLM results file path", "data/llm_results/llm_don_300_gpt4o.json"),
        textInput("evaluation_path_num", "Evaluation results path", "data/evaluation/"),  # New input for evaluation directory
        textInput("llm_num", "LLM", "gpt4o"),
        textInput("target_num", "Target column", "CasesTotal"),
        textInput("llm_prediction_num", "LLM prediction column", "cases"),
        textInput("experiment_num", "Experiment", "don_300"),
        
        actionButton("refresh_code_num", "Refresh settings"),
        h4("Pipeline steps"),
        p("Execute each step sequentially. Results and messages will appear in the right panel."),
        fluidRow(
          column(6, 
                 actionButton("import_dataset_num", "1. Import dataset", style = "display: block; width: 100%; margin-bottom: 10px;"),
                 actionButton("join_data_num", "3. Join target & LLM results", style = "display: block; width: 100%; margin-bottom: 10px;")
          ),
          column(6, 
                 actionButton("import_llm_num", "2. Import LLM results", style = "display: block; width: 100%; margin-bottom: 10px;"),
                 actionButton("calc_metrics", "4. Calculate, view & save metrics", style = "display: block; width: 100%; margin-bottom: 10px;")
          )
        ),
        fluidRow(
          column(12,
                 actionButton("llm_bench_num", "5. LLM benchmarking (import all evaluation results)", style = "display: block; width: 100%; margin-bottom: 10px; background-color: #f0f0f0; font-weight: bold;")
          )
        ),
        hr()
      ),
      mainPanel(
        style = "padding-top: 200px;",  
        h4("Pipeline Results"),
        uiOutput("results_num") # Dynamic output for either messages or tables
      )
    )
  )
)
)
)

# Define server logic -------------
server <- function(input, output, session) {
  dataset_path <- reactive({
    req(input$dataset)  # Ensure the input is not empty or NULL
    input$dataset       # Return the dataset path
  })
  
  llm_results_path <- reactive({
    req(input$llm_results)  # Ensure the input is not empty or NULL
    input$llm_results       # Return the LLM results path
  })
  
  dataset_path_num <- reactive({
    req(input$dataset_num)  # Ensure the input is not empty or NULL
    input$dataset_num       # Return the dataset path
  })
  
  llm_results_path_num <- reactive({
    req(input$llm_results_num)  # Ensure the input is not empty or NULL
    input$llm_results_num       # Return the LLM results path
  })

  # Reactive values to store intermediate results
  values <- reactiveValues(
    dataset = NULL,
    df_target = NULL,
    df_llm_results = NULL,
    df_target_llm = NULL,
    conf_matrix = NULL,
    stats_conf_matrix = NULL,
    accuracy = NULL,
    dataset_num = NULL,
    df_target_num = NULL,
    df_llm_results_num = NULL,
    df_target_llm_num = NULL,
    accuracy_num = NULL
  )
  
  ## Functions for categorical variables ------------
  ### Refresh settings -----------
  observeEvent(input$refresh_code, {
    values$dataset <- NULL
    values$df_target <- NULL
    values$df_llm_results <- NULL
    values$df_target_llm <- NULL
    
    output$results <- renderUI({
      h5("Settings refreshed. Ready for new inputs.")
    })
  })
  
  ### Import dataset -------------
  observeEvent(input$import_dataset, {
    tryCatch({
      values$df_target <- read_csv(dataset_path())  # Read dataset
      
      output$results <- renderUI({
        tagList(
          h5("Dataset successfully imported."),
          DT::dataTableOutput("preview_table")  # Add table output
        )
      })
      
      output$preview_table <- DT::renderDataTable({
        req(values$df_target)  # Ensure data exists
        head(values$df_target, 5)  # Show first 5 rows
      }, options = list(pageLength = 5, scrollX = TRUE))  # Limit to 5 rows
      
    }, error = function(e) {
      output$results <- renderUI({
        h5(paste("Error importing dataset:", as.character(e)))
      })
    })
  })
  
  ### Import LLM results --------------
  observeEvent(input$import_llm, {
    #df_llm_results <- input$llm_results
    tryCatch({
      llm_file <- llm_results_path()
      file_ext <- tools::file_ext(llm_file)  # Get the file extension (e.g., "csv", "json")
      
      clean_json <- function(json_str) {
        tryCatch({
          # Attempt to fix trailing garbage by removing text after the final valid JSON token.
          jsonlite::fromJSON(json_str)
        }, error = function(e) {
          cleaned_str <- sub("}}.*$", "}}", json_str)
          tryCatch(jsonlite::fromJSON(cleaned_str), error = function(e2) NA)
        })
      }
      
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
                   variables = map(variables, clean_json)) %>%
            unnest_wider(variables) %>%
            mutate(across(everything(), ~ tolower(as.character(.))),
                   id = as.numeric(id))
          
          output$results <- renderUI({
            tagList(
              h5("LLM results successfully imported and transformed (JSON)."),
              DT::dataTableOutput("llm_preview_table")  # Table output
            )
          })
          
        } else {
          # No transformation, just flatten JSON
          values$df_llm_results <- as.data.frame(df_llm)
          output$results <- renderUI({
            tagList(
              h5("LLM results successfully imported (JSON)."),
              DT::dataTableOutput("llm_preview_table")  # Table output
            )
          })
        }
      } else if (file_ext == "csv") {
        # Handle CSV files
        values$df_llm_results <- read_csv(llm_file)
        output$results <- renderUI({
          tagList(
            h5("LLM results successfully imported (CSV)."),
            DT::dataTableOutput("llm_preview_table")  # Table output
          )
        })
      } else {
        stop("Unsupported file type. Please provide a CSV or JSON file.")
      }
      
      # Render the first 5 rows of the LLM results
      output$llm_preview_table <- DT::renderDataTable({
        req(values$df_llm_results)  # Ensure data exists
        head(values$df_llm_results, 5)  # Show first 5 rows
      }, options = list(pageLength = 5, scrollX = TRUE))  # Limit to 5 rows
      
      
    }, error = function(e) {
      output$results <- renderUI({
        h5(paste("Error importing LLM results:", as.character(e)))
      })
    })
  })
  
  ### Join target and LLM results -------------
  observeEvent(input$join_data, {
    req(input$target, input$llm_prediction)
    tryCatch({
      req(values$df_target, values$df_llm_results)
      
      values$df_target_llm <- values$df_target %>%
        left_join(values$df_llm_results, by = "id") %>%
        mutate(across(everything(), ~ tolower(as.character(.))))
      # Display success message and preview table
      output$results <- renderUI({
        tagList(
          h5("Target and LLM results successfully joined."),
          DT::dataTableOutput("joined_preview_table")  # Add table output
        )
      })
      
      # Render first 5 rows of joined data
      output$joined_preview_table <- DT::renderDataTable({
        req(values$df_target_llm)  # Ensure data exists
        head(values$df_target_llm, 5)  # Show first 5 rows
      }, options = list(pageLength = 5, scrollX = TRUE))  # Limit to 5 rows
      
    }, error = function(e) {
      output$results <- renderUI({
        h5(paste("Error joining target and LLM results:", as.character(e)))
      })
    })
  })
  
  ### Calculate confusion matrix -----------------------
  observeEvent(input$calc_conf_matrix, {
    tryCatch({
      req(values$df_target_llm)  # Ensure the data exists
      
      # Extract the target and prediction column names
      target_column <- input$target
      prediction_column <- input$llm_prediction
      
      # Replace NA with "unknown" and ensure both columns are factors with consistent levels
      values$df_target_llm <- values$df_target_llm %>%
        mutate(
          !!target_column := factor(ifelse(is.na(.data[[target_column]]), "unknown", .data[[target_column]])),
          !!prediction_column := factor(ifelse(is.na(.data[[prediction_column]]), "unknown", .data[[prediction_column]]))
        ) %>%
        mutate(
          !!target_column := factor(.data[[target_column]], levels = unique(c(
            pull(values$df_target_llm, target_column),
            pull(values$df_target_llm, prediction_column)
          ))),
          !!prediction_column := factor(.data[[prediction_column]], levels = unique(c(
            pull(values$df_target_llm, target_column),
            pull(values$df_target_llm, prediction_column)
          )))
        )
      
      # Calculate the confusion matrix
      values$conf_matrix <- confusionMatrix(
        values$df_target_llm %>% pull(prediction_column),
        values$df_target_llm %>% pull(target_column),
        mode = "everything"
      )
      
      # Update the UI with success message
      output$results <- renderUI({
        h5("Confusion matrix successfully calculated with NA values replaced by 'unknown'.")
      })
    }, error = function(e) {
      output$results <- renderUI({
        h5(paste("Error calculating confusion matrix:", as.character(e)))
      })
    })
  })
  
  
  ### View and save statistics of the confusion matrix -----------------
  observeEvent(input$view_stats, {
    tryCatch({
      req(values$conf_matrix, input$target) 
      req(values$df_target_llm)
      
      class_frequencies <- values$df_target_llm %>%
        as.data.frame() %>% 
        count(.data[[input$target]]) %>% 
        mutate(Frequency = round((n/sum(n))*100, digits = 1)) %>% 
        select(-n)
      
      values$stats_conf_matrix <- values$conf_matrix$byClass %>%
        as.data.frame() %>%
        rownames_to_column() %>%
        rename(Class = rowname) %>% 
        mutate(Class = str_remove(Class, "Class: "),
               Experiment = input$experiment,
               Target = input$target,
               LLM = input$llm) %>% 
        left_join(class_frequencies, by = c(Class = input$target)) %>% 
        select(Experiment, Target,  
               LLM, Class, Frequency, everything()) %>% 
        mutate(across(where(is.numeric), 
                            ~ round(., digits = 4)))
        
        write_csv(
          values$stats_conf_matrix, 
          paste0(input$evaluation_path, "eval_stats_", input$experiment, "_", input$llm, "_", input$target, ".csv")
          )
            
        output$results <- renderUI({
              DT::dataTableOutput("conf_matrix_stats")
            })
        
        output$conf_matrix_stats <- DT::renderDataTable(values$stats_conf_matrix, 
                                                            options = list(pageLength = 25, scrollX = TRUE),
                                                            filter = 'top')
          }, error = function(e) {
            output$results <- renderUI({
              h5(paste("Error generating statistics:", as.character(e)))
            })
          })
        })
        
  ### View and save accuracy --------------
  observeEvent(input$view_accuracy, {
    tryCatch({
      req(values$conf_matrix)
      values$accuracy <- values$conf_matrix$overall %>%
        as.data.frame() %>%
        rownames_to_column() %>%
        rename("Accuracy (value)" = ".",
               "Accuracy (variable)" = "rowname") %>% 
        mutate(Experiment = input$experiment,
               Target = input$target,
               LLM = input$llm) %>% 
        select(Experiment, LLM, Target, everything()) %>% 
        mutate(across(where(is.numeric), 
                      ~ round(., digits = 4)))
      
      write_csv(
        values$accuracy, 
        paste0(input$evaluation_path, "eval_accuracy_", input$experiment, "_", input$llm, "_", input$target, ".csv")
      )
      
      output$results <- renderUI({
        DT::dataTableOutput("accuracy_table")
      })
      output$accuracy_table <- DT::renderDataTable(values$accuracy, 
                                                   options = list(pageLength = 25, scrollX = TRUE),
                                                   filter = 'top')
    }, error = function(e) {
      output$results <- renderUI({
        h5(paste("Error calculating accuracy:", as.character(e)))
      })
    })
  })
  
  
  ### Merge csv for confusion matrix stats -----------------
  observeEvent(input$llm_bench_stats, {
    tryCatch({
      # Ensure the evaluation path is provided and exists
      eval_path <- input$evaluation_path
      req(eval_path, dir.exists(eval_path))
      
      # Find all files starting with "eval_stats" in the directory
      files <- list.files(eval_path, pattern = "^eval_stats.*\\.csv$", full.names = TRUE)
      req(length(files) > 0, "No files starting with 'eval_stats' were found in the specified directory.")
      
      # Read and merge all files
      merged_stats <- files %>%
        map_dfr(~ read_csv(.x, show_col_types = FALSE)) %>%   # Combine all CSV files into one data frame
        filter(!is.na(`Balanced Accuracy`))
      # Store the merged data in reactiveValues for further use
      values$merged_stats <- merged_stats
      
      # Write csv
      write_csv(
        values$merged_stats, 
        paste0(input$evaluation_path, "eval_all_stats.csv")
      )
      
      # Render the merged data table in the UI
      output$results <- renderUI({
        DT::dataTableOutput("merged_stats_table")
      })
      output$merged_stats_table <- DT::renderDataTable(
        merged_stats,
        options = list(pageLength = 25, scrollX = TRUE),
                       filter = 'top'
      )
    }, error = function(e) {
      output$results <- renderUI({
        h5(paste("Error merging stats files:", as.character(e)))
      })
    })
  })
  
  
  ### Merge csv for accuracy ---------------
  observeEvent(input$llm_bench_accuracy, {
    tryCatch({
      # Ensure the evaluation path is provided and exists
      eval_path <- input$evaluation_path
      req(eval_path, dir.exists(eval_path))
      
      # Find all files starting with "eval_stats" in the directory
      files_a <- list.files(eval_path, pattern = "^eval_accuracy.*\\.csv$", full.names = TRUE)
      req(length(files_a) > 0, "No files starting with 'eval_accuracy' were found in the specified directory.")
      
      # Read and merge all files
      merged_accuracy <- files_a %>%
        map_dfr(~ read_csv(.x, show_col_types = FALSE))  # Combine all CSV files into one data frame
      
      # Store the merged data in reactiveValues for further use
      values$merged_accuracy <- merged_accuracy
      
      # Write csv
      write_csv(
        values$merged_accuracy, 
        paste0(input$evaluation_path, "eval_all_accuracy.csv")
      )
      
      # Render the merged data table in the UI
      output$results <- renderUI({
        DT::dataTableOutput("merged_accuracy_table")
      })
      output$merged_accuracy_table <- DT::renderDataTable(
        merged_accuracy,
        options = list(pageLength = 25, scrollX = TRUE),
        filter = 'top'
      )
    }, error = function(e) {
      output$results <- renderUI({
        h5(paste("Error merging accuracy files:", as.character(e)))
      })
    })
  })
  
  ## Functions for numerical variables -------------
  ### Import dataset ---------------
  observeEvent(input$import_dataset_num, {
    #dataset <- input$dataset
    tryCatch({
      values$df_target_num <- read_csv(dataset_path_num())
      output$results_num <- renderUI({
        h5("Dataset successfully imported.")
      })
    }, error = function(e) {
      output$results_num <- renderUI({
        h5(paste("Error importing dataset:", as.character(e)))
      })
    })
  })
  
  ### Import LLM results --------------
  observeEvent(input$import_llm_num, {
    #df_llm_results <- input$llm_results
    tryCatch({
      llm_file_num <- llm_results_path_num()
      file_ext_num <- tools::file_ext(llm_file_num)  # Get the file extension (e.g., "csv", "json")
      
      clean_json <- function(json_str) {
        tryCatch({
          # Attempt to fix trailing garbage by removing text after the final valid JSON token.
          jsonlite::fromJSON(json_str)
        }, error = function(e) {
          cleaned_str <- sub("}}.*$", "}}", json_str)
          tryCatch(jsonlite::fromJSON(cleaned_str), error = function(e2) NA)
        })
      }
      
      if (file_ext_num == "json") {
        # Handle JSON files
        df_llm_num <- fromJSON(llm_file_num)
        
        if (input$apply_transform_num) {
          # Apply transformation for JSON
          values$df_llm_results_num <- df_llm_num$annotations %>%
            unlist() %>%
            as.data.frame() %>%
            rownames_to_column(var = "id") %>%
            rename(variables = ".") %>%
            mutate(id = str_extract(id, "\\d+"),
                   variables = map(variables, clean_json)) %>%
            unnest_wider(variables) %>%
            mutate(across(everything(), ~ tolower(as.character(.))),
                   id = as.numeric(id))
          
          output$results_num <- renderUI({
            h5("LLM results successfully imported and transformed (JSON).")
          })
        } else {
          # No transformation, just flatten JSON
          values$df_llm_results_num <- as.data.frame(df_llm_num)
          output$results_num <- renderUI({
            h5("LLM results successfully imported (JSON).")
          })
        }
      } else if (file_ext_num == "csv") {
        # Handle CSV files
        values$df_llm_results_num <- read_csv(llm_file_num)
        output$results_num <- renderUI({
          h5("LLM results successfully imported (CSV).")
        })
      } else {
        stop("Unsupported file type. Please provide a CSV or JSON file.")
      }
      
    }, error = function(e) {
      output$results_num <- renderUI({
        h5(paste("Error importing LLM results:", as.character(e)))
      })
    })
  })
  
  ### Join target and LLM results ------------
  observeEvent(input$join_data_num, {
    req(input$target_num, input$llm_prediction_num)
    tryCatch({
      req(values$df_target_num, values$df_llm_results_num)
      values$df_target_llm_num <- values$df_target_num %>%
        full_join(values$df_llm_results_num, by = "id") %>%
        mutate(across(everything(), ~ tolower(as.character(.))))
      output$results_num <- renderUI({
        h5("Target and LLM results successfully joined.")
      })
    }, error = function(e) {
      output$results_num<- renderUI({
        h5(paste("Error joining target and LLM results:", as.character(e)))
      })
    })
  })
  
  ### Calculate metrics ------------------
  observeEvent(input$calc_metrics, {
    tryCatch({
      req(values$df_target_llm_num)  # Ensure the joined data exists
      
      # Extract the target and prediction column names
      target_column_num <- input$target_num
      prediction_column_num <- input$llm_prediction_num
      
      # Ensure the columns exist and convert to numeric
      values$df_target_llm_num <- values$df_target_llm_num %>%
        mutate(across(all_of(c(target_column_num, prediction_column_num)), ~ as.numeric(as.character(.))))
      
      values$df_target_llm_num <- values$df_target_llm_num %>%
        mutate(
          !!target_column_num := coalesce(as.numeric(.data[[target_column_num]]), 0),
          !!prediction_column_num := coalesce(as.numeric(.data[[prediction_column_num]]), 0)
        )
      
      values$df_target_llm_num <- values$df_target_llm_num %>%
        mutate(comp = case_when(
          .data[[target_column_num]] == .data[[prediction_column_num]] ~ 1,  # Logical condition
          TRUE ~ 0  
        ))
      
      # Calculate metrics
      accuracy_num_total <- values$df_target_llm_num %>%
        mutate(comp = case_when(
          !is.na(.data[[target_column_num]]) & 
            !is.na(.data[[prediction_column_num]]) & 
            .data[[target_column_num]] == .data[[prediction_column_num]] ~ 1, 
          TRUE ~ 0  
        )) %>% 
        filter(comp == 1) %>%
        nrow() %>% 
        as.numeric()
      
      accuracy_num <- round(accuracy_num_total/nrow(values$df_target_llm_num) * 100, digits = 1) 
      
      accuracy_num <- accuracy_num %>%
        as.data.frame() %>% rename("Value" = ".") %>%
        mutate(Metric = "Accuracy")
      
      df_cleaned <- values$df_target_llm_num

      rmse <- round(sqrt(mean((df_cleaned[[target_column_num]] - df_cleaned[[prediction_column_num]])^2)), digits = 2) %>%
        as.data.frame() %>% rename("Value" = ".") %>%
        mutate(Metric = "RMSE")
      
      mae <- round(mean(abs(df_cleaned[[target_column_num]] - df_cleaned[[prediction_column_num]])), digits = 2) %>%
        as.data.frame() %>% rename("Value" = ".") %>%
        mutate(Metric = "MAE")
      
      # Combine metrics into a single data frame
      values$metrics <- bind_rows(
        accuracy_num, 
        rmse, mae) %>%
        mutate(
          LLM = input$llm_num,
          Experiment = input$experiment_num,
          Target = target_column_num,
          across(where(is.numeric), ~ round(., 4))
        ) %>% 
       select(Experiment, Target, LLM, Metric, Value)
      
      # Save metrics to CSV
      file_path <- paste0(input$evaluation_path_num, "eval_metrics_", input$experiment_num, "_", input$llm_num, "_", input$target_num, ".csv")
      write_csv(values$metrics, file_path)
      
      # Render results in the appropriate panel
      output$results_num <- renderUI({
        DT::dataTableOutput("metrics_table_num")
      })
      output$metrics_table_num <- DT::renderDataTable(values$metrics, 
                                                      options = list(pageLength = 25, scrollX = TRUE),
                                                      filter = 'top')
      
    }, error = function(e) {
      output$results_num <- renderUI({
        h5(paste("Error calculating metrics:", as.character(e)))
      })
    })
  })
  
  
  
  ### Merge csv for metrics ---------------
  observeEvent(input$llm_bench_num, {
    tryCatch({
      # Ensure the evaluation path is provided and exists
      eval_path_num <- input$evaluation_path_num
      req(eval_path_num, dir.exists(eval_path_num))
      
      # Find all files starting with "eval_stats" in the directory
      files_num <- list.files(eval_path_num, pattern = "^eval_metrics.*\\.csv$", full.names = TRUE)
      req(length(files_num) > 0, "No files starting with 'eval_metrics' were found in the specified directory.")
      
      # Read and merge all files
      merged_metrics <- files_num %>%
        map_dfr(~ read_csv(.x, show_col_types = FALSE))  # Combine all CSV files into one data frame
      
      # Store the merged data in reactiveValues for further use
      values$merged_metrics <- merged_metrics
      
      # Save metrics to CSV
      file_path_metrics_all <- paste0(input$evaluation_path_num, "eval_all_metrics.csv")
      write_csv(values$merged_metrics, file_path_metrics_all)
      
      
      # Render the merged data table in the UI
      output$results_num <- renderUI({
        DT::dataTableOutput("merged_metrics_table")
      })
      output$merged_metrics_table <- DT::renderDataTable(
        merged_metrics,
        options = list(pageLength = 25, scrollX = TRUE),
        filter = 'top'
      )
    }, error = function(e) {
      output$results_num <- renderUI({
        h5(paste("Error merging metrics files:", as.character(e)))
      })
    })
  })
  
  ### Refresh settings -----------------
  observeEvent(input$refresh_code_num, {
    values$dataset <- NULL
    values$df_target <- NULL
    values$df_llm_results <- NULL
    values$df_target_llm <- NULL
    
    output$results_num <- renderUI({
      h5("Settings refreshed. Ready for new inputs.")
    })
  })
}

# Run the application ------------
shinyApp(ui = ui, server = server)