library(shiny)
library(devtools)
library(stringi)
library(rlang)
library(dplyr)
library(ggplot2)
library(data.table)
library(farff)
library(shinydashboard)
library(shinydashboardPlus)


ui = dashboardPagePlus(
  header = dashboardHeaderPlus(
  ),
  sidebar = dashboardSidebar(),
  body = dashboardBody(),
  
  
   # Application title
   titlePanel("Inverse ECDF"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("color",
          "Color / Grouping facet",
          choices = c("search.type", "n", "learner.id", "task.id", "search.typeXn", "task.idXn"),
          selected = "search.typeXn",
          multiple = FALSE
        ),
        selectInput("learner",
          "Learner",
          choices = c("glmnet", "rpart", "xgboost"),
          selected = "rpart",
          multiple = TRUE
        ),
        selectInput("search.type",
          "Search Type:",
          choices = c("design", "mbo", "package-default", "random"),
          selected = c("design", "random"),
          multiple = TRUE
        ),
        selectInput("nrs",
         "Number of randomSearch evaluations:",
          choices = c(4, 8, 16, 32, 64),
          selected = c(8, 16, 32),
          multiple = TRUE
        ),
        selectInput("ndef",
          "Number of defaults:",
          choices = c(1, 2, 4, 6, 8, 10, 16, 32),
          selected = 4,
          multiple = TRUE
        ),
        selectInput("defslrn",
                    "Show defaults for:",
                    choices = c("glmnet", "rpart", "xgboost", "adaboost", "svm_svc", "random_forest"),
                    selected = "rpart",
                    multiple = FALSE
        ),
        width = 2),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("invECDF"),
        h3(textOutput("restab")),
        dataTableOutput("table"),
        h3(textOutput("deftab")), h6(textOutput("defsinfo")),
        dataTableOutput("deftable")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  get_long_learner_name = function(learner) {
    sapply(learner, function(x) {
    learner = switch(x, 
      "glmnet" = "classif.glmnet.tuned",
      "rpart" = "classif.rpart.tuned",
      "svm" = "classif.svm.tuned",
      "xgboost" = "classif.xgboost.dummied.tuned"
    )
    })
  }
  
  print_defaults = function(input) {
    df = farff::readARFF(paste0("default_sets/median_defaults_", input$defslrn, ".arff"))
    df$default_number = seq_len(nrow(df))
    df %>% data.table()
  }
  
  preproc_data = function(df, input, learner) {
   df %>%
      filter(search.type %in% input$search.type) %>%
      filter(n %in% input$ndef | !(search.type %in% c("design", "defaults_mean", "defaults_cycle", "hodges-lehmann"))) %>%
      filter(n %in% input$nrs | !(search.type %in% c("random"))) %>%
      filter(learner.id %in% learner) %>%
      mutate(n = as.factor(n)) %>%
      mutate(search.typeXn = paste(search.type, n, sep = "_")) %>%
      mutate(task.idXn = paste(task.id, n, sep = "_")) %>%
      mutate(search.type = ifelse(search.type == "design", "defaults", search.type))
  }
  
  create_table = function(data, input, learner) {
    
    variable = rlang::sym(input$color)
    
    data = data %>%
      group_by(task.id) %>%
      mutate(
        rnk = dense_rank(desc(auc.test.mean)), 
        auc.test.normalized = (auc.test.mean - min(auc.test.mean)) / (max(auc.test.mean) - min(auc.test.mean))
      )  %>%
      ungroup()
    
    if (input$color == "learner.id") {
      data = data %>%
        group_by(learner.id, search.type, n) 
    } else if (input$color == "task.id") {
      data = data %>%
        group_by(task.id)
    } else if (input$color == "search.typeXn") {
      data = data %>%
        group_by(search.typeXn)
    } else if (input$color == "task.idXn") {
      data = data %>%
        group_by(task.idXn)
    } else {
      data = data %>%
        group_by(search.type, n) 
    }
    
    data %>%
      summarise(
        mean_rank_auc = mean(rnk),
        mean_auc = mean(auc.test.mean),
        mn_auc_norm. = mean(auc.test.normalized),
        median_auc = median(auc.test.mean),
        cnt = n(),
        cnt_na = sum(is.na(auc.test.mean))) %>%
      group_by(!! variable) %>%
      summarize(mean_rank_auc = mean(mean_rank_auc), mean_auc = mean(mean_auc),
        mean_auc_norm. =  mean(mn_auc_norm.), mean_med_auc = mean(median_auc))
  }
  
  output$table <- renderDataTable({
    
    learner = get_long_learner_name(input$learner)
    
    # Get Data
    data = readRDS("full_results.Rds")$oob.perf %>% preproc_data(input, learner)
    
    data = create_table(data, input, learner)

    data %>% data.table()
    })
  
  output$deftable <- renderDataTable({
    print_defaults(input)
  })
  
   output$invECDF <- renderPlot({
     
     learner = get_long_learner_name(input$learner)
     
     # Get Data
     data = readRDS("full_results.Rds")$oob.perf %>% preproc_data(input, learner)
     
     ggplot(data, aes_string(x = "auc.test.mean", color = input$color)) +
       stat_ecdf() +
       coord_flip() +
       ylab("Quantile")
   })
   
   output$deftab = renderText("Defaults Table:")
   output$restab = renderText("Results Table:")
   output$defsinfo = renderText("'-11' indicates no value")
}

# Run the application 
shinyApp(ui = ui, server = server)

