library(shiny)
library(devtools)
library(stringi)
library(rlang)
library(dplyr)
library(ggplot2)
library(data.table)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Inverse ECDF"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("color",
          "Color / Grouping facet",
          choices = c("search.type", "n", "learner.id", "task.id", "searchXn"),
          selected = "search.type",
          multiple = FALSE
        ),
        selectInput("learner",
          "Learner",
          choices = c("glmnet", "rpart", "xgboost"), #"svm",
          selected = "rpart",
          multiple = TRUE
        ),
        selectInput("search.type",
          "Search Type:",
          choices = c("design", "mbo", "package-default", "random", "defaults_mean", "defaults_cycle", "hodges-lehmann"),
          selected = c("design", "random"),
          multiple = TRUE
        ),
        selectInput("nrs",
         "Number of randomSearch evaluations:",
          choices = c(4, 8, 16, 32, 64),
          selected = 4,
          multiple = TRUE
        ),
        selectInput("ndef",
          "Number of defaults:",
          choices = c(1, 2, 4, 6, 8, 10),
          selected = 4,
          multiple = TRUE
        ),
        width = 2),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("invECDF"),
        dataTableOutput("table")
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
  

  
  output$table <- renderDataTable({
    
    learner = get_long_learner_name(input$learner)
    
    variable = rlang::sym(input$color)
    # Get Data
    data = readRDS("full_results.Rds")$oob.perf %>%
      filter(search.type %in% input$search.type) %>%
      filter(n %in% input$ndef | !(search.type %in% c("design", "defaults_mean", "defaults_cycle", "hodges-lehmann"))) %>%
      filter(n %in% input$nrs | !(search.type %in% c("random"))) %>%
      filter(learner.id %in% learner) %>%
      mutate(n = as.factor(n)) %>%
      mutate(searchXn = paste(search.type, n, sep = "_"))
    
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
      } else if (input$color == "searchXn") {
        data = data %>%
          group_by(searchXn)
      } else {
        data = data %>%
          group_by(search.type, n) 
      }
    
      data = data %>%
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

      data %>% data.table()
    })
  
   output$invECDF <- renderPlot({
     
     learner = get_long_learner_name(input$learner)
     
     # Get Data
     data = readRDS("full_results.Rds")$oob.perf %>%
       filter(search.type %in% input$search.type) %>%
       filter(n %in% input$ndef | !(search.type %in% c("design", "defaults_mean", "defaults_cycle", "hodges-lehmann"))) %>%
       filter(n %in% input$nrs | !(search.type %in% c("random"))) %>%
       filter(learner.id %in% learner) %>%
       mutate(n = as.factor(n)) %>%
       mutate(searchXn = paste(search.type, n, sep = "_"))
    
     ggplot(data, aes_string(x = "auc.test.mean", color = input$color)) +
       stat_ecdf() +
       coord_flip() +
       ylab("Quantile")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

