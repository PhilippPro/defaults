library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Inverse ECDF"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("color",
          "Color / Grouping facet",
          choices = c("search.type", "n", "learner.id", "task.id"),
          selected = "n",
          multiple = FALSE
        ),
        selectInput("learner",
          "Learner",
          choices = c("glmnet", "rpart", "svm", "xgboost"),
          selected = "rpart",
          multiple = TRUE
        ),
        selectInput("search.type",
          "Number of defaults:",
          choices = c("design", "mbo", "package-default", "random", "defaults_mean", "defaults_cycle", "hodges-lehmann"),
          selected = "design",
          multiple = TRUE
        ),
        selectInput("n",
         "Number of evaluations:",
          choices = c(1, 2, 4, 6, 8, 10, 16, 32, 64),
          selected = 10,
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
    data = readRDS("../defaultLOOCV/full_results.Rds")$oob.perf %>%
      filter(search.type %in% input$search.type) %>%
      filter(n %in% input$n) %>%
      filter(learner.id %in% learner) %>%
      mutate(n = as.factor(n))
    
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
     data = readRDS("../defaultLOOCV/full_results.Rds")$oob.perf %>%
       filter(search.type %in% input$search.type) %>%
       filter(n %in% input$n) %>%
       filter(learner.id %in% learner) %>%
       mutate(n = as.factor(n))
    
     ggplot(data, aes_string(x = "auc.test.mean", color = input$color)) +
       stat_ecdf() +
       coord_flip() +
       ylab("Quantile")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

