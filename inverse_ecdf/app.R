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
library(shinyjs)

source("helpers.R")

# --------------------------------------------------------------------------------------------------
ui = dashboardPagePlus(
  # Header
  dashboardHeaderPlus(
    fixed = TRUE,
    title = "Learning multiple Defaults",
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "bars",
    left_menu = tagList(
      
      dropdownBlock(
        id = "colorblock",
        title = "Color",
        icon = "sliders",
        prettyCheckboxGroup(
          inputId = "color",
          label = "Grouping / Facet",
          choices = c("search.type", "n", "learner.id", "task.id", "search.type and n", "task.id and n"),
          selected = "search.type and n"
        )
      ),
      
      dropdownBlock(
        id = "learnerblock",
        title = "Algorithm",
        icon = "sliders",
        prettyRadioButtons(
          inputId = "framework",
          label = "framework",
          thick = TRUE,
          choices = c("mlr", "sklearn"),
          selected = "mlr"
        ),
        conditionalPanel(condition = "input.framework == 'mlr'",
          prettyCheckboxGroup(
            inputId = "learner.mlr",
            label = "mlr learner",
            thick = TRUE,
            choices = c("glmnet", "rpart", "xgboost"),
            selected = "rpart"
          )),
        conditionalPanel(condition = "input.framework == 'sklearn'",
          prettyCheckboxGroup(
            inputId = "learner.sklearn",
            label = "sklearn learner",
            thick = TRUE,
            choices = c("adaboost", "random_forest", "libsvm_svc"),
            selected = NULL
          ))
      ),
      
      dropdownBlock(
        id = "subsetblock",
        title = "Subsetting",
        icon = "sliders",
        prettyCheckboxGroup(
          inputId = "search.type",
          label = "result.type",
          choices = c("defaults", "mbo", "package-default", "random"),
          selected = c("defaults", "random")
        ),
        prettyCheckboxGroup(
          inputId = "nrs",
          label = "Random search iterations",
          choices = c(4, 8, 16, 32, 64),
          selected = c(8, 16, 32)
        ),
        prettyCheckboxGroup(
          inputId = "ndef",
          label = "Number of defaults",
          choices = c(1, 2, 4, 6, 8, 10, 16, 32),
          selected = 4
        )
      )
    )
  ),
  
  # ------------------------------------------------------------------------------------------------ 
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visualization", tabName = "viz",
        badgeLabel = NULL, badgeColor = "green",
        icon = icon("image")),
      menuItem("Data", tabName = "data",
        badgeLabel = NULL, badgeColor = "green",
        icon = icon("table")),
      menuItem("Defaults", tabName = "defs",
        badgeLabel = NULL, badgeColor = "green",
        icon = icon("cubes"))
    )
  ),
  
  
  # ------------------------------------------------------------------------------------------------ 
  # Body
  dashboardBody(
    setShadow("dropdown-menu"),
    br(), br(),
    tabItems(
      tabItem(tabName = "viz",
        h4(textOutput("plottitle")),
        plotOutput("invECDF", width = "80%")
      ),
      tabItem(tabName = "data", 
        h4(textOutput("restabtitle")),
        dataTableOutput("restable"),
        h6(textOutput("restabinfo"))
      ),
      tabItem(tabName = "defs",
        h4(textOutput("deftabtitle")),
        h6(textOutput("deftabinfo")),
        dataTableOutput("deftable")
      )
    )
  )
)


# --------------------------------------------------------------------------------------------------
server = function(input, output) {
  
  # Conditional reactvie values (learner, ...)
  learner = reactive({
    if (input$framework == "mlr") {
      lrn = input$learner.mlr
    } else {
      lrn = input$learner.sklearn
    }
    return(lrn)
  })
  measure_str = reactive({ifelse(input$framework == "mlr", "auc.test.mean", "acc.test.mean")})
  
  # Plot
  output$plottitle = renderText("Inverted Emipirical Cumulative Distribution Plot")
  output$invECDF = renderPlot({
    
    facet_color = switch(input$color[1],
      "search.type and n" = "search.typeXn",
      "task.id and n" = "task.idXn",
      input$color)
    measure_pretty = ifelse(measure_str() == "auc.test.mean", "Area under the Curve", "Accuracy")
    
    p = preproc_data(input, learner()) %>%
      ggplot(., aes_string(x = measure_str(), color = facet_color)) +
      stat_ecdf() +
      coord_flip() +
      ylab("Quantile") +
      xlab(measure_pretty) +
      labs(colour= input$color)
    if (facet_color %in% c("task.id", "task.idXn")) 
      p = p + guides(color = FALSE)
    p
  })
  
  
  # Results as table
  output$restabtitle = renderText("Results Table:")
  output$restable = renderDataTable({
    preproc_data(input, learner()) %>% aggregate_data(., input, measure_str()) %>% data.table()
  }, options = list(dom = 't'))
  output$restabinfo = renderText("'mean_*_norm.' indicates the mean performance achieved
    as percentage of the maximum for each task.")
  
  
  # Defaults
  output$deftable = renderDataTable({
    if(!is.null(learner()))
      print_defaults(learner())
  }, options = list(dom = 't'))
  output$deftabtitle = renderText("Defaults Table:")
  output$deftabinfo = renderText("'-11' indicates no value")
}


# -------------------------------------------------------------------------------------------------- 
shinyApp(ui, server)