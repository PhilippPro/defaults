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
          choices = c("search.type", "n", "learner.id", "task.id", "search.typeXn", "task.idXn"),
          selected = "search.typeXn"
        )
      ),
      dropdownBlock(
        id = "learnerblock",
        title = "Algorithm",
        icon = "sliders",
        prettyCheckboxGroup(
          inputId = "learner",
          label = "Grouping / Facet",
          thick = TRUE,
          choices = c("glmnet", "rpart", "xgboost", "adaboost", "random forest", "svm"),
          selected = "rpart"
        )
      ),
      dropdownBlock(
        id = "subsetblock",
        title = "Subsetting",
        icon = "sliders",
        prettyCheckboxGroup(
          inputId = "search.type",
          label = "result.type",
          choices = c("design", "mbo", "package-default", "random"),
          selected = c("design", "random")
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
        badgeLabel = "new", badgeColor = "green",
        icon = icon("image")),
      menuItem("Data", tabName = "data",
        badgeLabel = "new", badgeColor = "green",
        icon = icon("table")),
      menuItem("Defaults", tabName = "defs",
        badgeLabel = "new", badgeColor = "green",
        icon = icon("cubes"))
    )
  ),
  
  
  # ------------------------------------------------------------------------------------------------ 
  # Body
  dashboardBody(
    setShadow("dropdown-menu"),
    br(), br(),
    tabItems(
      tabItem(tabName = "viz", plotOutput("invECDF")),
      tabItem(tabName = "data", 
        h4(textOutput("restabtitle"))
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
  # Plot
  output$invECDF = renderPlot({
    ggplot(iris) + geom_point(aes(x = Sepal.Width, y = Sepal.Length))
  })
  
  
  # Results as table
  output$restabtitle = renderText("Results Table:")
  
  
  # Defaults
  output$deftable <- renderDataTable({
    print_defaults(input)
  }, options = list(dom = 't'))
  output$deftabtitle = renderText("Defaults Table:")
  output$deftabinfo = renderText("'-11' indicates no value")
}


# -------------------------------------------------------------------------------------------------- 
shinyApp(ui, server)