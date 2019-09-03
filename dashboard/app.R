#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dygraphs)
library(xts)

l<- as.xts(ldeaths) #convert the time series to xts for dygrpah later

ui <- dashboardPage(
   
  dashboardHeader(title="Lung Disease Deaths in UK"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Trend", tabName = "Time_series", icon = icon("chart-line")),
      menuItem("Widgets", icon = icon("th"), tabName = "widgets",
               badgeLabel = "new", badgeColor = "green")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "Time_series",
              fluidRow(
                box(title="Frequency", status="info", width = 4, collapsible = TRUE, solidHeader = TRUE,
                    height = 250,
                    selectInput("select1", label=NULL, choices = c("Weekly", "Monthy", "Quarterly")),
                    p(em("Please select a frequency."))
                    ),
                
                box(title="Prediction", status="success", width = 4, collapsible = TRUE, solidHeader = TRUE,
                    height = 250,
                  numericInput("months", label = "Months to Predict", 
                               value = 72, min = 12, max = 144, step = 12),
                  selectInput("interval", label = "Prediction Interval",
                              choices = c("0.80", "0.90", "0.95", "0.99"),
                              selected = "0.95"),
                  checkboxInput("showgrid", label = "Show Grid", value = TRUE)
                ),
                
                box(title="Product Type", status="primary", width = 4, collapsible = TRUE, solidHeader = TRUE,
                    height = 250,
                   checkboxGroupInput("check1", label = NULL, choices = c("Type A", "Type B", "Type C"))
                )
              ),
              
              fluidRow(
                box(width = 12,
                  dygraphOutput("dygraph")
                )
              ),
              fluidRow(
                box(
                  tableOutput("table")
                )
              )
            ),
      
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)
  
  
  # 
  # titlePanel("Predicted Deaths from Lung Disease (UK)"),
  # 
  # sidebarLayout(
  #   sidebarPanel(
  #     numericInput("months", label = "Months to Predict", 
  #                  value = 72, min = 12, max = 144, step = 12),
  #     selectInput("interval", label = "Prediction Interval",
  #                 choices = c("0.80", "0.90", "0.95", "0.99"),
  #                 selected = "0.95"),
  #     checkboxInput("showgrid", label = "Show Grid", value = TRUE)
  #   ),
  #   mainPanel(
  #     dygraphOutput("dygraph")
  #   )
  # )



# Define server logic required to draw a histogram
server <- function(input, output) {
   
  predicted <- reactive({
    hw <- HoltWinters(ldeaths)
    #predict.holtwinters produces a time series
    #as.xts changes a ts object to an xts object
    as.xts(predict(hw, n.ahead = input$months, 
            prediction.interval = TRUE,
            level = as.numeric(input$interval)))
  })
  
 
  

  output$dygraph <- renderDygraph({


    #merge with historic data
    pl <- merge(predicted(),l)
    
    #dygraph(predicted(), main = "Predicted Deaths/Month") %>%
    dygraph(pl, main = "Predicted Deaths/Month") %>%
      dyRangeSelector(dateWindow = c("1974-01-01", "1984-01-01")) %>%
      dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
      dyOptions(drawGrid = input$showgrid)
  })
  
  output$table <- renderTable({
    xts(predicted(), order.by = index(predicted()))}, rownames = TRUE)
  #index function gives the values of the time dimension of an ts, xts object
}

# Run the application 
shinyApp(ui = ui, server = server)

