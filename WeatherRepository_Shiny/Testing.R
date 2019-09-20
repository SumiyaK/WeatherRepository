library(shiny)

library(tidyverse)

library(lubridate)

simpleData <- read.csv(file = "C:/Github/WeatherRepository/WeatherRepository_Shiny/PFR089_MtAlbert_Daily_Short.csv")

simpleData <- simpleData %>%
    mutate(Date = dmy_hm(TIMESTAMP)) %>%
    mutate(Date = as_date(Date)) %>%
    select(Date, AirTemperature_Avg)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Mt Albert Weather data - Temperature"),

    fluidRow(
        
        # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateRangeInput("dateRange", 
                        label = "Date Range:",
                        start = "2016/01/20",
                        end = "2019/09/12",
                        min = "2016/01/01",
                        max = "2019/12/30",
                        format = "yyyy-mm-dd")
        ),
        
        # Main panel with interactive graph
        
        mainPanel(
        column(width = 12, class = "well",
               h4("Brush and double-click to zoom"),
               plotOutput("tempPlot", height = 300,
                          dblclick = "plot1_dblclick",
                          brush = brushOpts(
                              id = "plot1_brush",
                              resetOnNew = TRUE
                          )
                   )
            )
        
        )
    )
    
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # -------------------------------------------------------------------
    # Single zoomable plot (on left)
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    output$tempPlot <- renderPlot({
        
        
        # draw the ggplot line graph of simple data
        suppressWarnings(ggplot(simpleData)+
                             geom_line(aes(x = Date, y = AirTemperature_Avg)) +
                             coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
                             theme_bw()
                         
        )
    })
    
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$plot1_dblclick, {
        brush <- input$plot1_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })
    
    # ------------------------------------------------------------------
}

# Run the application 
shinyApp(ui = ui, server = server)
