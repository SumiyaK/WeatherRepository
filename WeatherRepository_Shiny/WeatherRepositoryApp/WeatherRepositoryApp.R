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
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("tempPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$tempPlot <- renderPlot({
        
        
        # draw the ggplot line graph of simple data
        suppressWarnings(ggplot(simpleData)+
                             geom_line(aes(x = Date, y = AirTemperature_Avg)) +
                             scale_x_date(limits = c(input$dateRange[1], input$dateRange[2])) +
                             theme_bw()
                         
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)