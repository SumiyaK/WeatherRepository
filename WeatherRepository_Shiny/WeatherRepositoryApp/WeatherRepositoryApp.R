library(shiny)

library(tidyverse)

library(lubridate)

# simpleData <- read.csv(file = "C:/Github/WeatherRepository/WeatherRepository_Shiny/PFR089_MtAlbert_Daily_Short.csv")
# 
# simpleData <- simpleData %>%
#     mutate(Date = dmy_hm(TIMESTAMP)) %>%
#     mutate(Date = as_date(Date)) %>%
#     select(Date, AirTemperature_Avg)

allData <-
    read.csv(file = "C:/Github/WeatherRepository/WeatherRepository_Shiny/PFR089_MtAlbert_Daily.csv")

allData <- allData %>%
    mutate(Date = dmy_hm(TIMESTAMP)) %>%
    mutate(Date = as_date(Date)) %>%
    select(Date,
           AirTemperature_Avg,
           Rain_Tot,
           WindRun_Tot,
           SolarEnergy_Tot)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Mt Albert Weather data - Temperature"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateRangeInput("dateRange", 
                           label = "Date Range:",
                           start = min(allData$Date),
                           end = max(allData$Date),
                           min = min(allData$Date - 1),
                           max = max(allData$Date + 1),
                           format = "yyyy-mm-dd"),
            selectInput("weather_var","Variables:",
                        choices = colnames(allData[,2:length(names(allData))])),
            hr(),
            helpText("Weather data from PFR Weather Station Network around New Zealand")
        ),
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("tempPlot")
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output){
    
    
    output$tempPlot <- renderPlot({
        
        
        # draw the ggplot line graph of simple data
        suppressWarnings(ggplot(allData, aes_string(x = allData$Date, y = input$weather_var))+
                             geom_line() +
                             scale_x_date(limits = c(input$dateRange[1], input$dateRange[2])) +
                             theme_bw()
                         
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)