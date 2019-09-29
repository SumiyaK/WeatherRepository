library(shiny)

library(tidyverse)

library(lubridate)

library(readxl)

library(openxlsx)

library(RCurl)

library(plotly)

# Download data from FTP server

### Set proxy portal to allow FTP server connection ###

Sys.setenv(ftp_proxy="http://proxy.pfr.co.nz:8080")

### Set URL connection ###

url <- "ftp://pfauckland:9x7yujth@ftp.scottech.net/"


### Identify all files in directory ###

filenames <- getURL(url,ftp.use.epsv = FALSE,dirlistonly = TRUE)

### Split filenames into manageable lines ###

destnames <-  strsplit(filenames, "\r*\n")[[1]]

### Capture weather station names for all sites - HOURLY AND DAILY DATA ###

weatherstations <- str_extract(destnames[9:34],"(?<=\">)(.*?)(?=<)") ### using lookahead/lookbehind character string capturing

#weatherstations <- str_extract(destnames[9:14],"(?<=\">)(.*?)(?=<)") ### using lookahead/lookbehind character string capturing


### Hourly and daily names of each weather station ###

#hourly_names <- paste0(weatherstations,"_Hourly.dat")

daily_names <- paste0(weatherstations[1:26],"_Daily.dat")

#daily_names <- paste0(weatherstations[1:6],"_Daily.dat")


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Plant and Food Research Weather Station Network"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            selectInput("Station","Weather station:",
                        choices = weatherstations
            ),
            actionButton("update","Select station"
            ),
            hr(),
        
            
            uiOutput("weatherVariable"
            ),
            dateRangeInput("dateRange",
                           label = "Date Range:",
                           start = "2016/01/20",
                           end = "2019/09/12",
                           min = "2016/01/01",
                           max = "2019/12/30",
                           format = "yyyy-mm-dd"
            ),

            hr(),
            helpText("Weather data from PFR Weather Station Network around New Zealand")
        ),
        
        # Show a plot of weather variable
        mainPanel(
            plotlyOutput("weatherPlot")
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output){
    
    weather_station <- reactive({
        
        input$update
        
        isolate({
            # Progress bar
            withProgress({
                
                setProgress(message = "Processing ...")
                
                index <- grep(input$Station,weatherstations)
                
                weather_stations_daily <- read.delim(paste("ftp://pfauckland:9x7yujth@ftp.scottech.net", weatherstations[index],daily_names[index],sep = "/"),skip = 1, sep = ",") %>%
                    slice(-c(1:2)) %>%
                    rename_at(vars(matches("AirTemperatu_Avg")), funs(sub("AirTemperatu_Avg", "AirTemperature_Avg", .))) %>%
                    rename_at(vars(matches("AirTC_Avg")), funs(sub("AirTC_Avg", "AirTemperature_Avg", .))) %>%
                    rename_at(vars(matches("AirTemperatu_Min")), funs(sub("AirTemperatu_Min", "AirTemperature_Min", .))) %>%
                    rename_at(vars(matches("AirTC_Min")), funs(sub("AirTC_Min", "AirTemperature_Min", .))) %>%
                    rename_at(vars(matches("AirTemperatu_Max")), funs(sub("AirTemperatu_Max", "AirTemperature_Max", .))) %>%
                    rename_at(vars(matches("AirTC_Max")), funs(sub("AirTC_Max", "AirTemperature_Max", .))) %>%
                    rename_at(vars(matches("RelativeHumi_Avg")), funs(sub("RelativeHumi_Avg", "RelativeHumidity_Avg", .))) %>%
                    rename_at(vars(matches("RH_Avg")), funs(sub("RH_Avg", "RelativeHumidity_Avg", .))) %>%
                    rename_at(vars(matches("SoilTemperat_Avg")), funs(sub("SoilTemperat_Avg", "SoilTemperature_Avg", .))) %>%
                    rename_at(vars(matches("SoilTemperat_Max")), funs(sub("SoilTemperat_Max", "SoilTemperature_Max", .))) %>%
                    rename_at(vars(matches("SoilMoisture")), funs(sub("SoilMoisture", "SoilMoisture_Avg", .))) %>%
                    rename_at(vars(matches("SoilMoisture_Avg_Avg")), funs(sub("SoilMoisture_Avg_Avg", "SoilMoisture_Avg", .))) %>%
                    rename_at(vars(matches("SolarRadiati_Avg")), funs(sub("SolarRadiati_Avg", "SolarRadiation_Avg", .))) %>%
                    rename_at(vars(matches("WS_ms_Avg")), funs(sub("WS_ms_Avg", "WindSpeed_Avg", .))) %>%
                    rename_at(vars(matches("WS_ms_Max")), funs(sub("WS_ms_Max", "WindSpeed_Max", .))) %>%
                    rename_at(vars(matches("WindDirectio_Avg")), funs(sub("WindDirectio_Avg", "WindDirection_Avg", .))) %>%
                    rename_at(vars(matches("BatteryVoltage_Avg")), funs(sub("BatteryVoltage_Avg", "BatteryVoltage", .))) %>%
                    mutate(Date = as.Date(TIMESTAMP)) %>%
                    mutate_at(vars(c(ends_with("_Avg"),ends_with("_Min"),ends_with("_Max"),ends_with("_Tot"))),funs(as.numeric(as.character(.)))) %>%
                    select(-c(TIMESTAMP,RECORD)) %>%
                    select(Date,SiteID, AirTemperature_Avg, everything())
                
            })
        })
        
        
    })
    
    
    # Dynamic selection of variables in dataset based on dataset selected
    
    output$weatherVariable <- renderUI({
        selectInput("variable", "Weather variables:", 
                    choices = names(weather_station()[3:length(weather_station())]),
        ) 
    })
    
    
    # reactive dataset 
    temporary_data <- reactive({
        
        weather_station() %>%
            select(c(Date, input$variable))
        
    })
    
    
    # ###### Dynamic date range
    # 
    # output$dateRange <- renderUI({
    # 
    #     dateRangeInput("date", "Select the date range:",
    #                    start = min(weather_station()$Date),
    #                    end = max(weather_station()$Date),
    #                    min = min(weather_station()$Date),
    #                    min = max(weather_station()$Date)
    #     )
    # 
    # 
    # })
    
    # Plotting the graph
    
    output$weatherPlot <- renderPlotly({
        
        # My data
        #mydata <- weather_station()
        
        # draw a plotly graph
        
        # p <- ggplot(mydata, aes(x = Date, y = input$secondSelection)) +
        #     geom_line()
        # 
        # ggplotly(p)
        
        plot_ly(x = temporary_data()[[1]], y = temporary_data()[[2]], type = 'scatter', mode = 'lines')
        
        #plot_ly(weather_station(), x = ~Date, y = ~AirTemperature_Avg, type = 'scatter', mode = 'lines')
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)