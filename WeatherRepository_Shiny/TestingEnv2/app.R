library(shiny)

library(tidyverse)

library(lubridate)

library(readxl)

library(openxlsx)

library(RCurl)

library(plotly)

# # Download data from FTP server ---------------------------------------------------------------------
# 
# ### Set proxy portal to allow FTP server connection ###
# 
# Sys.setenv(ftp_proxy="http://proxy.pfr.co.nz:8080")
# 
# ### Set URL connection ###
# 
# url <- "ftp://pfauckland:9x7yujth@ftp.scottech.net/"
# 
# 
# ### Identify all files in directory ###
# 
# filenames <- getURL(url,ftp.use.epsv = FALSE,dirlistonly = TRUE)
# 
# ### Split filenames into manageable lines ###
# 
# destnames <-  strsplit(filenames, "\r*\n")[[1]]
# 
# ### Capture weather station names for all sites - HOURLY AND DAILY DATA ###
# 
# weatherstations <- str_extract(destnames[9:34],"(?<=\">)(.*?)(?=<)") ### using lookahead/lookbehind character string capturing
# 
# 
# ### Hourly and daily names of each weather station ###
# 
# daily_names <- paste0(weatherstations[1:26],"_Daily.dat")


#### Home stuff ------------------------------------------------------------------

# Download data from FTP server

### Set URL connection ###

url <- "ftp://pfauckland:9x7yujth@ftp.scottech.net/"


### Identify all files in directory ###

filenames <- getURL(url,ftp.use.epsv = FALSE,dirlistonly = TRUE)

### Split filenames into manageable lines ###

destnames <-  strsplit(filenames, "\r*\n")[[1]]

### Capture weather station names for all sites - HOURLY AND DAILY DATA ###

weatherstations <- str_extract(destnames,"(?=PFR)(.*?)(?=$)")

index_1 <- complete.cases(weatherstations)

weatherstations <- sort(weatherstations[index_1])

weatherstations <- weatherstations[1:26]

### Weather station names only

weatherstation_name <- str_extract(weatherstations,"(?<=_)(.*?)(?=$)")

### Hourly and daily names of each weather station ###

daily_names <- paste0(weatherstations[1:26],"_Daily.dat")



# # Define UI for application that draws a histogram
# ui <- fluidPage(
#     
#     # Application title
#     titlePanel("Plant and Food Research Weather Station Network"),
#     
#     fluidRow(
#         column(3,
#                wellPanel(
#                    selectInput("Station","Weather station:",
#                                choices = weatherstations),
#                    actionButton("update","Select station")
#                ),
#                wellPanel(
#                    uiOutput("weatherVariable"),
#                    dateRangeInput("dateRange",
#                                   label = "Date Range:",
#                                   start = "2016/01/20",
#                                   end = "2019/09/12",
#                                   min = "2016/01/01",
#                                   max = "2019/12/30",
#                                   format = "yyyy-mm-dd"),
#                    helpText("Weather data from PFR Weather Station Network around New Zealand")
#                )
#         ),
#         
#     ),
#     
#     # Show a plot of weather variable
#     mainPanel(
#         plotlyOutput("weatherPlot")
#     )
#     
# )


ui <- fluidPage(
    
    # Application title
    titlePanel("Plant and Food Research Weather Station Network"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # ATTEMPTING TO GET CHECKED BOXES FOR CHOOSING MULTIPLE STATIONS (WILL NEED TO ALTER SUBSET DATA AND GRAPHING SIMULTANEOUSLY)
            # checkboxGroupInput("Station", "Weather stations:",
            #                    names(weatherstations), selected = names(weatherstations)),
            selectInput("Station","Weather station:",
                        choices = weatherstations
            ),
            actionButton("update","Select station"
            ),
            hr(),
            
            uiOutput("weatherVariable"
            ),
            
            uiOutput("dateRange"),
            
            hr(),
            
            # Download button
            #downloadButton("downloadData", "Download"),
            
            hr(),
            helpText("Weather data from PFR Weather Station Network around New Zealand")
        ),
        
        # Show a plot of weather variable
        mainPanel(
            plotlyOutput("weatherPlot", height = "850px")
        )
        
    )
)


# Define server logic required to draw a histogram
server <- function(input, output,session){
    
    ### Dynamic selection of weather station ---------------------------------------------------------------------------------------------
    
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
    
    
    ### Dynamic selection of variables in dataset --------------------------------------------------------------------------------------
    
    output$weatherVariable <- renderUI({
        selectInput("variable", "Weather variables:", 
                    choices = names(weather_station()[3:length(weather_station())]),
        ) 
    })
    
    ### Dynamic date range selection -------------------------------------------------------------------------------------------------
    
    output$dateRange <- renderUI({
        dateRangeInput("date", "Select the date range:",
                       start = 
                           as.character(format(as.Date(min(weather_station()$Date))),"yyyy-mm-dd"), # Start 
                       end = 
                           as.character(format(as.Date(max(weather_station()$Date))),"yyyy-mm-dd"),
                       format = "yyyy-mm-dd")
        
    })
    
    
    ### Plotting the graph ------------------------------------------------------------------------------------------------------------
    
    output$weatherPlot <- renderPlotly({
        
        # Temp objects for storing weather variable name and weather station name
        
        weather_variable <- input$variable
        
        index <- grep(input$Station,weatherstations)
        
        weather_station_name <- weatherstation_name[index]
        
        # Generating a temporary dataset with 
        
        temporary_data <- weather_station() %>%
            select(c(Date, weather_variable)) %>%
            filter(Date >= input$date[[1]] & Date <= input$date[[2]])
        
        
        # Setting custom font, size and colour for x and y axis titles
        
        t <- list(
            family = "Times New Roman, monospace",
            size = 16,
            color = "Black"
        )
        
        # Generate Plotly graph
        
        plot_ly(temporary_data) %>%
            add_trace(x = ~Date, y = ~temporary_data[,weather_variable], type = 'scatter', mode = 'lines') %>%
            layout(xaxis = list(
                rangeselector = list(
                    buttons = list(
                        list(
                            count = 3, 
                            label = "3 mo", 
                            step = "month",
                            stepmode = "backward"),
                        list(
                            count = 6, 
                            label = "6 mo", 
                            step = "month",
                            stepmode = "backward"),
                        list(
                            count = 1, 
                            label = "1 yr", 
                            step = "year",
                            stepmode = "backward"),
                        list(
                            count = 1, 
                            label = "YTD", 
                            step = "year",
                            stepmode = "todate"),
                        list(step = "all"))),
                rangeslider = list(type = "date")),
                yaxis = list(title = input$variable, automargin = TRUE),
                title = paste(input$variable, "over time at", weather_station_name, "weather station", sep = " "), 
                font = t,
                margin = list(t = 50, b = 50, l = 50))
        
    })
    
    
    ### Downloadable csv of selected dataset ---------------------------------------------------------------------------------------
    
    output$downloadData <- downloadHandler(

        filename = function(){
            paste("test", "_",Sys.Date(), ".csv", sep = "")
        },
        
        content = function(file){
            write.csv(weather_station, file, row.names = FALSE)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)