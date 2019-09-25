library(shiny)

library(tidyverse)

library(lubridate)

library(readxl)

library(openxlsx)

library(RCurl)

# simpleData <- read.csv(file = "C:/Github/WeatherRepository/WeatherRepository_Shiny/PFR089_MtAlbert_Daily_Short.csv")
# 
# simpleData <- simpleData %>%
#     mutate(Date = dmy_hm(TIMESTAMP)) %>%
#     mutate(Date = as_date(Date)) %>%
#     select(Date, AirTemperature_Avg)

# allData <-
#     read.csv(file = "C:/Github/WeatherRepository/WeatherRepository_Shiny/PFR089_MtAlbert_Daily.csv")
# 
# allData <- allData %>%
#     mutate(Date = dmy_hm(TIMESTAMP)) %>%
#     mutate(Date = as_date(Date)) %>%
#     select(Date,
#            AirTemperature_Avg,
#            Rain_Tot,
#            WindRun_Tot,
#            SolarEnergy_Tot)

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

weatherstations <- str_extract(destnames[9:39],"(?<=\">)(.*?)(?=<)") ### using lookahead/lookbehind character string capturing

### Hourly and daily names of each weather station ###

#hourly_names <- paste0(weatherstations,"_Hourly.dat")

daily_names <- paste0(weatherstations[1:26],"_Daily.dat")


test <- read.delim(paste("ftp://pfauckland:9x7yujth@ftp.scottech.net", weatherstations[28],daily_names[28],sep = "/"),skip = 1, sep = ",") %>%
    slice(-c(1:2))

# vector of weather stations

weather_stations_daily <- vector("list",length(daily_names))

### Daily data with warnings printed for each iteration that fails ###

for(i in 1:length(daily_names)) {
    tryCatch({
        print(daily_names[i])
        weather_stations_daily[[i]] <- read.delim(paste("ftp://pfauckland:9x7yujth@ftp.scottech.net", weatherstations[i],daily_names[i],sep = "/"),skip = 1, sep = ",") %>%
            slice(-c(1:2))
        
    }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
    
}


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Mt Albert Weather data - Temperature"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("Station","Weather station:",
                        choices = colnames(weather_stations_daily)),
            selectInput("weather_var","Variables:",
                        choices = colnames(allData[,2:length(names(allData))])),
            dateRangeInput("dateRange", 
                           label = "Date Range:",
                           start = min(allData$Date),
                           end = max(allData$Date),
                           min = min(allData$Date - 1),
                           max = max(allData$Date + 1),
                           format = "yyyy-mm-dd"),
            
            
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
