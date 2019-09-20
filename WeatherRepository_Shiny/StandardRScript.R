library(tidyverse)

library(lubridate)

simpleData <- read.csv(file = "C:/Github/WeatherRepository/WeatherRepository_Shiny/PFR089_MtAlbert_Daily_Short.csv")

simpleData <- simpleData %>%
  mutate(Date = dmy_hm(TIMESTAMP)) %>%
  mutate(Date = as_date(Date)) %>%
  select(Date, AirTemperature_Avg)



ggplot(simpleData)+
  geom_line(aes(x = Date, y = AirTemperature_Avg)) +
  scale_x_date(limits = as.Date(c(simpleData$Date[500],simpleData$Date[1000])))





ui <- fluidPage(
  fluidRow(
    column(width = 4, class = "well",
           h4("Brush and double-click to zoom"),
           plotOutput("plot1", height = 300,
                      dblclick = "plot1_dblclick",
                      brush = brushOpts(
                        id = "plot1_brush",
                        resetOnNew = TRUE
                      )
           )
    ),
    column(width = 8, class = "well",
           h4("Left plot controls right plot"),
           fluidRow(
             column(width = 6,
                    plotOutput("plot2", height = 300,
                               brush = brushOpts(
                                 id = "plot2_brush",
                                 resetOnNew = TRUE
                               )
                    )
             ),
             column(width = 6,
                    plotOutput("plot3", height = 300)
             )
           )
    )
    
  )
)

