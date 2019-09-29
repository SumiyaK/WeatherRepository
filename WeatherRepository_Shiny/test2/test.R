library(shiny)

csv_write = data.frame(a=c(1,2,3),b=c(1,2,3))

server <- shinyServer(function(input, output, session) {
    
    output$downloadData <- downloadHandler(
        
        filename = function() {
            paste('Final Report', '.csv', sep='')
        },
        content = function(file){
            write.csv(csv_write,file,row.names=FALSE, na="")
        })
    
    
})

ui <-shinyUI(fluidPage(
    
    downloadButton('downloadData', 'Download data')
    
))

shinyApp(ui,server)
