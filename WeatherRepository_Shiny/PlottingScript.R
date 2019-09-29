wq_plotly <- function(data){
  data %>%
    plot_ly(
      x = ~Date,
      y = ~result,
      type = "scatter",
      mode = "lines+markers",
      marker = list(
        size = 4,
        color = "blue"
      ),
      line = list(
        color = "blue"
      ),
      hoverinfo = "text",
      text = ~paste(
        "Site:", station_nm,
        "<br>Parameter:", srsname,
        "<br>Date Time:", format(Date),
        "<br>Result:", result,
        "<br>Units:", parameter_units
      )
    ) %>%
    layout(
      title = paste(
        unique(data$station_nm), "<br>", 
        unique(data$srsname), 
        paste0("(", unique(data$parameter_units), ")")
      ),
      titlefont = list(
        size = 10
      ),
      xaxis = list(
        title = ""
      ),
      yaxis = list(
        title = ""
      ),
      margin = list(
        t = 40
      )
    )
}
