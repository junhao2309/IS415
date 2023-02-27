pacman::p_load(sf, raster, spatstat, tmap, tidyverse, sfdep, maptools)

vaccination <- read_rds("vaccination.rds")

library(shiny)

date <- c("2021-07-01", "2021-08-01", "2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01", "2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01", "2022-05-01", "2022-06-01")
ui <- fluidPage(
  selectInput("dates", "Pick a date",
              date, selected = "2021-07-01",
              multiple = FALSE),

  tmapOutput("my_map")
)

# Define the server
server <- function(input, output) {
  # Render the tmap in the output element
  output$my_map <- renderTmap({
    a <- vaccination |>
      filter(date == input$dates)
    
    tm_shape(a) +
      tm_fill("vaccination_rate",
              n = 6,
              style = "quantile",
              palette = "Blues",
              title = "Vaccination Rate") +
      tm_layout(main.title = paste(input$dates),
                main.title.position = "left",
                legend.height = 0.8, 
                legend.width = 0.8,
                frame = TRUE) +
      tm_borders(alpha = 0.5) +
      tm_grid(alpha =0.2)
  })
}

# Run the app
shinyApp(ui, server)








