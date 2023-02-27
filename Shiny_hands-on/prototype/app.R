pacman::p_load(shiny, tmap, tidyverse)

sgpools <-read_csv("data/aspatial/SGPools_svy21.csv")
sgpools_sf <- st_as_sf(sgpools, 
                       coords = c("XCOORD", 
                                  "YCOORD"),
                       crs = 3414)


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Static Proportional Symbol Map"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      plotOutput("mapPlot")
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  output$mapPlot <- renderPlot({
    tm_shape(sgpools_sf) +
    tm_bubbles(col = "OUTLET TYPE",
               size = "Gp1Gp2 Winnings")
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
