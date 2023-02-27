pacman::p_load(shiny,sf, tidyverse, tmap)

mpsz <- st_read(dsn = "data/geospatial",
                   layer = "MP14_SUBZONE_WEB_PL") %>%
  st_transform(crs = 3414)

popdata <- read_csv("data/aspatial/respopagesextod2011to2020.csv")

popdata2020 <- popdata %>%
  filter(Time == 2019) %>%
  group_by(PA, SZ, AG) %>%
  summarise(`POP` = sum(`Pop`)) %>%
  ungroup() %>%
  pivot_wider(names_from=AG, 
              values_from=POP) %>%
  mutate(YOUNG = rowSums(.[3:6])
         +rowSums(.[12])) %>%
  mutate(`ECONOMY ACTIVE` = rowSums(.[7:11])+
           rowSums(.[13:15]))%>%
  mutate(`AGED`=rowSums(.[16:21])) %>%
  mutate(`TOTAL`=rowSums(.[3:21])) %>%  
  mutate(`DEPENDENCY` = (`YOUNG` + `AGED`)
         /`ECONOMY ACTIVE`) %>%
  select(`PA`, `SZ`, `YOUNG`, 
         `ECONOMY ACTIVE`, `AGED`, 
         `TOTAL`, `DEPENDENCY`)
popdata2020 <- popdata2020 %>%
  mutate_at(.vars = vars(PA, SZ), 
            .funs = funs(toupper)) %>%
  filter(`ECONOMY ACTIVE` > 0)
mpsz_pop2020 <- left_join(mpsz, popdata2020,
                          by = c("SUBZONE_N" = "SZ")) %>%
  st_make_valid() 


library(shiny)

ui <- fluidPage(
    titlePanel("Choropleth Mapping System"),
    sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "mapv",
                      label = "Mapping variable",
                      choices = c("Young" = "YOUNG",
                                  "Economy Active"="ECONOMY ACTIVE", 
                                  "Aged"= "AGED",
                                  "Dependency" = "DEPENDENCY"),
                      selected = "DEPENDENCY"),
          selectInput("class",
                      "Classification method",
                      c("quantile"="quantile", 
                        "equal" = "equal", 
                        "pretty" = "pretty",
                        "sd" = "sd",
                        "kmeans" = "kmeans",
                        "hclust" = "hclust",
                        "bclust" = "bclust",
                        "fisher" ="fisher",
                        "jenks" = "jenks"),
                      selected = "pretty"),
          sliderInput("classes", 
                      "Number of classes:",
                      min = 6,
                      max = 12,
                      value = c(6)),
          selectInput("colour",
                      "Colour scheme",
                      c("Blues","Reds"),
                      selected = "Reds")
        ),
    mainPanel(
      tmapOutput("mapPlot"))
    ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$mapPlot <- renderTmap({
    tm_shape(mpsz_pop2020)+
      tm_fill(input$mapv, 
              style = input$class, 
              palette = input$colour,
              n = input$classes,
              title = paste(input$mapv, "ratio")) +
                tm_borders(alpha = 0.5)
})
}


# Run the application 
shinyApp(ui = ui, server = server)
