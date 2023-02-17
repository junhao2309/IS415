pacman::p_load(sf, raster, spatstat, tmap, tidyverse, sfdep, maptools, readxl)
geoDKI <- st_read(dsn = "data/geospatial",
                  layer = "BATAS_DESA_DESEMBER_2019_DUKCAPIL_DKI_JAKARTA")
geoDKI <- geoDKI %>%
  st_transform(crs = 23878)
geoDKI <- filter(geoDKI, KAB_KOTA !="KEPULAUAN SERIBU")

# Set the working directory to the folder containing the Excel files
setwd("data/aspatial/")
# Get a list of all Excel files in the directory
aspatial_data <- list.files(pattern = ".xlsx")

# Loop through the files and read each one into a data frame
for (i in aspatial_data) {
  assign(gsub(".xlsx", "", i), read_excel(i))
}

list_mth<- list(`2021-7`,`2021-8`,`2021-9`,`2021-10`,`2021-11`,`2021-12`,`2022-1`,`2022-2`,`2022-3`,`2022-4`,`2022-5`,`2022-6`)

date <- c("July 2021", "August 2021", "September 2021", "October 2021", "November 2021", "December 2021", "January 2022", "February 2022", "March 2022", "April 2022", "May 2022", "June 2022")

lists <- list()
for (i in c(1:12)){
  lists[[i]]<- list_mth[[i]] %>% 
    rename(village_code=`KODE KELURAHAN`, 
           city_region =`WILAYAH KOTA`, 
           subdistrict=`KECAMATAN`, 
           ward=`KELURAHAN`, 
           total_vaccination= `TOTAL VAKSIN\r\nDIBERIKAN`, 
           not_vaccinated =`BELUM VAKSIN`) %>% 
    select(village_code, city_region, subdistrict, not_vaccinated ,total_vaccination) %>%
    mutate(date = date[i], 
           .before=1)
}

aspatial_data <- Reduce(rbind, lists)

aspatial_data <- aspatial_data %>%
  na.exclude() %>%
  filter(city_region != "KAB.ADM.KEP.SERIBU") %>%
  mutate(total_population = total_vaccination + not_vaccinated, vaccination_rate = total_vaccination/total_population)

vaccination <- left_join(aspatial_data, geoDKI, 
                         by = c("village_code" = "KODE_DESA"))

vaccination <- st_as_sf(vaccination) %>%
  mutate(date = as.factor(date))

library(shiny)

date <- c("July 2021", "August 2021", "September 2021", "October 2021", "November 2021", "December 2021", "January 2022", "February 2022", "March 2022", "April 2022", "May 2022", "June 2022")
# Define the UI
ui <- fluidPage(
  selectInput("dates", "Pick a month",
              date, selected = "July 2021",
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
              style = "quantile",
              palette ="Blues")
  })
}

# Run the app
shinyApp(ui, server)








