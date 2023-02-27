pacman::p_load(sf, raster, spatstat, tmap, tidyverse, sfdep, maptools, readxl, plotly)
geoDKI <- st_read(dsn = "data/geospatial",
                  layer = "BATAS_DESA_DESEMBER_2019_DUKCAPIL_DKI_JAKARTA")
geoDKI <- geoDKI %>%
  st_transform(crs = 23878)

geoDKI$KAB_KOTA[243]<-"JAKARTA UTARA"
geoDKI$KAB_KOTA[244]<-"JAKARTA UTARA"

geoDKI$KODE_DESA[243]<-"3188888801"
geoDKI$KODE_DESA[244]<-"3188888802"
geoDKI <- geoDKI %>%
  select(2, "geometry") %>%
  rename(village_code = `KODE_DESA`)
# Set the working directory to the folder containing the Excel files
setwd("data/aspatial/")
# Get a list of all Excel files in the directory
aspatial_data <- list.files(pattern = ".xlsx")

# Loop through the files and read each one into a data frame
for (i in aspatial_data) {
  assign(gsub(".xlsx", "", i), read_excel(i))
}

list_mth<- list(`2021-7`,`2021-8`,`2021-9`,`2021-10`,`2021-11`,`2021-12`,`2022-1`,`2022-2`,`2022-3`,`2022-4`,`2022-5`,`2022-6`)

date <- c("2021-07-01", "2021-08-01", "2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01",
          "2022-01-01", "2022-02-01", "2022-03-01","2022-04-01", "2022-05-01", "2022-06-01")

lists <- list()
for (i in c(1:12)){
  lists[[i]]<- list_mth[[i]] %>% 
    rename(village_code=`KODE KELURAHAN`,
           city_region =`WILAYAH KOTA`,
           vaccinated= `SASARAN`, 
           not_vaccinated =`BELUM VAKSIN`) %>% 
    select(village_code,city_region, not_vaccinated ,vaccinated) %>%
    mutate(date = as.Date(date[i]), 
           .before=1)
}
aspatial_data <- Reduce(rbind, lists)
aspatial_data <- aspatial_data %>%
  na.exclude() %>%
  filter(city_region != "KAB.ADM.KEP.SERIBU") %>% 
  mutate(total_population = vaccinated + not_vaccinated, 
         vaccination_rate = as.numeric(vaccinated/total_population)) %>%
  select(date, village_code, vaccination_rate)
aspatial_data <- rbind(aspatial_data, c("2021-07-01", 3188888801,NA),
                       c("2021-08-01", 3188888801,NA),
                       c("2021-09-01", 3188888801,NA),
                       c("2021-10-01", 3188888801,NA),
                       c("2021-11-01", 3188888801,NA),
                       c("2021-12-01", 3188888801,NA),
                       c("2022-01-01", 3188888801,NA),
                       c("2022-02-01", 3188888801,NA),
                       c("2022-03-01", 3188888801,NA),
                       c("2022-04-01", 3188888801,NA),
                       c("2022-05-01", 3188888801,NA),
                       c("2022-06-01", 3188888801,NA),
                       c("2021-07-01", 3188888802,NA),
                       c("2021-08-01", 3188888802,NA),
                       c("2021-09-01", 3188888802,NA),
                       c("2021-10-01", 3188888802,NA),
                       c("2021-11-01", 3188888802,NA),
                       c("2021-12-01", 3188888802,NA),
                       c("2022-01-01", 3188888802,NA),
                       c("2022-02-01", 3188888802,NA),
                       c("2022-03-01", 3188888802,NA),
                       c("2022-04-01", 3188888802,NA),
                       c("2022-05-01", 3188888802,NA),
                       c("2022-06-01", 3188888802,NA))
vaccination <- left_join(aspatial_data, geoDKI, 
                         by = c("village_code")) %>%
  mutate(vaccination_rate = as.numeric(vaccination_rate)) %>%
  na.exclude()
vaccination <- st_as_sf(vaccination)

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








