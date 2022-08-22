# module for the second page of the app
# contains functions for the UI and Server that produce a map of a user-specified landcover class within a user-specified county, as well as a table showing percentages of each landcover within the county
url = "https://github.com/Deitsch-John/landcover.ga/raw/main/shapefiles.zip"
download.file(url, "shapefiles.zip")
unzip("shapefiles.zip")

UScounties <- read_sf("US_counties", dsn = ".")
UScities = read_sf("USA_Major_Cities", dsn = ".")

urlb = "https://github.com/Deitsch-John/landcover.ga/raw/main/nlcd_ga.zip"
download.file(urlb, "nlcd_ga.zip")
unzip("nlcd_ga.zip")

NLCD <- rast("nlcd_ga.IMG")

GeorgiaCounties <- UScounties%>%
  filter(STATE=="GA")%>%
  st_transform(., st_crs(NLCD))

GeorgiaCities <- UScities %>%
  filter(ST=="GA")%>%
  st_transform(., st_crs(NLCD))

landcover_type_options <- c("Shrubs and Wetlands", "Deciduous Forest",
                            "Evergreen Forest", "Mixed Forest",
                            "Developed", "Water", "Pasture/Crops")

county_options <- UScounties%>%
  filter(STATE=="GA")%>%
  st_drop_geometry()%>%
  dplyr::select(COUNTYNAME)%>%
  arrange(COUNTYNAME)%>%
  unique()%>%
  pull()

exploretabUI <- function(id){
  fluidRow(
    selectInput(NS(id, "county"), label = "Choose County",
                choices = county_options),
    column(4, tableOutput(NS(id, "CountyTable"))),
    column(8, selectInput(NS(id, "class"), label = "Choose Landcover",
                          choices = landcover_type_options),
           plotOutput(NS(id, "CountyMap2"))
    ))
}
exploretabServer <- function(id){
  moduleServer(id, function(input, output, session){
    output$CountyTable <- render_gt(landcover_table(input$county))
    output$CountyMap2 <- renderPlot(landcover_map_class(input$county,
                                                        input$class))
  })
}
