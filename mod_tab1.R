# module for the first page of the app
# contains functions for the UI and Server that produce a map of a user-specified county, showing landcover, as well as a map showing the location of the county in Georgia

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

countymaptabUI <- function(id){
  fluidRow(
    selectInput(NS(id, "county"), label = "Choose County",
                choices = county_options),
    column(7, plotOutput(NS(id, "CountyMap"))),
    column(5, plotOutput(NS(id, "StateMap")))
  )
}
countymaptabServer <- function(id){
  moduleServer(id, function(input, output, session){
    output$CountyMap <- renderPlot(landcover_map(input$county))
    output$StateMap <- renderPlot(county_location_map(input$county))
  })
}