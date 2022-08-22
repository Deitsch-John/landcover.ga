# module for the first page of the app
# contains functions for the UI and Server that produce a map of a user-specified county, showing landcover, as well as a map showing the location of the county in Georgia

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