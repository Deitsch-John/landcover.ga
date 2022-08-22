# module for the second page of the app
# contains functions for the UI and Server that produce a map of a user-specified landcover class within a user-specified county, as well as a table showing percentages of each landcover within the county

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
