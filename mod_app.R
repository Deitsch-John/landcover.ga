# function that runs the two modules and creates the app

landcoverApp <- function(){
  ui <- fluidPage(
    theme = shinytheme("journal"),
    titlePanel("Georgia Landcover by County"),
    tabsetPanel(
      tabPanel("Landcover Map",
               countymaptabUI("map")),
      tabPanel("Explore by Landcover Type",
               exploretabUI("info")
      )
    )
  )
  
  server <-function(input, output, session){
    countymaptabServer("map")
    exploretabServer("info")
  }
  shinyApp(ui, server)
}