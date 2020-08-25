library(shiny)
library(leaflet)

# Code borrowed from: https://stackoverflow.com/questions/31415301/shiny-responds-to-enter
js <- '
$(document).on("keyup", function(e) {
  if(e.keyCode == 13){
    Shiny.onInputChange("keyPressed", Math.random());
  }
});
'

shinyUI(
        pageWithSidebar(
                headerPanel("Distance/Time-Zone Calculator"),
                sidebarPanel(
                        textInput("from", "Where are you traveling from:",value="Caracas, Venezuela"),
                        textInput("to", "Where are you going to:",value="San Francisco, California"),
                        actionButton("keyPressed", "Submit"),
                        tags$script(js)
                ),
                mainPanel(
                        leafletOutput("theMap"),
                        textOutput("ofrom"),
                        textOutput("oto"),
                        textOutput("text3")
                )
        )
)