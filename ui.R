library(shiny)
library(leaflet)

# Code borrowed from: https://stackoverflow.com/questions/31415301/shiny-responds-to-enter
# This generates "keyPressed" when <CR> is pressed
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
                        h3("Enter addresses:"),
                        textInput("from", "Where are you traveling from:",value="Caracas, Venezuela"),
                        textInput("to", "Where are you going to:",value="San Francisco, California"),
                        actionButton("keyPressed", "Submit"),
                        tags$script(js),
                        hr(),
                        tags$b("Instructions:"),
                        p("Type the names of two places, and either click 'Submit' or hit <Return>!")
                ),
                mainPanel(
                        leafletOutput("theMap"),
                        em("Click on the circles for details ..."),
                        hr(),
                        h3("This is what I found:"),
                        tags$b("From:"),
                        textOutput("foundFrom"),
                        textOutput("fromTime"),
                        tags$b("To:"),
                        textOutput("foundTo"),
                        textOutput("toTime"),
                        textOutput("timeDifference"),
                        hr(),
                        tags$footer("Timezone data provided by www.geonames.org/"),
                        tags$footer("Location data provided by openstreetmap.org")
                )
        )
)