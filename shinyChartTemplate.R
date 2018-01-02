library(ggplot2)
library(dplyr)
library(shiny)
# Data pre-processing ----
# Tweak the "am" variable to have nicer factor labels -- since this
# doesn't rely on any user inputs, we can do this once at startup
# and then use the value throughout the lifetime of the app
mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {

  # Filter the gear type before visualising (should be inside "reactive" to 
  # respond to user actions)
  mtcars_gear <- reactive({
    mpgData %>% filter(am == input$gear)
  })

  # Generate a plot filtered on the gear
  # Attention! mtcars_gear is called as a function with ()
  output$mpgPlot <- renderPlot({
    ggplot(data=mtcars_gear(),aes(x=disp, y = mpg))+geom_point() 
  })

}


# Define UI for miles per gallon app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Miles Per Gallon"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Selector for gear type
      selectInput("gear", "Gear",
                  c("Automatic",
                    "Manual"))

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Plot of the requested variable against mpg ----
      plotOutput("mpgPlot")

    )
  )
)


shinyApp(ui, server)
