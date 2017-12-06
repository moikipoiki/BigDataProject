library(ggplot2)
library(dplyr)
library(shiny)
source("shiny_function.R")

####  UI #####
ui <- fluidPage(
  titlePanel("Big Data Project"),
  
  fluidPage(
    
    # Copy the chunk below to make a group of checkboxes
    checkboxGroupInput("programming", label = h3("Programming Languages"), 
                       choices = list("Not relevant" = 999,"Python" = 1, "PHP" = 2, "JavaScript" = 3, "C" = 4, "Java" = 5),
                       selected = 999),
    # Python, PHP, JavaScript, C, Java, 
    
    hr(), 
    
    radioButtons("temperature", label = h3("Temperature"),
                 choices = list("Not relevant" = 999,"Cold" = 1, "Middle" = 2, "Warm" = 3), 
                 selected = 999),

    hr(),
    
    radioButtons("rain", label= h3("Rain"),
                 choices = list("Not relevant" = 999,"No" = 1, "Middle" = 2, "Much" = 3), 
                 selected = 999),

    hr(),
    
    radioButtons("snow", label= h3("Snow"),
                 choices = list("Not relevant" = 999,"0cm" = 1, "<10cm" = 2, "<20cm" = 3, ">=20cm"=4), 
                 selected = 999),

    hr(), 
    
    radioButtons("sun", label= h3("Sun"),
                 choices = list("Not relevant" = 999,"No" = 1, "Middle" = 2, "Much" = 3), 
                 selected = 999),

    hr(),
    

    
    # Input: Decimal interval with step value ----
    sliderInput("conf", "Confidence:",
                min = 0.01, max = 0.3,
                value = 0.2, step = 0.01),
    
    # Input: Decimal interval with step value ----
    sliderInput("support", "Support:",
                min = 0.01, max = 0.3,
                value = 0.2, step = 0.01)
    
    
  ),
  #fluidRow(column(1, verbatimTextOutput("valueSupport")))
  tableOutput("values")
)



# Define server logic ----
server <- function(input, output) {
  # output$valueProgramming <- renderPrint({ input$programming })
  # output$valueTemperature <- renderPrint({ test(input$temperature, input$programming, input$rain) })
  # output$valueRain <- renderPrint({ input$rain })
  # output$valueSupport <- renderPrint({input$support})

  
  sliderValues <- reactive({
    
    # data.frame(
    #   Name = c("Support",
    #            "Snow"),
    #   Value = as.character(c(input$support,
    #                          input$snow)),
    #   stringsAsFactors = FALSE)
    data.frame(
      test(input$support, input$conf, input$snow, input$rain, input$sun, input$temperature)
    )
  })
  
  output$values <- renderTable({
    sliderValues()
  })

  
}



# Run the app ----
shinyApp(ui = ui, server = server)