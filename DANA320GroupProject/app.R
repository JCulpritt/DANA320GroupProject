library(shiny)
library(tidyverse)

companies <- read_csv(file = "./Top_12_German_Companies.csv")

variables <- setdiff(names(companies), "Companies")


ui <- fluidPage(
  
  titlePanel("Top 12 German Companies"),
  
  # Sidebar with inputs for variables
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "xVar",
                  label = "Choose an X variable",
                  choices = variables),
      selectInput(inputId = "yVar",
                  label = "Choose a Y variable",
                  choices = variables,
                  selected = variables[[2]]),
      
      #Checkbox for coloring by companies
      checkboxInput(inputId = "company",
                    label = "Color by Company",
                    value = TRUE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput(outputId = "plot1")
    )
  )
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    
    #Base case
    p1 <- ggplot() + theme_bw()
    
    #Condition to check for checkbox
    if(input$company == TRUE){
      p1 <- p1 +
        geom_point(aes(x = .data[[input$xVar]],
                       y = .data[[input$yVar]],
                       color = Company),
                   data = companies)
    } else {
      p1 <- p1 + 
        geom_point(aes(x = .data[[input$xVar]],
                       y = .data[[input$yVar]]),
                   data = companies)
    }
    
    print(p1)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
