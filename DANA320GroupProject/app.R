library(shiny)
library(tidyverse)

companies_data <- read_csv(file = "./Top_12_German_Companies.csv")

variables <- setdiff(names(companies_data), "Companies")
variablesFiltered = variables[-(1:2)]
companies <- companies_data %>% distinct(Company) %>% pull()

ui <- fluidPage(
  titlePanel("12 German Companies"),
  tabsetPanel(   
    type = "pills",
    
    tabPanel("Page 1",
             
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
    ),
    
    tabPanel("Page2", 
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "company",
                             label = "Choose a company",
                             choices = companies,
                             selected = companies),
                 selectInput(inputId = "yVar",
                             label = "Choose a Y variable",
                             choices = variablesFiltered,
                             selected = variablesFiltered[[2]]),
                 dateRangeInput("dates", 
                                "Date range",
                                start = "2017-03-31", 
                                end = "2024-12-31",
                                min = "2017-03-31",
                                max = "2024-12-31")

               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput(outputId = "plot2")
               )
             )
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
                   data = companies_data)
    } else {
      p1 <- p1 + 
        geom_point(aes(x = .data[[input$xVar]],
                       y = .data[[input$yVar]]),
                   data = companies_data)
    }
    
    print(p1)
    
  })
  output$plot2 <- renderPlot({
    company_specific_df <- companies_data %>% 
      filter(Company == input$company) %>%
      mutate(Period = as.Date(Period, "%m/%d/%Y")) %>%
      filter(Period > input$dates[1] & Period < input$dates[2])
    

    p2 <- ggplot() + theme_bw()
    p2 <- p2 + geom_col(aes(
      x = .data$Period,
      y = .data[[input$yVar]]),
      data = company_specific_df)
    
    print(p2)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
