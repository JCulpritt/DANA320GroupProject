library(shiny)
library(tidyverse)
library(plotly)
library(RColorBrewer)

# This app was made to develop plots based on the Top 12 German Companies. The user can dive into the data to discover the trends that may be present.
# There are a number of different pages for the user to explore, including pages for a scatter plot, a bar plot, a 3D scatter plot, a box plot, and a 
# correlation heat map. Within these pages are a number of variables that can be changed to manipulate the data, some of which include the following:
# coloration, a selection of the companies, date ranges, and the general variables. Have fun exploring the app! Find out as much as you can about the 
# financial circumstances of these companies!

# Load data
companies <- read_csv(file = "./Top_12_German_Companies_NEW.csv")

# Prepare variables
variables <- setdiff(names(companies), c("Company", "Period"))
variables2 <- setdiff(names(companies), c("Company", "Period"))
variablesFiltered = variables[-(1:2)]
numericVariables <- names(companies)[sapply(companies, is.numeric)]
companies$Period <- as.Date(companies$Period, format = "%Y-%m-%d")
companies_list <- companies %>% distinct(Company) %>% pull()

# UI
ui <- fluidPage(
  
  titlePanel("Top 12 German Companies"),
  
  tabsetPanel(
    type = "pills",
    
    tabPanel("Scatter Plot",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "xVar", label = "Choose an X variable", choices = variables),
                 selectInput(inputId = "yVar", label = "Choose a Y variable", choices = variables, selected = variables[[2]]),
                 checkboxInput(inputId = "company", label = "Color by Company", value = TRUE),
                 checkboxGroupInput(inputId = "selectedCompanies",
                                    label = "Select Companies to Display",
                                    choices = companies_list,
                                    selected = companies_list)
               ),
               mainPanel(
                 plotOutput(outputId = "plot1", height = "800px", width = "100%")
               )
             )
    ),
    
    tabPanel("Bar Plot",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "company", label = "Choose a company", choices = companies_list, selected = companies),
                 selectInput(inputId = "yVar", label = "Choose a Y variable", choices = variablesFiltered, selected = variablesFiltered[[2]]),
                 dateRangeInput("dates", "Date range", start = "2017-03-31", end = "2024-12-31", min = "2017-03-31", max = "2024-12-31")
               ),
               mainPanel(
                 plotOutput(outputId = "plot2", height = "800px", width = "100%")
               )
             )
    ),
    
    tabPanel("3D Scatter Plot",
             titlePanel("- 3D Scatter Plot - Use in a full window please!"),
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "yVar", label = "Choose Y variable", choices = variables2),
                 selectInput(inputId = "zVar", label = "Choose Z variable", choices = variables2),
                 sliderInput(inputId = "dateRange", label = "Select Date Range", min = min(companies$Period), max = max(companies$Period), value = c(min(companies$Period), max(companies$Period)), timeFormat = "%Y-%m-%d"),
                 width = 3
               ),
               mainPanel(
                 plotlyOutput(outputId = "scatter3d", height = "800px", width = "100%")
               )
             )
    ),
    
    tabPanel("Box Plot",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "boxCompany", label = "Choose a Company", choices = companies_list),
                 selectInput(inputId = "boxVar", label = "Choose a Variable", choices = variablesFiltered)
               ),
               mainPanel(
                 plotOutput(outputId = "boxPlot", height = "800px", width = "100%")
               )
             )
    ),
    tabPanel("Correlation Heatmap",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput(
                   inputId = "corrVars",
                   label = "Select Variables",
                   choices = numericVariables,
                   selected = numericVariables
                 ),
                 dateRangeInput("corrDates", "Date range", 
                                start = min(companies$Period), 
                                end = max(companies$Period),
                                min = min(companies$Period), 
                                max = max(companies$Period)),
               ),
               mainPanel(
                 plotOutput("corrPlot", height = "800px", width = "100%")
               )
             )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Scatter plot
  output$plot1 <- renderPlot({
    filtered_data <- companies[companies$Company %in% input$selectedCompanies, ]
    if (nrow(filtered_data) == 0) {
      return(
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No companies selected", size = 5, hjust = 0.5) + 
          theme_void()
      )
    }
    p1 <- ggplot() + theme_bw()
    if (input$company == TRUE) {
      p1 <- p1 + geom_point(aes(x = .data[[input$xVar]], y = .data[[input$yVar]], color = Company), data = filtered_data) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.margin = margin(1, 1, 2, 1))
    } else {
      p1 <- p1 + geom_point(aes(x = .data[[input$xVar]], y = .data[[input$yVar]]), data = filtered_data) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.margin = margin(1, 1, 2, 1))
    }
    print(p1)
  })
  
  # Bar plot
  output$plot2 <- renderPlot({
    company_specific_df <- companies %>% 
      filter(Company == input$company) %>% 
      mutate(Period = as.Date(Period, "%m/%d/%Y")) %>% 
      filter(Period > input$dates[1] & Period < input$dates[2])
    p2 <- ggplot() + 
      geom_col(aes(x = .data$Period, y = .data[[input$yVar]]), data = company_specific_df) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      theme_bw()
    print(p2)
  })
  
  # 3D Scatter plot
  output$scatter3d <- renderPlotly({
    filtered_data <- companies %>% 
      filter(Period >= input$dateRange[1] & Period <= input$dateRange[2])
    
    plot_3d <- plot_ly(data = filtered_data, x = ~Period, y = ~get(input$yVar), z = ~get(input$zVar), type = "scatter3d", mode = "markers", color = ~Company, colors = brewer.pal(8, "Set1"))
    plot_3d <- plot_3d %>% layout(
      title = list(text = paste("Graph of", input$yVar, "and", input$zVar, "from", input$dateRange[1], "to", input$dateRange[2]), x = 0.4, y = 0.9, xanchor = "center", yanchor = "top"),
      scene = list(xaxis = list(title = "Period", tickformat = "%Y-%m-%d"), yaxis = list(title = input$yVar), zaxis = list(title = input$zVar))
    )
    plot_3d
  })
  
  # Box plot
  output$boxPlot <- renderPlot({
    box_data <- companies %>% filter(Company == input$boxCompany)
    p_box <- ggplot(box_data, aes(x = as.factor(quarter(Period)), y = .data[[input$boxVar]], fill = as.factor(quarter(Period)))) +
      geom_boxplot() +
      labs(x = "Quarter", y = input$boxVar, fill = "Quarter", title = paste("Box Plot of", input$boxVar, "for", input$boxCompany)) +
      theme_bw()
    print(p_box)
  })
  #Correlation Heatmap
  output$corrPlot <- renderPlot({
    req(input$corrVars) 
    
    corr_data <- companies %>%
      filter(Period >= input$corrDates[1] & Period <= input$corrDates[2])
    
    selected_data <- corr_data %>%
      select(any_of(input$corrVars))
    
    selected_data <- selected_data %>% select_if(is.numeric)
    
    if (ncol(selected_data) < 2) {
      return(NULL)
    }
    
    cormat <- cor(selected_data, use = "pairwise.complete.obs")
    
    cormat_long <- as.data.frame(as.table(cormat))
    colnames(cormat_long) <- c("Var1", "Var2", "Correlation")
    
    ggplot(cormat_long, aes(x = Var1, y = Var2, fill = Correlation)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                           midpoint = 0, limit = c(-1, 1), space = "Lab",
                           name = "Corr") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      coord_fixed() +
      labs(title = "Correlation Heatmap",
           subtitle = paste("Variables from", input$corrDates[1], "to", input$corrDates[2]))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
