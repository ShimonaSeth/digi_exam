library(shiny)
library(bslib)
library(ggpubr)
library(ggplot2)
library(ggtext)

source("data.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Adverse Event"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("county_selector", "Select County", choices = unique(data$County)),
      sliderInput("year_range", "Select Year Range", min = min(data$Year), max = max(data$Year), value = c(min(data$Year), max(data$Year)))
    ),
    
    mainPanel(
      tabsetPanel(
        # Tab for Plot
        tabPanel("Plot",
                 h3("Observed Rate of PSI Description"),
                 plotlyOutput("ObservedRateofEachPSIDescription"),
                 br(),
                 div(
                   class = "description",
                   p("In this plot, the", tags$b(style = "color: blue;", "Observed Rate"), "refers to the observed rate of adverse events associated with each", tags$b(style = "color: green;", "PSI"), " (Patient Safety Indicator) Description."),
                
                 )
        ),
        
        # Tab for Summary
        tabPanel("Summary",
                 h3("Observed Rate Table"),
                 tableOutput("ObservedRateTable"),
                 br(),
                 div(
                   class = "description",
                   p("The table above shows the summary of the observed rates of adverse events for each year.")
                 )
        ),
        
        # Tab for Table
        tabPanel("Table",
                 h3("Filtered Data Table"),
                 DTOutput("table"),
                 br(),
                 div(
                   class = "description",
                   p("The table above displays the filtered data based on the selected county and year range.")
                 )
        )
      )
    )
  ),
  
  # Apply custom CSS
  tags$head(
    tags$style(HTML("
      .description {
        margin-top: 20px;
        font-size: 14px;
        color: #555;
      }
    "))
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  df_table <- data.frame(
    Year = AE_table_sum$Year,
    Sum_ObsRate = AE_table_sum$Sum_ObsRate
  )
  filtered_data <- reactive({
    if (!is.null(input$county_selector)) {
      data[data$County == input$county_selector & data$Year >= input$year_range[1] & data$Year <= input$year_range[2], ]
    } else {
      data[data$Year >= input$year_range[1] & data$Year <= input$year_range[2], ]
    }
  })
  output$ObservedRateofEachPSIDescription <- renderPlotly({
    plot_ObsRate <- ggplot(filtered_data(), aes(x = Year, y = ObsRate, fill = PSIDescription)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(
        title = paste("Observed Rate for Each PSIDescription in", input$county_selector, "(2005-2015)"),
        x = "Year",
        y = "Observed Rate (per 100,000)",
        fill = "PSIDescription"
      ) +
      theme(legend.position = "right")
    
    ggplotly(plot_ObsRate, tooltip = c("PSIDescription", "ObsRate"))
  })
  
  output$ObservedRateTable <- renderTable({
    AE_table_sum <- data %>%
      group_by(Year) %>%
      summarise(Sum_ObsRate = sum(ObsRate)) %>%
      ungroup()
    
    AE_table_sum
  })
  output$table <- renderDT({ # Changed renderTable to renderDT here
    DT::datatable(
      filtered_data(),
      options = list(pageLength = 10)
    )
  })
  # Dynamic UI for description
  output$description <- renderUI({
    HTML(paste(
      "In this application, <b style='color: blue;'>ObsRate</b> refers to the observed rate of adverse events associated with each <b style='color: green;'>PSI</b> (Patient Safety Indicator) Description.",
      "<br>",
      "A <b style='color: green;'>PSI</b> stands for Patient Safety Indicator, which is a measure used to assess the quality and safety of healthcare services."
    ))
  })
}



# Run the application 
shinyApp(ui = ui, server = server)