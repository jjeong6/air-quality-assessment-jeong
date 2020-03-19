library(shiny)
library(ropenaq)
library(dplyr)
library(lubridate)
library(tidyverse)
library(plotly)


city_list <- aq_cities()

ui <- fluidPage(
    
    # Application title
    titlePanel("OpenAQ - Compare air quality of two cities"),
    
    # Sidebar Layout 
    sidebarLayout(
        sidebarPanel(
            #Select two cities to compare
            selectizeInput("city_search2",
                           "Pick two cities",
                           choices = unique(city_list$city),
                           multiple = TRUE,
                           options = list(maxItems = 2),
                           selected = c("Marlo","Dubai")),
            
            #Select a parameter for the graph
            selectInput("parameter_search",
                        "Choose your units",
                        choices = c("pm25","pm10","so2","no2","o3","co","bc"),
                        selected = "pm25"),
            
            #Select the date range of the graph
            dateRangeInput("date_to_search",
                           "Date Range",
                           start = Sys.Date() - 30, 
                           end = Sys.Date() - 20),
            #Action button to find data
            actionButton("get_data", "Search", class = "btn-primary")
            
        ),
        #Main panel that shows layout of the information
        mainPanel(
            plotlyOutput("city_values4"),
            textOutput("chosen_city1"),
            verbatimTextOutput("summary1"),
            textOutput("chosen_city2"),
            verbatimTextOutput("summary2"),
            dataTableOutput("tableofvalues"))
    )
)
server <- function(input, output) {
    
    
    #Finding the dataset for the first city
    first_test <- eventReactive(input$get_data,{
        aq_measurements(city = toString(input$city_search2[1]),
                        parameter = input$parameter_search,
                        date_from = input$date_to_search[1],
                        date_to = input$date_to_search[2])})
    #Reduce the data and rename variables
    first_test_data <- reactive({
        validate(
            need(first_test(), "NO DATA")
        )
        req(first_test())
        first_test() %>%
            select(value,dateUTC,city) %>%
            select(Values = value, Date = dateUTC, City_Name = city)
    })
    
    #Finding the dataset for the second city
    second_test <- eventReactive(input$get_data,{
        aq_measurements(city = toString(input$city_search2[2]),
                        parameter = input$parameter_search,
                        date_from = input$date_to_search[1],
                        date_to = input$date_to_search[2])})
    
    #Reduce the data and rename variables
    second_test_data <- reactive({
        req(second_test())
        second_test() %>%
            select(value,dateUTC,city) %>%
            select(Values = value, Date = dateUTC, City_Name = city)
    })
    
    #Stack the datasets using rbind with rows added on top.
    combined <- reactive({
        rbind(first_test_data(),second_test_data())
    })
    
    #Outputs the plot using both datasets
    output$city_values4 <- renderPlotly({
        city_2 <- combined()
        
        p <- ggplot(city_2, aes(Date, Values)) + 
            geom_line(aes(colour = City_Name))+
            geom_point(size = .5)
        ggplotly(p)
    })
    
    #Title of summary statistics
    output$chosen_city1 <- renderText({
        paste("Summary Statistics of ",first_test_data()$City_Name[1])
    })
    #Outputs a summary statistic of the first city
    output$summary1 <- renderPrint({
        citysummary1 <- first_test_data()
        summary(citysummary1$Values)
    })    
    #Title of summary statistics
    output$chosen_city2 <- renderText({
        paste("Summary Statistics of ",second_test_data()$City_Name[1])
    })
    #Ouputs a summary statistic of the second city
    output$summary2 <- renderPrint({
        citysummary2 <- second_test_data()
        summary(citysummary2$Values)
    })    
}

# Run the application 
shinyApp(ui = ui, server = server)

