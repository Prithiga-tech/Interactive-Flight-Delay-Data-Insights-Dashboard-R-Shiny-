# ================================
# 📦 Load Libraries
# ================================
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

# ================================
# 📊 Create Dataset
# ================================
flights_data <- data.frame(
  Flight_ID = c("AA101","BA202","DL303","UA404","AA105","BA206","DL307","UA408"),
  Airline = c("American","British Airways","Delta","United","American","British Airways","Delta","United"),
  Origin = c("JFK","LAX","ORD","DFW","JFK","LAX","ORD","DFW"),
  Destination = c("LAX","ORD","JFK","SFO","ORD","JFK","SFO","LAX"),
  Departure_Delay = c(5,-3,12,0,25,-7,30,45),
  Arrival_Delay = c(10,-5,15,5,35,-10,40,50)
)

weather_data <- data.frame(
  Origin = c("JFK","LAX","ORD","DFW"),
  Weather_Condition = c("Rain","Clear","Fog","Storm"),
  Temperature = c(15,22,8,30)
)

# Merge
merged_data <- flights_data %>%
  left_join(weather_data, by = "Origin")

# Reshape
long_data <- merged_data %>%
  pivot_longer(cols = c(Departure_Delay, Arrival_Delay),
               names_to = "Delay_Type",
               values_to = "Delay_Value")

# ================================
# 🖥️ UI
# ================================
ui <- fluidPage(
  
  titlePanel("✈️ Flight Delay Dashboard (Interactive Colors)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("airline","Select Airline:",
                  choices = unique(merged_data$Airline)),
      
      selectInput("delayType","Select Delay Type:",
                  choices = unique(long_data$Delay_Type))
    ),
    
    mainPanel(
      plotOutput("delayPlot"),
      plotOutput("weatherPlot"),
      tableOutput("dataTable")
    )
  )
)

# ================================
# ⚙️ Server
# ================================
server <- function(input, output) {
  
  filtered <- reactive({
    merged_data %>% filter(Airline == input$airline)
  })
  
  # 🎨 Dynamic color palette
  get_colors <- reactive({
    if(input$airline == "American") return("Blues")
    if(input$airline == "Delta") return("Reds")
    if(input$airline == "United") return("Greens")
    return("Purples")
  })
  
  # 📊 Delay Plot (Gradient Colors)
  output$delayPlot <- renderPlot({
    long_data %>%
      filter(Airline == input$airline,
             Delay_Type == input$delayType) %>%
      ggplot(aes(x = Flight_ID, y = Delay_Value, fill = Delay_Value)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      theme_minimal() +
      labs(title = paste("Delay for", input$airline),
           x = "Flight",
           y = "Delay (min)")
  })
  
  # 🌦️ Weather Plot (Colorful)
  output$weatherPlot <- renderPlot({
    filtered() %>%
      ggplot(aes(x = Weather_Condition,
                 y = Arrival_Delay,
                 fill = Weather_Condition)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal() +
      labs(title = "Weather Impact",
           x = "Weather",
           y = "Delay")
  })
  
  # 📋 Table
  output$dataTable <- renderTable({
    filtered()
  })
}

# ================================
# 🚀 Run App
# ================================
shinyApp(ui = ui, server = server)