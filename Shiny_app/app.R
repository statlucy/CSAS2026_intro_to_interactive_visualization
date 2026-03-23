library(shiny)
library(plotly)
library(tidyverse)

# -----------------------
# Load data
# -----------------------
stones <- read_csv("Stones.csv")
teams  <- read_csv("Teams.csv")

data <- left_join(stones, teams, by = "TeamID")

# Task labels
task_labels <- c(
  "0" = "Draw",
  "1" = "Front",
  "2" = "Guard",
  "3" = "Raise / Tap-back",
  "4" = "Wick / Soft Peeling",
  "5" = "Freeze",
  "6" = "Take-out",
  "7" = "Hit and Roll",
  "8" = "Clearing",
  "9" = "Double Take-out",
  "10" = "Promotion Take-out",
  "11" = "Through",
  "13" = "No statistics"
)

# -----------------------
# UI
# -----------------------
ui <- fluidPage(
  
  titlePanel("Curling Shot Explorer"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("task",
                  "Shot Type:",
                  choices = c("All", task_labels),
                  selected = "All"),
      
      sliderInput("points",
                  "Execution score (0–4):",
                  min = 0,
                  max = 4,
                  value = c(0, 4),
                  step = 1),
      
      checkboxInput("in_play",
                    "Only stones in play (remove 0 and 4095)",
                    value = TRUE),
      
      checkboxInput("sample",
                    "Use Sample (faster)",
                    value = FALSE)  # default OFF
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Scatter Plot",
                 plotlyOutput("scatter")),
        
        tabPanel("Density",
                 plotlyOutput("density")),
        
        tabPanel("Summary",
                 tableOutput("summary"))
      )
    )
  )
)

# -----------------------
# Server
# -----------------------
server <- function(input, output) {
  
  filtered <- reactive({
    
    df <- data
    
    # Filter by task
    if (input$task != "All") {
      df <- df %>% filter(Task == as.numeric(names(task_labels)[task_labels == input$task]))
    }
    
    # Filter by points RANGE
    df <- df %>%
      filter(Points >= input$points[1], Points <= input$points[2])
    
    # Remove invalid stone positions
    if (input$in_play) {
      df <- df %>%
        filter(stone_1_x > 0, stone_1_x < 4095,
               stone_1_y > 0, stone_1_y < 4095)
    }
    
    # Optional sampling
    if (input$sample && nrow(df) > 5000) {
      df <- sample_n(df, 5000)
    }
    
    # Add TaskName column
    df <- df %>%
      mutate(TaskName = task_labels[as.character(Task)])
    
    # Prevent empty dataset crash
    validate(
      need(nrow(df) > 0, "No data for selected filters")
    )
    
    df
  })
  
  # -----------------------
  # Scatter Plot
  # -----------------------
  output$scatter <- renderPlotly({
    
    plot_ly(filtered(),
            x = ~stone_1_x,
            y = ~stone_1_y,
            color = ~TaskName,   # use TaskName for color
            text = ~paste(
              "Points:", Points,
              "<br>Player:", PlayerID,
              "<br>Team:", TeamID,
              "<br>Shot Type:", TaskName
            ),
            mode = "markers") %>%
      layout(
        title = "Shot Locations",
        xaxis = list(title = "X Position (0–1500)"),
        yaxis = list(title = "Y Position (0–3000)")
      )
  })
  
  # -----------------------
  # Density Plot
  # -----------------------
  output$density <- renderPlotly({
    plot_ly(
      filtered(),
      x = ~stone_1_x,
      y = ~stone_1_y,
      type = "histogram2d",
      nbinsx = 30,   # reduce number of bins along x
      nbinsy = 30    # reduce number of bins along y
    ) %>%
      layout(
        title = "Shot Density",
        xaxis = list(title = "X Position"),
        yaxis = list(title = "Y Position"),
        coloraxis = list(colorbar = list(title = "Count"))
      )
  })
  
  # -----------------------
  # Summary Table
  # -----------------------
  output$summary <- renderTable({
    
    filtered() %>%
      group_by(TaskName) %>%   # group by TaskName
      summarise(
        Count = n(),
        Avg_Points = round(mean(Points, na.rm = TRUE), 2)
      ) %>%
      arrange(desc(Count))
  })
}

# -----------------------
# Run App
# -----------------------
shinyApp(ui, server)