
############################################
#
# Speed Limits and Fatal Car Crashes
# Group Shiny App
# Sydney Garbe
# November 11th, 2026
#
############################################

library(shiny)
library(readxl)
library(dplyr)
library(bslib)
library(plotly)

# import dataset containing all laws and populations
# clean the data and select speed limit columns

traffic_data <- read_excel(
  "2024StateTrafficLawsSummary.xlsx"
) %>%
  mutate(
    STATE = toupper(trimws(as.character(STATE))),
    STATE_NAME = as.character(STATE_NAME),
    MAX_SPEED_LIMIT_MPH = as.numeric(MAX_SPEED_LIMIT_MPH),
    FATAL_CAR_CRASHES = as.numeric(FATAL_CAR_CRASHES),
    
    SPEED_GROUP = case_when(
      MAX_SPEED_LIMIT_MPH < 65 ~ "Under 65",
      MAX_SPEED_LIMIT_MPH == 65 ~ "65",
      MAX_SPEED_LIMIT_MPH == 70 ~ "70",
      MAX_SPEED_LIMIT_MPH == 75 ~ "75",
      MAX_SPEED_LIMIT_MPH >= 80 ~ "80 or Higher",
      TRUE ~ "Other"
    )
  )

rate_range <- range(
  traffic_data$FATAL_CAR_CRASHES,
  na.rm = TRUE
)

# establish UI

ui <- fluidPage(
  
  titlePanel("Speed Limits and Fatal Car Crashes"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # drop down selection panel
      selectInput(
        inputId = "speed_filter",
        label = "Select Maximum Speed Limit:",
        choices = c(
          "All States" = "All",
          "Under 65 MPH" = "Under 65",
          "65 MPH" = "65",
          "70 MPH" = "70",
          "75 MPH" = "75",
          "80 MPH or Higher" = "80 or Higher"
        ),
        selected = "All",
        width = "100%"
      ),
      
      # explanation of speed limit groups
      tags$details(
        
        style = "margin-top: 15px;",
        
        tags$summary(
          style = paste0(
            "cursor: pointer;",
            "font-weight: bold;",
            "padding: 10px;",
            "background-color: #E8EEF5;",
            "border-radius: 6px;"
          ),
          
          "What do the speed limits mean?"
        ),
        
        tags$div(
          style = "padding: 10px;",
          
          tags$p(
            tags$b("Maximum Speed Limit: "),
            "The highest posted legal speed limit
            within a state, measured in miles per hour."
          ),
          
          tags$p(
            tags$b("Under 65 MPH: "),
            "States with a maximum speed limit
            below 65 miles per hour."
          ),
          
          tags$p(
            tags$b("65 MPH: "),
            "States with a maximum speed limit
            of 65 miles per hour."
          ),
          
          tags$p(
            tags$b("70 MPH: "),
            "States with a maximum speed limit
            of 70 miles per hour."
          ),
          
          tags$p(
            tags$b("75 MPH: "),
            "States with a maximum speed limit
            of 75 miles per hour."
          ),
          
          tags$p(
            tags$b("80 MPH or Higher: "),
            "States with a maximum speed limit
            of 80 miles per hour or higher."
          )
        )
      ),
      
      # explanation of fatal crash data
      tags$details(
        
        style = "margin-top: 10px;",
        
        tags$summary(
          style = paste0(
            "cursor: pointer;",
            "font-weight: bold;",
            "padding: 10px;",
            "background-color: #E8EEF5;",
            "border-radius: 6px;"
          ),
          
          "About the Fatal Crash Data"
        ),
        
        tags$div(
          style = "padding: 10px;",
          
          tags$p(
            tags$b("Fatal Car Crashes: "),
            "The fatal crash value recorded
            for each state in the 2024 dataset."
          ),
          
          tags$p(
            tags$b("Map Colors: "),
            "Lighter turquoise represents lower
            fatal crash values, while darker purple
            represents higher fatal crash values."
          ),
          
          tags$p(
            tags$b("Gray States: "),
            "States that do not match the selected
            speed limit category appear gray."
          )
        )
      ),
      
      width = 3
    ),
    
    # create map
    mainPanel(
      
      plotlyOutput(
        "speed_map",
        height = "650px"
      ),
      
      width = 9
    )
    
  )
)

# create server

server <- function(input, output, session) {
  
  speed_data <- reactive({
    
    traffic_data %>%
      mutate(
        speed_match = if (input$speed_filter == "All") {
          TRUE
        } else {
          SPEED_GROUP == input$speed_filter
        },
        
        hover_text = paste0(
          "<b>", STATE_NAME, "</b>",
          "<br>Maximum Speed Limit: ",
          MAX_SPEED_LIMIT_MPH, " MPH",
          "<br>Fatality Value: ",
          round(FATAL_CAR_CRASHES, 4)
        )
      )
  })
  
  # make map interactive
  
  output$speed_map <- renderPlotly({
    
    df <- speed_data()
    
    matched <- df %>%
      filter(speed_match %in% TRUE)
    
    unmatched <- df %>%
      filter(
        is.na(speed_match) | !speed_match
      )
    
    p <- plot_ly()
    
    # Gray states that don't match
    if (nrow(unmatched) > 0) {
      
      p <- p %>%
        add_trace(
          type = "choropleth",
          locationmode = "USA-states",
          locations = unmatched$STATE,
          z = rep(1, nrow(unmatched)),
          
          colorscale = list(
            c(0, "#D3D3D3"),
            c(1, "#D3D3D3")
          ),
          
          showscale = FALSE,
          text = unmatched$hover_text,
          hoverinfo = "text",
          
          marker = list(
            line = list(
              color = "white",
              width = 0.7
            )
          ),
          
          inherit = FALSE
        )
    }
    
    # states colored by number of fatalities
    if (nrow(matched) > 0) {
      
      p <- p %>%
        add_trace(
          type = "choropleth",
          locationmode = "USA-states",
          locations = matched$STATE,
          z = matched$FATAL_CAR_CRASHES,
          
          zmin = rate_range[1],
          zmax = rate_range[2],
          
          # Turquoise to deep purple
          colorscale = list(
            c(0, "#42BDD5"),
            c(0.25, "#168ACB"),
            c(0.5, "#3454B4"),
            c(0.75, "#7042A5"),
            c(1, "#35146B")
          ),
          
          colorbar = list(
            title = "Fatality Value"
          ),
          
          text = matched$hover_text,
          hoverinfo = "text",
          
          marker = list(
            line = list(
              color = "white",
              width = 0.7
            )
          ),
          
          inherit = FALSE
        )
    }
    
    # map layout
    p %>%
      layout(
        geo = list(
          scope = "usa",
          projection = list(
            type = "albers usa"
          ),
          showlakes = FALSE,
          bgcolor = "white"
        ),
        
        margin = list(
          l = 0,
          r = 0,
          t = 10,
          b = 0
        )
      )
    
  })
  
}

# run the app

shinyApp(
  ui = ui,
  server = server
)
