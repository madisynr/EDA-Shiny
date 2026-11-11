############################################
#
# Minimum Driving Age and Fatal Crashes
# Group Shiny App
# Celia Kucker
# November 11th, 2026
#
###########################################

library(shiny)
library(readxl)
library(dplyr)
library(bslib)
library(plotly)

# import dataset

traffic_data <- read_excel(
  "2024StateTrafficLawsSummary(1).xlsx"
) %>%
  mutate(
    STATE = toupper(trimws(as.character(STATE))),
    STATE_NAME = as.character(STATE_NAME),
    MIN_DRIVING_AGE = as.numeric(MIN_DRIVING_AGE),
    FATAL_CAR_CRASHES = as.numeric(FATAL_CAR_CRASHES),
    MINOR_FATALITY_RATE = as.numeric(MINOR_FATALITY_RATE),
    AGE_GROUP = case_when(
      MIN_DRIVING_AGE < 15   ~ "Under 15",
      MIN_DRIVING_AGE == 15  ~ "15",
      MIN_DRIVING_AGE < 16   ~ "15 to 16",
      MIN_DRIVING_AGE == 16  ~ "16",
      TRUE                   ~ "Other"
    )
  )

age_range <- range(
  traffic_data$MIN_DRIVING_AGE, na.rm = TRUE)

# establish UI

ui <- fluidPage(
  
  titlePanel("Minimum Driving Age by State"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # drop down selection panel
      selectInput(
        inputId = "age_filter",
        label = "Select Minimum Driving Age Group:",
        choices = c(
          "All States"        = "All",
          "Under 15"          = "Under 15",
          "Exactly 15"        = "15",
          "Between 15 and 16" = "15 to 16",
          "Exactly 16"        = "16"
        ),
        selected = "All",
        width = "100%"
      ),
      
      # radio button to toggle fatality rate type
      radioButtons(
        inputId = "rate_type",
        label = "Select Fatality Rate:",
        choices = c(
          "Overall Fatality Rate" = "overall",
          "Minor Fatality Rate"   = "minor"
        ),
        selected = "overall"
      ),
      
      # explanation of age groups
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
          "What do the age groups mean?"
        ),
        
        tags$div(
          style = "padding: 10px;",
          
          tags$p(
            tags$b("Under 15: "),
            "States where a learner's permit or restricted license can be obtained before age 15."
          ),
          
          tags$p(
            tags$b("Exactly 15: "),
            "States where a learner's permit or restricted license can be obtained at age 15."
          ),
          
          tags$p(
            tags$b("Between 15 and 16:"),
            "States with a fractional minimum age between 15 and 16 (e.g. 15 and 6 months)."
          ),
          
          tags$p(
            tags$b("Exactly 16: "),
            "States where a learner's permit or restricted license requires the driver to be at least 16."
          )
        )
      ),
      
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
          "About the Minor Fatality Rate"
        ),
        
        tags$div(
          style = "padding: 10px;",
          
          tags$p(
            tags$b("Minor Fatality Rate: "),
            "The number of fatal car crashes involving drivers under the age of 18,
            per 100,000 minor residents in that state."
          ),
          
          tags$p(
            tags$b("Minor Population Source: "),
            "HDPulse: An Ecosystem of Health Disparities and Specific Populations Health Resources. National Institute on Minority Health and Health Disparities. Created 11/11/2026. Available from https://hdpulse.nimhd.nih.gov"
          )
        )
      ),
      
      width = 3
    ),
    
    
    
    # create map
    mainPanel(
      
      plotlyOutput(
        "age_map",
        height = "650px"
      ),
      
      width = 9
    )
    
  )
)

# create server

server <- function(input, output, session) {
  
  map_data <- reactive({
    
    rate_col <- if (input$rate_type == "minor") {
      "MINOR_FATALITY_RATE"
    } else {
      "FATAL_CAR_CRASHES"
    }
    
    traffic_data %>%
      mutate(
        age_match = if (input$age_filter == "All") {
          TRUE
        } else {
          AGE_GROUP == input$age_filter
        },
        
        selected_rate = .data[[rate_col]],
        
        # only log-transform for minor fatality rate
        plot_value = if (input$rate_type == "minor") {
          log1p(.data[[rate_col]])
        } else {
          .data[[rate_col]]
        },
        
        hover_text = paste0(
          "<b>", STATE_NAME, "</b>",
          "<br>Minimum Driving Age: ", MIN_DRIVING_AGE,
          "<br>Fatality Value: ",
          round(.data[[rate_col]], 4)
        )
      )
  })
  
  # make map interactive
  
  output$age_map <- renderPlotly({
    
    df <- map_data()
    
    rate_col <- if (input$rate_type == "minor") {
      "MINOR_FATALITY_RATE"
    } else {
      "FATAL_CAR_CRASHES"
    }
    
    colorbar_title <- if (input$rate_type == "minor") {
      "Minor Fatality Rate"
    } else {
      "Fatality Rate"
    }
    
    # set scale and colorbar depending on rate type
    if (input$rate_type == "minor") {
      rate_range <- log1p(
        range(traffic_data$MINOR_FATALITY_RATE, na.rm = TRUE)
      )
      tick_vals  <- log1p(c(1, 5, 10, 50, 100, 500))
      tick_text  <- c("1", "5", "10", "50", "100", "500")
      colorbar   <- list(
        title    = colorbar_title,
        tickvals = tick_vals,
        ticktext = tick_text
      )
    } else {
      rate_range <- range(
        traffic_data$FATAL_CAR_CRASHES, na.rm = TRUE
      )
      colorbar <- list(
        title = colorbar_title
      )
    }
    
    matched <- df %>%
      filter(age_match %in% TRUE)
    
    unmatched <- df %>%
      filter(is.na(age_match) | !age_match)
    
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
    
    # states colored by selected fatality rate
    if (nrow(matched) > 0) {
      
      p <- p %>%
        add_trace(
          type = "choropleth",
          locationmode = "USA-states",
          locations = matched$STATE,
          z = matched$plot_value,
          
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
          
          colorbar = colorbar,
          
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