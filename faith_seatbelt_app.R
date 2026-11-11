# =====================================
# SEATBELT LAWS AND TRAFFIC FATALITIES
# =====================================

library(shiny)
library(readxl)
library(dplyr)
library(plotly)

# -----------
# LOAD DATA
# -----------

traffic_data <- read_excel(
  "2024StateTrafficLawsSummary.xlsx"
)

traffic_data <- traffic_data %>%
  mutate(
    STATE = toupper(trimws(as.character(STATE))),
    STATE_NAME = as.character(STATE_NAME),
    SEATBELT_FS = trimws(as.character(SEATBELT_FS)),
    SEATBELT_RS = trimws(as.character(SEATBELT_RS)),
    FATAL_CAR_CRASHES = as.numeric(FATAL_CAR_CRASHES)
  )

# Consistent color scale
rate_range <- range(
  traffic_data$FATAL_CAR_CRASHES,
  na.rm = TRUE
)

# ---------------
# USER INTERFACE
# ---------------

ui <- fluidPage(
  
  titlePanel("Seatbelt Laws and Traffic Fatalities"),

  
  # Sidebar on left, map on right
  sidebarLayout(
    
    sidebarPanel(
      
      # Seatbelt law dropdown
      selectInput(
        inputId = "seatbelt_law",
        label = "Select Seatbelt Law:",
        choices = c(
          "Front Seat - All" = "FS_All",
          "Front Seat - Primary" = "FS_Primary",
          "Front Seat - Secondary" = "FS_Secondary",
          "Front Seat - None" = "FS_None",
          "Rear Seat - All" = "RS_All",
          "Rear Seat - Primary" = "RS_Primary",
          "Rear Seat - Secondary" = "RS_Secondary",
          "Rear Seat - None" = "RS_None"
        ),
        selected = "FS_All",
        width = "100%"
      ),
      
      # Collapsible explanation
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
          
          "What do the laws mean?"
        ),
        
        tags$div(
          style = "padding: 10px;",
          
          tags$p(
            tags$b("Primary: "),
            "Police can pull you over just for not wearing a seatbelt."
          ),
          
          tags$p(
            tags$b("Secondary: "),
            "Police can ticket you for not wearing a seatbelt after stopping you for another violation."
          ),
          
          tags$p(
            tags$b("None: "),
            "No applicable adult seatbelt law."
          )
        )
      ),
      
      width = 3
    ),
    
    # Map on the right
    mainPanel(
      
      plotlyOutput(
        "seatbelt_map",
        height = "650px"
      ),
      
      width = 9
    )
    
  )
)

# -------
# SERVER
# -------

server <- function(input, output, session) {
  
  seatbelt_data <- reactive({
    
    parts <- strsplit(
      input$seatbelt_law, "_"
    )[[1]]
    
    seat <- parts[1]
    enforcement <- parts[2]
    
    law_column <- if (seat == "FS") {
      "SEATBELT_FS"
    } else {
      "SEATBELT_RS"
    }
    
    traffic_data %>%
      mutate(
        seatbelt_match = if (enforcement == "All") {
          TRUE
        } else {
          .data[[law_column]] == enforcement
        },
        
        hover_text = paste0(
          "<b>", STATE_NAME, "</b>",
          "<br>Front Seat Law: ", SEATBELT_FS,
          "<br>Rear Seat Law: ", SEATBELT_RS,
          "<br>Fatality Value: ",
          round(FATAL_CAR_CRASHES, 4)
        )
      )
  })
  
  # -------------------
  # INTERACTIVE US MAP
  # -------------------
  
  output$seatbelt_map <- renderPlotly({
    
    df <- seatbelt_data()
    
    matched <- df %>%
      filter(seatbelt_match %in% TRUE)
    
    unmatched <- df %>%
      filter(
        is.na(seatbelt_match) | !seatbelt_match
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
    
    # Matching states colored by fatalities
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
    
    # Map layout
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

# --------
# RUN APP
# --------

shinyApp(
  ui = ui,
  server = server
)
