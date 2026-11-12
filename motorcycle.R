
# =====================================
# MOTORCYCLE SAFETY AND FATAL CRASHES
# =====================================

library(shiny)
library(readxl)
library(dplyr)
library(plotly)

# -----------
# LOAD DATA
# -----------

traffic_data <- read_excel(file.choose())

# -----------
# CLEAN DATA
# -----------

traffic_data <- traffic_data %>%
  mutate(
    STATE = toupper(trimws(as.character(STATE))),
    STATE_NAME = as.character(STATE_NAME),
    FATAL_MOTORCYCLE_CRASHES =
      as.numeric(FATAL_MOTORCYCLE_CRASHES),
    
    HELMET_REQUIREMENT = as.character(HELMET_REQUIREMENT),
    
    HELMET_REQUIREMENT = ifelse(
      is.na(HELMET_REQUIREMENT),
      NA_character_,
      gsub(
        "\\s+",
        " ",
        trimws(HELMET_REQUIREMENT)
      )
    ),
    
    HELMET_GROUP = case_when(
      is.na(HELMET_REQUIREMENT) ~ "Missing",
      HELMET_REQUIREMENT == "All" ~ "Universal",
      grepl("^Under [0-9]+$", HELMET_REQUIREMENT) ~ "Age-Based",
      TRUE ~ "Other"
    )
  )

# Consistent color scale

value_range <- range(
  traffic_data$FATAL_MOTORCYCLE_CRASHES,
  na.rm = TRUE
)

# -----------
# CHECK DATA
# -----------

summary(traffic_data$FATAL_MOTORCYCLE_CRASHES)

sum(is.na(traffic_data$HELMET_REQUIREMENT))

# -----------
# USER INTERFACE
# -----------

ui <- fluidPage(
  
  titlePanel("Motorcycle Safety and Fatal Crash Analysis"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # Helmet law dropdown
      
      selectInput(
        inputId = "helmet_law",
        label = "Select Helmet Law:",
        choices = c(
          "All States" = "All",
          "Universal Helmet Law" = "Universal",
          "Age-Based Requirement" = "Age-Based"
        ),
        selected = "All",
        width = "100%"
      ),
      
      # Helmet law explanation
      
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
          
          "Understanding Helmet Laws"
        ),
        
        tags$div(
          
          style = "padding: 10px;",
          
          tags$p(
            tags$b("Universal: "),
            "Helmet required for all motorcycle riders."
          ),
          
          tags$p(
            tags$b("Age-Based: "),
            "Helmet required for riders below a specified age."
          )
        )
      ),
      
      width = 3
    ),
    
    # Main panel
    
    mainPanel(
      
      h3("1. Motorcycle Crash Values Across the USA"),
      
      plotlyOutput(
        "motorcycle_map",
        height = "600px"
      ),
      
      tags$hr(),
      
      h3("2. Top 10 States by Motorcycle Crash Value"),
      
      plotlyOutput(
        "top_states",
        height = "500px"
      ),
      
      width = 9
    )
  )
)

# -------
# SERVER
# -------

server <- function(input, output, session) {
  
  # Filter data
  
  motorcycle_data <- reactive({
    
    if (input$helmet_law == "All") {
      
      traffic_data
      
    } else {
      
      traffic_data %>%
        filter(HELMET_GROUP == input$helmet_law)
      
    }
    
  })
  
  # -------------------
  # GRAPH 1: US MAP
  # -------------------
  
  output$motorcycle_map <- renderPlotly({
    
    df <- motorcycle_data()
    
    matched <- df %>%
      filter(!is.na(FATAL_MOTORCYCLE_CRASHES))
    
    unmatched <- traffic_data %>%
      filter(!STATE %in% matched$STATE)
    
    p <- plot_ly()
    
    # Gray states that do not match
    
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
          
          text = paste0(
            "<b>", unmatched$STATE_NAME, "</b>",
            "<br>Helmet Law: ",
            ifelse(
              is.na(unmatched$HELMET_REQUIREMENT),
              "Not Available",
              unmatched$HELMET_REQUIREMENT
            )
          ),
          
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
    
    # Matching states colored by crash values
    
    if (nrow(matched) > 0) {
      
      p <- p %>%
        add_trace(
          type = "choropleth",
          locationmode = "USA-states",
          locations = matched$STATE,
          z = matched$FATAL_MOTORCYCLE_CRASHES,
          
          zmin = value_range[1],
          zmax = value_range[2],
          
          colorscale = list(
            c(0, "#42BDD5"),
            c(0.25, "#168ACB"),
            c(0.5, "#3454B4"),
            c(0.75, "#7042A5"),
            c(1, "#35146B")
          ),
          
          colorbar = list(
            title = "Crash Value"
          ),
          
          text = paste0(
            "<b>", matched$STATE_NAME, "</b>",
            "<br>Helmet Law: ",
            ifelse(
              is.na(matched$HELMET_REQUIREMENT),
              "Not Available",
              matched$HELMET_REQUIREMENT
            ),
            "<br>Motorcycle Crash Value: ",
            round(matched$FATAL_MOTORCYCLE_CRASHES, 4)
          ),
          
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
  
  # -------------------
  # GRAPH 2: TOP 10 STATES
  # -------------------
  
  output$top_states <- renderPlotly({
    
    top10 <- motorcycle_data() %>%
      
      filter(
        !is.na(FATAL_MOTORCYCLE_CRASHES)
      ) %>%
      
      arrange(
        desc(FATAL_MOTORCYCLE_CRASHES)
      ) %>%
      
      slice_head(n = 10) %>%
      
      mutate(
        STATE_NAME = factor(
          STATE_NAME,
          levels = rev(STATE_NAME)
        )
      )
    
    validate(
      need(
        nrow(top10) > 0,
        "No states available for this selection."
      )
    )
    
    plot_ly(
      data = top10,
      x = ~FATAL_MOTORCYCLE_CRASHES,
      y = ~STATE_NAME,
      type = "bar",
      orientation = "h",
      
      text = ~sprintf(
        "%.4f",
        FATAL_MOTORCYCLE_CRASHES
      ),
      
      textposition = "outside",
      
      marker = list(
        color = "#3454B4"
      ),
      
      hovertemplate = paste(
        "<b>%{y}</b>",
        "<br>Crash Value: %{x:.4f}",
        "<extra></extra>"
      )
      
    ) %>%
      
      layout(
        xaxis = list(
          title = "Motorcycle Crash Value",
          range = c(
            0,
            max(top10$FATAL_MOTORCYCLE_CRASHES) * 1.12
          ),
          tickformat = ".2f"
        ),
        
        yaxis = list(
          title = ""
        ),
        
        margin = list(
          l = 120,
          r = 60,
          t = 20,
          b = 60
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

