############################################
#
# Distracted Driving Laws (Phones) and Fatal Crashes
# Group Shiny App
# Madisyn Rust
# November 11th, 2026
#
###########################################

library(shiny)
library(readxl)
library(dplyr)
library(bslib)
library(plotly)

# import dataset containing all laws and populations
# clean the data and select distracted driving columns

traffic_data <- read_excel(
  "2024StateTrafficLawsSummary.xlsx"
) %>%
  mutate(
    STATE = toupper(trimws(as.character(STATE))),
    STATE_NAME = as.character(STATE_NAME),
    HANDHELD_PHONE_BAN = trimws(as.character(HANDHELD_PHONE_BAN)),
    TEXTING_BAN = trimws(as.character(TEXTING_BAN)),
    FATAL_CAR_CRASHES = as.numeric(FATAL_CAR_CRASHES)
  )

rate_range <- range(
  traffic_data$FATAL_CAR_CRASHES, na.rm = TRUE)

# establish UI

ui <- fluidPage(
  
  titlePanel("Distracted Driving Laws and Fatal traffic_data"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # drop down selection panel
      selectInput(
        inputId = "phone_laws",
        label = "Select Distracted Driving Law:",
        choices = c(
          "Handheld Phone Ban - All" = "HP_All",
          "Handheld Phone Ban - Yes" = "HP_Yes",
          "Handheld Phone Ban - No" = "HP_No",
          "Texting Ban - All" = "TB_All",
          "Texting Ban - Primary" = "TB_Primary",
          "Texting Ban - Secondary" = "TB_Secondary",
          "Texting Ban - None" = "TB_None"
        ),
        selected = "HP_All",
        width = "100%"
      ),
      
      # explanation of each law
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
            tags$b("Yes (Handheld Phone Ban): "),
            "This state has a ban on handheld phones while driving."
          ),
          
          tags$p(
            tags$b("No (Handheld Ban): "),
            "This state does not have a ban on handheld phones while driving."
          ),
          
          tags$p(
            tags$b("Primary: "),
            "Police can pull you over for texting while driving."
          ),
          
          tags$p(
            tags$b("Secondary: "),
            "Police can write you a ticket for texting after stopping you for another violation."
          ),
          
          tags$p(
            tags$b("None: "),
            "No law on texting while driving."
          )
        )
      ),
      
      width = 3
    ),
    
    # create map
    mainPanel(
      
      plotlyOutput(
        "phone_ban_map",
        height = "650px"
      ),
      
      width = 9
    )
    
  )
)

# create server

server <- function(input, output, session) {
  
  phone_data <- reactive({
    
    parts <- strsplit(
      input$phone_laws, "_"
    )[[1]]
    
    law_type <- parts[1]
    enforcement <- parts[2]
    
    law_column <- if (law_type == "HP") {
      "HANDHELD_PHONE_BAN"
    } else {
      "TEXTING_BAN"
    }
    
    traffic_data %>%
      mutate(
        phone_match = if (enforcement == "All") {
          TRUE
        } else {
          .data[[law_column]] == enforcement
        },
        
        hover_text = paste0(
          "<b>", STATE_NAME, "</b>",
          "<br>Handheld Phone Ban: ", HANDHELD_PHONE_BAN,
          "<br>Texting Ban: ", TEXTING_BAN,
          "<br>Fatality Value: ",
          round(FATAL_CAR_CRASHES, 4)
        )
      )
  })
  
  # make map interactive
  
  output$phone_ban_map <- renderPlotly({
    
    df <- phone_data()
    
    matched <- df %>%
      filter(phone_match %in% TRUE)
    
    unmatched <- df %>%
      filter(
        is.na(phone_match) | !phone_match
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