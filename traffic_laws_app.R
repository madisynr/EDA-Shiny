############################################
#
# 2024 State Traffic Laws & Fatal Crashes
# Combined Group Shiny App
# Members: Celia, Faith, Madisyn,
#          Sydney, Ashmi, Maddison
# November 11th, 2026
#
############################################

library(shiny)
library(readxl)
library(dplyr)
library(plotly)
library(bslib)

# ============================================================
# LOAD & CLEAN DATA
# ============================================================

traffic_data <- read_excel("2024StateTrafficLawsSummary.xlsx") %>%
  mutate(
    STATE      = toupper(trimws(as.character(STATE))),
    STATE_NAME = as.character(STATE_NAME),

    # Numeric columns
    FATAL_CAR_CRASHES        = as.numeric(FATAL_CAR_CRASHES),
    FATAL_MOTORCYCLE_CRASHES = as.numeric(FATAL_MOTORCYCLE_CRASHES),
    MAX_SPEED_LIMIT_MPH      = as.numeric(MAX_SPEED_LIMIT_MPH),
    MIN_DRIVING_AGE          = as.numeric(MIN_DRIVING_AGE),
    MINOR_FATALITY_RATE      = as.numeric(MINOR_FATALITY_RATE),

    # Character columns
    SEATBELT_FS        = trimws(as.character(SEATBELT_FS)),
    SEATBELT_RS        = trimws(as.character(SEATBELT_RS)),
    HANDHELD_PHONE_BAN = trimws(as.character(HANDHELD_PHONE_BAN)),
    TEXTING_BAN        = trimws(as.character(TEXTING_BAN)),
    HELMET_REQUIREMENT = trimws(gsub("\\s+", " ", as.character(HELMET_REQUIREMENT))),

    # Derived groupings
    AGE_GROUP = case_when(
      MIN_DRIVING_AGE < 15  ~ "Under 15",
      MIN_DRIVING_AGE == 15 ~ "15",
      MIN_DRIVING_AGE < 16  ~ "15 to 16",
      MIN_DRIVING_AGE == 16 ~ "16",
      TRUE                  ~ "Other"
    ),

    SPEED_GROUP = case_when(
      MAX_SPEED_LIMIT_MPH < 65  ~ "Under 65",
      MAX_SPEED_LIMIT_MPH == 65 ~ "65",
      MAX_SPEED_LIMIT_MPH == 70 ~ "70",
      MAX_SPEED_LIMIT_MPH == 75 ~ "75",
      MAX_SPEED_LIMIT_MPH >= 80 ~ "80 or Higher",
      TRUE                      ~ "Other"
    ),

    HELMET_GROUP = case_when(
      is.na(HELMET_REQUIREMENT)                   ~ "Missing",
      HELMET_REQUIREMENT == "All"                 ~ "Universal",
      grepl("^Under [0-9]+$", HELMET_REQUIREMENT) ~ "Age-Based",
      TRUE                                        ~ "Other"
    )
  )

# Pre-compute ranges used across tabs
car_crash_range  <- range(traffic_data$FATAL_CAR_CRASHES,        na.rm = TRUE)
moto_crash_range <- range(traffic_data$FATAL_MOTORCYCLE_CRASHES, na.rm = TRUE)

# ============================================================
# SHARED HELPERS
# ============================================================

# Consistent turquoise → deep-purple color scale used in every map
APP_COLORSCALE <- list(
  c(0,    "#42BDD5"),
  c(0.25, "#168ACB"),
  c(0.5,  "#3454B4"),
  c(0.75, "#7042A5"),
  c(1,    "#35146B")
)

# Standard map geo layout
map_geo <- list(
  scope      = "usa",
  projection = list(type = "albers usa"),
  showlakes  = FALSE,
  bgcolor    = "white"
)

map_margin <- list(l = 0, r = 0, t = 10, b = 0)

# Build a two-layer choropleth (gray unmatched, colored matched)
make_choropleth <- function(matched, unmatched,
                            z_col, z_range,
                            colorbar_title,
                            hover_text_matched,
                            hover_text_unmatched,
                            log_scale = FALSE) {

  z_vals  <- matched[[z_col]]
  z_min   <- if (log_scale) log1p(z_range[1]) else z_range[1]
  z_max   <- if (log_scale) log1p(z_range[2]) else z_range[2]
  z_plot  <- if (log_scale) log1p(z_vals)     else z_vals

  p <- plot_ly()

  if (nrow(unmatched) > 0) {
    p <- p %>%
      add_trace(
        type         = "choropleth",
        locationmode = "USA-states",
        locations    = unmatched$STATE,
        z            = rep(1, nrow(unmatched)),
        colorscale   = list(c(0, "#D3D3D3"), c(1, "#D3D3D3")),
        showscale    = FALSE,
        text         = hover_text_unmatched,
        hoverinfo    = "text",
        marker       = list(line = list(color = "white", width = 0.7)),
        inherit      = FALSE
      )
  }

  if (nrow(matched) > 0) {
    cb <- list(title = colorbar_title)
    if (log_scale) {
      cb$tickvals <- log1p(c(1, 5, 10, 50, 100, 500))
      cb$ticktext <- c("1", "5", "10", "50", "100", "500")
    }

    p <- p %>%
      add_trace(
        type         = "choropleth",
        locationmode = "USA-states",
        locations    = matched$STATE,
        z            = z_plot,
        zmin         = z_min,
        zmax         = z_max,
        colorscale   = APP_COLORSCALE,
        colorbar     = cb,
        text         = hover_text_matched,
        hoverinfo    = "text",
        marker       = list(line = list(color = "white", width = 0.7)),
        inherit      = FALSE
      )
  }

  p %>% layout(geo = map_geo, margin = map_margin)
}

# Collapsible sidebar accordion panel
info_box <- function(summary_text, ...) {
  tags$details(
    style = "margin-top: 15px;",
    tags$summary(
      style = paste0(
        "cursor:pointer; font-weight:bold; padding:10px;",
        "background-color:#E8EEF5; border-radius:6px;"
      ),
      summary_text
    ),
    tags$div(style = "padding:10px;", ...)
  )
}

# ============================================================
# UI
# ============================================================

ui <- page_navbar(
  title = div(
    tags$b("2024 U.S. Traffic Laws & Fatal Crashes"),
    style = "font-size:1.1rem;"
  ),

  theme = bs_theme(
    version    = 5,
    bootswatch = "flatly",
    primary    = "#3454B4",
    font_scale = 0.95
  ),

  # ── Global CSS tweaks ──────────────────────────────────────
  header = tags$head(tags$style(HTML("
    .nav-link.active { font-weight: bold !important; }
    .sidebar-panel   { background: #f8f9fa; border-radius: 8px;
                       padding: 16px; height: 100%; }
    .section-title   { color: #3454B4; font-weight: 600;
                       margin: 18px 0 6px; }
    hr.tab-divider   { border-top: 2px solid #dee2e6; margin: 20px 0; }
    .stat-badge      { background: #E8EEF5; border-radius: 8px;
                       padding: 10px 14px; margin: 6px 0;
                       font-size: 0.88rem; }
    .stat-badge b    { color: #3454B4; }
  "))),

  # ------------------------------------------------------------------
  # TAB 0 — OVERVIEW / ABOUT
  # ------------------------------------------------------------------
  nav_panel(
    title = tagList(icon("house"), "Overview"),

    fluidRow(
      column(12,
        div(
          style = paste0(
            "max-width:860px; margin:40px auto; padding:30px;",
            "background:#fff; border-radius:12px;",
            "box-shadow:0 2px 12px rgba(0,0,0,.08);"
          ),

          h2("2024 U.S. Traffic Laws & Fatal Crashes",
             style = "color:#3454B4; margin-bottom:6px;"),

          tags$p(
            style = "color:#555; font-size:1.05rem;",
            "Exploring how state traffic laws relate to fatal crash rates",
            " (crashes per 100,000 people) across all 50 U.S. states.",
            " Use the tabs above to dig into each topic."
          ),

          tags$hr(),

          fluidRow(
            column(4,
              div(class = "stat-badge",
                tags$b("🚗 Driving Age"),
                tags$br(), "Min. age to drive vs. fatal crash rates."
              )
            ),
            column(4,
              div(class = "stat-badge",
                tags$b("🪑 Seatbelt Laws"),
                tags$br(), "Front & rear seat enforcement levels."
              )
            ),
            column(4,
              div(class = "stat-badge",
                tags$b("📱 Distracted Driving"),
                tags$br(), "Handheld phone & texting bans."
              )
            )
          ),

          fluidRow(
            style = "margin-top:8px;",
            column(4,
              div(class = "stat-badge",
                tags$b("🏎 Speed Limits"),
                tags$br(), "Max posted speed vs. fatal crashes."
              )
            ),
            column(4,
              div(class = "stat-badge",
                tags$b("🏍 Motorcycle Safety"),
                tags$br(), "Helmet law type vs. moto crash rates."
              )
            ),
            column(4,
              div(class = "stat-badge",
                tags$b("📊 Top 10 Rankings"),
                tags$br(), "Highest-crash states in every category."
              )
            )
          ),

          tags$hr(),
          tags$p(
            style = "color:#888; font-size:0.85rem;",
            "Created November 2026 · ",
            "Celia · Faith · Madisyn · Sydney · Ashmi · Maddison"
          )
        )
      )
    )
  ),

  # ------------------------------------------------------------------
  # TAB 1 — MINIMUM DRIVING AGE (Celia)
  # ------------------------------------------------------------------
  nav_panel(
    title = "Driving Age",

    sidebarLayout(
      sidebarPanel(
        width = 3,

        div(class = "sidebar-panel",

          h5(class = "section-title", "Minimum Driving Age"),

          selectInput(
            inputId  = "age_filter",
            label    = "Filter by Age Group:",
            choices  = c(
              "All States"        = "All",
              "Under 15"          = "Under 15",
              "Exactly 15"        = "15",
              "Between 15 and 16" = "15 to 16",
              "Exactly 16"        = "16"
            ),
            selected = "All",
            width    = "100%"
          ),

          radioButtons(
            inputId  = "rate_type",
            label    = "Fatality Metric:",
            choices  = c(
              "Overall Fatal Car Crashes" = "overall",
              "Minor Fatality Rate"       = "minor"
            ),
            selected = "overall"
          ),

          info_box(
            "What do the age groups mean?",
            tags$p(tags$b("Under 15: "), "Permit obtainable before age 15."),
            tags$p(tags$b("Exactly 15: "), "Permit obtainable at age 15."),
            tags$p(tags$b("Between 15 and 16: "), "Fractional minimum age (e.g., 15 and 6 months)."),
            tags$p(tags$b("Exactly 16: "), "Permit requires the driver to be at least 16.")
          ),

          info_box(
            "About the Minor Fatality Rate",
            tags$p(tags$b("Definition: "), "Fatal car crashes involving drivers under 18, per 100,000 minor residents."),
            tags$p(tags$b("Source: "), "HDPulse, National Institute on Minority Health and Health Disparities.")
          )
        )
      ),

      mainPanel(
        width = 9,
        h4("Fatal Crashes by Minimum Driving Age", class = "section-title", style = "margin-top:10px;"),
        plotlyOutput("age_map", height = "480px"),
        tags$hr(class = "tab-divider"),
        h4("Top 10 States by Fatal Car Crashes", class = "section-title"),
        plotlyOutput("age_top10", height = "400px")
      )
    )
  ),

  # ------------------------------------------------------------------
  # TAB 2 — SEATBELT LAWS (Faith)
  # ------------------------------------------------------------------
  nav_panel(
    title = "Seatbelt Laws",

    sidebarLayout(
      sidebarPanel(
        width = 3,

        div(class = "sidebar-panel",

          h5(class = "section-title", "Seatbelt Law Filter"),

          selectInput(
            inputId  = "seatbelt_law",
            label    = "Select Seatbelt Law:",
            choices  = c(
              "Front Seat — All"       = "FS_All",
              "Front Seat — Primary"   = "FS_Primary",
              "Front Seat — Secondary" = "FS_Secondary",
              "Front Seat — None"      = "FS_None",
              "Rear Seat — All"        = "RS_All",
              "Rear Seat — Primary"    = "RS_Primary",
              "Rear Seat — Secondary"  = "RS_Secondary",
              "Rear Seat — None"       = "RS_None"
            ),
            selected = "FS_All",
            width    = "100%"
          ),

          info_box(
            "What do the enforcement levels mean?",
            tags$p(tags$b("Primary: "), "Officers can stop a driver solely for not wearing a seatbelt."),
            tags$p(tags$b("Secondary: "), "A seatbelt ticket can only be issued after a stop for another violation."),
            tags$p(tags$b("None: "), "No applicable adult seatbelt law for that seat position.")
          )
        )
      ),

      mainPanel(
        width = 9,
        h4("Fatal Crashes by Seatbelt Law", class = "section-title", style = "margin-top:10px;"),
        plotlyOutput("seatbelt_map", height = "480px"),
        tags$hr(class = "tab-divider"),
        h4("Top 10 States by Fatal Car Crashes", class = "section-title"),
        plotlyOutput("seatbelt_top10", height = "400px")
      )
    )
  ),

  # ------------------------------------------------------------------
  # TAB 3 — DISTRACTED DRIVING (Madisyn)
  # ------------------------------------------------------------------
  nav_panel(
    title = "Distracted Driving",

    sidebarLayout(
      sidebarPanel(
        width = 3,

        div(class = "sidebar-panel",

          h5(class = "section-title", "Phone Law Filter"),

          selectInput(
            inputId  = "phone_laws",
            label    = "Select Distracted Driving Law:",
            choices  = c(
              "Handheld Phone Ban — All"      = "HP_All",
              "Handheld Phone Ban — Yes"      = "HP_Yes",
              "Handheld Phone Ban — No"       = "HP_No",
              "Texting Ban — All"             = "TB_All",
              "Texting Ban — Primary"         = "TB_Primary",
              "Texting Ban — Secondary"       = "TB_Secondary",
              "Texting Ban — None"            = "TB_None"
            ),
            selected = "HP_All",
            width    = "100%"
          ),

          info_box(
            "What do the laws mean?",
            tags$p(tags$b("Handheld Ban — Yes: "), "State prohibits handheld phone use while driving."),
            tags$p(tags$b("Handheld Ban — No: "), "No statewide handheld ban exists."),
            tags$p(tags$b("Texting — Primary: "), "Officers can pull you over for texting alone."),
            tags$p(tags$b("Texting — Secondary: "), "Ticket for texting only after a stop for another violation."),
            tags$p(tags$b("Texting — None: "), "No statewide texting-while-driving law.")
          )
        )
      ),

      mainPanel(
        width = 9,
        h4("Fatal Crashes by Distracted Driving Law", class = "section-title", style = "margin-top:10px;"),
        plotlyOutput("phone_ban_map", height = "480px"),
        tags$hr(class = "tab-divider"),
        h4("Top 10 States by Fatal Car Crashes", class = "section-title"),
        plotlyOutput("phone_top10", height = "400px")
      )
    )
  ),

  # ------------------------------------------------------------------
  # TAB 4 — SPEED LIMITS (Sydney)
  # ------------------------------------------------------------------
  nav_panel(
    title = "Speed Limits",

    sidebarLayout(
      sidebarPanel(
        width = 3,

        div(class = "sidebar-panel",

          h5(class = "section-title", "Speed Limit Filter"),

          selectInput(
            inputId  = "speed_filter",
            label    = "Select Maximum Speed Limit:",
            choices  = c(
              "All States"       = "All",
              "Under 65 MPH"     = "Under 65",
              "65 MPH"           = "65",
              "70 MPH"           = "70",
              "75 MPH"           = "75",
              "80 MPH or Higher" = "80 or Higher"
            ),
            selected = "All",
            width    = "100%"
          ),

          info_box(
            "About the Speed Limit Groups",
            tags$p(tags$b("Maximum Speed Limit: "), "The highest posted legal speed limit within a state."),
            tags$p(tags$b("Color scale: "), "Lighter turquoise = fewer fatal crashes; darker purple = more fatal crashes."),
            tags$p(tags$b("Gray states: "), "Do not match the selected speed limit category.")
          )
        )
      ),

      mainPanel(
        width = 9,
        h4("Fatal Crashes by Maximum Speed Limit", class = "section-title", style = "margin-top:10px;"),
        plotlyOutput("speed_map", height = "480px"),
        tags$hr(class = "tab-divider"),
        h4("Top 10 States by Fatal Car Crashes", class = "section-title"),
        plotlyOutput("speed_top10", height = "400px")
      )
    )
  ),

  # ------------------------------------------------------------------
  # TAB 5 — MOTORCYCLE SAFETY
  # ------------------------------------------------------------------
  nav_panel(
    title = "Motorcycle Safety",

    sidebarLayout(
      sidebarPanel(
        width = 3,

        div(class = "sidebar-panel",

          h5(class = "section-title", "Helmet Law Filter"),

          selectInput(
            inputId  = "helmet_law",
            label    = "Select Helmet Law:",
            choices  = c(
              "All States"             = "All",
              "Universal Helmet Law"   = "Universal",
              "Age-Based Requirement"  = "Age-Based"
            ),
            selected = "All",
            width    = "100%"
          ),

          info_box(
            "Understanding Helmet Laws",
            tags$p(tags$b("Universal: "), "Helmets required for ALL motorcycle riders, regardless of age."),
            tags$p(tags$b("Age-Based: "), "Helmets required only for riders below a specified age threshold.")
          )
        )
      ),

      mainPanel(
        width = 9,

        h4("Fatal Motorcycle Crashes by Helmet Law",
           class = "section-title", style = "margin-top:10px;"),

        plotlyOutput("motorcycle_map", height = "480px"),

        tags$hr(class = "tab-divider"),

        h4("Top 10 States by Fatal Motorcycle Crashes",
           class = "section-title"),

        plotlyOutput("top_states_bar", height = "420px")
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {

  # Shared helper: horizontal top-10 bar chart for car crashes
  make_top10_bar <- function(df, x_col, x_label) {
    top10 <- df %>%
      filter(!is.na(.data[[x_col]])) %>%
      arrange(desc(.data[[x_col]])) %>%
      slice_head(n = 10) %>%
      mutate(STATE_NAME = factor(STATE_NAME, levels = rev(STATE_NAME)))

    validate(need(nrow(top10) > 0, "No states available for this selection."))

    plot_ly(
      data          = top10,
      x             = ~.data[[x_col]],
      y             = ~STATE_NAME,
      type          = "bar",
      orientation   = "h",
      text          = ~sprintf("%.2f", .data[[x_col]]),
      textposition  = "outside",
      marker        = list(
        color = "#3454B4",
        line  = list(color = "#35146B", width = 0.6)
      ),
      hovertemplate = paste0("<b>%{y}</b><br>", x_label, ": %{x:.2f}<extra></extra>")
    ) %>%
      layout(
        xaxis  = list(
          title      = x_label,
          range      = c(0, max(top10[[x_col]]) * 1.15),
          tickformat = ".2f",
          gridcolor  = "#eee"
        ),
        yaxis         = list(title = ""),
        margin        = list(l = 130, r = 70, t = 20, b = 60),
        plot_bgcolor  = "#fafafa",
        paper_bgcolor = "#fafafa"
      )
  }

  # ── TAB 1: Driving Age ──────────────────────────────────
  age_filtered <- reactive({
    rate_col <- if (input$rate_type == "minor") "MINOR_FATALITY_RATE" else "FATAL_CAR_CRASHES"
    colorbar_title <- if (input$rate_type == "minor") "Per 100k (Minors)" else "Per 100k Population"
    log_scale      <- input$rate_type == "minor"

    df <- traffic_data %>%
      mutate(
        age_match = if (input$age_filter == "All") TRUE else AGE_GROUP == input$age_filter,
        hover_text = paste0(
          "<b>", STATE_NAME, "</b>",
          "<br>Min. Driving Age: ", MIN_DRIVING_AGE,
          "<br>", colorbar_title, ": ", round(.data[[rate_col]], 2)
        )
      )
    list(df = df, rate_col = rate_col, colorbar_title = colorbar_title, log_scale = log_scale)
  })

  output$age_map <- renderPlotly({
    r         <- age_filtered()
    df        <- r$df
    matched   <- df %>% filter(age_match %in% TRUE)
    unmatched <- df %>% filter(is.na(age_match) | !age_match)
    z_range   <- if (r$log_scale) range(traffic_data$MINOR_FATALITY_RATE, na.rm = TRUE) else car_crash_range

    make_choropleth(matched, unmatched,
      z_col = r$rate_col, z_range = z_range,
      colorbar_title = r$colorbar_title,
      hover_text_matched   = matched$hover_text,
      hover_text_unmatched = unmatched$hover_text,
      log_scale = r$log_scale)
  })

  output$age_top10 <- renderPlotly({
    r       <- age_filtered()
    df_filt <- r$df %>% filter(age_match %in% TRUE)
    make_top10_bar(df_filt, r$rate_col, r$colorbar_title)
  })

  # ── TAB 2: Seatbelt ─────────────────────────────────────
  seatbelt_filtered <- reactive({
    parts       <- strsplit(input$seatbelt_law, "_")[[1]]
    seat        <- parts[1]
    enforcement <- parts[2]
    law_col     <- if (seat == "FS") "SEATBELT_FS" else "SEATBELT_RS"

    traffic_data %>%
      mutate(
        sb_match   = if (enforcement == "All") TRUE else .data[[law_col]] == enforcement,
        hover_text = paste0(
          "<b>", STATE_NAME, "</b>",
          "<br>Front Seat: ", SEATBELT_FS,
          "<br>Rear Seat: ",  SEATBELT_RS,
          "<br>Crashes per 100k: ", round(FATAL_CAR_CRASHES, 2)
        )
      )
  })

  output$seatbelt_map <- renderPlotly({
    df        <- seatbelt_filtered()
    matched   <- df %>% filter(sb_match %in% TRUE)
    unmatched <- df %>% filter(is.na(sb_match) | !sb_match)
    make_choropleth(matched, unmatched,
      z_col = "FATAL_CAR_CRASHES", z_range = car_crash_range,
      colorbar_title = "Per 100k Population",
      hover_text_matched   = matched$hover_text,
      hover_text_unmatched = unmatched$hover_text)
  })

  output$seatbelt_top10 <- renderPlotly({
    df_filt <- seatbelt_filtered() %>% filter(sb_match %in% TRUE)
    make_top10_bar(df_filt, "FATAL_CAR_CRASHES", "Crashes per 100k")
  })

  # ── TAB 3: Distracted Driving ───────────────────────────
  phone_filtered <- reactive({
    parts       <- strsplit(input$phone_laws, "_")[[1]]
    law_type    <- parts[1]
    enforcement <- parts[2]
    law_col     <- if (law_type == "HP") "HANDHELD_PHONE_BAN" else "TEXTING_BAN"

    traffic_data %>%
      mutate(
        phone_match = if (enforcement == "All") TRUE else .data[[law_col]] == enforcement,
        hover_text  = paste0(
          "<b>", STATE_NAME, "</b>",
          "<br>Handheld Phone Ban: ", HANDHELD_PHONE_BAN,
          "<br>Texting Ban: ",        TEXTING_BAN,
          "<br>Crashes per 100k: ", round(FATAL_CAR_CRASHES, 2)
        )
      )
  })

  output$phone_ban_map <- renderPlotly({
    df        <- phone_filtered()
    matched   <- df %>% filter(phone_match %in% TRUE)
    unmatched <- df %>% filter(is.na(phone_match) | !phone_match)
    make_choropleth(matched, unmatched,
      z_col = "FATAL_CAR_CRASHES", z_range = car_crash_range,
      colorbar_title = "Per 100k Population",
      hover_text_matched   = matched$hover_text,
      hover_text_unmatched = unmatched$hover_text)
  })

  output$phone_top10 <- renderPlotly({
    df_filt <- phone_filtered() %>% filter(phone_match %in% TRUE)
    make_top10_bar(df_filt, "FATAL_CAR_CRASHES", "Crashes per 100k")
  })

  # ── TAB 4: Speed Limits ─────────────────────────────────
  speed_filtered <- reactive({
    traffic_data %>%
      mutate(
        speed_match = if (input$speed_filter == "All") TRUE else SPEED_GROUP == input$speed_filter,
        hover_text  = paste0(
          "<b>", STATE_NAME, "</b>",
          "<br>Max Speed Limit: ", MAX_SPEED_LIMIT_MPH, " MPH",
          "<br>Crashes per 100k: ", round(FATAL_CAR_CRASHES, 2)
        )
      )
  })

  output$speed_map <- renderPlotly({
    df        <- speed_filtered()
    matched   <- df %>% filter(speed_match %in% TRUE)
    unmatched <- df %>% filter(is.na(speed_match) | !speed_match)
    make_choropleth(matched, unmatched,
      z_col = "FATAL_CAR_CRASHES", z_range = car_crash_range,
      colorbar_title = "Per 100k Population",
      hover_text_matched   = matched$hover_text,
      hover_text_unmatched = unmatched$hover_text)
  })

  output$speed_top10 <- renderPlotly({
    df_filt <- speed_filtered() %>% filter(speed_match %in% TRUE)
    make_top10_bar(df_filt, "FATAL_CAR_CRASHES", "Crashes per 100k")
  })

  # ── TAB 5: Motorcycle Map ───────────────────────────────
  moto_filtered <- reactive({
    if (input$helmet_law == "All") {
      traffic_data
    } else {
      traffic_data %>% filter(HELMET_GROUP == input$helmet_law)
    }
  })

  output$motorcycle_map <- renderPlotly({

    df      <- moto_filtered()
    matched <- df %>% filter(!is.na(FATAL_MOTORCYCLE_CRASHES))

    # Gray = all states NOT in the filtered set
    unmatched <- traffic_data %>%
      filter(!STATE %in% matched$STATE)

    hover_matched <- paste0(
      "<b>", matched$STATE_NAME, "</b>",
      "<br>Helmet Law: ",
      ifelse(is.na(matched$HELMET_REQUIREMENT), "Not Available", matched$HELMET_REQUIREMENT),
      "<br>Crashes per 100k: ", round(matched$FATAL_MOTORCYCLE_CRASHES, 2)
    )

    hover_unmatched <- paste0(
      "<b>", unmatched$STATE_NAME, "</b>",
      "<br>Helmet Law: ",
      ifelse(is.na(unmatched$HELMET_REQUIREMENT), "Not Available", unmatched$HELMET_REQUIREMENT)
    )

    make_choropleth(
      matched, unmatched,
      z_col                = "FATAL_MOTORCYCLE_CRASHES",
      z_range              = moto_crash_range,
      colorbar_title       = "Per 100k Population",
      hover_text_matched   = hover_matched,
      hover_text_unmatched = hover_unmatched
    )
  })

  # ── TAB 5: Top-10 Bar Chart ─────────────────────────────
  output$top_states_bar <- renderPlotly({
    make_top10_bar(moto_filtered(), "FATAL_MOTORCYCLE_CRASHES", "Crashes per 100k")
  })
}

# ============================================================
# RUN
# ============================================================

shinyApp(ui = ui, server = server)
