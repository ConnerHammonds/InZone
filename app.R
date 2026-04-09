# app.R
# InZone Analytics — Umpire Scorecard Prototype
# Run with: shiny::runApp()

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(readr)
library(rmarkdown)

# Files in R/ are auto-sourced by Shiny, but source explicitly for clarity
source("R/data_processing.R")
source("R/visualization.R")

# ──────────────────────────────────────────────
# UI
# ──────────────────────────────────────────────
ui <- page_sidebar(
  title = "InZone Analytics",
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly"
  ),

  # ---- Dark background for plot containers ----
  tags$style(HTML("
    .card-body { background-color: #222222 !important; }
    .card { background-color: #222222 !important; border-color: #444444 !important; }
    .nav-tabs .nav-link { color: #cccccc !important; }
    .nav-tabs .nav-link.active { background-color: #222222 !important; color: white !important; border-color: #444444 !important; }
  ")),

  # ---- Sidebar ----
  sidebar = sidebar(
    width = 320,
    h5("Upload Game Data"),
    fileInput("csv_file", "Choose CSV File",
              accept = c("text/csv", "text/comma-separated-values", ".csv")),
    hr(),
    uiOutput("source_badge"),
    uiOutput("pitch_count"),
    hr(),
    downloadButton("download_pdf", "Download PDF Report", class = "btn-primary w-100")
  ),

  # ---- Main Panel ----
  navset_card_tab(
    id = "main_tabs",
    title = "Umpire Scorecard",

    nav_panel(
      "Overall Map",
      card_body(plotOutput("plot_overall", height = "520px"))
    ),
    nav_panel(
      "False Positives",
      card_body(plotOutput("plot_fp", height = "520px"))
    ),
    nav_panel(
      "False Negatives",
      card_body(plotOutput("plot_fn", height = "520px"))
    ),
    nav_panel(
      "Summary",
      card_body(tableOutput("summary_table"))
    )
  )
)

# ──────────────────────────────────────────────
# Server
# ──────────────────────────────────────────────
server <- function(input, output, session) {

  # Reactive: run full pipeline when a file is uploaded
  processed <- reactive({
    req(input$csv_file)
    process_csv(input$csv_file$datapath)
  })

  # Reactive: summary stats table
  summary_df <- reactive({
    req(processed())
    compute_summary(processed()$data)
  })

  # ---- Sidebar info ----
  output$source_badge <- renderUI({
    req(processed())
    src <- processed()$source
    colour <- switch(src,
      Trackman    = "success",
      FlightScope = "info",
      "secondary"
    )
    tags$span(class = paste0("badge bg-", colour, " fs-6"),
              paste("Detected:", src))
  })

  output$pitch_count <- renderUI({
    req(processed())
    d <- processed()$data
    tags$div(
      class = "mt-2",
      tags$strong(nrow(d)), " taken pitches from ",
      tags$strong(processed()$n_raw), " total rows"
    )
  })

  # ---- Plots ----
  output$plot_overall <- renderPlot({ req(processed()); plot_overall(processed()$data) }, bg = "#222222")
  output$plot_fp      <- renderPlot({ req(processed()); plot_false_positives(processed()$data) }, bg = "#222222")
  output$plot_fn      <- renderPlot({ req(processed()); plot_false_negatives(processed()$data) }, bg = "#222222")

  # ---- Summary Table ----
  output$summary_table <- renderTable({
    req(summary_df())
    summary_df()
  }, striped = TRUE, hover = TRUE, width = "100%")

  # ---- PDF Download ----
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("InZone_Scorecard_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Copy template to a temp directory so knitting doesn't pollute the app dir
      temp_dir <- tempdir()
      temp_rmd <- file.path(temp_dir, "report_template.Rmd")
      file.copy("report_template.Rmd", temp_rmd, overwrite = TRUE)

      # Copy R/ helpers so the template can source them
      dir.create(file.path(temp_dir, "R"), showWarnings = FALSE)
      file.copy("R/data_processing.R", file.path(temp_dir, "R/data_processing.R"), overwrite = TRUE)
      file.copy("R/visualization.R",   file.path(temp_dir, "R/visualization.R"),   overwrite = TRUE)

      rmarkdown::render(
        input       = temp_rmd,
        output_file = file,
        params      = list(
          data          = processed()$data,
          summary_table = summary_df(),
          source        = processed()$source
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
}

# ──────────────────────────────────────────────
# Launch
# ──────────────────────────────────────────────
shinyApp(ui, server)
