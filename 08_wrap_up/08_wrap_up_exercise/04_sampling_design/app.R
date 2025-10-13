# app.R
library(shiny)
library(tidyverse)
library(sf)
library(bslib)

# Load the pre-computed results
res <- read.csv("eas_design_eval.csv")

# Calculate the true population mean (assuming 'eas' is also available)
# This part might need adjustment if your `eas_strata.shp` file is not local.
# The `st_read` and `st_drop_geometry` functions are for reading spatial data.
# Make sure your data source is correct.
eas <- st_read("eas_strata.shp") |>
  st_drop_geometry() |>
  mutate(y = bare + built)
true_mean <- mean(eas$y)

# Define the list of designs for the dropdown menu
# The names on the left are user-friendly labels.
# The values on the right MUST EXACTLY MATCH the 'design' column in your CSV.
designs_list <- c(
  "Simple Random Sampling" = "SRS",
  "Stratified sampling (proportional)" = "Stratified sampling (proportional)",
  "Stratified sampling (Neyman)" = "Stratified sampling (Neyman)",
  "Weighted stratified sampling (proportional)" = "Weighted stratified sampling (proportional)",
  "Weighted stratified sampling (Neyman)" = "Weighted stratified sampling (Neyman)"
)

# UI
ui <- fluidPage(
  # Add a theme from bslib for a modern look
  theme = bs_theme(
    version = 5,
    bootswatch = "cerulean"
  ),
  titlePanel(
    div(
      class = "py-3",
      h1("Sampling Design Simulation Explorer", class = "text-left")
    )
  ),
  layout_sidebar(
    sidebar = sidebar(
      h4("Controls"),
      selectInput(
        "selected_designs",
        "Select Designs to Compare:",
        choices = designs_list,
        selected = designs_list,
        multiple = TRUE
      ),
      hr(),
      h4("Choose a metric:"),
      checkboxInput("show_mean", "Show Mean of Estimates", value = TRUE),
      checkboxInput("show_se", "Show Standard Error", value = TRUE),
      hr(),
      h4("Sample Size Explorer"),
      sliderInput(
        "sample_size_slider",
        "Select Sample Size (n):",
        min = min(res$n), max = max(res$n), value = min(res$n), step = 1
      )
    ),
    card(
      full_screen = TRUE,
      card_header("Performance Plot"),
      plotOutput("performance_plot")
    ),
    card(
      full_screen = TRUE,
      card_header(textOutput("true_mean_text")),
      tableOutput("sample_size_table")
    )
  )
)

# Server
server <- function(input, output, session) {
  selected_n <- reactive({
    req(input$sample_size_slider)
    input$sample_size_slider
  })
  
  output$true_mean_text <- renderText({
    paste("True Population Mean:", round(true_mean, 3))
  })
  
  # Reactive filtering of the data based on user input
  filtered_data <- reactive({
    req(input$selected_designs)
    res %>%
      filter(design %in% input$selected_designs)
  })
  
  output$performance_plot <- renderPlot({
    plot_data <- filtered_data()
    
    if (nrow(plot_data) == 0) {
      return(ggplot() + labs(title = "Select a design to plot."))
    }
    
    p <- ggplot(plot_data, aes(x = n, color = design)) +
      theme_minimal() +
      labs(
        title = "Comparison of Sampling Designs",
        x = "Sample Size (n)",
        y = "Metric Value",
        color = "Sampling Design"
      )
    
    if (input$show_mean) {
      p <- p + geom_line(aes(y = mean)) +
        geom_hline(yintercept = true_mean, linetype = "dashed", color = "gray50")
    }
    
    if (input$show_se) {
      p <- p + geom_line(aes(y = se))
    }
    
    # Add the vertical line for the selected sample size
    p <- p + geom_vline(xintercept = selected_n(), linetype = "dotted", color = "black")
    
    p
  })
  
  # Reactive data for the slider table
  slider_data <- reactive({
    req(input$sample_size_slider, input$selected_designs)
    filtered_data() %>%
      filter(n == input$sample_size_slider) %>%
      select(design, n, mean, se, bias, mse, cv) # Select and order the columns
  })
  
  # Render the table
  output$sample_size_table <- renderTable({
    slider_data()
  })
}

# Run the app
shinyApp(ui, server)