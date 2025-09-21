# ===============================================
# Elevation Clustering Explorer (Shiny App)
# With Leaflet Mapping, Clustering, and Styling
# ===============================================

# ---- Load required libraries ----
library(shiny)      # Web application framework for R
library(sf)         # For handling vector spatial data (shapefiles, etc.)
library(terra)      # For raster data manipulation
library(tidyverse)  # Data wrangling (dplyr, purrr, ggplot2, etc.)
library(leaflet)    # Interactive web maps
library(DT)         # Interactive tables
library(bslib)      # Slick themes for Shiny apps
library(plotly)     # Interactive scatterplots

# ---- Data Processing ----

# Read administrative boundaries (level 3) from shapefile
admin3 <- st_read("data/pak_adm_wfp_20220909_shp/pak_admbnda_adm3_wfp_20220909.shp")

# Read elevation raster
elevation <- rast("data/pak_elevation_merit103_10km_v1.tif")

# Compute mean elevation per admin3 polygon
elevation_mean <- elevation |>
  terra::extract(admin3, fun = mean, na.rm = TRUE) |>
  select(elevation_mean = elevation_merit103_100m_v1)

# Compute SD of elevation per admin3 polygon
elevation_sd <- elevation |>
  terra::extract(admin3, fun = sd, na.rm = TRUE) |>
  select(elevation_sd = elevation_merit103_100m_v1)

# Combine shapefile + elevation statistics into one sf object
admin3_elevation <- elevation_mean |>
  cbind(elevation_sd) |>
  cbind(admin3) |>
  dplyr::select(
    adm_1 = ADM3_EN,  # Admin 1 name
    adm_2 = ADM2_EN,  # Admin 2 name
    adm_3 = ADM1_EN,  # Admin 3 name
    elevation_mean,   # Mean elevation
    elevation_sd,     # Elevation standard deviation
    geometry          # Spatial geometry
  ) |>
  dplyr::filter(!if_any(where(is.numeric), is.nan)) |>  # Remove rows with NaN
  st_as_sf()  # Ensure object is an sf object

# Clean up intermediate objects
rm(elevation_mean, elevation_sd, admin3, elevation)

# ---- Define UI ----
ui <- page_fluid(  # Use bslib’s page_fluid for slick design
  theme = bs_theme(bootswatch = "cosmo"),  # Choose a bootswatch theme (Cosmo)
  
  titlePanel("🌍 Elevation Clustering Explorer"),  # Title with emoji for style
  
  sidebarLayout(
    # ---- Sidebar Panel ----
    sidebarPanel(
      wellPanel(
        HTML("This app groups administrative areas into clusters based on their elevation characteristics.
             You can adjust the number of clusters, view the results on an interactive map, 
             explore the relationship between mean elevation and variability, 
             and inspect the data in table form.<br>Please do explore the logic behind this app.")
      ),
      tags$br(),
      # Number of clusters input
      sliderInput(
        "clusters", "Number of Clusters:",
        min = 2, max = 5, value = 3, step = 1
      ),
      
      # Action button to run clustering
      actionButton("runClust", "🚀 Run Clustering", class = "btn-primary")
    ),
    
    # ---- Main Panel with Tabs ----
    mainPanel(
      tabsetPanel(
        tabPanel("🗺️ Map", leafletOutput("mapPlot", height = "600px")),
        tabPanel("📊 Scatterplot", plotlyOutput("scatterPlot")),
        tabPanel("📋 Table", DTOutput("tableOut"))
      )
    )
  )
)

# ---- Define Server Logic ----
server <- function(input, output, session) {
  
  # Reactive expression for clustering results
  clust_results <- eventReactive(input$runClust, {
    
    # Drop geometry for clustering (numeric variables only)
    df <- admin3_elevation |> st_drop_geometry()
    
    # Perform K-means clustering on elevation mean & SD
    kmod <- kmeans(df |> select(elevation_mean, elevation_sd), centers = input$clusters)
    
    # Add cluster results back to the spatial dataset
    admin3_elevation$cluster <- as.factor(kmod$cluster)
    
    # Return both raw dataframe & spatial object
    list(df = df, shp = admin3_elevation)
  })
  
  # ---- Leaflet Map Output ----
  output$mapPlot <- renderLeaflet({
    req(clust_results())  # Ensure clustering was run
    shp <- clust_results()$shp
    
    # Define color palette based on clusters
    pal <- colorFactor("Set2", shp$cluster)
    
    # Build leaflet map
    leaflet(shp) |>
      addProviderTiles("CartoDB.Positron") |>  # Clean basemap
      addPolygons(
        fillColor = ~pal(cluster),   # Fill color by cluster
        color = "white",             # Border color
        weight = 0.7,                # Border thickness
        opacity = 1,                 # Border opacity
        fillOpacity = 0.7,           # Polygon fill opacity
        highlight = highlightOptions( # Highlight on hover
          weight = 2,
          color = "black",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~paste0(
          "<b>Adm1:</b> ", adm_1,
          "<br><b>Adm2:</b> ", adm_2,
          "<br><b>Adm3:</b> ", adm_3,
          "<br><b>Cluster:</b> ", cluster
        ) |> lapply(htmltools::HTML) # Render labels as HTML
      ) |>
      addLegend(
        "bottomright", pal = pal, values = ~cluster,
        title = "Clusters", opacity = 0.8
      )
  })
  
  # ---- Scatterplot Output ----
  output$scatterPlot <- renderPlotly({
    req(clust_results())
    shp <- clust_results()$shp |> st_drop_geometry()
    
    # Interactive scatterplot of elevation mean vs SD
    plot_ly(
      shp, x = ~elevation_mean, y = ~elevation_sd,
      color = ~cluster, type = "scatter", mode = "markers",
      marker = list(size = 10, opacity = 0.7, line = list(width = 1, color = "black")),
      text = ~paste(adm_1, adm_2, adm_3)
    ) |>
      layout(
        title = "Elevation Mean vs SD by Cluster",
        xaxis = list(title = "Mean Elevation"),
        yaxis = list(title = "Elevation SD")
      )
  })
  
  # ---- Data Table Output ----
  output$tableOut <- renderDT({
    req(clust_results())
    shp <- clust_results()$shp |> st_drop_geometry()
    
    # Interactive table
    datatable(
      shp,
      options = list(
        pageLength = 10,
        searchHighlight = TRUE,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
}

# ---- Run Shiny App ----
shinyApp(ui, server)
