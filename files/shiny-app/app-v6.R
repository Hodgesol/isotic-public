## Shiny App Development from 'env-regression-tree-v2-decision-matrix.R'
## R. Hodges and D. Beaudette
## Nov 18, 2025

## cleanup
# rm(list = ls())
gc(reset = TRUE)

library(shiny)
library(terra)
library(leaflet)
library(dplyr)
library(viridis)

# --- App metadata (edit as needed) ---
app_version <- "v1.0.0 (Jan 9, 2026)"
github_url  <- "https://github.com/Hodgesol/isotic/tree/main/clean-project-files-extendedwork/files/shiny-app"
cite_text   <- "Hodges, R. C., Beaudette, D. E., & Shaw, J. N. (2026). ISOTIC: An Isotic Mineralogy Spatial Prediction Tool for the Contiguous US. Shiny application."

# --- raster path (pre-projected) ---
raster_path <- "isotic-p-round-4326.tif"

# set wd
# setwd("C:/Users/Ryan.Hodges/OneDrive - USDA/Desktop/isotic/clean-project-files-extendedwork/files/shiny-app")


# Load the SpatRaster object
# The app assumes "isotic-p-round.tif" is in the same directory as app.R
# and assumes is available when running the app.
# tryCatch({
#   decision_raster <- rast("isotic-p-round.tif")
#   names(decision_raster) <- "Isotic Probability"
# }, error = function(e) {
#   # Add the error message to the stop function for better debugging
#   stop("Missing 'isotic-p-round.tif' in app directory.")
# })

# print(decision_raster)

# --- reproject the raster to WGS 84 (EPSG: 4326) ---

# The standard CRS for web mapping and user Lat/Lon input is WGS 84 (EPSG: 4326)
target_crs <- "EPSG:4326" 

# Check current CRS and project if necessary"
# if (crs(decision_raster) != target_crs) {
#   # Use 'near' method for categorical/decision data (0s and 1s)
#   decision_raster <- project(decision_raster, target_crs, method = "near")
#   print("Raster reprojected to EPSG:4326 (WGS 84).")
# }

# print(decision_raster) # Print the updated SpatRaster info


## NOTE:
## Don't create a palette at the global scope using `decision_raster`, because
## `decision_raster` is loaded inside `server()` (to reduce memory use when
## deploying). Create the palette after the raster is loaded.

# --- UI (User Interface) ---
ui <- fluidPage(
  titlePanel("ISOTIC: An Isotic Mineralogy Spatial Prediction Tool for the Contiguous US"),
  
  # --- Authors Subheading Added Here ---
  # Use div and HTML for precise styling and better control over the layout.
  div(
    HTML("<h4>By: Ryan C. Hodges, Dylan E. Beaudette, & Joey N. Shaw</h4>"),
    style = "color: gray; margin-top: 10px; margin-bottom: 20px;"
  ),
  
  tabsetPanel(
    tabPanel(
      "Map",
      sidebarLayout(
        sidebarPanel(
          # Section 1: Explanation of the data premise
          HTML("<p>This map is based on research by Hodges et al. (in preparation). The isotic probability raster follows the recursive partitioning decision tree of Figure 9 (Hodges et al., in prep) provided below. Each grid cell indicates the probability for the presence of soils with isotic mineralogy. Displayed raster is dynamically downsampled to ~200,000 cells total to allow for visualization. However, reported values from queries are based on the full native resolution (800 m).</p>"),
          
          
          # Section 2: Displaying the image
          # The 'src' path assumes 'env-rast-tree-v4.png' is located in a 'www' folder within your app directory.
          img(src = "env-rast-tree-v4.png", alt = "Figure 9 Environmental Covariate Decision Tree", style = "max-width: 100%; height: auto; margin-bottom: 20px;"),
          
          # Section 2.1: Adding the Caption right after the image
          HTML("<p style='font-size: 0.8em; color: #555; margin-top: 5px; margin-bottom: 20px;'>
            <b>Figure 5.</b> Environmental Covariate Decision Tree. Source: Hodges et al. (2026, in prep). Rasters: Andisols and Andic Subgroups, effective precipitation, and mean annual air temperature (MAAT).
          </p>"),
          
          HTML("<p><b>Instructions:</b> Input coordinates below or select a point on the interactive map to create a pin. Select the pin to retrieve location and isotic probability data.</p>"),
          
          # Section 3: Original input controls
          h4("Input Coordinates (WGS 84)"),
          p("Enter latitude and longitude in decimal degrees to pinpoint a location on the map."),
          numericInput("lat_input", "Latitude (Y, e.g., 34.05):", value = NULL, min = -90, max = 90),
          numericInput("lon_input", "Longitude (X, e.g., -118.24):", value = NULL, min = -180, max = 180),
          actionButton("submit_coords", "Locate on Map"),

          hr(),
          h4("Cite This App"),
          HTML(paste0(
            "<p>If you use this application or its outputs in a publication, please cite:</p>",
            "<p style='margin-left:10px;'>", cite_text, "</p>"
          )),
          actionButton("copy_citation", "Copy Citation"),

          # hr(),
          # h4("Source Code"),
          # HTML(paste0(
          #   "<p>The full source code for this application is available on GitHub:</p>",
          #   "<p><a href='", github_url, "' target='_blank'>", github_url, "</a></p>"
          # )),
          # HTML(paste0("<p style='font-size:0.85em;color:#666;'>Version: ", app_version, "</p>"))
        ),
        
        mainPanel(
          # The interactive map output area
          leafletOutput("map_output", height = 600)
        )
      )
    ),

    tabPanel(
      "About",
      h3("About ISOTIC"),
      p("Soil mineralogy is a foundational component of soil taxonomy because it helps explain how soils function—how they retain water, interact with nutrients, and influence the behavior of contaminants. Soils influenced by volcanic ash exhibit unique behavior—such as low bulk density and high phosphate retention—driven by a mineralogy rich in short-range-order minerals. These characteristics are collectively defined as andic soil properties. The isotic mineralogy family class, introduced in “Keys to Soil Taxonomy” in 1996, was intended to capture soils that do not meet the criteria for amorphic mineralogy or andic intergrades but nonetheless exhibit properties similar to volcanic ash-influenced soils, including elevated surface reactivity."),
        
      p("ISOTIC is an interactive Shiny application for exploring modeled probability of isotic mineralogy across the contiguous United States. This tool was designed to help field staff assess the likelihood of encountering soils with isotic mineralogy. It  provides a raster-based probability (percent) overlay for isotic mineralogy across the contiguous United States at an 800-m resolution. As the three environmental rasters are evaluated against the decision tree’s critical thresholds at a given location, each grid cell is assigned to one of five terminal nodes indicated, returning a corresponding probability value. Users may either input known coordinates or select a point of interest directly on the interactive map. Selecting a populated map icon returns both the coordinates and the predicted probability of isotic mineralogy."),

      h4("Version"),
      HTML(paste0("<p>", app_version, "</p>")),

      h4("Cite This App"),
      tags$pre(style = "white-space: pre-wrap;", cite_text),
      actionButton("copy_citation_about", "Copy Citation"),

      # h4("Source Code"),
      # HTML(paste0("<p><a href='", github_url, "' target='_blank'>", github_url, "</a></p>"))
    )
  )
)

server <- function(input, output, session) {

  # ---- Load raster once when the server starts ----
  if (!file.exists(raster_path)) {
    stop(
      "Raster file not found: '", raster_path, "'.\n",
      "Place it in the app directory (next to app.R), or update 'raster_path'."
    )
  }

  # NOTE: Do not load the full-resolution raster at startup (OOM risk on shinyapps.io).
# We load a *display* version after the basemap renders, and load the full-res raster only on click.
  raster_path_full <- raster_path

  # ---- CRS note ----
  # Full-res raster is loaded on-demand for clicks; display raster is reprojected to EPSG:3857 in session$onFlushed().

  # ---- Palette (must be created AFTER decision_raster exists) ----
  # Avoid pulling all raster values into memory (which can trigger "signal: killed"
  # on shinyapps.io / Posit Connect). Use the raster's min/max instead.
  # --- Citation modal (used by both buttons) ---
  show_citation_modal <- function() {
    showModal(modalDialog(
      title = "Citation",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tags$textarea(
        rows = 6,
        style = "width:100%;",
        cite_text
      )
    ))
  }
  
  # ---- Map ----
  output$map_output <- renderLeaflet({
  leaflet(options = leafletOptions(
    worldCopyJump = TRUE,
    minZoom = 2
  )) %>%
    addProviderTiles(
      providers$Esri.WorldTopoMap,
      options = providerTileOptions(noWrap = TRUE)
    ) %>%
    setMaxBounds(lng1 = -180, lat1 = -85.0511, lng2 = 180, lat2 = 85.0511) %>%
    setView(lng = -98.5795, lat = 39.8283, zoom = 4)
})


# ---- Add raster overlay after initial map render (prevents shinyapps.io disconnect/timeouts) ----
  session$onFlushed(function() {

        # ---- Load a lightweight display raster (avoid keeping full-res in memory) ----
    # Read full-res briefly, then downsample/project for display and immediately free the full raster.
    r_full_startup <- terra::rast(raster_path_full)
    names(r_full_startup) <- "Isotic Probability"

    # Aggressive downsample target for shinyapps.io (keeps leaflet overlay memory low)
    max_cells <- 2e5
    if (terra::ncell(r_full_startup) > max_cells) {
      fact <- ceiling(sqrt(terra::ncell(r_full_startup) / max_cells))
      r_disp <- terra::aggregate(r_full_startup, fact = fact, fun = mean, na.rm = TRUE)
    } else {
      r_disp <- r_full_startup
    }

    # Ensure display raster is Web Mercator so leaflet does not reproject during addRasterImage()
    # (leaflet basemap tiles are EPSG:3857)
    if (!grepl("3857", terra::crs(r_disp), fixed = TRUE)) {
      r_disp <- terra::project(r_disp, "EPSG:3857", method = "bilinear")
    }

    # Free full-res raster from startup to reduce peak RAM
    rm(r_full_startup)
    gc()

    # Palette + legend domain without pulling all raster values into memory
    # ---- robust min/max for palette domain ----
mm <- terra::minmax(r_disp)

if (length(mm) >= 2) {
  rng <- range(as.numeric(mm), na.rm = TRUE)
} else {
  gg <- terra::global(r_disp, c("min", "max"), na.rm = TRUE)
  rng <- c(gg[1, 1], gg[1, 2])
}

# Fallback safety (all-NA raster edge case)
if (any(!is.finite(rng)) || length(rng) != 2) {
  rng <- c(0, 1)
}

    pal <- leaflet::colorNumeric(
      palette = "viridis",
      domain  = rng,
      na.color = "transparent",
      reverse = TRUE
    )
    
    leafletProxy("map_output") %>%
      clearImages() %>%
      clearControls() %>%
      addRasterImage(r_disp, colors = pal, opacity = 0.7, project = TRUE) %>%
      {
        # Custom legend to force HIGH values at the TOP and LOW values at the BOTTOM
        legend_cols <- pal(seq(rng[2], rng[1], length.out = 10))  # top -> bottom
        legend_css <- paste0("linear-gradient(to bottom, ", paste(legend_cols, collapse = ", "), ");")
        
        legend_html <- htmltools::tags$div(
          style = "background: rgba(255,255,255,0.95); padding: 8px 10px; border-radius: 4px; box-shadow: 0 1px 5px rgba(0,0,0,0.4);",
          htmltools::tags$div("Isotic Probability (%)", style = "font-weight: 600; margin-bottom: 6px;"),
          htmltools::tags$div(
            style = "display:flex; flex-direction:row; align-items:stretch;",
            htmltools::tags$div(style = paste0("width:18px; height:120px; margin-right:8px; background:", legend_css, ";")),
            htmltools::tags$div(
              style = "display:flex; flex-direction:column; justify-content:space-between; height:120px; font-size:12px; line-height:12px;",
              htmltools::tags$div(sprintf("%.1f", rng[2])),
              htmltools::tags$div(sprintf("%.1f", rng[1]))
            )
          )
        )
        
        addControl(., html = htmltools::HTML(as.character(legend_html)), position = "bottomright")
      }

  }, once = TRUE)


  observeEvent(input$copy_citation, {
    show_citation_modal()
  })

  observeEvent(input$copy_citation_about, {
    show_citation_modal()
  })
  
  # --- Logic for Coordinate Input (Left Panel) ---
  observeEvent(input$submit_coords, {
    req(input$lat_input, input$lon_input)
    lat <- as.numeric(input$lat_input)
    lon <- as.numeric(input$lon_input)
    
    if (is.finite(lat) && is.finite(lon)) {
      
      # 1. Extract raster value for the input coordinates
      content <- tryCatch({
        # Load full-resolution raster only when needed
        r_full <- terra::rast(raster_path_full)
        names(r_full) <- "Isotic Probability"

        pt_ll <- terra::vect(cbind(lon, lat), crs = "EPSG:4326")
        pt <- terra::project(pt_ll, terra::crs(r_full))

        val_df <- terra::extract(r_full, pt)
        prob_val <- val_df[[2]] # Extracts the value column (index 2 in terra extract df)

        rm(r_full)
        gc()
        
        if (is.na(prob_val)) {
          paste("<b>Input Location:</b>", round(lon, 4), "Lon,", round(lat, 4), "Lat<br/>",
                "<i>No raster data at this location.</i>")
        } else {
          paste("<b>Coordinates:</b>", round(lon, 4), "Lon,", round(lat, 4), "Lat<br/>",
                "<b>Isotic Probability:</b>", round(prob_val, 2), "%")
        }
      }, error = function(e) { "<b>Error extracting data.</b>" })
      
      # 2. Update Map
      leafletProxy("map_output") %>%
        clearMarkers() %>%
        addMarkers(lng = lon, lat = lat, popup = content) %>%
        setView(lng = lon, lat = lat, zoom = 10)
      
    } else {
      showNotification("Please enter valid numeric coordinates.", type = "error")
    }
  })
  
  # --- Logic for Map Clicks ---
  observeEvent(input$map_output_click, {
    click <- input$map_output_click
    if (is.null(click$lat)) return()
    
    content <- tryCatch({
      # Load full-resolution raster only when needed (keeps baseline RAM low on shinyapps.io)
      r_full <- terra::rast(raster_path_full)
      names(r_full) <- "Isotic Probability"

      # Leaflet click coordinates are lon/lat (EPSG:4326)
      click_point_ll <- terra::vect(cbind(click$lng, click$lat), crs = "EPSG:4326")
      click_point <- terra::project(click_point_ll, terra::crs(r_full))

      raster_value_df <- terra::extract(r_full, click_point)
      probability_value <- raster_value_df[[2]] # Using index [[2]] is safer than names

      # Free full raster ASAP to reduce peak RAM
      rm(r_full)
      gc()
      
      if (is.na(probability_value)) {
        "<b>No data available for this location.</b>"
      } else {
        paste("<b>Coordinates:</b>", round(click$lng, 4), "Lon,", round(click$lat, 4), "Lat<br/>",
              "<b>Isotic Probability:</b>", round(probability_value, 2), "%")
      }
    }, error = function(e) { "<b>Error extracting data.</b>" })
    
    leafletProxy("map_output") %>%
      clearMarkers() %>%
      addMarkers(lng = click$lng, lat = click$lat, popup = content)
  })
}



# Run the application
shinyApp(ui = ui, server = server)

# shiny::runApp("shiny-app") .... this doesn't work...

