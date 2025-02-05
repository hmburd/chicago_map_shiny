if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("leaflet", quietly = TRUE)) install.packages("leaflet")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("shinythemes", quietly = TRUE)) install.packages("shinythemes")
if (!requireNamespace("mapview", quietly = TRUE)) install.packages("mapview")
if (!requireNamespace("webshot2", quietly = TRUE)) install.packages("webshot2")

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(shinythemes)
library(mapview)
library(webshot2)


# Load Data
data <- read_csv("data/merged_turnout_data_cleaned.csv")

# Convert geometry column (assuming WKT format)
data <- data %>%
  mutate(geometry = st_as_sfc(geometry, crs = 4326)) %>%
  st_as_sf()

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Custom CSS for layout
  tags$head(
    tags$style(HTML("
      body { background-color: white; }
      .title-section { text-align: center; margin-bottom: 20px; }
      .title { font-size: 28px; font-weight: bold; color: #2C3E50; }
      .subtitle { font-size: 16px; color: #7B8A8B; }
      .banner { display: block; margin: auto; width: 100%; max-width: 800px; }
      .sidebar { background-color: white; padding: 20px; border-radius: 10px; box-shadow: 0px 2px 10px rgba(0,0,0,0.1); }
      .map-container { width: 100%; height: 600px; }
      .source { text-align: center; font-size: 12px; color: #555; margin-top: 10px; }
    "))
  ),
  
  # Title Section
  div(class = "title-section",
      img(src = "banner-growth.png", class = "banner"),  # Ensure this image exists in www/ folder
      div(class = "title", "Chicago Growth Project: Internal Insights Dashboard"),
      div(class = "subtitle", "Interactive voter turnout & spatial analysis for internal strategy"),
      div(class = "source", "Source: Races - 2023 Municipality Turnout")
  ),
  
  # Sidebar & Map Layout
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      selectInput("var", "Select Data to Visualize:", 
                  choices = c("Turnout", "Registered Voters", "Ballots Cast"),
                  selected = "Turnout"),
      downloadButton("export_map", "Download Map as PNG"),
      p("This internal tool enables spatial data exploration for team insights.")
    ),
    
    mainPanel(
      div(class = "map-container",
          leafletOutput("map", width = "100%", height = "650px")  # Adjust height
      )
    )
  ),
  
  # Footer
  tags$footer(
    div(
      style = "text-align: center; padding: 10px; font-size: 14px; color: #555;",
      HTML("Made with ❤️ by <b>HollyTech</b>")
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    pal <- colorNumeric(
      palette = c("#fee5d9", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15"),  
      domain = data[[input$var]]
    )
    
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%  
      addPolygons(
        fillColor = ~pal(get(input$var)), 
        weight = 1, 
        color = "#444444", 
        fillOpacity = 0.7,
        highlight = highlightOptions(weight = 3, color = "#000000", bringToFront = TRUE),
        label = ~paste0("Ward: ", ward, "<br>Precinct: ", precinct, "<br>", input$var, ": ", get(input$var))
      ) %>%
      addLegend(pal = pal, values = data[[input$var]], title = input$var, opacity = 1)
  })
  
  if (!requireNamespace("webshot2", quietly = TRUE)) install.packages("webshot2")
  
  output$downloadMap <- downloadHandler(
    filename = function() { paste0("chicago_turnout_map_", input$var, ".png") },
    content = function(file) {
      webshot::install_phantomjs()  # Ensure PhantomJS is installed
      mapshot(output$map(), file = file, vwidth = 1000, vheight = 800)
    }
  )
  
}

# Run App
shinyApp(ui = ui, server = server)

