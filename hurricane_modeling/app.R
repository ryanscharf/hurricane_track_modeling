
library(shiny)

source('utils.R')

necessary_packages <-
  c(
    "sf",
    "htmltools",
    "htmlwidgets",
    "RColorBrewer",
    "leaflet",
    "tidyverse",
    'maps',
    'mapdata',
    'maptools',
    "rgeos",
    'lubridate',
    'zoo',
    'colorspace'
  )

if (!require("pacman")) install.packages("pacman")
pacman::p_load(necessary_packages, character.only = T)



tmp_file <- tempdir(check = T)

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("hurrdydurdycane"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(sidebarPanel(
    fluidRow(column(
      3,
      numericInput(
        inputId = "Year",
        label = "Year:",
        value = 2021
      ),
      numericInput(
        inputId = "StormNum",
        label = "Storm Number:",
        value = 10
      ),     
      numericInput(
        inputId = 'Month',
        label = 'Month: ',
        value = 6,
        min = 1,
        max = 12
      ),
      numericInput(
        inputId = 'Day',
        label = 'Day: ',
        value = 1,
        min = 1,
        max = 31
      ),
      numericInput(
        inputId = 'Hour',
        label = 'Hour: ',
        value = '18',
        min = 0,
        max = 24
      ),
      numericInput(
        inputId = 'Rmax',
        label = 'Rmax: ',
        value = 29
      ),
      numericInput(
        inputId = 'IntensityMod',
        label = 'Intensity Modifier: ',
        value = 1.4
      ),
      numericInput(
        inputId = 'ForecastHour',
        label = 'Forecast Hour: ',
        value = 120,
        min = 0,
        max = 172
      ),
      checkboxInput(
        inputId = 'CurrentModels',
        label = 'Use Only Current Models: ',
        value = F
      ),
      checkboxInput(
        inputId = 'DecayWindspeed',
        label = 'Decay Windspeeds: ',
        value = T
      ),
      numericInput(
        inputId = 'NumClusters',
        label = 'Number of Clusters: ',
        value = 3
      )
      
    )
    )
    ),
    # Show a plot of the generated distribution
    mainPanel(leafletOutput("mcleaflet"))
  ))

server <- function(input, output) {
  map <- leaflet() %>% addTiles() %>% setView(-93.65, 42.0285, zoom = 17)
  output$mcleaflet <- renderLeaflet(map)
}

# Run the application 
shinyApp(ui = ui, server = server)
