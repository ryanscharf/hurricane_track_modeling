
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
ui <- fluidPage(

    # Application title
    titlePanel("hurrdydurdycane"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(3,
                   textInput(inputId = "Year",
                             label = "Year:",
                             value = "2021",
                             width = "100px"),
                   textInput(inputId = "StormNum",
                             label = "Storm Number:",
                             value = "10",
                             width = "100px"),
                   textInput(inputId = "",
                             label = "Year:",
                             value = "2021",
                             width = "100px")
                   ),

        # Show a plot of the generated distribution
        mainPanel(
          leafletOutput("mcleaflet")
        )
    )
)
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
