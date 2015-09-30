library(shinydashboard)
library(shiny)

header <- dashboardHeader(title = "Accidentologie à Paris en 2012-2013")
sidebar <- dashboardSidebar()
body <- dashboardBody(
  fluidRow(
    box(leafletOutput("mainMap"),
        textOutput("textTest"),
        width = 12,
        title = "Carte des arrondissements"),
    box(plotOutput("plotNbAccMois"),
        width = 12,
        title = "Nombre d'accidents par mois")
  )
  
)

dashboardPage(
  header,
  sidebar,
  body
)