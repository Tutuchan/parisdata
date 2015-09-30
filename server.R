library(shinydashboard)
library(shiny)
source("global.R")

shinyServer(function(input, output, session) {
  output$textTest <- renderPrint({
    # spPolygons@data$insee[input$mainMap_shape_click$id]-100
  })
  
  output$mainMap <- renderLeaflet({
    pal = colorNumeric("RdBu", dfNbAccs$n)
    leaflet(spPolygons) %>% 
      addTiles() %>% 
      addProviderTiles("Acetate.terrain") %>% 
      addPolygons(stroke = TRUE, weight = 2, color = "black", 
                  fillColor = "blue", smoothFactor = 0.2, fillOpacity = 0.6, popup = ~arrs, layerId = 1:20)
    })
  
  dataClick <- reactive({
    validate(
      need(!is.null(input$mainMap_shape_click), "Choisissez un arrondissement.")
    )
    arr = spPolygons@data$insee[input$mainMap_shape_click$id]-100
    dfDataAccidents %>% filter(cp == arr)
  })
  
  output$plotNbAccMois <- renderPlot({
    dfPlot <- dataClick() %>% 
      select(date, starts_with("vehic")) %>% 
      mutate(date = as.Date(date),
             mois = format(date, "%m"),
             annee = format(date, "%Y")) %>% 
      count(annee, mois) %>% 
      mutate(date = as.Date(paste(annee, mois, "01", sep = "/")))
    ggplot(dfPlot, aes(date, n)) + 
      geom_bar(stat = "identity", fill = RColorBrewer::brewer.pal(4, "Paired")[2]) + 
      theme_linedraw() + 
      xlab("") + ylab("") + 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
      scale_x_date(breaks = date_breaks("month"), 
                   labels = date_format("%m/%Y"))
  })
})