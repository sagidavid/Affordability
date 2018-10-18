library(rgdal)
library(dplyr)
library(leaflet)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
library(ggplot2)
library(raster)
library(plotly)
library(RColorBrewer)

function(input, output) {

  RentDataPolygons_F_RY <- reactive ({
    subset(
      subset(
        RentDataPolygons,
          Room == input$Room_Input),
            Year == input$Year_Input)})
  
  RentDataTable_F_YR <- reactive({
    subset(
      subset(
        RentDataTable,
          Room == input$Room_Input),
            Year == input$Year_Input)})
  
  bins <- c(0, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)
  pal <-  colorBin("RdYlGn", domain = RentDataPolygons$RentData, bins = bins)
  labels <- sprintf("<strong>%s</strong>",
                    RentDataPolygons$BRMA) %>% lapply(htmltools::HTML)
  
  #LEAFLET TAB
  #Setting up the base renderLeaflet
  output$affordabilityMap <- 
    renderLeaflet({
      leaflet("affordabilityMap") %>%
        setView(lat=54.5, lng=-2.5 , zoom=6) %>%
        addMapPane("Cities", zIndex = 10) %>% 
        addMapPane("Urban_Filter", zIndex = 9) %>% 
        addMapPane("Rural_Filter", zIndex = 8) %>% 
        addMapPane("Coastal_Filter", zIndex = 7) %>% 
        addMapPane("Inland_Filter", zIndex = 6) %>% 
        addMapPane("England_Filter", zIndex = 5) %>% 
        addMapPane("Scotland_Filter", zIndex = 4) %>% 
        addMapPane("Wales_Filter", zIndex = 3) %>% 
        addMapPane("Rents", zIndex = 2) 
    })
  
  #Using observe for the Year and Room Entitlement controls
  observe({RentDataPolygons_F_RY
    leafletProxy("affordabilityMap") %>% 
      clearShapes() %>%
      clearControls() %>% 
      addPolygons(
        data = RentDataPolygons,
        fillColor = ~pal(RentDataPolygons_F_RY()$RentData),
        weight = 0.5, 
        smoothFactor = 0.5,
        dashArray = "1",
        opacity = 2, 
        fillOpacity = 2, 
        highlightOptions = highlightOptions(
          color = "white", 
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        options = pathOptions(pane = "Rents")) %>% 
      addLegend(
        pal = pal,
        values = RentDataPolygons_F_RY()$RentData,
        title = "Affordability",
        opacity = 1,
        position = "bottomright")
  })  
  
  #Using observe for the Urban/Rural filters
  observe({Urban_Selection = input$Urban_Input
    if(Urban_Selection == "Rural")
      {leafletProxy("affordabilityMap") %>% 
        clearGroup(group = "UR") %>% 
        addPolygons(group = "UR", data = Filter_Rural, fillColor = "LightGrey", fillOpacity = 1, weight = 0,
                  options = pathOptions(pane = "Rural_Filter"))}
    else if(Urban_Selection == "Urban")
      {leafletProxy("affordabilityMap") %>%
        clearGroup(group = "UR") %>%
        addPolygons(group = "UR", data = Filter_Urban, fillColor = "LightGrey", fillOpacity = 1, weight = 0,
                  options = pathOptions(pane = "Urban_Filter"))}
    else
      {leafletProxy("affordabilityMap") %>%
        clearGroup(group = "UR")}
  })
  
  #Using observe for the Coastal/Inland filters
  observe({Coastal_Selection = input$Coastal_Input
    if(Coastal_Selection == "Inland")
      {leafletProxy("affordabilityMap") %>% 
        clearGroup(group = "CI") %>% 
        addPolygons(group = "CI", data = Filter_Inland, fillColor = "LightGrey", fillOpacity = 1, weight = 0,
                  options = pathOptions(pane = "Inland_Filter"))}
    else if(Coastal_Selection == "Coastal")
      {leafletProxy("affordabilityMap") %>%
        clearGroup(group = "CI") %>%
        addPolygons(group = "CI", data = Filter_Coastal, fillColor = "LightGrey", fillOpacity = 1, weight = 0,
                  options = pathOptions(pane = "Coastal_Filter"))}
    else
      {leafletProxy("affordabilityMap") %>%
        clearGroup(group = "CI")}
  })
  
  #Using observe for the Country filters
  observe({Country_Selection = input$Country_Input
    if(Country_Selection == "England")
      {leafletProxy("affordabilityMap") %>% 
        clearGroup(group = "Country") %>% 
        addPolygons(group = "Country", data = Filter_England, fillColor = "LightGrey", fillOpacity = 1, weight = 0,
                  options = pathOptions(pane = "England_Filter"))}
    else if(Country_Selection == "Scotland")
      {leafletProxy("affordabilityMap") %>%
        clearGroup(group = "Country") %>%
        addPolygons(group = "Country", data = Filter_Scotland, fillColor = "LightGrey", fillOpacity = 1, weight = 0,
                  options = pathOptions(pane = "Scotland_Filter"))}
    else if(Country_Selection == "Wales")
      {leafletProxy("affordabilityMap") %>%
        clearGroup(group = "Country") %>%
        addPolygons(group = "Country", data = Filter_Wales, fillColor = "LightGrey", fillOpacity = 1, weight = 0,
                  options = pathOptions(pane = "Wales_Filter"))}
    else
      {leafletProxy("affordabilityMap") %>%
        clearGroup(group = "Country")}
  })
  
  #Using observe to zoom in on map by clicking on it
  observe({click = input$affordabilityMap_shape_click
    if(is.null(click))
      return()
    else
      leafletProxy("affordabilityMap") %>% 
      setView(lng = click$lng , lat = click$lat, zoom = 9)
  })
  
  #Using observe to reset map view
  observe({input$Reset_Input
    leafletProxy("affordabilityMap") %>%
      setView(lat=54.5, lng=-2.5 , zoom=6)
  })
  
  #Using observe to zoom to selected BRMA from drop-down
  observe({input$BRMA_Input
    BRMA_BoundingTable_F <- subset(BRMA_BoundingTable, BRMA == input$BRMA_Input)
    
    leafletProxy("affordabilityMap") %>%
      fitBounds(lng1 = BRMA_BoundingTable_F$X1, lat1 = BRMA_BoundingTable_F$X2,
                lng2 = BRMA_BoundingTable_F$X3, lat2 = BRMA_BoundingTable_F$X4)
  })
  
  #Using observe to turn city layer on and off
  observe({input$City_Input
    if(input$City_Input){
      leafletProxy("affordabilityMap") %>% 
        addCircleMarkers(
          data = Cities,
          group = "Cities",
          radius = 2,
          color = "black",
          fillOpacity = 1,
          label = Cities$name,
          labelOptions = labelOptions(textsize = "15px", style = list("color" = "blue")),
          options = pathOptions(pane = "Cities"))
    } else
      {leafletProxy("affordabilityMap") %>% 
        clearGroup(group = "Cities")}
  })
  
  #GRAPH TAB
  #Defining the 1st (smaller) plot of the tab
  output$affordabilityGraphSummary <- renderPlotly({
    
  #Formatting RentDataTable (Note that subsetting of the table is done in rows 22-27)
    RentDataTable_Filtered2 <- RentDataTable_F_YR()
    
    graph <- ggplot(
      data = RentDataTable_Filtered2, 
      aes(x = reorder(BRMA, desc(RentData)), y = RentData)
    ) + ggtitle("Affordability Graph"
    ) + xlab(""
    ) + ylab("Affordability"
    ) + geom_bar(stat = "identity", width = 1, fill = "steelblue"
    ) + theme_classic(
    ) + coord_flip(
    ) + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank()
    )
    
    ggplotly(graph) %>% 
      layout(autosize = TRUE)
  }) 
  
  #Defining the 2nd (taller) plot of the tab
  output$affordabilityGraph <- renderPlotly({
    
    #Formatting RentDataTable (Note that subsetting of the table is done in rows 22-27)
    RentDataTable_Filtered <- RentDataTable_F_YR()
    
    graph <- ggplot(
      data = RentDataTable_Filtered, 
      aes(x = reorder(BRMA, desc(RentData)), y = RentData)
    ) + ggtitle("Affordability Graph"
    ) + xlab(""
    ) + ylab("Affordability"
    ) + geom_bar(stat = "identity", width = 0.5, fill = "steelblue"
    ) + theme_classic(
    ) + coord_flip()
    
    ggplotly(graph) %>% 
      layout(autosize = TRUE)
  })
  
  #TABLE TAB
  
  output$affordabilityTable <- renderDataTable(
    RentDataTable_F_YR(), options = list(pageLength = 50, order = list(list(6, "asc"))))
  
  #DOWNLOAD BUTTON
  
  output$Download_Input <- downloadHandler(
    filename = function() {
      paste("RentData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(RentDataTable, file, row.names = FALSE)
    })
  
  #SUMMARY TAB
  
  summaryTable_F2 <- reactive ({
    subset(
      subset(
        summaryTable, 
          Year == input$Year_Input), 
            Room == input$Room_Input)})
  
  summaryTable_F3 <- reactive ({
    subset(
      subset(
        summaryTable, 
          Year == input$Year_Input_Comparison), 
            Room == input$Room_Input_Comparison)})
  
  output$summaryGraph <- renderPlotly({
    
    summaryTable_F2 <- summaryTable_F2()
    summaryTable_F3 <- summaryTable_F3()
    summaryTable_F2$Type2 <- factor(summaryTable_F2$Type, as.character(summaryTable_F2$Type))
    summaryTable_F3$Type2 <- factor(summaryTable_F3$Type, as.character(summaryTable_F3$Type))
    summaryTable_F4 <- reactive({rbind(summaryTable_F2, summaryTable_F3)})
    summaryTable_F4 <- summaryTable_F4()
    
    if(input$Reorder_Input){
      graph <- ggplot(
          data = summaryTable_F4,
          aes(x = reorder(interaction(displayText, Type2), desc(RentData)), y = RentData, fill = displayText,
            label=sprintf("%0.2f", round(RentData, digits = 2)))
        ) + geom_bar(stat = "identity", position = position_dodge(), show.legend = FALSE
        ) + coord_flip(
        ) + xlab(""
        ) + theme_classic(
        ) + ggtitle("Comparison Graph"
        ) + ylab("Affordability"
        ) + scale_y_continuous(limits = c(0, 30)
        ) + guides(fill = FALSE
        ) + geom_text(size = 4, hjust = 1.2, position = position_fill()
        ) + scale_fill_manual(values=c('#E69F00', '#999999')
        )
      ggplotly(graph) %>%
        layout(autosize = TRUE)
    }else{
      graph <- ggplot(
          data = summaryTable_F4,
          aes(x = interaction(displayText, Type2), y = RentData, fill = displayText,
            label=sprintf("%0.2f", round(RentData, digits = 2)))
        ) + geom_bar(stat = "identity", position = position_dodge(), show.legend = FALSE
        ) + coord_flip(
        ) + xlab(""
        ) + theme_classic(
        ) + ggtitle("Comparison Graph"
        ) + ylab("Affordability"
        ) + scale_y_continuous(limits = c(0, 30)
        ) + guides(fill = FALSE
        ) + geom_text(size = 4, hjust = 1.2, position = position_fill()
        ) + scale_fill_manual(values=c('#E69F00', '#999999')
        )
      ggplotly(graph) %>%
        layout(autosize = TRUE)
    }
  })
  
  output$Download_Input_Comparison <- downloadHandler(
    filename = function() {
      paste("SummaryTable", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(summaryTable, file, row.names = FALSE)
    }
  )
}