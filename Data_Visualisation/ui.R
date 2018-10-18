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

dashboardPage(
  skin = "black",
  dashboardHeader(title = "Housing Affordability of Great Britain for Benefit Claimants (2015-2018)",
                  titleWidth = 650),
  dashboardSidebar(
    wellPanel(
      helpText("This interactive app explores affordability change for housing benefit claimants since 2015, 
               defined as the percentage of the private rented sector that is avaliable to claimants without a shortfall."),
      helpText("Use the Year slider and the Room Entitlement dropdown to refresh data on any tab.
               Further controls can be used within the indivudual tabs.")),
    wellPanel(style = "background-color: #999999;",
              selectInput("Room_Input", 
                          "Room Entitlement", 
                          choices = c("SAR", "BED1", "BED2", "BED3", "BED4"),
                          selected = "BED2"),
              sliderInput("Year_Input",
                          "Year", 
                          min = 2015, 
                          max = 2018, 
                          value = 2018, 
                          step = 1,
                          animate = TRUE,
                          ticks = FALSE,
                          sep = "")),
    downloadButton("Download_Input",
                   "Rent Data")),
  dashboardBody(
    tabsetPanel(
      tabPanel(
        "Map",
        fluidRow(
          box(
            width = 3,
            selectInput("BRMA_Input",
                        "BRMA",
                        choices = RentDataTable$BRMA),
            radioButtons("Urban_Input",
                         "Urban/Rural Filter",
                         choices = c("None", "Urban", "Rural"),
                         selected = "None",
                         inline = FALSE),
            radioButtons("Coastal_Input",
                         "Coastal/Inland Filter",
                         choices = c("None", "Coastal", "Inland"),
                         selected = "None",
                         inline = FALSE),
            radioButtons("Country_Input",
                         "Country Filter",
                         choices = c("None", "England", "Scotland", "Wales"),
                         selected = "None",
                         inline = FALSE),
            actionButton("Reset_Input",
                         "Reset Map View"),
            checkboxInput("City_Input",
                          "Cities and towns with pop<100,000"),
            height = 602),
          box(
            width = 9,
            leafletOutput("affordabilityMap", height = 580)))),
      tabPanel(
        "Graph",
        fluidRow(
          box(
            width = 12,
            plotlyOutput("affordabilityGraphSummary", height = 250)),
          box(
            width = 12,
            plotlyOutput("affordabilityGraph", height = 3000)))),
      tabPanel(
        "Table",
        fluidRow(
          box(
            width = 12,
            dataTableOutput("affordabilityTable", height = 500)))),
      tabPanel(
        "Summary",
        fluidRow(
          box(
            width = 12,
            checkboxInput("Reorder_Input",
                          "Reorder by Rent Data"),
            helpText("The graph below allows you to compare affordability of different types of geographies
                     across years and rent sizes."),
            helpText("Use the left hand controls to adjust the grey bars and the 
                     right hand controls to adjust the yellow bars. To automatically reorder the bars based 
                     on affordability, tick the box above.")),
          box(
            width = 10,
            plotlyOutput("summaryGraph", height = 400)),
          box(
            width = 2,
            helpText("Use these controls to adjust the yellow bars, 
                     to compare affordability across different geography types."),
            wellPanel(
              style = "background-color: #E69F00;",
              selectInput("Room_Input_Comparison", 
                          "Room", 
                          choices = c("SAR", "BED1", "BED2", "BED3", "BED4"),
                          selected = "BED2"),
              sliderInput("Year_Input_Comparison",
                          "Year", 
                          min = 2015, 
                          max = 2018, 
                          value = 2015, 
                          step = 1,
                          animate = TRUE,
                          ticks = FALSE,
                          sep = "")),
            downloadButton("Download_Input_Comparison",
                           "Summary Table")))),
      tabPanel(
        "Data",
        box(
          width = 12,
          helpText("Rent data for this dashboard was collected by the Rent Officers of England, Scotland and Wales,
                   and they can be downloaded from these pages:"),
          helpText(a("Rent Officer Data: England", 
                     href = "https://www.gov.uk/government/publications/local-housing-allowance-lha-rates-applicable-from-april-2015-march-2016", 
                     target = "_blank")),
          helpText(a("Rent Officer Data: Scotland", 
                     href = "https://beta.gov.scot/publications/local-housing-allowance-rates-2018-2019", 
                     target = "_blank")),
          helpText(a("Rent Officer Data: Wales",
                     href = "https://gov.wales/topics/housing-and-regeneration/housing-supply/renting/rent-officers-wales/how-local-housing-allowance-rates-are-set/local-housing-allowance-rates/?lang=en",
                     target = "_blank")),
          helpText("The geographical units are Broad Rental Market Areas (BRMA), which are described here:"),
          helpText(a("BRMA descriptions",
                     href = "https://data.gov.uk/dataset/fa698b77-3fd2-4643-ba4c-d2165613aa15/broad-rental-market-areas-brma",
                     target = "_blank")),
          helpText("BRMA shapefiles can be downloaded from here:"),
          helpText(a("England shapefile", 
                     href = "http://datashare.is.ed.ac.uk/download/DS_10283_2613.zip", 
                     target = "_blank")),
          helpText(a("Scotland shapefile", 
                     href = "http://datashare.is.ed.ac.uk/download/DS_10283_2618.zip", 
                     target = "_blank")),
          helpText(a("Wales shapefile",
                     href = "http://datashare.is.ed.ac.uk/download/DS_10283_2615.zip",
                     target = "_blank")),
          helpText("Known problems with the input data:"),
          helpText("   1. Scotland shapefile does not contain West Dunbartonshire BRMA"),
          helpText("   2. West Chesire BRMA appears in both England and Scotland shapefiles, but they are not topologically correct"),
          helpText("   3. The shapefiles are not topologically correct with each other, therefore there are overlaps and gaps along the English-Scottish and English-Welsh borders.")
    )))))