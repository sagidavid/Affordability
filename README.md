# Housing Affordability for Benefit Claimants App

## Introduction

This repository was made for the app that I have developed for the GDS Data Science Accelerator.

The app is hosted here:

https://davidsagi.shinyapps.io/Affordability/

An overview of the topic can be accessed from here:

LINK TO GOOGLE SLIDES!

A description of the GDS Data Science Accelerator can be found here:

https://www.gov.uk/government/publications/data-science-accelerator-programme/introduction-to-the-data-science-accelerator

## Repository Structure

The repository is divided into a Data Analysis and a Data Visualisation folder.

The Data Analysis folder contains all the input files (geogrpahies and rent data) and the code that formats it prior to visualisation.

The Data Visualisation folder only contains code that is neccessary for the Shiny app to run.

NOTE: When reproducing the code Data Analysis has to be run before Data Visualisation

### Data Analysis

This folder contains the code that processes and formats the data that is stored in the Input_Geographies and Input_Rent Data folders.

The code needs to run from the work directory and input data have to be stored in the folders named as above.

The main steps in the code are:
__1. Merging datsets__ (spatial with spatial, and spatial with rent data)
__2. Spatial analysis__ (defining Urban/Rural, Inland/Coastal and English/Scottish/Welsh geographies based on spatial intersections)
__3. Creating summary tables__(calculating aveage rent values for different geography types)

### Data Visualisation
