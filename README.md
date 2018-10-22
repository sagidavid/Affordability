 # Housing Affordability for Benefit Claimants App

## Overview

This repository was made for the app that I have developed for the GDS Data Science Accelerator.

The app is hosted here:

https://davidsagi.shinyapps.io/Affordability/

An overview of the topic can be accessed from here:

__LINK TO GOOGLE SLIDES, PUT A VERSION OF THE APPPLICATION ON GOOGLE DOCS AND ADD HERE!__

A description of the GDS Data Science Accelerator can be found here:

https://www.gov.uk/government/publications/data-science-accelerator-programme/introduction-to-the-data-science-accelerator

## Repository Structure

The repository is divided into a _Data Analysis_ and a _Data Visualisation_ folder.

The _Data Analysis_ folder contains all the input files (geographies and rent data) and the code needed prior to visualisation.

The _Data Visualisation_ folder only contains code that is necessary for the __Shiny__ app to run.

NOTE: When reproducing the code, _Data Analysis_ has to be run before _Data Visualisation_.

### Data Analysis

This folder contains the code that processes and formats the data that is stored in the  
_Input_Geographies_ and _Input_Rent Data_ folders.

The code needs to run from the set work directory and input data have to be stored in the folders named as above.

The main steps in the code are:  
  
__1. Merging datasets__ (spatial with spatial, and spatial with rent data)  
  
__2. Spatial analysis__ (defining Urban/Rural, Inland/Coastal and English/Scottish/Welsh geographies) 
  
__3. Creating summary tables__ (calculating average rent values for different geography types)  

The code outputs files into two folders: _Prepared_Data_ throughout the code and _Deploy_ in one block at the end.  

Files in the _Deploy_ folder are needed to run the __Shiny__ app in the _Data Visualisation_ folder. 

### Data Visualisation

This folder contains the codes and input data that run with the __Shiny__ app. It can run directly from R Studio.
