#-------------------------------------------------------------------------------------------------------
# Protection rates of refugees in Europe
#-------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------
# Libraries that are required ....

# for the joins and lots of other cool things
library(dplyr)      
# for loading Json
library(jsonlite)
# for the string manipulation
library(stringr)
# For formatting pretty numbers
library(scales)
# for reordering arrays
library(forcats)

# for pretty fonts - see https://cran.r-project.org/web/packages/extrafont/README.html
#install.packages('extrafont')
library(extrafont)
# Run this once to load in all the available fonts (this will take a few mins) and restart R afterwards 
#font_import()
#fonts() # Check which fonts are now available
# Lets try to use Trebuchet for the charts
fontsForCharts <- c( "Trebuchet MS" )


# for visualisation on maps and in plots
library(leaflet)
library(ggplot2)

# for taking screenshots of the map - note that you also need to install phantomjs
#install.packages("mapview")
#webshot::install_phantomjs()
library(mapview)
# to read the png files
library(png)


# to render the images as graphical object to display in an arrangement
library(grid)
# for the final arrangements
library(gridExtra)


#-------------------------------------------------------------------------------------------------------
#-----Step 1----- Load the country spatial data and load the Asylum decision data from EuroStat
source("04_Scripts/LoadData.R")

#-----Step 2----- Transform the data 
source("04_Scripts/TransformData.R")

#-----Step 3----- Visualise the data 
source("04_Scripts/VisualiseData.R")







