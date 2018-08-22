#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------
# Visualise Eurostat asylum decisions
# Step 3 - Visualise the data
#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------
#--0-- install the relevant libraries
# for the joins and lots of other cool things
library(dplyr)
# for loading Json
library(jsonlite)
# for the string manipulation
library(stringr)

# for visualisation
# install.packages("leaflet")
library(leaflet)
library(ggplot2)

# for taking screenshots
#install.packages("mapview")
#webshot::install_phantomjs()
library(mapview)

# for reordering arrays
library(forcats)

# for pretty fonts - see https://cran.r-project.org/web/packages/extrafont/README.html
#install.packages('extrafont')
library(extrafont)
# One time hit
#font_import()
#fonts()
# Lets try to use Trebuchet for the charts
fontsForCharts <- c( "Trebuchet MS" )

# For formatting pretty numbers ...
library(scales)

# to read the png files
library(png)

# to render the images in a grob
library(grid)

# for the final arrangements
library(gridExtra)



#-------------------------------------------------------------------------------------------------------
# Worker functions ...
#-------------------------------------------------------------------------------------------------------

#----- Lookup for the country of origin name
LabelGetCoOName <- 
  function(value) {
    message(str_c(value, "    ", citizenList[value]))
    value <- citizenList[value]
}  




#-------------------------------------------------------------------------------------------------------
# Summary 1 - the total number of decisions by the top 12 countries of asylum and the others grouped together

# Maybe use a log scale here?
plot1 <- ggplot(dcvtTotal, 
    aes(x=fct_reorder(GeoName, CountForOrder, desc=TRUE), 
    # Lets plot in '000s to reduce the number of zeros shown        
    y=(Count),
    label=CountLabel)) +
  geom_bar(stat="identity") +
  # And this is to present the labels ...
  geom_text(hjust=-0.2, size = 4, colour="#505050") + 
  scale_y_continuous(limits=c(0,max(dcvtTotal$Count)*1.15)) +
  coord_flip() +
  labs(
    title="Decisions by country of asylum", 
    y="", 
    x="", 
    #    caption="Source: Eurostat",
    caption="",
    family=fontsForCharts) +
  # set a very minimal theme
  theme_minimal(base_family=fontsForCharts) +   
  # Tweak the axis text
  theme(
      axis.text.x=element_text(family=fontsForCharts, colour="#aaaaaa", size=10), 
      axis.text.y=element_text(family=fontsForCharts, size=12))

  
plot1

#-------------------------------------------------------------------------------------------------------
# Summary 2 - the total number of decisions by the top 12 countries of asylum and top 5 countries of origin
# Maybe use a log scale here?
plot2 <- ggplot(dcvtTotalCitizens, 
      aes(x=fct_reorder(GeoName, CountForOrder, desc=TRUE), 
      y=Count,
      label=CountLabel)) +
  geom_bar(stat="identity") +
  # And this is to present the labels ...
  geom_text(hjust=-0.2, size = 3, colour="#505050") + 
  coord_flip() +
  facet_wrap(~Citizen, labeller=as_labeller(LabelGetCoOName), ncol=5) +
  labs(
    title="Number of decisions by nationality", 
    y="", 
    x="", 
#    caption="Source: Eurostat",
    caption="",
    family=fontsForCharts) +
  # set a very minimal theme
  theme_minimal(base_family=fontsForCharts) +   
  # Tweak the axis text
  theme(
    axis.text.x=element_text(family=fontsForCharts, colour="#aaaaaa", size=7), 
    axis.text.y=element_text(family=fontsForCharts, size=12))

plot2



#-------------------------------------------------------------------------------------------------------
# Summary 3 - the % of each decision type by country and broken out by country of origin
pos <- position_fill(vjust=0.47)

plot3 <- ggplot(dcvDecisions,
    # The x axis is the names of the countries ordered by the overall count                
    aes(x=fct_reorder(GeoName, CountForOrder, desc=TRUE),
    # and the y axis is the percentage based on the variable (with the zeros removed)
    y=Percent,
    # and the labels are the percentage label strings
    label=PercentLabel,    
    # and the fill is the decisions
    fill=DecisionTitle, 
    na.rm=TRUE)) +
  geom_bar(position=pos, stat="identity") +
  geom_text(position=pos, size = 3, colour="#ffffff") + 
  # Then set our colours and legend labels using the parameters of scale_fill_manual
  # note that we strim as needed to avoid the total count
  scale_fill_manual(values=as.vector(decisionList$DecisionLegend[2:5])) +
  # flip the coordinates
  coord_flip() +
  # set the labels
  labs(title="Type of decisions by nationality (%)", 
       y="", 
       x="", 
       fill="", 
       caption="Source: Eurostat", 
       family=fontsForCharts) +
  # set a very minimal theme
  theme_minimal(base_family=fontsForCharts) +   
  # These two lines tweak the positioning of the legend and hide the x axis ticks need to go AFTER the call to theme_minimal
  theme(
    axis.text.x=element_blank(), 
    axis.text.y=element_text(family=fontsForCharts, size=12)) +
  theme(legend.position="bottom" ) +
  facet_wrap(. ~Citizen, labeller=as_labeller(LabelGetCoOName), ncol=5)  
  
plot3



#-------------------------------------------------------------------------------------------------------
#cof <- colorFactor(c("#53a7dd"), domain=c("Total"))

dcvMap <- left_join(allCountries, countryCentroids, by=c("GeoIso"="country") )
# View(dcvMap)

# Get the range of values ...
## Ok would need to logarithm the counts here ... to make them look prettier
#radiusFactor = (pi * 20 * 20)/log10(max(dcvMap$CountForOrder))
radiusFactor = (40)/log10(max(dcvMap$CountForOrder)) 

mapBoxURL <- "https://api.mapbox.com/styles/v1/edgarscrase/cjl1c78tn37v72sofn80jo9en/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiZWRnYXJzY3Jhc2UiLCJhIjoiY2pram90c3M3MWRxdjNxcWhzOXRzY3N6ZCJ9._HQxSBcViAYVr9Bg1OWI_A"
# old https://api.mapbox.com/styles/v1/edgarscrase/cjl1c78tn37v72sofn80jo9en.html?fresh=true&title=true&access_token=pk.eyJ1IjoiZWRnYXJzY3Jhc2UiLCJhIjoiY2pram90c3M3MWRxdjNxcWhzOXRzY3N6ZCJ9._HQxSBcViAYVr9Bg1OWI_A#3.4/57.750060/17.037912/0
# mapbox://styles/edgarscrase/cjl1c78tn37v72sofn80jo9en

# Try using leaflet but actually this is not going to look that clean!
m <- leaflet(dcvMap)  %>% 
  addTiles(urlTemplate = mapBoxURL,
           attribution="MapBox") %>%
  setView(5, 50, zoom = 4) %>% 
  addCircleMarkers(~longitude, ~latitude, popup=dcvMap$name, weight = 3, radius=round(radiusFactor * log10(dcvMap$CountForOrder),0), 
                   color="#e77b37", stroke = F, fillOpacity = 0.5) 

# Magic.  we got there finally - after 750 lines of code - lets take a screenshot and print out the map using the MapView library
mapshot(m,file="MapScreenShot.png")
# and then read it back in.
mapImage <- readPNG("ScreenShot.png")

m

# Add the map image to another plot
plotMap <- qplot(1,1) + annotation_custom(rasterGrob(mapImage)) +
  labs(title="Number of decisions on asylum applications in Europe by country", 
     y="", 
     x="", 
     fill="", 
     caption="", 
     family=fontsForCharts) +
  # set a very minimal theme
  theme_minimal(base_family=fontsForCharts) +   
  # These two lines tweak the positioning of the legend and hide the x axis ticks need to go AFTER the call to theme_minimal
  theme(
    axis.ticks = element_blank(),  
    axis.line = element_blank(),
    axis.text.x=element_blank(), 
    axis.text.y=element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )


plotMap



#-------------------------------------------------------------------------------------------------------
# Finally - output - lets bring it all together!

# Try summary chart and map and then the two detailed charts
firstRow <- grid.arrange(plot1, plotMap, ncol=2)
grid.arrange(firstRow, plot2, plot3, nrow=3)

# Other alternatives
#chartsCol <- grid.arrange(plot1, plot2, plot3, nrow=3)
#grid.arrange(plotMap, chartsCol, ncol=2)

#grid.arrange(plot1, plotMap, plot2, plot3, nrow=4)

