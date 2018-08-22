#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------
# Visualise Eurostat asylum decisions
# Step 2 - Transform the data
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
# for reordering arrays
library(forcats)
# For formatting pretty numbers ...
library(scales)


#-------------------------------------------------------------------------------------------------------
# And read the data cube back out again
dataCube <- read.csv2(str_c("EuroStatsData_", jsonTimePeriod, ".csv"))
# View(dataCube)


#-------------------------------------------------------------------------------------------------------
# Worker functions ...
#-------------------------------------------------------------------------------------------------------

#----- Generates a percentage
GetPercent <- 
  function(enum, denom, rounding) {
    pc <- 0
    rounding <- as.numeric(rounding)
    enum <- as.numeric(enum)
    denom <- as.numeric(denom)
    
    if(enum > 0 && denom > 0) {
      pc <- round(enum/denom*100,rounding)
    }
    
    returnValue <- as.numeric(pc)
}

#----- Percent label creator for the stacked bar chart
GetPercentLabel <- 
  function(pc, threshold) {

    pc <- as.numeric(pc)
    threshold <- as.numeric(threshold)

    percStr <- ifelse(
      (is.na(pc) == FALSE && pc >= threshold),
      str_c(as.character(pc), "%"),
      ""
    )
    
    #    message(str_c("\nEnum: ", enum, " Denom: ", denom, " Threshold:", threshold, " Percent:", pc, " Str:", percStr))
    returnValue <- percStr
}  







#-------------------------------------------------------------------------------------------------------
# Lets build the dataCube that will support our map and charts ...
dataCubeVis <- dataCube
dataCubeVis[is.na(dataCubeVis)] <- 0

# lets filter down to the 12 most common countries and then group all the others into an other category...
allCountries <- dataCubeVis %>% 
  filter(Citizen == "TOTAL" ) %>%
  filter(Decision == "TOTAL" ) %>%  
  group_by(GeoIso) %>%
  summarise(CountForOrder=sum(Count)) %>%
  ungroup() %>%
  arrange(desc(CountForOrder)) 
#View(allCountries)

# Get the counts of most common and others ...
totalCount <- sum( allCountries$CountForOrder)
# Here is our sliced data
mostCommonCountries <- allCountries[1:12,]
mostCommonCount <- sum( mostCommonCountries$CountForOrder)
otherCount <- totalCount - mostCommonCount

dcvSummary <- rbind( mostCommonCountries, data.frame(GeoIso="Other", CountForOrder=otherCount))

# Check it
#View(dcvSummary)  


#-------------------------------------------------------------------------------------------------------
# Now lets create our actual data cube
# Lets join it to the most common countries to pull accross the CountForOrder col which will be NA fo other countres
dcvt <- left_join(dataCubeVis, mostCommonCountries, by=c("GeoIso"="GeoIso") )
# Important - ensure that the levels of the Citizen data are consistent with the order in the citizen list...
levels(dcvt$Citizen) <- citizenList

# Then we split our dataCube into two by filtering the other countries and set the GeoIso col and a new GeoName col to 0
# We also want to reset the CountForOrder as we want other to appear at the end...
dataCubeTemp <- filter(dcvt, is.na(CountForOrder)) %>% 
  mutate(GeoIso="Other", GeoName="Other", CountForOrder=0)

dcvt <- filter(dcvt, is.na(CountForOrder) == FALSE)  %>% mutate(GeoName=name)
# Then we join it back together again
dcvt <- rbind( dcvt, dataCubeTemp)
# View(dcvt)
# Collapse all the "other" rows by grouping the data
dcvt <- dcvt %>% 
  group_by(GeoIso, GeoName, Citizen, Decision) %>%
  summarise(Count=sum(Count), CountForOrder=max(CountForOrder)) %>%
  ungroup() %>%
  arrange(desc(CountForOrder))
# Double check that the names are gucci  
names(dcvt) <- c("GeoIso", "GeoName", "Citizen","Decision","Count", "CountForOrder")

# Filter the data summary to include just the total counts
dcvtTotal <- dcvt %>% 
  filter(Citizen == "TOTAL") %>% 
  filter(Decision == "TOTAL")
# Filter the data summary to include just the total counts
dcvtTotalCitizens <- dcvt %>% 
  filter(Citizen != "TOTAL") %>% 
  filter(Decision == "TOTAL")

# And for the detailed views, remove the citizens and decisions total, which is not relevant for these charts
dcvtDetails <- dcvt %>% 
  filter(Citizen != "TOTAL") %>% 
  filter(Decision != "TOTAL")

#levels(dcvtDetails$Citizen) <- citizenList

# We're going to try to show a few totals on the chart directly, so lets create a well formatted total
dcvtTotal <- dcvtTotal %>% mutate(CountLabel=comma(Count))
dcvtTotalCitizens <- dcvtTotalCitizens %>% mutate(CountLabel=comma(Count))

# Check them
#View(dcvtTotal)
#View(dcvtTotalCitizens)
#View(dcvtDetails)



#-------------------------------------------------------------------------------------------------------
# Summary 3 - the % of each decision type by country and broken out by country of origin

# Lets pull across the proper name for the Decisions from the decisionList data fram
dcvDecisions <- left_join(dcvtDetails, decisionList, by=c("Decision"="DecisionKey")) 
# And lets remove the totals as they are not necessary for this view
dcvDecisions <- filter(dcvDecisions, Decision != "TOTAL")

# This creates the percent and the percent label columns - it looks a little intense, 
dcvDecisions <- dcvDecisions %>% 
  group_by(GeoIso, Citizen) %>% 
  # okay, we've got the total, now we can do some math with mutate
  mutate( GeoCitizenTotal=sum(Count, na.rm=T)) %>%    
  ungroup() %>%    
  group_by(GeoIso, Citizen, Decision) %>% 
  # okay, now lets also create a label for all columns
  mutate(
    Percent=GetPercent(Count, GeoCitizenTotal, 0), 
    PercentLabel=GetPercentLabel(Percent, 15.0)) %>% 
  ungroup()

# Then strip out all the zeros - this is probably not necessary, but they will also not be shown..
dcvDecisions <- filter(dcvDecisions, Percent > 0 )
#warnings()  
# Good to have a quick look at the data here
#View(dcvDecisions)



# This is a good point to save this data cube - so we can get back to it if needed ...
write.csv2(dcvtTotal, str_c("02_OutputData/Summary_Totals.csv"))
# This is a good point to save this data cube - so we can get back to it if needed ...
write.csv2(dcvtTotalCitizens, str_c("02_OutputData/Summary_Totals_by_CountryOfOrigin.csv"))
# This is a good point to save this data cube - so we can get back to it if needed ...
write.csv2(dcvDecisions, str_c("02_OutputData/Summary_Decisions.csv"))



