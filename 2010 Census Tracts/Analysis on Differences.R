load("./Comorbidity_df.RData")
library(data.table)
library(stringr)
library(leaflet)
library(sf)
library(htmltools)
library(dplyr)

#Just how good is this Severity Index at predicting where the deaths are?
#To answer this question, we need to transform the severity index upwards to the zipcodes, where the fatality data is recorded

# Crosswalk between census tract and zcta
URL_ZCTAtoCT <- "https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_tract_rel_10.txt?#"
ZCTAtoCT_df <- fread(URL_ZCTAtoCT)
ZNYC <- ZCTAtoCT_df[STATE=="36" & (COUNTY=="05" | COUNTY=="5" | COUNTY=="47" | COUNTY=="81" | COUNTY=="85" | COUNTY=="61")]

# Get fatality data from DOHMH
URL_Fatality<- "https://raw.githubusercontent.com/nychealth/coronavirus-data/master/data-by-modzcta.csv"
Fatality_df <- fread(URL_Fatality)

# Get MZCTA conversion data from DOHMH
URL_ZCTAtoMZCTA <- "https://raw.githubusercontent.com/nychealth/coronavirus-data/master/Geography-resources/ZCTA-to-MODZCTA.csv"
ZCTAtoMZCTA_df <- fread(URL_ZCTAtoMZCTA)

# Merge Fatality Data and MODZCTA with ZNYC
ZNYC[,"MODIFIED_ZCTA" := .(sapply(ZCTA5, function(x) ZCTAtoMZCTA_df[ZCTA == x,MODZCTA]))]
ZNYC_Fatality <- merge(ZNYC, Fatality_df,all.x = TRUE)

# Convert boroughs to the ones we have
ZNYC_Fatality[,boro_code := stringr::str_sub(GEOID,-8,-7)] #Extract the boro code from GEOID
ZNYC_Fatality[,TRACT := as.character(TRACT/100)]
ZNYC_Fatality[TRACT == "170.1",TRACT := "170.10"] #Census Tract 170.1 is coded as '170.10' in our shapefile

Convertboro <- data.table(Census_boro_code = c('05','47','81','85','61'), 
                          boro_code=c(2,3,4,5,1), 
                          boro_name = c("Bronx","Brooklyn","Queens","Staten Island","Manhattan"))

ZNYC_Fatality[,c("boro_code","boro_name") := .(sapply(boro_code, function(x) Convertboro[Census_boro_code == x,boro_code]),
                                               sapply(boro_code, function(x) Convertboro[Census_boro_code == x,boro_name]))][
                                                 ,name := paste0('Census Tract ',TRACT,', ',boro_name,' Boro')]

# Create the percentage variables
## County and Tract Population
ZNYC_Fatality[, `:=` (COUNTYTRACTPOP = sum(POPPT)), by = c("COUNTY","TRACT")]
### Percentage of Population in Census Tract accounted for in the County
ZNYC_Fatality[, `:=` (PERCENT_COUNTYTRACT = POPPT/COUNTYTRACTPOP)]

## Zip and County Population
### ZCTA5 Population
ZNYC_Fatality[, `:=` (ZCTAPOP = sum(POPPT)), by = "ZCTA5"]

## Modified Zip and Zip Population
### Modified Zip Population
ZNYC_Fatality[, `:=` (MODIFIEDZCTAPOP = sum(POPPT)), by = "MODIFIED_ZCTA"]
### Population of Zip / Modified Zip
ZNYC_Fatality[, PERCENT_MODZZCTA := (ZCTAPOP)/MODIFIEDZCTAPOP]

# Merge severity index variable with Zip and Fatality data
ZNYC_Fatality <- merge(x = ZNYC_Fatality, y = Comorbidities_df[ , c("name", "severity_index","severity_index_norm")], by = "name", all.x=TRUE)

# Create weighted SI by MZCTA
## Multiply SI with POPPT and COUNTYTRACTPOP
ZNYC_Fatality[, RAW_MZCTA_SI := severity_index_norm * POPPT]
## Divide by MODIFIEDZCTAPOP and sum them up for a weighted average SI by MODIFIED_ZCTA
ZNYC_Fatality[,`:=` (WEIGHTED_MZCTA_SI = sum(RAW_MZCTA_SI,na.rm=T)/MODIFIEDZCTAPOP),by = "MODIFIED_ZCTA"]

#######
# Plot it!
library(plotly)
MZCTA_df <- unique(ZNYC_Fatality, by = c("COVID_DEATH_RATE","WEIGHTED_MZCTA_SI"))


## SI vs. COVID Death Rate
plotly_labels <- paste("<b>",paste0("Modified ZIP: ",MZCTA_df$MODIFIED_ZCTA),"</b><br>",
                       "<b>Neighborhood:",MZCTA_df$NEIGHBORHOOD_NAME,"</b><br>",
                       "<b>SI:</b>",round(MZCTA_df$WEIGHTED_MZCTA_SI,2),"<br>",
                       "<b>DEATH RATE:</b>",MZCTA_df$COVID_DEATH_RATE)


fig <- plot_ly(data = MZCTA_df,
               x = ~WEIGHTED_MZCTA_SI,
               y = ~COVID_DEATH_RATE,
               type = 'scatter',
               mode = 'markers',
               name = "",
               hoverinfo = 'text',
               text = lapply(plotly_labels,HTML)) %>%
  layout(
    title = "Severity Index vs. NYC Death Rates",
    xaxis = list(title = "Severity Index"),
    yaxis = list(title = "COVID Death Rate (per 100,000)"))

fig


lm_fit <- lm(COVID_DEATH_RATE~WEIGHTED_MZCTA_SI, data = MZCTA_df)

summary(lm_fit)

fig %>%
  add_trace(x = ~WEIGHTED_MZCTA_SI, y = fitted(lm_fit), mode = "lines") %>%
  layout(showlegend = F)


#####
# Map of areas with highest fatalities
URL_MZCTA <- "./MZCTA_2010"
MZCTA_shp <- st_read(URL_MZCTA) %>%
  st_transform("+proj=longlat +datum=WGS84")

MZCTA_sf <- st_sf(merge(MZCTA_df[,MODIFIED_ZCTA := as.factor(MODIFIED_ZCTA)],
                        MZCTA_shp,
                        by.x = "MODIFIED_ZCTA",
                        by.y = "MODZCTA"))

# Consider breaking the SIs and Death Rates into 9 bins, how well do the SIs match up quantile-wise?
bins_MODSI <- round(c(0,quantile(MZCTA_sf$WEIGHTED_MZCTA_SI, 0.15, quantile(MZCTA_sf$WEIGHTED_MZCTA_SI, 0.30, na.rm = T),na.rm = T),quantile(MZCTA_sf$WEIGHTED_MZCTA_SI, 0.40, na.rm = T),quantile(MZCTA_sf$WEIGHTED_MZCTA_SI, 0.50, na.rm = T),quantile(MZCTA_sf$WEIGHTED_MZCTA_SI, 0.60, na.rm = T),quantile(MZCTA_sf$WEIGHTED_MZCTA_SI, 0.70, na.rm = T),quantile(MZCTA_sf$WEIGHTED_MZCTA_SI, 0.80, na.rm = T),quantile(MZCTA_sf$WEIGHTED_MZCTA_SI, 0.90, na.rm = T),quantile(MZCTA_sf$WEIGHTED_MZCTA_SI, 0.99, na.rm = T), max(MZCTA_sf$WEIGHTED_MZCTA_SI, na.rm = T) + 1),4)
bins_COVIDDEATH <- round(c(0,quantile(MZCTA_sf$COVID_DEATH_RATE, 0.15, quantile(MZCTA_sf$COVID_DEATH_RATE, 0.30, na.rm = T),na.rm = T),quantile(MZCTA_sf$COVID_DEATH_RATE, 0.40, na.rm = T),quantile(MZCTA_sf$COVID_DEATH_RATE, 0.50, na.rm = T),quantile(MZCTA_sf$COVID_DEATH_RATE, 0.60, na.rm = T),quantile(MZCTA_sf$COVID_DEATH_RATE, 0.70, na.rm = T),quantile(MZCTA_sf$COVID_DEATH_RATE, 0.80, na.rm = T),quantile(MZCTA_sf$COVID_DEATH_RATE, 0.90, na.rm = T),quantile(MZCTA_sf$COVID_DEATH_RATE, 0.99, na.rm = T), max(MZCTA_sf$COVID_DEATH_RATE, na.rm = T) + 1),4)

cuts_MODSI <- cut(MZCTA_sf$WEIGHTED_MZCTA_SI,
                  bins_MODSI, 
                  labels = c(1,2,3,4,5,6,7,8,9))
cuts_COVIDDEATH <- cut(MZCTA_sf$COVID_DEATH_RATE,
                       bins_COVIDDEATH, 
                       include.lowest=TRUE,
                       labels = c(1,2,3,4,5,6,7,8,9))

cuts_diff <- abs(as.numeric(cuts_MODSI) - as.numeric(cuts_COVIDDEATH)) + 1

#test <- data.table(MZCTA_sf$MODIFIED_ZCTA,cuts_diff,cuts_COVIDDEATH,cuts_MODSI,MZCTA_sf$WEIGHTED_MZCTA_SI,MZCTA_sf$COVID_DEATH_RATE,
#                  SI_Norm = scale(MZCTA_sf$WEIGHTED_MZCTA_SI), C_Norm = scale(MZCTA_sf$COVID_DEATH_RATE), Diff_Norm = abs(scale(MZCTA_sf$WEIGHTED_MZCTA_SI) - scale(MZCTA_sf$COVID_DEATH_RATE)),
#                  standard_Diff = abs(scale(MZCTA_sf$WEIGHTED_MZCTA_SI - MZCTA_sf$COVID_DEATH_RATE)))

#test_lm_resid <- abs(resid(lm_fit))

#bins_diff <- round(c(0,quantile(cuts_diff, 0.5, na.rm = T),quantile(cuts_diff, 0.99, na.rm = T),max(cuts_diff, na.rm = T) + 1))
#bins_diff <- round(c(0,quantile(test$Diff_Norm.V1, 0.5, na.rm = T),quantile(test$Diff_Norm.V1, 0.99, na.rm = T),max(test$Diff_Norm.V1, na.rm = T) + 1))
#bins_diff <- round(c(0,quantile(test$standard_Diff.V1, 0.5, na.rm = T),quantile(test$standard_Diff.V1, 0.99, na.rm = T),max(test$standard_Diff.V1, na.rm = T) + 1))
#bins_diff <- round(c(0,quantile(test_lm_resid, 0.50, na.rm = T),quantile(test_lm_resid, 0.95, na.rm = T),max(test_lm_resid, na.rm = T) + 1),4)

# Create Labels and color bins for map
labels_MOD<- paste("<h3>",paste0("Neighborhood Name: ",MZCTA_sf$NEIGHBORHOOD_NAME),"</h3>",
                   "<h3>","Modified Zip: ",MZCTA_sf$MODIFIED_ZCTA,"</h3>",
                   "<p>","Modified SI: ",format(MZCTA_sf$WEIGHTED_MZCTA_SI,nsmall = 0, big.mark = ","),"</p>",
                   "<p>","COVID Death Rate (per 100,000): ",format(MZCTA_sf$COVID_DEATH_RATE,nsmall = 0,big.mark = ","),"</p>")

labels_Diff<- paste("<h3>",paste0("Neighborhood Name: ",MZCTA_sf$NEIGHBORHOOD_NAME),"</h3>",
                    "<h3>","Modified Zip: ",MZCTA_sf$MODIFIED_ZCTA,"</h3>",
                    "<p>","Modified SI: ",format(MZCTA_sf$WEIGHTED_MZCTA_SI,nsmall = 0, big.mark = ","),"</p>",
                    "<p>","COVID Death Rate (per 100,000): ",format(MZCTA_sf$COVID_DEATH_RATE,nsmall = 0,big.mark = ","),"</p>",
                    "<p>","Quantile Difference: ",format(cuts_diff,nsmall = 0, big.mark = ","),"</p>")

pal_MODSI <- colorBin("YlOrRd", domain = MZCTA_sf$WEIGHTED_MZCTA_SI, bins = bins_MODSI)
pal_COVIDDEATH <- colorBin("YlOrRd", domain = MZCTA_sf$COVID_DEATH_RATE, bins = bins_COVIDDEATH)
pal_diff <- colorNumeric("RdPu", domain = cuts_diff)

# Map it!
leaflet(MZCTA_shp, options = leafletOptions(zoomSnap = 0.5, 
                                            zoomDelta = 0.5)) %>%
  setView(-73.935242,40.730610,10.5) %>%
  addProviderTiles("CartoDB.Positron") %>%
  #Polygon Layers
  addPolygons(data = MZCTA_sf,
              weight = 1,
              color = "grey",
              group = "Differences",
              fillOpacity = 1,
              fillColor = pal_diff(cuts_diff),
              label = lapply(labels_Diff,HTML)
  ) %>%
  addPolygons(data = MZCTA_sf,
              weight = 1,
              color = "grey",
              group = "Modified SI",
              fillColor = pal_MODSI(MZCTA_sf$WEIGHTED_MZCTA_SI),
              label = lapply(labels_MOD,HTML)) %>%
  addPolygons(data = MZCTA_sf,
              weight = 1,
              color = "grey",
              group = "COVID19 Death Rate",
              fillColor = pal_COVIDDEATH(MZCTA_sf$COVID_DEATH_RATE),
              label = lapply(labels_MOD,HTML)) %>%
  #Legend Layers
  addLegend("bottomleft", pal = pal_diff, values = ~cuts_diff,
            group = "Differences",
            title = "Accuracy",
            opacity = 0.7,
            labFormat =  function(type, cuts, p) {
              n = length(cuts) 	
              cuts[n] = "way off" 	
              for (i in 2:(n-1)){cuts[i] = ""} 	
              cuts[1] = "on point" 	
              paste0(cuts[-n], cuts[-1])}) %>%
  addLegend("bottomright", pal = pal_MODSI, values = ~MZCTA_sf$WEIGHTED_MZCTA_SI,
            group = "Modified SI",
            title = "Severity",
            opacity = 0.7,
            labFormat =  function(type, cuts, p) {
              n = length(cuts) 	
              cuts[n] = "High" 	
              for (i in 2:(n-1)){cuts[i] = ""} 	
              cuts[1] = "Low" 	
              paste0(cuts[-n], cuts[-1])}) %>%
  #Layers Control
  addLayersControl(
    baseGroups =c("COVID19 Death Rate", "Modified SI","Differences"),
    options = layersControlOptions(collapsed=FALSE),
    position = "topright"
  )