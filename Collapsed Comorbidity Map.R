library(data.table)
library(stringr)

CDClink <- "https://chronicdata.cdc.gov/resource/6vp6-wxuq.csv?$limit=99999999999"

CDCComorbidity <- fread(CDClink)

NYComorbidities <- CDCComorbidity[statedesc == "New York" & cityname == "New York"]

unique(NYComorbidities[,year]) #Look at the years we have
NYC2016 <- NYComorbidities[year == 2016] #look at the data differences
NYC2017 <- NYComorbidities[year == 2017]

#look at the categories we're interested in
unique(NYC2016[,short_question_text]) 
unique(NYC2017[,short_question_text]) #bingo. This is the dataset we need

#explore the different comorbidities in the dataset
NYCCOPD <- NYC2017[short_question_text == "COPD"]
NYCDiabetes <- NYC2017[short_question_text == "Diabetes"]
NYCCancer <- NYC2017[short_question_text == "Cancer (except skin)"]
NYCCHD <- NYC2017[short_question_text == "Coronary Heart Disease"]
NYCHyper <- NYC2017[short_question_text == "High Blood Pressure"]
NYCCKD <- NYC2017[short_question_text == "Chronic Kidney Disease"]
NYCStroke <- NYC2017[short_question_text == "Stroke"]

#combining all comorbidities into one file
Comorbidities_df <- merge(NYCCOPD[,c("COPD_values","COPD_low_CL","COPD_high_CL") := .(data_value,low_confidence_limit,high_confidence_limit)][,list(tractfips,COPD_values,COPD_low_CL,COPD_high_CL)],
                          NYCDiabetes[,c("Diabetes_values","Diabetes_low_CL","Diabetes_high_CL") := .(data_value,low_confidence_limit,high_confidence_limit)][,list(tractfips,Diabetes_values,Diabetes_low_CL,Diabetes_high_CL)],
                          by = "tractfips")
Comorbidities_df <- Comorbidities_df[NYCCancer[,c("Cancer_values","Cancer_low_CL","Cancer_high_CL") := .(data_value,low_confidence_limit,high_confidence_limit)][,list(tractfips,Cancer_values,Cancer_low_CL,Cancer_high_CL)],on = "tractfips"]
Comorbidities_df <- Comorbidities_df[NYCHyper[,c("Hyper_values","Hyper_low_CL","Hyper_high_CL") := .(data_value,low_confidence_limit,high_confidence_limit)][,list(tractfips,Hyper_values,Hyper_low_CL,Hyper_high_CL)],on = "tractfips"]
Comorbidities_df <- Comorbidities_df[NYCCKD[,c("CKD_values","CKD_low_CL","CKD_high_CL") := .(data_value,low_confidence_limit,high_confidence_limit)][,list(tractfips,CKD_values,CKD_low_CL,CKD_high_CL)],on = "tractfips"]
Comorbidities_df <- Comorbidities_df[NYCStroke[,c("Stroke_values","Stroke_low_CL","Stroke_high_CL") := .(data_value,low_confidence_limit,high_confidence_limit)][,list(tractfips,Stroke_values,Stroke_low_CL,Stroke_high_CL)],on = "tractfips"]
Comorbidities_df <- Comorbidities_df[NYCCHD[,c("CHD_values","CHD_low_CL","CHD_high_CL") := .(data_value,low_confidence_limit,high_confidence_limit)][,list(tractfips,populationcount,CHD_values,CHD_low_CL,CHD_high_CL)],on = "tractfips"][tractfips != 0]

#data transformation for easier graphing
Comorbidities_df[,tract := as.character(as.numeric(stringr::str_sub(tractfips,-6,-1))/100)] #extract census tract number from tractfips
Comorbidities_df[,boro_code := stringr::str_sub(tractfips,-8,-7)]

Comorbidities_df[tract == "170.1",tract := "170.10"] #Census Tract 170.1 is coded as '170.10' in our shapefile

Convertboro <- data.table(Census_boro_code = c('05','47','81','85','61'), 
                          boro_code=c(2,3,4,5,1), 
                          boro_name = c("Bronx","Brooklyn","Queens","Staten Island","Manhattan"))

Comorbidities_df[,c("boro_code","boro_name") := .(lapply(boro_code, function(x) Convertboro[Census_boro_code == x,boro_code]),
                                         lapply(boro_code, function(x) Convertboro[Census_boro_code == x,boro_name]))][
                                           ,name := paste0('Census Tract ',tract,', ',boro_name,' Boro')]


#HARDCODE NYS FATALITY DATA: https://covid19tracker.health.ny.gov/views/NYS-COVID19-Tracker/NYSDOHCOVID-19Tracker-Fatalities?%3Aembed=yes&%3Atoolbar=no&%3Atabs=n
NYS_Co_Deaths <- data.table(Deaths = 22170,
                            Hyper_Death = 12060,
                            Diabetes_Death = 7966,
                            Hyperlipidemia_Death = 4669,
                            CHD_Death = 2694,
                            Dementia = 2822,
                            CKD_Death = 2348,
                            COPD_Death = 1989,
                            Atrial_Death = 1680,
                            Cancer_Death = 1617,
                            Stroke_Death = 1460)[,c("Hyper_weight", "Diabetes_weight", "CHD_weight", "CKD_weight","COPD_weight","Cancer_weight", "Stroke_weight") :=
                                                  .(Hyper_Death/Deaths,
                                                    Diabetes_Death/Deaths,
                                                    CHD_Death/Deaths,
                                                    CKD_Death/Deaths,
                                                    COPD_Death/Deaths,
                                                    Cancer_Death/Deaths,
                                                    Stroke_Death/Deaths)]


#Create severity index based on those weights
Comorbidities_df[,c("severity_index", "low_severity_index","high_severity_index") := .((Hyper_values * NYS_Co_Deaths[,Hyper_weight] + Diabetes_values * NYS_Co_Deaths[,Diabetes_weight] + CHD_values * NYS_Co_Deaths[,CHD_weight] + CKD_values * NYS_Co_Deaths[,CKD_weight] + COPD_values * NYS_Co_Deaths[,COPD_weight] + Cancer_values * NYS_Co_Deaths[,Cancer_weight] + Stroke_values * NYS_Co_Deaths[,Stroke_weight]) * populationcount/100,
                                                                                       (Hyper_low_CL * NYS_Co_Deaths[,Hyper_weight] + Diabetes_low_CL * NYS_Co_Deaths[,Diabetes_weight] + CHD_low_CL * NYS_Co_Deaths[,CHD_weight] + CKD_low_CL * NYS_Co_Deaths[,CKD_weight] + COPD_low_CL * NYS_Co_Deaths[,COPD_weight] + Cancer_low_CL * NYS_Co_Deaths[,Cancer_weight] + Stroke_low_CL * NYS_Co_Deaths[,Stroke_weight]) * populationcount/100,
                                                                                       (Hyper_high_CL * NYS_Co_Deaths[,Hyper_weight] + + Diabetes_high_CL * NYS_Co_Deaths[,Diabetes_weight] + CHD_high_CL * NYS_Co_Deaths[,CHD_weight] + CKD_high_CL * NYS_Co_Deaths[,CKD_weight] + COPD_high_CL * NYS_Co_Deaths[,COPD_weight] + Cancer_high_CL * NYS_Co_Deaths[,Cancer_weight] + Stroke_high_CL * NYS_Co_Deaths[,Stroke_weight]) * populationcount/100)]

#Normalize it
Comorbidities_df[,c("severity_index_norm","low_severity_index_norm","high_severity_index_norm") := .((severity_index / populationcount * 1000),
                                                                                                     (low_severity_index / populationcount * 1000),
                                                                                                     (high_severity_index / populationcount) * 1000)]

#Map it!
library(leaflet)
library(sf)
library(htmltools)
library(dplyr)

nyctractjson <- data.table(read_sf("./2010 Census Tracts/geo_export_3837d0c5-90cf-43f6-97ba-24cc79104946.shp") %>%
                           st_transform("+proj=longlat +datum=WGS84"))
nyctractjson[,name := paste0('Census Tract ',ctlabel,', ',boro_name,' Boro')]


##NON-Normalized
map_sf <- st_sf(merge(nyctractjson,Comorbidities_df[,!c("tractfips","boro_code","boro_name")], by = "name"))

labels <- paste("<h3>",paste0("Census Tract ",map_sf$tract),"</h3>",
                "<h3>","Boro: ",map_sf$boro_name,"</h3>",
                "<h3>","NTA: ",map_sf$ntaname,"</h3>",
                "<p>","Population: ",format(map_sf$populationcount,nsmall = 0,big.mark = ","),"</p>",
                "<p>","Hypertension: ",paste0(round(map_sf$Hyper_values,2),"%"),"</p>",
                "<p>","Diabetes: ", paste0(round(map_sf$Diabetes_values,2),"%"),"</p>",
                "<p>","Coronary Heart Disease: ", paste0(round(map_sf$CHD_values,2),"%"),"</p>",
                "<p>","Chronic Kidney Disease: ", paste0(round(map_sf$CKD_values,2),"%"),"</p>",
                "<p>","COPD: ",paste0(round(map_sf$COPD_values,2),"%"),"</p>",
                "<p>","Cancer: ",paste0(round(map_sf$Cancer_values,2),"%"),"</p>",
                "<p>","Stroke: ",paste0(round(map_sf$Stroke_values,2),"%"),"</p>")

bins <- round(c(0,quantile(map_sf$severity_index, 0.33, na.rm = T),quantile(map_sf$severity_index, 0.66, na.rm = T),quantile(map_sf$severity_index, 0.99, na.rm = T), max(map_sf$severity_index, na.rm = T) + 1))
pal <- colorBin("YlOrRd", domain = map_sf$severity_index, bins = bins)

leaflet(map_sf) %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 1,
              color = "grey",
              fillColor = pal(map_sf$severity_index),
              fillOpacity = 0.5,
              label = lapply(labels,HTML)) %>%
  addLegend(title = "Severity Index",
            colors = c("#FFFFB2","#FECC5C","#FD8D3C","#E31A1C"),
            labels = c("Low","Medium","High","Very High"),
            opacity = 0.6,
            position = "topright")


##Normalized
map_sf <- st_sf(merge(nyctractjson,Comorbidities_df[,!c("tractfips","boro_code","boro_name")], by = "name"))

labels_norm <- paste("<h3>",paste0("Census Tract ",map_sf$tract),"</h3>",
                "<h3>","Boro: ",map_sf$boro_name,"</h3>",
                "<h3>","NTA: ",map_sf$ntaname,"</h3>",
                "<p>","SI: ",map_sf$severity_index_norm,"</p>",
                "<p>","Population: ",format(map_sf$populationcount,nsmall = 0,big.mark = ","),"</p>",
                "<p>","Hypertension: ",paste0(round(map_sf$Hyper_values,2),"%"),"</p>",
                "<p>","Diabetes: ", paste0(round(map_sf$Diabetes_values,2),"%"),"</p>",
                "<p>","Coronary Heart Disease: ", paste0(round(map_sf$CHD_values,2),"%"),"</p>",
                "<p>","Chronic Kidney Disease: ", paste0(round(map_sf$CKD_values,2),"%"),"</p>",
                "<p>","COPD: ",paste0(round(map_sf$COPD_values,2),"%"),"</p>",
                "<p>","Cancer: ",paste0(round(map_sf$Cancer_values,2),"%"),"</p>",
                "<p>","Stroke: ",paste0(round(map_sf$Stroke_values,2),"%"),"</p>")

bins_norm <- round(c(0,quantile(map_sf$severity_index_norm, 0.33, na.rm = T),quantile(map_sf$severity_index_norm, 0.66, na.rm = T),quantile(map_sf$severity_index_norm, 0.99, na.rm = T), max(map_sf$severity_index_norm, na.rm = T) + 1))
pal_norm <- colorBin("YlOrRd", domain = map_sf$severity_index_norm, bins = bins_norm)

leaflet(map_sf) %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 1,
              color = "grey",
              fillColor = pal_norm(map_sf$severity_index_norm),
              fillOpacity = 0.5,
              label = lapply(labels_norm,HTML)) %>%
  addLegend(title = "Severity Index",
            colors = c("#FFFFB2","#FECC5C","#FD8D3C","#E31A1C"),
            labels = c("Low","Medium","High","Very High"),
            opacity = 0.6,
            position = "topright")
