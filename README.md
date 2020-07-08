# Comorbidity-Index-Map

## Description
This is a normalized by census tract population 'Severity Index' map, which shows a census tract’s relative risk when considering their comorbidity makeup and each of the comorbidities’ respective fatality rates. It helps visualize the areas where, if infected, carries larger risks for fatalities.

## Data
The comorbidity by census tract data is from the 500 Cities project collaboration with CDC: https://www.cdc.gov/500cities/. Their (latest) 2017 estimations for the percentage of people with comorbidities by census tracts uses data from the CDC Behavioral Risk Factor Surveillance System, the Census 2010 population, and the American Community Survey estimates. You can read more about their methodology on their webpage.

The number of fatalities by comorbidity is from the NYS Department of Health Dashboard: https://covid19tracker.health.ny.gov/views/NYS-COVID19-Tracker/NYSDOHCOVID-19Tracker-Fatalities?%3Aembed=yes&%3Atoolbar=no&%3Atabs=n (last updated 5/14/20). This data is used to find the intersection between the comorbidities tracked on this dashboard and the 500 Cities project data’s categories. The 500 Cities project data is then filtered for NYC and relevant comorbidities. In addition, the NYS Department of Health Dashboard fatality numbers are used to calculate relative risks per type of comorbidity.

## Methodology
The Severity Index (SI) is a unitless number that collapses all the percentage comorbidities makeup by census tract by their relative risk factors. The comorbidities included in the calculation are: COPD, diabetes, cancer (except skin), coronary heart disease, hypertension, chronic kidney disease, and stroke. 

The SI is calculated by multiplying each census tract’s population by each comorbidity percent estimation (500 Cities) and each comorbidity’s respective fatality ratio (# of deaths with comorbidity divided by total number of deaths). Then this SI number is then normalized by census tract population: it is divided by its respective census tract’s population then multiplied by 1000, transforming the interpretation of the SI to the SI per 1000 people. We are using this Severity Index purely to categorize relative risks based on a census tract’s comorbidity makeup between the census tracts. The lowest 33% of SIs are categorized as “Low”, 33% to 66% are categorized as “Medium”, 66% to 99% are categorized as “High”, and the top 1% are categorized as “Very High”.

## Conclusions
There are hotspots in Bronx, Brooklyn, and Queens where large proportion of the population is estimated to have the riskiest comorbidities (i.e. hypertension, diabetes, and coronary heart disease).
