# This is R files to read input data and run sensitivitiy analysis on the outputs of the migration agent-based model in the CEC2019 paper

Data were downloaded from 
* General Statistics Office (GSO) of Vietnam 
* Vietnam Household Living Standards Survey (VHLSS)
* Vietnam provincial-level map (read and extracted from ArcGIS)
* Inventar 

Data were formatted (though not in a "conventionally tidy" format) to feed back into ArcGis, before being exported into shapefile format, then read by the Migration Agent-Based Model (ABM). 

# GSO Data

Certain demographics and socio-economic data downloaded from General Statistics Office (GSO) of Vietnam have been used to implement an Agent-based modelling of internal migration in the Mekong Delta (MKD) region. Data of each province (provincial level) in annual time-series format from 2005 to 2016 was downloaded:  

* Average Population (thousand people)
  + Data in 2004 was included to initialize the model at time *t=0*
* Natural increase rate
* In-, Out-, Net-Migration rates
* Poverty rate 
* Employment rate 
  + Percentage of trained employed workers at 15 years of age and above over total workers
  + Un-, and Under-Employment rates (regional level)
* Education 
  + Number of pupils in general education
  + Number of students in universities and colleges by province
* Public Service (Health)
  + Number of patient beds under provincial departments of health OVER 10.000 PEOPLE
  + Number of health establishments under provincial departments of health 
* Spatial cost of living index

# VHLSS data

Relevant Data (Bi-Yearly) of average income and expense by income quintile and by province was recorded from VHLSS data from 2006 to 2016

* Monthly average income per capita at current prices by income quintile
* Monthly expenditure per capita at current prices by income quintile (adapted to spatial cost of living index). Pre-processing data was saved in *raw_expenditure.csv* file
  
# Distance data 

Distance of each pair of provinces in the MKD region and neighbouring region is read and extracted from shapefile format of Vietnam provincial-level map. Data is saved in *distance.xls* format (exported from ArcGIS)

# Extreme weathers data 

Extreme weather data has been downloaded from INVENTAR database (spatial-level). Different impacts were chosen based on relevant data availability. Tidied and formatted data is saved in *raw_climate.csv* file

# Normalizing data

All the above data is either normalized or reversely normazlied (depending on how the data is read and used in the Migration ABM model) following the normalization function scaling values between zero and one: 

![](https://latex.codecogs.com/gif.latex?z_i%20%3D%20%5Cfrac%7Bx_i%20-%20min%28x%29%7D%7Bmax%28x%29%20-%20min%28x%29%7D)


  
