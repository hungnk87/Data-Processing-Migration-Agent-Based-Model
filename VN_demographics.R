library(tidyverse)
library(magrittr)

remove(list=ls())
setwd("/Users/hungnguyenkhanh/Downloads/ABM Migration Data")

# DEMOGRAPHICS DATA -------------------------------------------------------------------------------

# Total Population (thousand person) --------------------------------------------------------------
data_pop_total <- read_delim("E02.03-07.csv", delim = ";", skip = 2)

# List provinces: 13 provinces and city in MKD region and 
# other 4 including: Binh Duong, Dong Nai, Vung Tau, HCM city
provinces <- data_pop_total$`Cities, provincies`
provinces[3] = "Vung Tau"
export_province <- data.frame(Province = provinces)

# Population 2004 (Initialization)
data_pop_2004 <- data_pop_total$`Total 2004`
# Create data frame and Round up to the 10s
export_pop_2004 <- data.frame(Pop04 = ceiling(data_pop_2004 / 10) * 10)
# 4 provinces NOT in MKD region set to 0
export_pop_2004[c(1:4),] = 0

# Remove Province and Pop04 attribute
data_pop_total %<>% select(-c(1:2))

# Update attribute names
colnames(data_pop_total) <- data_pop_total %>% 
  colnames() %>% 
  str_replace_all(c("Prel. " = "", "Total " = "", "20" = "Pop"))

# Natural Population rate (per thousand) ----------------------------------------------------------
data_pop_natural_rate <- read_delim("E02.12-14.csv", delim = ";", skip = 2)
data_pop_natural_rate[1] = NULL

# Update attribute names
colnames(data_pop_natural_rate) <- data_pop_natural_rate %>% 
  colnames() %>% 
  str_replace_all(c("Prel. " = "", "increase " = "", " rate 20" = ""))

# Update attribute
data_pop_natural_rate %<>% 
  mutate(Natural06 = (Natural05 + Natural07)/2) %>% 
  select(order(colnames(.)))
# Export data and set 4 provinces in SE region to 0
export_pop_natural_rate <- as.data.frame(data_pop_natural_rate)
export_pop_natural_rate[c(1:4),] = 0


# In-, Out-, Net-Migration rate (per thousand) ----------------------------------------------------
data_migration <- read_delim("E02.21-23.csv", delim = ";", skip = 2)
data_migration[1] = NULL

# Update attribute names
colnames(data_migration) <- data_migration %>% 
  colnames() %>% 
  str_replace_all(c("Prel. " = "", " " = "", "-migrationrate20" = "Rate"))
# Divide names example 
# names[str_detect(names, "In")]

# Divide into seperate in-, out-, net- migration and update relevant names
data_migration_in_rate <- data_migration %>% select(starts_with("In"))
data_migration_out_rate <- data_migration %>% select(starts_with("Out"))
data_migration_net_rate <- data_migration %>% select(starts_with("Net"))

data_migration_in_rate %<>% 
  mutate(InRate06 = (InRate05 + InRate07)/2) %>% 
  select(order(colnames(.)))
data_migration_out_rate %<>% 
  mutate(OutRate06 = (OutRate05 + OutRate07)/2) %>% 
  select(order(colnames(.)))
data_migration_net_rate %<>% 
  mutate(NetRate06 = (NetRate05 + NetRate07)/2) %>% 
  select(order(colnames(.)))

# SOCIO-ECONOMIC DATA -----------------------------------------------------------------------------

# Functions to normalize and reverse normalize data in range (0,1) --------------------------------
normalize <- function(x){ round((x - min(x)) / (max(x) - min(x)), 3) }
normalize_reverse <- function(x){ round(1 - (x - min(x)) / (max(x) - min(x)), 3) }

# Function to normalize attribute by total population
normalize_pop <- function(x) { round((x / data_pop_total), 3) }

# Poverty Rate ------------------------------------------------------------------------------------
data_poverty <- read_delim("E11.35.csv", delim = ";", skip = 2)
data_poverty[1] = NULL

# Update attribute names
colnames(data_poverty) <- data_poverty %>% 
  colnames() %>% 
  str_replace_all(c("Pre. " = "", "20" = "Poverty" ))

# Update values of missing attributes
data_poverty %<>% 
  mutate(Poverty07 = (Poverty06 + Poverty08) / 2,
         Poverty09 = (Poverty08 + Poverty10) / 2,
         Poverty11 = (Poverty10 + Poverty12) / 2,
         Poverty05 = Poverty06 * 2 - Poverty07) %>% 
  select(order(colnames(.)))

# Normalize Poverty Data
export_poverty <- as.data.frame(sapply(data_poverty, normalize_reverse))
colnames(export_poverty) = colnames(data_poverty)

# Employment --------------------------------------------------------------------------------------

# Percentage of trained employed workers at 15 years of age and above over total workers
data_employment_trained_worker <- read_delim("E02.46.csv", delim = ";", skip = 2)
data_employment_trained_worker[1] = NULL

# Update attribute names
colnames(data_employment_trained_worker) <- data_employment_trained_worker %>% 
  colnames() %>% 
  str_replace_all(c("Prel. " = "", "20" = "Worker"))

# Linear calculation from recent existing data (including the current avail. 2008 data)
worker05 = c(11.7, 10.7, 15.44, 21.14, # First 4 in South East region
             6, 7.3, 5, 4.18, 5, 3.44, 5.56, 5.42, 11.1, 4.62, 4.02, 7.24, 4.56)
worker06 = c(12.1, 11.60, 16.26, 22.38, 
             6.6, 7.7, 5.65, 4.86, 5.85, 4.18, 5.97, 6.04, 11.9, 5.09, 4.54, 7.48, 5.07)
worker07 = c(12.5, 12.50, 17.07, 23.62, 
             7.2, 8.1, 6.3, 5.54, 6.7, 4.92, 6.38, 6.66, 12.7, 5.56, 5.06, 7.72, 5.58)
worker08 = c(12.9, 13.40, 17.89, 24.86, 
             7.8, 8.5, 6.95, 6.22, 7.55, 5.66, 6.79, 7.28, 13.5, 6.03, 5.58, 7.96, 6.09)
data_employment_trained_worker %<>%  
  mutate(Worker05 = worker05,
         Worker06 = worker06, 
         Worker07 = worker07, 
         Worker08 = worker08) %>% 
  select(order(colnames(.)))

# Unemployment rate is similar in South East and Mekong region 
# Annual Underemployment rate data 2005 - 2016 (from GSO): 
SouthEast = c(2.76,2.55,2.34,2.13,3.31,1.22,0.81,0.94,0.92,0.61,0.50,0.45)
Mekong = c(7.64,7.23,6.81,6.39,9.33,5.57,4.79,4.57,5.20,4.20,3.05,3.05)

data_employment = data.frame()
for (i in 1:nrow(data_employment_trained_worker)){
  for (j in 1:ncol(data_employment_trained_worker)){
    if (i == 1 | i == 2 | i == 3 | i == 4){ # Applying to 4 cities in South East region
      data_employment[i,j] = 
        round(data_employment_trained_worker[i,j] / 100 * (1 - SouthEast[j] / 100), 3)
    } else { # The rest are provinces in MKD region
      data_employment[i,j] = 
        round(data_employment_trained_worker[i,j] / 100 * (1 - Mekong[j] / 100), 3)
    }
  }
}
colnames(data_employment) = colnames(data_employment_trained_worker)

# Normalize of employment data
export_employment <- as.data.frame(sapply(data_employment, normalize))  
colnames(export_employment) = colnames(data_employment)

# Education ---------------------------------------------------------------------------------------

# Number of pupils in general education (Primary & Lower & Upper) by province
data_education_no_pupils <- read_delim("E10.11.csv", delim = ";", skip = 2)
data_education_no_pupils[1] = NULL

# Update attribute names
colnames(data_education_no_pupils) <- data_education_no_pupils %>% 
  colnames() %>% 
  str_replace_all(c("Total" = "", "20" = "Pupil"))

# Rate no. pupils over total population
data_education_rate_pupil_pop <- normalize_pop(data_education_no_pupils)
colnames(data_education_rate_pupil_pop) = colnames(data_education_no_pupils)

# No. students in universities and colleges (EXCLUDING data in 2016)
data_education_no_students <- read_delim("E10.18.csv", delim = ";", skip = 2)
data_education_no_students[1] = NULL

# Update attribute names
colnames(data_education_no_students) <- data_education_no_students %>% 
  colnames() %>% 
  str_replace_all(c("Prel. " = "", "\\(\\*\\*\\)" = "", 
                    " Student" = "", "20" = "Student"))

# Update data in 2016
data_education_no_students %<>% mutate(Student16 = Student15)

# Rate between no. students and total population
data_education_rate_student_pop <- normalize_pop(data_education_no_students)
colnames(data_education_rate_student_pop) = colnames(data_education_no_students)

# Combine pupil rate and student rate data 
data_education <- data_education_rate_pupil_pop * data_education_rate_student_pop

# Normalize of education data
export_education <- as.data.frame(sapply(data_education, normalize))

# Update name is export_education data
colnames(export_education) <- data_education_no_students %>% 
  colnames() %>% 
  str_replace("Student", "Edu")

# Public Service (Health) -------------------------------------------------------------------------

# Number of patient beds under provincial departments of health (over 10.000 people)
data_health_no_beds <- read_delim("E11.08.csv", delim = ";", skip = 2)
data_health_no_beds[1] = NULL

# Update attribute name 
colnames(data_health_no_beds) <- data_health_no_beds %>% 
  colnames() %>% 
  str_replace_all(c("Prel. " = "", " Total" = "", "20" = "Beds"))

# Rate no. of hospital beds and total population 
data_health_rate_bed_pop = normalize_pop(data_health_no_beds)
colnames(data_health_rate_bed_pop) = colnames(data_health_no_beds)

# Number of health establishments (hospitals) under provincial departments of health 
data_health_no_hospital <- read_delim("E11.05.csv", delim = ";", skip = 2)
data_health_no_hospital[1] = NULL

# Update attribute name 
colnames(data_health_no_hospital) <- data_health_no_hospital %>% 
  colnames() %>% 
  str_replace_all(c("Prel. " = "", " Total" = "", "20" = "Hos"))

# Rate no. hospital and total population 
data_health_rate_hos_pop = normalize_pop(data_health_no_hospital)
colnames(data_health_rate_hos_pop) = colnames(data_health_no_hospital)

# Combine no. bed and no. hospital data 
data_health = data_health_rate_bed_pop * data_health_rate_hos_pop
colnames(data_health) <- data_health_rate_hos_pop %>% 
  colnames() %>% 
  str_replace_all(c("Hos" = "Pub"))

# Normalize of health data
export_health = as.data.frame(sapply(data_health, normalize))
colnames(export_health) = colnames(data_health)

# DISTANCE ----------------------------------------------------------------------------------------
# Province order in table followed output in exported from ARCGIS *****

# Import and format data 
library(readxl)
distance = read_xls("distance.xls")
data_distance = data.frame()
for (i in 1:length(provinces)){
  for (j in 1:length(provinces)){
    if (i == j) data_distance[i,j] = 0
    for (k in 1:nrow(distance)){
      if(distance[k,2] == i - 1 & distance[k,3] == j - 1){
        data_distance[i,j] = distance[k,4]
      }
    }
  }
}

# Update names
colnames(data_distance) = c("disDN", "disDT", "disAG", "disVT", "disBD", "disBL", 
                            "disBT", "disCM", "disCT", "disHCM", "disHG", "disKG", 
                            "disLA", "disST", "disTG", "disTV", "disVL")

# Normalize Distance between two different provinces 
min_dis <- min(data_distance[data_distance > 0])
max_dis <- max(data_distance)
export_distance = data.frame()
for (i in 1:nrow(data_distance)){
  for (j in 1:ncol(data_distance)){
    if (i == j) export_distance[i,j] = 0
    else export_distance[i,j] = 
        round((data_distance[i,j] - min_dis) / (max_dis - min_dis),3)
  }
}
colnames(export_distance) = colnames(data_distance)

# INCOME QUINTILES --------------------------------------------------------------------------------

update_income <- function(x, y){ 
  # x: quintile; y: index (1 = 2004, 2 = 2006, 3 = 2008)
  c(income1[y, x], income2[y, x], income3[y, x], income4[y, x], income5[y, x], 
    income6[y, x], income7[y, x], income8[y, x], income9[y, x], income10[y, x], 
    income11[y, x], income12[y, x], income13[y, x], income14[y, x], income15[y, x], 
    income16[y, x], income17[y, x])
}

# Income data extracted from VHLSS reports 2004, 2006, 2008 ---------------------------------------
income1 <- data.frame(q1 = c(285.3, 447.9, 686.5),
                      q2 = c(447.5, 714.4, 1127.4),
                      q3 = c(607.2, 923.5, 1500.2),
                      q4 = c(850.5, 1204.5, 1988.7),
                      q5 = c(1705.4, 2786.6, 4326.9))
income2 <- data.frame(q1 = c(243.2, 296.7, 449.9),
                      q2 = c(388.9, 501.6, 741.7),
                      q3 = c(520.3, 710.5, 1053.2),
                      q4 = c(722.4, 933.1, 1412.9),
                      q5 = c(1513.4, 1891.8, 2930.7))
income3 <- data.frame(q1 = c(205.7, 240.1, 357.7),
                      q2 = c(325.1, 381.1, 617.8),
                      q3 = c(473.3, 551.4, 907.0),
                      q4 = c(657.2, 768.9, 1260.3),
                      q5 = c(1635.3, 1939.5, 2982.1))
income4 <- data.frame(q1 = c(430.8, 553.6, 826.7),
                      q2 = c(635.4, 823.5, 1182.6),
                      q3 = c(870.0, 1076.2, 1542.4),
                      q4 = c(1219.0, 1493.3, 2139.6),
                      q5 = c(2668.3, 3452.8, 5252.3))
income5 <- data.frame(q1 = c(172.0, 213.0, 331.3),
                      q2 = c(297.7, 370.1, 533.5),
                      q3 = c(408.8, 522.7, 732.9),
                      q4 = c(554.1, 703.8, 1017.5),
                      q5 = c(1069.8, 1326.6, 2076.9))
income6 <- data.frame(q1 = c(162.5, 212.9, 323.0),
                      q2 = c(283.1, 363.8, 510.8),
                      q3 = c(384.7, 511.7, 728.2),
                      q4 = c(531.6, 701.6, 1060.2),
                      q5 = c(1030.3, 1358.6, 2158.4))
income7 <- data.frame(q1 = c(156.0, 209.1, 287.7),
                      q2 = c(244.8, 357.4, 504.2),
                      q3 = c(318.3, 483.8, 713.9),
                      q4 = c(429.1, 673.7, 952.9),
                      q5 = c(943.6, 1344.7, 1896.6))
income8 <- data.frame(q1 = c(122.7, 158.8, 247.5),
                      q2 = c(219.0, 279.6, 416.2),
                      q3 = c(297.0, 378.4, 536.0),
                      q4 = c(432.9, 546.0, 790.1),
                      q5 = c(903.9, 1184.2, 1853.0))
income9 <- data.frame(q1 = c(157.2, 209.1, 335.4),
                      q2 = c(243.6, 336.1, 525.6),
                      q3 = c(333.8, 477.3, 693.5),
                      q4 = c(450.1, 660.8, 967.8),
                      q5 = c(930.4, 1216.0, 1963.7))
income10 <- data.frame(q1 = c(156.8, 196.7, 279.4),
                       q2 = c(239.4, 304.0, 448.5),
                       q3 = c(321.8, 411.9, 618.8),
                       q4 = c(462.2, 631.1, 931.6),
                       q5 = c(1193.2, 1503.4, 2169.2))
income11 <- data.frame(q1 = c(182.3, 242.8, 350.6),
                       q2 = c(283.0, 384.9, 605.2),
                       q3 = c(388.7, 508.3, 810.1),
                       q4 = c(551.0, 727.3, 1110.3),
                       q5 = c(1188.4, 1592.5, 2448.5))
income12 <- data.frame(q1 = c(157.9, 210.0, 327.1),
                       q2 = c(261.4, 358.6, 497.2),
                       q3 = c(369.6, 484.9, 675.1),
                       q4 = c(536.8, 674.8, 985.8),
                       q5 = c(1238.6, 1648.6, 2600.2))
income13 <- data.frame(q1 = c(210.7, 294.6, 381.2),
                       q2 = c(333.9, 494.2, 623.6),
                       q3 = c(430.0, 645.1, 954.1),
                       q4 = c(571.7, 851.8, 1267.5),
                       q5 = c(1083.3, 1614.1, 2426.3))
income14 <- data.frame(q1 = c(160.9, 212.3, 293.7),
                       q2 = c(269.3, 342.6, 462.3),
                       q3 = c(367.5, 465.1, 682.2),
                       q4 = c(497.5, 661.4, 965.6),
                       q5 = c(947.0, 1353.7, 2022.1))
income15 <- data.frame(q1 = c(133.0, 157.5, 242.8),
                       q2 = c(221.0, 273.4, 389.5),
                       q3 = c(296.2, 371.5, 502.8),
                       q4 = c(418.5, 531.1, 729.7),
                       q5 = c(906.0, 1139.8, 1769.6))
income16 <- data.frame(q1 = c(150.5, 197.6, 299.3),
                       q2 = c(268.5, 325.1, 497.6),
                       q3 = c(381.9, 490.3, 694.5),
                       q4 = c(520.7, 684.7, 993.4),
                       q5 = c(1020.2, 1356.8, 2136.0))
income17 <- data.frame(q1 = c(162.1, 209.1, 277.4),
                       q2 = c(289.8, 361.0, 537.2),
                       q3 = c(406.6, 506.3, 767.9),
                       q4 = c(581.4, 734.3, 1091.3),
                       q5 = c(1128.7, 1517.4, 2173.5))

# Monthly average income per capita by income quintile 2010, 2012, 2014, 2016
income <- read_delim("E11.24.csv", delim = ";", skip = 2)
income[1] = NULL

# Update attribute name
colnames(income) <- income %>% 
  colnames() %>% 
  str_replace_all(c("Pre. " = "", "General" = "Income", 
                    "Quintile 1 20" = "Inc1st",
                    "Quintile 2 20" = "Inc2nd",
                    "Quintile 3 20" = "Inc3rd",
                    "Quintile 4 20" = "Inc4th",
                    "Quintile 5 20" = "Inc5th", " " = "."))

data_inc_q1 <- income %>% select(starts_with("Inc1st"))
data_inc_q2 <- income %>% select(starts_with("Inc2nd"))
data_inc_q3 <- income %>% select(starts_with("Inc3rd"))
data_inc_q4 <- income %>% select(starts_with("Inc4th"))
data_inc_q5 <- income %>% select(starts_with("Inc5th"))

# Income quintile 1 -------------------------------------------------------------------------------
data_inc_q1 %<>% mutate(Inc1st04 = update_income(1,1),
                        Inc1st06 = update_income(1,2),
                        Inc1st08 = update_income(1,3),
                        Inc1st05 = (Inc1st04 + Inc1st06) / 2, 
                        Inc1st07 = (Inc1st06 + Inc1st08) / 2,
                        Inc1st09 = (Inc1st08 + Inc1st10) / 2,
                        Inc1st11 = (Inc1st10 + Inc1st12) / 2,
                        Inc1st13 = (Inc1st12 + Inc1st14) / 2,
                        Inc1st15 = (Inc1st14 + Inc1st16) / 2) %>% 
  select(order(colnames(.)))

# Normalize data 
data_inc_q1[1] = NULL
export_inc_q1 <- as.data.frame(sapply(data_inc_q1, normalize))
colnames(export_inc_q1) = colnames(data_inc_q1)

# Income quintile 2 -------------------------------------------------------------------------------
data_inc_q2 %<>% mutate(Inc2nd04 = update_income(2,1),
                        Inc2nd06 = update_income(2,2),
                        Inc2nd08 = update_income(2,3),
                        Inc2nd05 = (Inc2nd04 + Inc2nd06) / 2, 
                        Inc2nd07 = (Inc2nd06 + Inc2nd08) / 2,
                        Inc2nd09 = (Inc2nd08 + Inc2nd10) / 2,
                        Inc2nd11 = (Inc2nd10 + Inc2nd12) / 2,
                        Inc2nd13 = (Inc2nd12 + Inc2nd14) / 2,
                        Inc2nd15 = (Inc2nd14 + Inc2nd16) / 2) %>% 
  select(order(colnames(.)))

# Normalize data 
data_inc_q2[1] = NULL
export_inc_q2 <- as.data.frame(sapply(data_inc_q2, normalize))
colnames(export_inc_q2) = colnames(data_inc_q2)

# Income quintile 3 -------------------------------------------------------------------------------
data_inc_q3 %<>% mutate(Inc3rd04 = update_income(3,1),
                        Inc3rd06 = update_income(3,2),
                        Inc3rd08 = update_income(3,3),
                        Inc3rd05 = (Inc3rd04 + Inc3rd06) / 2, 
                        Inc3rd07 = (Inc3rd06 + Inc3rd08) / 2,
                        Inc3rd09 = (Inc3rd08 + Inc3rd10) / 2,
                        Inc3rd11 = (Inc3rd10 + Inc3rd12) / 2,
                        Inc3rd13 = (Inc3rd12 + Inc3rd14) / 2,
                        Inc3rd15 = (Inc3rd14 + Inc3rd16) / 2) %>% 
  select(order(colnames(.)))

# Normalize data 
data_inc_q3[1] = NULL
export_inc_q3 <- as.data.frame(sapply(data_inc_q3, normalize))
colnames(export_inc_q3) = colnames(data_inc_q3)

# Income quintile 4 -------------------------------------------------------------------------------
data_inc_q4 %<>% mutate(Inc4th04 = update_income(4,1),
                        Inc4th06 = update_income(4,2),
                        Inc4th08 = update_income(4,3),
                        Inc4th05 = (Inc4th04 + Inc4th06) / 2, 
                        Inc4th07 = (Inc4th06 + Inc4th08) / 2,
                        Inc4th09 = (Inc4th08 + Inc4th10) / 2,
                        Inc4th11 = (Inc4th10 + Inc4th12) / 2,
                        Inc4th13 = (Inc4th12 + Inc4th14) / 2,
                        Inc4th15 = (Inc4th14 + Inc4th16) / 2) %>% 
  select(order(colnames(.)))

# Normalize data 
data_inc_q4[1] = NULL
export_inc_q4 <- as.data.frame(sapply(data_inc_q4, normalize))
colnames(export_inc_q4) = colnames(data_inc_q4)

# Income quintile 5 -------------------------------------------------------------------------------
data_inc_q5 %<>% mutate(Inc5th04 = update_income(5,1),
                        Inc5th06 = update_income(5,2),
                        Inc5th08 = update_income(5,3),
                        Inc5th05 = (Inc5th04 + Inc5th06) / 2, 
                        Inc5th07 = (Inc5th06 + Inc5th08) / 2,
                        Inc5th09 = (Inc5th08 + Inc5th10) / 2,
                        Inc5th11 = (Inc5th10 + Inc5th12) / 2,
                        Inc5th13 = (Inc5th12 + Inc5th14) / 2,
                        Inc5th15 = (Inc5th14 + Inc5th16) / 2) %>% 
  select(order(colnames(.)))

# Normalize data 
data_inc_q5[1] = NULL
export_inc_q5 <- as.data.frame(sapply(data_inc_q5, normalize))
colnames(export_inc_q5) = colnames(data_inc_q5)

# EXPENDITURE QUINTILES ---------------------------------------------------------------------------
# Calculated based on different factors: 
# * Spatial living cost index
# * Monthly average living expenditure in South East region 
# * Monthly average living expenditure in Mekong Delta region 

# Expenditure quintile 1 --------------------------------------------------------------------------
data_exp_q1 <- read_csv("raw_expenditure.csv", skip = 60, n_max = 17)

colnames(data_exp_q1) <- data_exp_q1 %>% 
  colnames() %>% 
  str_replace_all(c("Expenditure.Quintile.1.20" = "Exp1st"))

export_exp_q1 = as.data.frame(sapply(data_exp_q1, normalize))
colnames(export_exp_q1) = colnames(data_exp_q1)

# Expenditure quintile 2 --------------------------------------------------------------------------
data_exp_q2 <- read_csv("raw_expenditure.csv", skip = 81, n_max = 17)

colnames(data_exp_q2) <- data_exp_q2 %>% 
  colnames() %>% 
  str_replace_all(c("Expenditure.Quintile.2.20" = "Exp2nd"))

export_exp_q2 = as.data.frame(sapply(data_exp_q2, normalize))
colnames(export_exp_q2) = colnames(data_exp_q2)

# Expenditure quintile 3 --------------------------------------------------------------------------
data_exp_q3 <- read_csv("raw_expenditure.csv", skip = 102, n_max = 17)

colnames(data_exp_q3) <- data_exp_q3 %>% 
  colnames() %>% 
  str_replace_all(c("Expenditure.Quintile.3.20" = "Exp3rd"))

export_exp_q3 = as.data.frame(sapply(data_exp_q3, normalize))
colnames(export_exp_q3) = colnames(data_exp_q3)

# Expenditure quintile 4 --------------------------------------------------------------------------
data_exp_q4 <- read_csv("raw_expenditure.csv", skip = 123, n_max = 17)

colnames(data_exp_q4) <- data_exp_q4 %>% 
  colnames() %>% 
  str_replace_all(c("Expenditure.Quintile.4.20" = "Exp4th"))

export_exp_q4 = as.data.frame(sapply(data_exp_q4, normalize))
colnames(export_exp_q4) = colnames(data_exp_q4)

# Expenditure quintile 5 --------------------------------------------------------------------------
data_exp_q5 <- read_csv("raw_expenditure.csv", skip = 144, n_max = 17)

colnames(data_exp_q5) <- data_exp_q5 %>% 
  colnames() %>% 
  str_replace_all(c("Expenditure.Quintile.5.20" = "Exp5th"))

export_exp_q5 = as.data.frame(sapply(data_exp_q5, normalize))
colnames(export_exp_q5) = colnames(data_exp_q5)

# EXTREME WEATHERS --------------------------------------------------------------------------------
data_climate <- as.data.frame(read_csv("raw_climate.csv"))

# Update Hau Giang data as the average of whole region due to missing data (Index: 14)
data_climate[14,] <- sapply(data_climate, mean)

# Impacts caused by ONE hazard (Average)
data_climate[-1] <- sapply(data_climate[-1], FUN = function(x) { x / data_climate[,1] })
# Normalize all impacts
data_climate[-1] <- sapply(data_climate[-1], normalize)

# Average no. of hazards during 27 years (from 1989 - 2015)
data_climate[1] <- round(data_climate[1] / 27, 3)

export_climate <- data.frame(NoHazards = data_climate$Hazard,
                             VulIndex = rowMeans(data_climate[-1]))

# EXPORT DATA -------------------------------------------------------------------------------------

# Add id and order taken from ArcGIS output
export_id = data.frame(ID = c(4, 0, 3, 9, 12, 14, 6, 15, 16, 1, 2, 11, 8, 10, 13, 5, 7))

# Combine all data except distance at the end, after sorting by ID
export_data <- cbind(export_id, export_province, export_pop_2004, export_pop_natural_rate,
                     export_employment, export_poverty, export_education, export_health,
                     export_inc_q1, export_inc_q2, export_inc_q3, export_inc_q4, export_inc_q5,
                     export_exp_q1, export_exp_q2, export_exp_q3, export_exp_q4, export_exp_q5,
                     export_climate) %>% 
  arrange(ID) %>% 
  # ID = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
  cbind(export_distance) %>% 
  select(-"ID")

library(WriteXLS)
WriteXLS(export_data, "data.xls")
