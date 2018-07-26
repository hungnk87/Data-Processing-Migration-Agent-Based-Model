library(dplyr)
library(ggplot2)
library(reshape2)
library(WriteXLS)
library(gdata)

remove(list=ls())

setwd("/Users/HUNGNGUYEN/Downloads")

# ********** TOTAL POPULATION / NATURAL RATE / IN-MIGRATION / OUT-MIGRATION / NET-MIGRATION ********** #

# Average Population (thousand person)
data_pop_total <- read.table("E02.03-07.csv",  header = FALSE, sep = ";", skip = 3)
provinces <- as.character(data_pop_total[,1])
provinces[3] = "Vung Tau"
data <- read.table("E02.03-07.csv",  header = FALSE, sep = ";", skip = 2, nrows = 1)
names = c()
names[1] = "Province"
for (i in 2:ncol(data)){
  names[i] = as.character(data[1,i])
  names[i] = gsub("Prel. ", "", names[i])
  names[i] = gsub("Total ", "", names[i])
  names[i] = gsub("20", "Pop", names[i])
}
colnames(data_pop_total) = names
data_pop_total <- mutate(data_pop_total, Province = provinces)

# Data Population 2004 (Initialization)
data_pop_2004 = data_pop_total$Pop04
for (i in 1:length(data_pop_2004)){
  data_pop_2004[i] = ceiling(data_pop_2004[i] / 10) * 10
}

data_pop_total[,1:2] = list(NULL)

# Natural increase rate  
data <- read.table("E02.12-14.csv",  header = FALSE, sep = ";", skip = 2, nrows = 1)
names = c()
names[1] = "Province"
for (i in 2:ncol(data)){
  names[i] = as.character(data[1,i])
  names[i] = gsub("Prel. ", "", names[i])
  names[i] = gsub("increase ", "", names[i])
  names[i] = gsub(" rate 20", "", names[i])
}
data_pop_natural_rate <- read.table("E02.12-14.csv",  header = FALSE, sep = ";", skip = 3)
colnames(data_pop_natural_rate) = names
data_pop_natural_rate <- mutate(data_pop_natural_rate, Province = provinces, Natural06 = (Natural05 + Natural07)/2)
data_pop_natural_rate <- select(data_pop_natural_rate, Province, Natural05, Natural06, everything())
data_pop_natural_rate[,1] = list(NULL)
data_pop_natural_rate[c(1:4),] = 0

# In-migration rate, out-migration rate and net-migration rate  
# Southeast & Southwest (MKD region) - 2005:2016 (excl. 2006)
data <- read.table("E02.21-23.csv",  header = FALSE, sep = ";", skip = 2, nrows = 1)
names = c()
for (i in 2:ncol(data)){
  names[i] = as.character(data[1,i])
  names[i] = gsub("Prel. ", "", names[i])
  names[i] = gsub(" ", "", names[i])
  names[i] = gsub("-migrationrate20", "Rate", names[i])
}
data <- read.table("E02.21-23.csv",  header = FALSE, sep = ";", skip = 3)
data_pop_in_migration_rate = data[,2:12]
data_pop_out_migration_rate = data[,13:23]
data_pop_net_migration_rate = data[,24:34]
colnames(data_pop_in_migration_rate) = names[2:12]
colnames(data_pop_out_migration_rate) = names[13:23]
colnames(data_pop_net_migration_rate) = names[24:34]
data_pop_in_migration_rate <- mutate(data_pop_in_migration_rate, Province = provinces, InRate06 = (InRate05 + InRate07)/2)
data_pop_in_migration_rate <- select(data_pop_in_migration_rate, Province, InRate05, InRate06, everything())
data_pop_out_migration_rate <- mutate(data_pop_out_migration_rate, Province = provinces, OutRate06 = (OutRate05 + OutRate07)/2)
data_pop_out_migration_rate <- select(data_pop_out_migration_rate, Province, OutRate05, OutRate06, everything())
data_pop_net_migration_rate <- mutate(data_pop_net_migration_rate, Province = provinces, NetRate06 = (NetRate05 + NetRate07)/2)
data_pop_net_migration_rate <- select(data_pop_net_migration_rate, Province, NetRate05, NetRate06, everything())
# # In-Migration plot
# test = melt(data_pop_in_migration_rate)
# ggplot(test, aes(variable, value, group = factor(Province))) + geom_line(aes(color=factor(Province)))
# # Out-Migration plot
# test = melt(data_pop_out_migration_rate)
# ggplot(test, aes(variable, value, group = factor(Province))) + geom_line(aes(color=factor(Province)))
# # Net-Migration plot
# data_pop_net_migration_rate = data_pop_net_migration_rate[-1,]
# data_pop_net_migration_rate = mutate(data_pop_net_migration_rate, Mean = round(rowMeans(data_pop_net_migration_rate[,-1]),3))
# test = melt(data_pop_net_migration_rate)
# ggplot(test, aes(variable, value, group = factor(Province))) + geom_line(aes(color=factor(Province)))
data_pop_in_migration_rate[,1] = list(NULL)
data_pop_out_migration_rate[,1] = list(NULL)
data_pop_net_migration_rate[,1] = list(NULL)


# ********** POVERTY ********** #

data_poverty <- read.table("E11.35.csv",  header = FALSE, sep = ";", skip = 3)
data <- read.table("E11.35.csv",  header = FALSE, sep = ";", skip = 2, nrows = 1)
names = c()
names[1] = "Province"
for (i in 2:ncol(data)){
  names[i] = as.character(data[1,i])
  names[i] = gsub("Pre. ", "", names[i])
  names[i] = gsub("20", "Poverty", names[i])
}
colnames(data_poverty) = names
data_poverty = mutate(data_poverty, Poverty11 = (Poverty10 + Poverty12) / 2, Poverty09 = (Poverty08 + Poverty10) / 2,
                      Poverty07 = (Poverty06 + Poverty08) / 2)
data_poverty = mutate(data_poverty, Poverty05 = Poverty06 * 2 - Poverty07)
data_poverty = select(data_poverty, Poverty05, Poverty06, Poverty07, Poverty08, Poverty09, Poverty10, Poverty11, 
                      Poverty12, Poverty13, Poverty14, Poverty15, Poverty16)

# Normalize Poverty Data 
export_poverty = data.frame()
for (i in 1:ncol(data_poverty)){
  minPov = min(data_poverty[,i])
  maxPov = max(data_poverty[,i])
  for (j in 1:nrow(data_poverty)){
    export_poverty[j,i] = 1 - round((data_poverty[j,i] - minPov) / (maxPov - minPov), 3)
  }
}

names = c()
for (i in 1:12){
  names[i] = colnames(data_poverty)[i]
}
colnames(export_poverty) = names

# ********** EMPLOYMENT ********** #

# Percentage of trained employed workers at 15 years of age and above over total workers
data_employment_rate_trained_worker <- read.table("E02.46.csv",  header = FALSE, sep = ";", skip = 3)
data <- read.table("E02.46.csv",  header = FALSE, sep = ";", skip = 2, nrows = 1)
names = c()
names[1] = "Province"
for (i in 2:ncol(data)){
  names[i] = as.character(data[1,i])
  names[i] = gsub("Prel. ", "", names[i])
  names[i] = gsub("20", "Worker", names[i])
}
colnames(data_employment_rate_trained_worker) = names
# Linear calculation from recent existing data 
worker05 = c(11.7, 10.7, 15.44, 21.14, 6, 7.3, 5, 4.18, 5, 3.44, 5.56, 5.42, 11.1, 4.62, 4.02, 7.24, 4.56)
worker06 = c(12.1, 11.60, 16.26, 22.38, 6.6, 7.7, 5.65, 4.86, 5.85, 4.18, 5.97, 6.04, 11.9, 5.09, 4.54, 7.48, 5.07)
worker07 = c(12.5, 12.50, 17.07, 23.62, 7.2, 8.1, 6.3, 5.54, 6.7, 4.92, 6.38, 6.66, 12.7, 5.56, 5.06, 7.72, 5.58)
worker08 = c(12.9, 13.40, 17.89, 24.86, 7.8, 8.5, 6.95, 6.22, 7.55, 5.66, 6.79, 7.28, 13.5, 6.03, 5.58, 7.96, 6.09)
data_employment_rate_trained_worker = mutate(data_employment_rate_trained_worker, Worker05 = worker05,
                                             Worker06 = worker06, Worker07 = worker07, Worker08 = worker08)
data_employment_rate_trained_worker = select(data_employment_rate_trained_worker, Province, Worker05, Worker06, Worker07, Worker08, everything())
# library(reshape2)
# test = melt(data_employment_rate_trained_worker)
# ggplot(test, aes(variable, value, group = factor(Province))) + geom_line(aes(color=factor(Province)))
data_employment_rate_trained_worker[,1] = list(NULL)

SouthEast = c(2.76,2.55,2.34,2.13,3.31,1.22,0.81,0.94,0.92,0.61,0.50,0.45)
Mekong = c(7.64,7.23,6.81,6.39,9.33,5.57,4.79,4.57,5.20,4.20,3.05,3.05)

data_employment_rate_trained_worker2 = data.frame()
for (i in 1:nrow(data_employment_rate_trained_worker)){
  for (j in 1:ncol(data_employment_rate_trained_worker)){
    if (i == 1 | i == 2 | i == 3 | i == 4){
      data_employment_rate_trained_worker2[i,j] = round(data_employment_rate_trained_worker[i,j] * (1 - SouthEast[j]/100),3)
    } else {
      data_employment_rate_trained_worker2[i,j] = round(data_employment_rate_trained_worker[i,j] * (1 - Mekong[j]/100),3)
    }
  }
}
colnames(data_employment_rate_trained_worker2) = colnames(data_employment_rate_trained_worker)

# Normalize of employment data
export_employment = data.frame()
for (i in 1:ncol(data_employment_rate_trained_worker2)){
  minEmp = min(data_employment_rate_trained_worker2[,i])
  maxEmp = max(data_employment_rate_trained_worker2[,i])
  for (j in 1:nrow(data_employment_rate_trained_worker2)){
    export_employment[j,i] = round((data_employment_rate_trained_worker2[j,i] - minEmp) / (maxEmp - minEmp), 3)
  }
}

names = c()
for (i in 1:12){
  names[i] = colnames(data_employment_rate_trained_worker2)[i]
}
colnames(export_employment) = names

# ********** EDUCATION ********** #

# Number of pupils in general education (Primary & Lower & Upper) by province
data_education_no_pupils <- read.table("E10.11.csv",  header = FALSE, sep = ";", skip = 3)
data <- read.table("E10.11.csv",  header = FALSE, sep = ";", skip = 2, nrows = 1)
names = c()
names[1] = "Province"
for (i in 2:ncol(data)){
  names[i] = as.character(data[1,i])
  names[i] = gsub(" Total", "", names[i])
  names[i] = gsub("20", "Pupil", names[i])
}
colnames(data_education_no_pupils) = names
data_education_no_pupils <- mutate(data_education_no_pupils, Province = provinces)
data_education_no_pupils[,1] = list(NULL)

# Rate between pupils and total population for each province
names2 = names[2:length(names)]
data_education_rate_pupil_pop = data.frame()
for (i in 1: nrow(data_education_no_pupils)){
  for (j in 1: ncol(data_pop_total)){
    data_education_rate_pupil_pop[i,j] = round(data_education_no_pupils[i,j] / data_pop_total[i,j], 3)
  }
}
colnames(data_education_rate_pupil_pop) = names2

# Number of students in universities and colleges by province
# EXCLUDING colleges in 2016 --> calculated from exponential smoothing (2010 - 2015)
data_education_no_students <- read.table("E10.18.csv",  header = FALSE, sep = ";", skip = 3)
data <- read.table("E10.18.csv",  header = FALSE, sep = ";", skip = 2, nrows = 1)
names = c()
names[1] = "Province"
for (i in 2:ncol(data)){
  names[i] = as.character(data[1,i])
  names[i] = gsub("Prel. ", "", names[i])
  names[i] = gsub("\\(\\*\\*\\)", "", names[i])
  names[i] = gsub(" Student", "", names[i])
  names[i] = gsub("20", "Student", names[i])
}
colnames(data_education_no_students) = names
data_education_no_students <- mutate(data_education_no_students, Province = provinces)
data_education_no_students <- mutate(data_education_no_students, Student16 = data_education_no_students$Student15)
data_education_no_students[,1] = list(NULL)

# Rate between students and total population for each province
names2 = names[2:length(names)]
names2[length(names2)+1] = "Student16"
data_education_rate_student_pop = data.frame()
for (i in 1: nrow(data_education_no_students)){
  for (j in 1: ncol(data_pop_total)){
    data_education_rate_student_pop[i,j] = round(data_education_no_students[i,j] / data_pop_total[i,j], 3)
  }
}
colnames(data_education_rate_student_pop) = names2

# Combine student rate and pupil rate data 
data_export_education = data.frame()
for (i in 1:ncol(data_education_no_pupils)){
  for (j in 1:nrow(data_education_no_pupils)){
    data_export_education[j,i] = data_education_rate_pupil_pop[j,i] * data_education_rate_student_pop[j,i]
  }
}

# Normalize of education data
export_education = data.frame()
for (i in 1:ncol(data_export_education)){
  minEdu= min(data_export_education[,i])
  maxEdu = max(data_export_education[,i])
  for (j in 1:nrow(data_export_education)){
    export_education[j,i] = round((data_export_education[j,i] - minEdu) / (maxEdu - minEdu), 3)
  }
}

names = c()
for (i in 1:12){
  names[i] = colnames(data_education_rate_student_pop)[i]
  names[i] = gsub("Student", "Edu", names[i])
}
colnames(export_education) = names


# ********** HEALTH ********** #

# Number of patient beds under provincial departments of health OVER 10.000 PEOPLE
# Hau Giang 2004 is a bit small --> remove
data_health_no_beds <- read.table("E11.08.csv",  header = FALSE, sep = ";", skip = 3)
data <- read.table("E11.08.csv",  header = FALSE, sep = ";", skip = 2, nrows = 1)
names = c()
names[1] = "Province"
for (i in 2:ncol(data)){
  names[i] = as.character(data[1,i])
  names[i] = gsub("Prel. ", "", names[i])
  names[i] = gsub(" Total", "", names[i])
  names[i] = gsub("20", "Beds", names[i])
}
colnames(data_health_no_beds) = names
data_health_no_beds <- mutate(data_health_no_beds, Province = provinces)
data_health_no_beds[,1] = list(NULL)

# Rate between No. of hospital beds and total population for each province
names2 = names[2:length(names)]
data_health_rate_bed_pop = data.frame()
for (i in 1: nrow(data_health_no_beds)){
  for (j in 1: ncol(data_pop_total)){
    data_health_rate_bed_pop[i,j] = round(data_health_no_beds[i,j] / data_pop_total[i,j], 3)
  }
}
colnames(data_health_rate_bed_pop) = names2

# Number of patient beds under provincial departments of health OVER 10.000 PEOPLE
# Hau Giang 2004 is a bit small --> remove
data_health_no_hospital <- read.table("E11.05.csv",  header = FALSE, sep = ";", skip = 3)
data <- read.table("E11.05.csv",  header = FALSE, sep = ";", skip = 2, nrows = 1)
names = c()
names[1] = "Province"
for (i in 2:ncol(data)){
  names[i] = as.character(data[1,i])
  names[i] = gsub("Prel. ", "", names[i])
  names[i] = gsub(" Total", "", names[i])
  names[i] = gsub("20", "Hos", names[i])
}
colnames(data_health_no_hospital) = names
data_health_no_hospital <- mutate(data_health_no_hospital, Province = provinces)
data_health_no_hospital[,1] = list(NULL)

# Rate between No. of hospital beds and total population for each province
names2 = names[2:length(names)]
data_health_rate_hos_pop = data.frame()
for (i in 1: nrow(data_health_no_hospital)){
  for (j in 1: ncol(data_pop_total)){
    data_health_rate_hos_pop[i,j] = round(data_health_no_hospital[i,j] / data_pop_total[i,j], 3)
  }
}
colnames(data_health_rate_hos_pop) = names2

# Combine bed and hospital data 
data_health_combine = data.frame()
for (i in 1:ncol(data_health_rate_bed_pop)){
  for (j in 1:nrow(data_health_rate_bed_pop)){
    data_health_combine[j,i] = data_health_rate_bed_pop[j,i] * data_health_rate_hos_pop[j,i]
  }
}

# Normalize of health data
export_health = data.frame()
for (i in 1:ncol(data_health_combine)){
  minHea = min(data_health_combine[,i])
  maxHea = max(data_health_combine[,i])
  for (j in 1:nrow(data_health_combine)){
    export_health[j,i] = round((data_health_combine[j,i] - minHea) / (maxHea - minHea), 3)
  }
}

names = c()
for (i in 1:12){
  names[i] = colnames(data_health_rate_bed_pop)[i]
}
colnames(export_health) = names


# ********** DISTANCE ********** #

# Distance Calculation (1 / DISTANCE)
# ***** ORDER OF THE PROVINCE SHOULD BE THE SAME WITH ARCGIS ***** 

distance = read.xls("distance.xls", header = TRUE)
data_distance = data.frame()
for (i in 1:17){
  for (j in 1:17){
    if (i == j) data_distance[i,j] = 0
    for (k in 1:nrow(distance)){
      if(distance[k,2] == i - 1 & distance[k,3] == j - 1){
        data_distance[i,j] = distance[k,4]
      }
    }
  }
}
names = c("disDN", "disDT", "disAG", "disVT", "disBD", "disBL", "disBT", "disCM", "disCT", 
          "disHCM", "disHG", "disKG", "disLA", "disST", "disTG", "disTV", "disVL")
colnames(data_distance) = names

# Find min, max
minDis = data_distance[1,2]
maxDis = data_distance[1,2]
for (i in 1:nrow(data_distance)){
  for (j in 1:ncol(data_distance)){
    if (i==j) next
    if (data_distance[i,j] < minDis) minDis = data_distance[i,j]
    if (data_distance[i,j] > maxDis) maxDis = data_distance[i,j]
  }
}

# Normalize Distance
export_distance = data.frame()
for (i in 1:nrow(data_distance)){
  for (j in 1:ncol(data_distance)){
    if (i == j) export_distance[i,j] = 0
    else export_distance[i,j] = round((data_distance[i,j] - minDis) / (maxDis - minDis),3)
  }
}
names = c()
for (i in 1:17){
  names[i] = colnames(data_distance)[i]
}
colnames(export_distance) = names


# ********** QUINTILES INCOME ********** #

# Monthly average income per capita at current prices by income quintile and by province
data <- read.table("E11.24.csv",  header = FALSE, sep = ";", skip = 2, nrows = 1)
names = c()
names[1] = "Province"
for (i in 2:ncol(data)){
  names[i] = as.character(data[1,i])
  names[i] = gsub("Pre. ", "", names[i])
  names[i] = gsub("General", "Income", names[i])
  names[i] = gsub("Quintile 1 20", "Inc1st", names[i])
  names[i] = gsub("Quintile 2 20", "Inc2nd", names[i])
  names[i] = gsub("Quintile 3 20", "Inc3rd", names[i])
  names[i] = gsub("Quintile 4 20", "Inc4th", names[i])
  names[i] = gsub("Quintile 5 20", "Inc5th", names[i])
  names[i] = gsub(" ", ".", names[i])
}
income1_1 = c(285.3, 447.9, 686.5)
income1_2 = c(447.5, 714.4, 1127.4)
income1_3 = c(607.2, 923.5, 1500.2)
income1_4 = c(850.5, 1204.5, 1988.7)
income1_5 = c(1705.4, 2786.6, 4326.9)

income2_1 = c(243.2, 296.7, 449.9)
income2_2 = c(388.9, 501.6, 741.7)
income2_3 = c(520.3, 710.5, 1053.2)
income2_4 = c(722.4, 933.1, 1412.9)
income2_5 = c(1513.4, 1891.8, 2930.7)

income3_1 = c(205.7, 240.1, 357.7)
income3_2 = c(325.1, 381.1, 617.8)
income3_3 = c(473.3, 551.4, 907.0)
income3_4 = c(657.2, 768.9, 1260.3)
income3_5 = c(1635.3, 1939.5, 2982.1)

income4_1 = c(430.8, 553.6, 826.7)
income4_2 = c(635.4, 823.5, 1182.6)
income4_3 = c(870.0, 1076.2, 1542.4)
income4_4 = c(1219.0, 1493.3, 2139.6)
income4_5 = c(2668.3, 3452.8, 5252.3)

income5_1 = c(172.0, 213.0, 331.3)
income5_2 = c(297.7, 370.1, 533.5)
income5_3 = c(408.8, 522.7, 732.9)
income5_4 = c(554.1, 703.8, 1017.5)
income5_5 = c(1069.8, 1326.6, 2076.9)

income6_1 = c(162.5, 212.9, 323.0)
income6_2 = c(283.1, 363.8, 510.8)
income6_3 = c(384.7, 511.7, 728.2)
income6_4 = c(531.6, 701.6, 1060.2)
income6_5 = c(1030.3, 1358.6, 2158.4)

income7_1 = c(156.0, 209.1, 287.7)
income7_2 = c(244.8, 357.4, 504.2)
income7_3 = c(318.3, 483.8, 713.9)
income7_4 = c(429.1, 673.7, 952.9)
income7_5 = c(943.6, 1344.7, 1896.6)

income8_1 = c(122.7, 158.8, 247.5)
income8_2 = c(219.0, 279.6, 416.2)
income8_3 = c(297.0, 378.4, 536.0)
income8_4 = c(432.9, 546.0, 790.1)
income8_5 = c(903.9, 1184.2, 1853.0)

income9_1 = c(157.2, 209.1, 335.4)
income9_2 = c(243.6, 336.1, 525.6)
income9_3 = c(333.8, 477.3, 693.5)
income9_4 = c(450.1, 660.8, 967.8)
income9_5 = c(930.4, 1216.0, 1963.7)

income10_1 = c(156.8, 196.7, 279.4)
income10_2 = c(239.4, 304.0, 448.5)
income10_3 = c(321.8, 411.9, 618.8)
income10_4 = c(462.2, 631.1, 931.6)
income10_5 = c(1193.2, 1503.4, 2169.2)

income11_1 = c(182.3, 242.8, 350.6)
income11_2 = c(283.0, 384.9, 605.2)
income11_3 = c(388.7, 508.3, 810.1)
income11_4 = c(551.0, 727.3, 1110.3)
income11_5 = c(1188.4, 1592.5, 2448.5)

income12_1 = c(157.9, 210.0, 327.1)
income12_2 = c(261.4, 358.6, 497.2)
income12_3 = c(369.6, 484.9, 675.1)
income12_4 = c(536.8, 674.8, 985.8)
income12_5 = c(1238.6, 1648.6, 2600.2)

income13_1 = c(210.7, 294.6, 381.2)
income13_2 = c(333.9, 494.2, 623.6)
income13_3 = c(430.0, 645.1, 954.1)
income13_4 = c(571.7, 851.8, 1267.5)
income13_5 = c(1083.3, 1614.1, 2426.3)

income14_1 = c(160.9, 212.3, 293.7)
income14_2 = c(269.3, 342.6, 462.3)
income14_3 = c(367.5, 465.1, 682.2)
income14_4 = c(497.5, 661.4, 965.6)
income14_5 = c(947.0, 1353.7, 2022.1)

income15_1 = c(133.0, 157.5, 242.8)
income15_2 = c(221.0, 273.4, 389.5)
income15_3 = c(296.2, 371.5, 502.8)
income15_4 = c(418.5, 531.1, 729.7)
income15_5 = c(906.0, 1139.8, 1769.6)

income16_1 = c(150.5, 197.6, 299.3)
income16_2 = c(268.5, 325.1, 497.6)
income16_3 = c(381.9, 490.3, 694.5)
income16_4 = c(520.7, 684.7, 993.4)
income16_5 = c(1020.2, 1356.8, 2136.0)

income17_1 = c(162.1, 209.1, 277.4)
income17_2 = c(289.8, 361.0, 537.2)
income17_3 = c(406.6, 506.3, 767.9)
income17_4 = c(581.4, 734.3, 1091.3)
income17_5 = c(1128.7, 1517.4, 2173.5)

data <- read.table("E11.24.csv",  header = FALSE, sep = ";", skip = 3)
data_income_quintile_1 = data[,6:9]
data_income_quintile_2 = data[,10:13]
data_income_quintile_3 = data[,14:17]
data_income_quintile_4 = data[,18:21]
data_income_quintile_5 = data[,22:25]
colnames(data_income_quintile_1) = names[6:9]
colnames(data_income_quintile_2) = names[10:13]
colnames(data_income_quintile_3) = names[14:17]
colnames(data_income_quintile_4) = names[18:21]
colnames(data_income_quintile_5) = names[22:25]
data_income_quintile_1 <- mutate(data_income_quintile_1, Province = provinces,
                                 Inc1st04 = c(income1_1[1], income2_1[1], income3_1[1], income4_1[1], income5_1[1], income6_1[1], 
                                              income7_1[1], income8_1[1], income9_1[1], income10_1[1], income11_1[1], income12_1[1], 
                                              income13_1[1], income14_1[1], income15_1[1], income16_1[1], income17_1[1]),
                                 Inc1st06 = c(income1_1[2], income2_1[2], income3_1[2], income4_1[2], income5_1[2], income6_1[2], 
                                              income7_1[2], income8_1[2], income9_1[2], income10_1[2], income11_1[2], income12_1[1], 
                                              income13_1[2], income14_1[2], income15_1[2], income16_1[2], income17_1[2]),
                                 Inc1st08 = c(income1_1[3], income2_1[3], income3_1[3], income4_1[3], income5_1[3], income6_1[3], 
                                              income7_1[3], income8_1[3], income9_1[3], income10_1[3], income11_1[3], income12_1[3], 
                                              income13_1[3], income14_1[3], income15_1[3], income16_1[3], income17_1[3]))
data_income_quintile_1 <- mutate(data_income_quintile_1, 
                                 Inc1st05 = (Inc1st04 + Inc1st06) / 2, 
                                 Inc1st07 = (Inc1st06 + Inc1st08) / 2,
                                 Inc1st09 = (Inc1st08 + Inc1st10) / 2,
                                 Inc1st11 = (Inc1st10 + Inc1st12) / 2,
                                 Inc1st13 = (Inc1st12 + Inc1st14) / 2,
                                 Inc1st15 = (Inc1st14 + Inc1st16) / 2)
data_income_quintile_1 <- select(data_income_quintile_1, Province, Inc1st04, Inc1st05, Inc1st06, 
                                 Inc1st07, Inc1st08, Inc1st09, Inc1st10, Inc1st11,
                                 Inc1st12, Inc1st13, Inc1st14, Inc1st15, Inc1st16)
data_income_quintile_1[,1:2] = list(NULL)
export_income_quintile_1 = data.frame()
for (i in 1:ncol(data_income_quintile_1)){
  minInc = min(data_income_quintile_1[,i])
  maxInc = max(data_income_quintile_1[,i])
  for (j in 1:nrow(data_income_quintile_1)){
    export_income_quintile_1[j,i] = round((data_income_quintile_1[j,i] - minInc) / (maxInc - minInc), 3)
  }
}
colnames(export_income_quintile_1) = colnames(data_income_quintile_1)

#####################################################

data_income_quintile_2 <- mutate(data_income_quintile_2, Province = provinces,
                                 Inc2nd04 = c(income1_2[1], income2_2[1], income3_2[1], income4_2[1], income5_2[1], income6_2[1], 
                                              income7_2[1], income8_2[1], income9_2[1], income10_2[1], income11_2[1], income12_2[1], 
                                              income13_2[1], income14_2[1], income15_2[1], income16_2[1], income17_2[1]),
                                 Inc2nd06 = c(income1_2[2], income2_2[2], income3_2[2], income4_2[2], income5_2[2], income6_2[2], 
                                              income7_2[2], income8_2[2], income9_2[2], income10_2[2], income11_2[2], income12_2[1], 
                                              income13_2[2], income14_2[2], income15_2[2], income16_2[2], income17_2[2]),
                                 Inc2nd08 = c(income1_2[3], income2_2[3], income3_2[3], income4_2[3], income5_2[3], income6_2[3], 
                                              income7_2[3], income8_2[3], income9_2[3], income10_2[3], income11_2[3], income12_2[3], 
                                              income13_2[3], income14_2[3], income15_2[3], income16_2[3], income17_2[3]))
data_income_quintile_2 <- mutate(data_income_quintile_2, 
                                 Inc2nd05 = (Inc2nd04 + Inc2nd06) / 2, 
                                 Inc2nd07 = (Inc2nd06 + Inc2nd08) / 2,
                                 Inc2nd09 = (Inc2nd08 + Inc2nd10) / 2,
                                 Inc2nd11 = (Inc2nd10 + Inc2nd12) / 2,
                                 Inc2nd13 = (Inc2nd12 + Inc2nd14) / 2,
                                 Inc2nd15 = (Inc2nd14 + Inc2nd16) / 2)
data_income_quintile_2 <- select(data_income_quintile_2, Province, Inc2nd04, Inc2nd05, Inc2nd06, 
                                 Inc2nd07, Inc2nd08, Inc2nd09, Inc2nd10, Inc2nd11,
                                 Inc2nd12, Inc2nd13, Inc2nd14, Inc2nd15, Inc2nd16)
data_income_quintile_2[,1:2] = list(NULL)
export_income_quintile_2 = data.frame()
for (i in 1:ncol(data_income_quintile_2)){
  minInc = min(data_income_quintile_2[,i])
  maxInc = max(data_income_quintile_2[,i])
  for (j in 1:nrow(data_income_quintile_2)){
    export_income_quintile_2[j,i] = round((data_income_quintile_2[j,i] - minInc) / (maxInc - minInc), 3)
  }
}
colnames(export_income_quintile_2) = colnames(data_income_quintile_2)

#####################################################

data_income_quintile_3 <- mutate(data_income_quintile_3, Province = provinces,
                                 Inc3rd04 = c(income1_3[1], income2_3[1], income3_3[1], income4_3[1], income5_3[1], income6_3[1], 
                                              income7_3[1], income8_3[1], income9_3[1], income10_3[1], income11_3[1], income12_3[1], 
                                              income13_3[1], income14_3[1], income15_3[1], income16_3[1], income17_3[1]),
                                 Inc3rd06 = c(income1_3[2], income2_3[2], income3_3[2], income4_3[2], income5_3[2], income6_3[2], 
                                              income7_3[2], income8_3[2], income9_3[2], income10_3[2], income11_3[2], income12_3[1], 
                                              income13_3[2], income14_3[2], income15_3[2], income16_3[2], income17_3[2]),
                                 Inc3rd08 = c(income1_3[3], income2_3[3], income3_3[3], income4_3[3], income5_3[3], income6_3[3], 
                                              income7_3[3], income8_3[3], income9_3[3], income10_3[3], income11_3[3], income12_3[3], 
                                              income13_3[3], income14_3[3], income15_3[3], income16_3[3], income17_3[3]))
data_income_quintile_3 <- mutate(data_income_quintile_3, 
                                 Inc3rd05 = (Inc3rd04 + Inc3rd06) / 2, 
                                 Inc3rd07 = (Inc3rd06 + Inc3rd08) / 2,
                                 Inc3rd09 = (Inc3rd08 + Inc3rd10) / 2,
                                 Inc3rd11 = (Inc3rd10 + Inc3rd12) / 2,
                                 Inc3rd13 = (Inc3rd12 + Inc3rd14) / 2,
                                 Inc3rd15 = (Inc3rd14 + Inc3rd16) / 2)
data_income_quintile_3 <- select(data_income_quintile_3, Province, Inc3rd04, Inc3rd05, Inc3rd06, 
                                 Inc3rd07, Inc3rd08, Inc3rd09, Inc3rd10, Inc3rd11,
                                 Inc3rd12, Inc3rd13, Inc3rd14, Inc3rd15, Inc3rd16)
data_income_quintile_3[,1:2] = list(NULL)
export_income_quintile_3 = data.frame()
for (i in 1:ncol(data_income_quintile_3)){
  minInc = min(data_income_quintile_3[,i])
  maxInc = max(data_income_quintile_3[,i])
  for (j in 1:nrow(data_income_quintile_3)){
    export_income_quintile_3[j,i] = round((data_income_quintile_3[j,i] - minInc) / (maxInc - minInc), 3)
  }
}
colnames(export_income_quintile_3) = colnames(data_income_quintile_3)

#####################################################

data_income_quintile_4 <- mutate(data_income_quintile_4, Province = provinces,
                                 Inc4th04 = c(income1_4[1], income2_4[1], income3_4[1], income4_4[1], income5_4[1], income6_4[1], 
                                              income7_4[1], income8_4[1], income9_4[1], income10_4[1], income11_4[1], income12_4[1], 
                                              income13_4[1], income14_4[1], income15_4[1], income16_4[1], income17_4[1]),
                                 Inc4th06 = c(income1_4[2], income2_4[2], income3_4[2], income4_4[2], income5_4[2], income6_4[2], 
                                              income7_4[2], income8_4[2], income9_4[2], income10_4[2], income11_4[2], income12_4[1], 
                                              income13_4[2], income14_4[2], income15_4[2], income16_4[2], income17_4[2]),
                                 Inc4th08 = c(income1_4[3], income2_4[3], income3_4[3], income4_4[3], income5_4[3], income6_4[3], 
                                              income7_4[3], income8_4[3], income9_4[3], income10_4[3], income11_4[3], income12_4[3], 
                                              income13_4[3], income14_4[3], income15_4[3], income16_4[3], income17_4[3]))
data_income_quintile_4 <- mutate(data_income_quintile_4, 
                                 Inc4th05 = (Inc4th04 + Inc4th06) / 2, 
                                 Inc4th07 = (Inc4th06 + Inc4th08) / 2,
                                 Inc4th09 = (Inc4th08 + Inc4th10) / 2,
                                 Inc4th11 = (Inc4th10 + Inc4th12) / 2,
                                 Inc4th13 = (Inc4th12 + Inc4th14) / 2,
                                 Inc4th15 = (Inc4th14 + Inc4th16) / 2)
data_income_quintile_4 <- select(data_income_quintile_4, Province, Inc4th04, Inc4th05, Inc4th06, 
                                 Inc4th07, Inc4th08, Inc4th09, Inc4th10, Inc4th11,
                                 Inc4th12, Inc4th13, Inc4th14, Inc4th15, Inc4th16)
data_income_quintile_4[,1:2] = list(NULL)
export_income_quintile_4 = data.frame()
for (i in 1:ncol(data_income_quintile_4)){
  minInc = min(data_income_quintile_4[,i])
  maxInc = max(data_income_quintile_4[,i])
  for (j in 1:nrow(data_income_quintile_4)){
    export_income_quintile_4[j,i] = round((data_income_quintile_4[j,i] - minInc) / (maxInc - minInc), 3)
  }
}
colnames(export_income_quintile_4) = colnames(data_income_quintile_4)

#####################################################

data_income_quintile_5 <- mutate(data_income_quintile_5, Province = provinces,
                                 Inc5th04 = c(income1_5[1], income2_5[1], income3_5[1], income4_5[1], income5_5[1], income6_5[1], 
                                              income7_5[1], income8_5[1], income9_5[1], income10_5[1], income11_5[1], income12_5[1], 
                                              income13_5[1], income14_5[1], income15_5[1], income16_5[1], income17_5[1]),
                                 Inc5th06 = c(income1_5[2], income2_5[2], income3_5[2], income4_5[2], income5_5[2], income6_5[2], 
                                              income7_5[2], income8_5[2], income9_5[2], income10_5[2], income11_5[2], income12_5[1], 
                                              income13_5[2], income14_5[2], income15_5[2], income16_5[2], income17_5[2]),
                                 Inc5th08 = c(income1_5[3], income2_5[3], income3_5[3], income4_5[3], income5_5[3], income6_5[3], 
                                              income7_5[3], income8_5[3], income9_5[3], income10_5[3], income11_5[3], income12_5[3], 
                                              income13_5[3], income14_5[3], income15_5[3], income16_5[3], income17_5[3]))
data_income_quintile_5 <- mutate(data_income_quintile_5, 
                                 Inc5th05 = (Inc5th04 + Inc5th06) / 2, 
                                 Inc5th07 = (Inc5th06 + Inc5th08) / 2,
                                 Inc5th09 = (Inc5th08 + Inc5th10) / 2,
                                 Inc5th11 = (Inc5th10 + Inc5th12) / 2,
                                 Inc5th13 = (Inc5th12 + Inc5th14) / 2,
                                 Inc5th15 = (Inc5th14 + Inc5th16) / 2)
data_income_quintile_5 <- select(data_income_quintile_5, Province, Inc5th04, Inc5th05, Inc5th06, 
                                 Inc5th07, Inc5th08, Inc5th09, Inc5th10, Inc5th11,
                                 Inc5th12, Inc5th13, Inc5th14, Inc5th15, Inc5th16)
data_income_quintile_5[,1:2] = list(NULL)
export_income_quintile_5 = data.frame()
for (i in 1:ncol(data_income_quintile_5)){
  minInc = min(data_income_quintile_5[,i])
  maxInc = max(data_income_quintile_5[,i])
  for (j in 1:nrow(data_income_quintile_5)){
    export_income_quintile_5[j,i] = round((data_income_quintile_5[j,i] - minInc) / (maxInc - minInc), 3)
  }
}
colnames(export_income_quintile_5) = colnames(data_income_quintile_5)


# ********** QUINTILES EXPENDITURE ********** #

# Living Expenditure by province
# Quintile 1
data_expenditure_quintile_1 <- read.table("raw_expenditure.csv",  header = FALSE, sep = ",", skip = 61, nrows = 17)
data <- read.table("raw_expenditure.csv",  header = FALSE, sep = ",", skip = 60, nrows = 1)
names = c()
names[1] = "Province"
for (i in 2:13){
  names[i] = as.character(data[1,i-1])
  names[i] = gsub("Expenditure.Quintile.1.20", "Exp1st", names[i])
}
data_expenditure_quintile_1 <- mutate(data_expenditure_quintile_1, Province = provinces)
data_expenditure_quintile_1 <- select(data_expenditure_quintile_1, Province, everything())
colnames(data_expenditure_quintile_1) = names
data_expenditure_quintile_1[,1] = list(NULL)

# Normalize of expenditure quintile 1
export_expenditure_quintile_1 = data.frame()
for (i in 1:ncol(data_expenditure_quintile_1)){
  minExp = min(data_expenditure_quintile_1[,i])
  maxExp = max(data_expenditure_quintile_1[,i])
  for (j in 1:nrow(data_expenditure_quintile_1)){
    export_expenditure_quintile_1[j,i] = round((data_expenditure_quintile_1[j,i] - minExp) / (maxExp - minExp), 3)
  }
}
colnames(export_expenditure_quintile_1) = names[-1]

# Quintile 2
data_expenditure_quintile_2 <- read.table("raw_expenditure.csv",  header = FALSE, sep = ",", skip = 82, nrows = 17)
data <- read.table("raw_expenditure.csv",  header = FALSE, sep = ",", skip = 81, nrows = 1)
names = c()
names[1] = "Province"
for (i in 2:13){
  names[i] = as.character(data[1,i-1])
  names[i] = gsub("Expenditure.Quintile.2.20", "Exp2nd", names[i])
}
data_expenditure_quintile_2 <- mutate(data_expenditure_quintile_2, Province = provinces)
data_expenditure_quintile_2 <- select(data_expenditure_quintile_2, Province, everything())
colnames(data_expenditure_quintile_2) = names
data_expenditure_quintile_2[,1] = list(NULL)

# Normalize of expenditure quintile 2
export_expenditure_quintile_2 = data.frame()
for (i in 1:ncol(data_expenditure_quintile_2)){
  minExp = min(data_expenditure_quintile_2[,i])
  maxExp = max(data_expenditure_quintile_2[,i])
  for (j in 1:nrow(data_expenditure_quintile_2)){
    export_expenditure_quintile_2[j,i] = round((data_expenditure_quintile_2[j,i] - minExp) / (maxExp - minExp), 3)
  }
}
colnames(export_expenditure_quintile_2) = names[-1]

# Quintile 3
data_expenditure_quintile_3 <- read.table("raw_expenditure.csv",  header = FALSE, sep = ",", skip = 103, nrows = 17)
data <- read.table("raw_expenditure.csv",  header = FALSE, sep = ",", skip = 102, nrows = 1)
names = c()
names[1] = "Province"
for (i in 2:13){
  names[i] = as.character(data[1,i-1])
  names[i] = gsub("Expenditure.Quintile.3.20", "Exp3rd", names[i])
}
data_expenditure_quintile_3 <- mutate(data_expenditure_quintile_3, Province = provinces)
data_expenditure_quintile_3 <- select(data_expenditure_quintile_3, Province, everything())
colnames(data_expenditure_quintile_3) = names
data_expenditure_quintile_3[,1] = list(NULL)

# Normalize of expenditure quintile 3
export_expenditure_quintile_3 = data.frame()
for (i in 1:ncol(data_expenditure_quintile_3)){
  minExp = min(data_expenditure_quintile_3[,i])
  maxExp = max(data_expenditure_quintile_3[,i])
  for (j in 1:nrow(data_expenditure_quintile_3)){
    export_expenditure_quintile_3[j,i] = round((data_expenditure_quintile_3[j,i] - minExp) / (maxExp - minExp), 3)
  }
}
colnames(export_expenditure_quintile_3) = names[-1]

# Quintile 4
data_expenditure_quintile_4 <- read.table("raw_expenditure.csv",  header = FALSE, sep = ",", skip = 124, nrows = 17)
data <- read.table("raw_expenditure.csv",  header = FALSE, sep = ",", skip = 123, nrows = 1)
names = c()
names[1] = "Province"
for (i in 2:13){
  names[i] = as.character(data[1,i-1])
  names[i] = gsub("Expenditure.Quintile.4.20", "Exp4th", names[i])
}
data_expenditure_quintile_4 <- mutate(data_expenditure_quintile_4, Province = provinces)
data_expenditure_quintile_4 <- select(data_expenditure_quintile_4, Province, everything())
colnames(data_expenditure_quintile_4) = names
data_expenditure_quintile_4[,1] = list(NULL)

# Normalize of expenditure quintile 4
export_expenditure_quintile_4 = data.frame()
for (i in 1:ncol(data_expenditure_quintile_4)){
  minExp = min(data_expenditure_quintile_4[,i])
  maxExp = max(data_expenditure_quintile_4[,i])
  for (j in 1:nrow(data_expenditure_quintile_4)){
    export_expenditure_quintile_4[j,i] = round((data_expenditure_quintile_4[j,i] - minExp) / (maxExp - minExp), 3)
  }
}
colnames(export_expenditure_quintile_4) = names[-1]

# Quintile 5
data_expenditure_quintile_5 <- read.table("raw_expenditure.csv",  header = FALSE, sep = ",", skip = 145, nrows = 17)
data <- read.table("raw_expenditure.csv",  header = FALSE, sep = ",", skip = 144, nrows = 1)
names = c()
names[1] = "Province"
for (i in 2:13){
  names[i] = as.character(data[1,i-1])
  names[i] = gsub("Expenditure.Quintile.5.20", "Exp5th", names[i])
}
data_expenditure_quintile_5 <- mutate(data_expenditure_quintile_5, Province = provinces)
data_expenditure_quintile_5 <- select(data_expenditure_quintile_5, Province, everything())
colnames(data_expenditure_quintile_5) = names
data_expenditure_quintile_5[,1] = list(NULL)

# Normalize of expenditure quintile 5
export_expenditure_quintile_5 = data.frame()
for (i in 1:ncol(data_expenditure_quintile_5)){
  minExp = min(data_expenditure_quintile_5[,i])
  maxExp = max(data_expenditure_quintile_5[,i])
  for (j in 1:nrow(data_expenditure_quintile_5)){
    export_expenditure_quintile_5[j,i] = round((data_expenditure_quintile_5[j,i] - minExp) / (maxExp - minExp), 3)
  }
}
colnames(export_expenditure_quintile_5) = names[-1]


##############################  EXTREME WEATHERS  ######################################

# Climate data (No of Hazards, Vulnerability Index, ID)
data_climate <- read.table("raw_climate.csv",  header = FALSE, sep = ",", skip = 1, nrows = 17)
# Update "Hau Giang" data as average of the whole region since missing data (Index: 14)
for (i in 1:ncol(data_climate)){
  data_climate[14,i] = mean(data_climate[,i])
}
# Average effect of one hazard by province
for (i in 2:ncol(data_climate)){
  for (j in 1:nrow(data_climate)){
    data_climate[j,i] = round(data_climate[j,i] / data_climate[j,1], 3)
  }
}

data_climate_nor = data.frame()
for (i in 1:ncol(data_climate)){
  minCli = min(data_climate[,i])
  maxCli = max(data_climate[,i])
  for (j in 1:nrow(data_climate)){
    if (i == 1) data_climate_nor[j,i] = data_climate[j,i]
    else data_climate_nor[j,i] = round((data_climate[j,i] - minCli) / (maxCli - minCli), 3)
  }
}
# 27 years (from 1989 - 2015)
data_climate_nor = mutate(data_climate_nor, NoHazards = V1 / 27, VulIndex = (V2 + V3 + V4 + V5 + V6) / 5)
data_climate_nor = select(data_climate_nor, NoHazards, VulIndex)

##############################  EXPORT DATA  ######################################

# Combine data (except distance data)
export_id = c(4, 0, 3, 9, 12, 14, 6, 15, 16, 1, 2, 11, 8, 10, 13, 5, 7)

export_pop_2004 = data.frame(Pop04 = data_pop_2004, ID = export_id)
# all provinces in South East is set to 0
export_pop_2004[c(1:4),1] = 0
export_pop_natural_rate = mutate(data_pop_natural_rate, ID = export_id)
export_employment = mutate(export_employment, ID = export_id)
export_poverty = mutate(export_poverty, ID = export_id)
# export_under = mutate(export_under, ID = export_id)
export_education = mutate(export_education, ID = export_id)
export_health = mutate(export_health, ID = export_id)
export_distance = mutate(export_distance, ID = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16))
export_climate = mutate(data_climate_nor, ID = export_id)
# export_saving_quintile_1 = mutate(export_saving_quintile_1, ID = export_id)
# export_saving_quintile_2 = mutate(export_saving_quintile_2, ID = export_id)
# export_saving_quintile_3 = mutate(export_saving_quintile_3, ID = export_id)
# export_saving_quintile_4 = mutate(export_saving_quintile_4, ID = export_id)
# export_saving_quintile_5 = mutate(export_saving_quintile_5, ID = export_id)
export_income_quintile_1 = mutate(export_income_quintile_1, ID = export_id)
export_income_quintile_2 = mutate(export_income_quintile_2, ID = export_id)
export_income_quintile_3 = mutate(export_income_quintile_3, ID = export_id)
export_income_quintile_4 = mutate(export_income_quintile_4, ID = export_id)
export_income_quintile_5 = mutate(export_income_quintile_5, ID = export_id)
export_expenditure_quintile_1 = mutate(export_expenditure_quintile_1, ID = export_id)
export_expenditure_quintile_2 = mutate(export_expenditure_quintile_2, ID = export_id)
export_expenditure_quintile_3 = mutate(export_expenditure_quintile_3, ID = export_id)
export_expenditure_quintile_4 = mutate(export_expenditure_quintile_4, ID = export_id)
export_expenditure_quintile_5 = mutate(export_expenditure_quintile_5, ID = export_id)

export_data = data.frame(ID = export_id, Province = provinces)
export_data = merge(export_data, export_pop_2004, by = "ID")
export_data = merge(export_data, export_pop_natural_rate, by = "ID")
export_data = merge(export_data, export_employment, by = "ID")
export_data = merge(export_data, export_poverty, by = "ID")
# export_data = merge(export_data, export_under, by = "ID")
export_data = merge(export_data, export_education, by = "ID")
export_data = merge(export_data, export_health, by = "ID")
export_data = merge(export_data, export_distance, by = "ID")
export_data = merge(export_data, export_climate, by = "ID")
# export_data = merge(export_data, export_saving_quintile_1, by = "ID")
# export_data = merge(export_data, export_saving_quintile_2, by = "ID")
# export_data = merge(export_data, export_saving_quintile_3, by = "ID")
# export_data = merge(export_data, export_saving_quintile_4, by = "ID")
# export_data = merge(export_data, export_saving_quintile_5, by = "ID")
export_data = merge(export_data, export_income_quintile_1, by = "ID")
export_data = merge(export_data, export_income_quintile_2, by = "ID")
export_data = merge(export_data, export_income_quintile_3, by = "ID")
export_data = merge(export_data, export_income_quintile_4, by = "ID")
export_data = merge(export_data, export_income_quintile_5, by = "ID")
export_data = merge(export_data, export_expenditure_quintile_1, by = "ID")
export_data = merge(export_data, export_expenditure_quintile_2, by = "ID")
export_data = merge(export_data, export_expenditure_quintile_3, by = "ID")
export_data = merge(export_data, export_expenditure_quintile_4, by = "ID")
export_data = merge(export_data, export_expenditure_quintile_5, by = "ID")

# WriteXLS(export_data, "data.xls")
