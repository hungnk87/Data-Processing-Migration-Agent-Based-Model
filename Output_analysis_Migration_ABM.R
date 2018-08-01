library(tidyverse)
library(magrittr)

remove(list=ls())

setwd("/Users/hungnguyenkhanh/Desktop/Model/ABMMigration/logs")

# MIGRATION DATA FROM GSO -------------------------------------------------------------------------

data_migration <- read_delim("E02.21-23.csv", delim = ";", skip = 2)

# Update attribute names
colnames(data_migration) <- data_migration %>% 
  colnames() %>% 
  str_replace_all(c("Prel. " = "", " " = "", "-migrationrate20" = "Rate"))
# Divide names example names[str_detect(names, "In")]

# Divide into seperate in-, out-, net- migration and update relevant attributes
in_data <- data_migration %>% 
  select(Province, starts_with("In")) %>% 
  mutate(InRate06 = (InRate05 + InRate07)/2) %>% 
  select(order(colnames(.)))
in_data %<>% mutate(Avg05_10 = round(rowMeans(in_data %>% select(c(InRate05:InRate10))),3), 
                    Avg11_16 = round(rowMeans(in_data %>% select(c(InRate11:InRate16))),3), 
                    Avg05_16 = round(rowMeans(in_data %>% select(c(InRate05:InRate16))),3)) %>% 
  select(-starts_with("In"))
in_data <- in_data[-c(1:4), ]

out_data <- data_migration %>% 
  select(Province, starts_with("Out")) %>% 
  mutate(OutRate06 = (OutRate05 + OutRate07)/2) %>% 
  select(order(colnames(.)))
out_data %<>% mutate(Avg05_10 = round(rowMeans(out_data %>% select(c(OutRate05:OutRate10))),3), 
                    Avg11_16 = round(rowMeans(out_data %>% select(c(OutRate11:OutRate16))),3), 
                    Avg05_16 = round(rowMeans(out_data %>% select(c(OutRate05:OutRate16))),3)) %>% 
  select(-starts_with("Out"))
out_data <- out_data[-c(1:4), ]

net_data <- data_migration %>% 
  select(Province, starts_with("Net")) %>% 
  mutate(NetRate06 = (NetRate05 + NetRate07)/2) %>% 
  select(order(colnames(.)))
net_data %<>% mutate(Avg05_10 = round(rowMeans(net_data %>% select(c(NetRate05:NetRate10))),3), 
                    Avg11_16 = round(rowMeans(net_data %>% select(c(NetRate11:NetRate16))),3), 
                    Avg05_16 = round(rowMeans(net_data %>% select(c(NetRate05:NetRate16))),3)) %>% 
  select(-starts_with("Net"))
net_data <- net_data[-c(1:4), ]

# Quick Plot --------------------------------------------------------------------------------------
net_data %>% 
  ggplot() + 
  # Sort alphabetically 
  geom_col(aes(Province, Avg05_10)) + 
  # Sort based on values: aes(x = reorder(Province, Avg05_10), y = Avg05_10)
  coord_flip() +
  labs(caption = "Data Source: General Statistics Office of Vietnam",
       y = "Net-Migration", 
       x = "Province")

# MIGRATION SIMULATION RESULT ---------------------------------------------------------------------

result = read_delim("CCIM_AllSteps_testInitial.txt", delim = ";", col_names = FALSE)

# Extract colnames 
row1 <- unlist(result[1,])
names <- c()
# Start from "Step"
for(i in 3:length(row1)){
  if (i %% 2 == 1){ names[(i - 1) / 2] = row1[i] }
}

# Create complete data with update colnames
data <- tibble(Step = unlist(result[,4])) # Firstly add "Step" into the tibble
# Then bind_cols could be performed without error
for(i in 6:ncol(result)){
  if (i %% 2 == 0){
    data <- bind_cols(data, result[,i])
  }
}
colnames(data) <- names
# Select "Step" and all In-, Out- Migration 
data %<>% select(Step, InMigr0:OutMigr16)

# Province list (ordered from shapefile and ABM model)
provinces = c("Dong Nai", "Dong Thap", "An Giang", "Vung Tau", "Binh Duong", "Bac Lieu", 
              "Ben Tre", "Ca Mau", "Can Tho", "Ho Chi Minh city", "Hau Giang", "Kien Giang", 
              "Long An", "Soc Trang", "Tien Giang", "Tra Vinh", "Vinh Long")

# Tibbles stored migration rate results 
in_result = out_result = net_result = tibble()

# Extracted individual Sensitivity Analysis run; 144 steps in each run
for (k in 1: (nrow(data) / 144)){
  
  min_row <- (k - 1) * 144 + 1
  max_row <- min_row + 144 - 1
  dataSA <- data[c(min_row:max_row),]
  # Filter the two months: Jun (5) and December (11) (time migration decision occurs)
  dataSA %<>% filter((Step + 1) %% 6 == 0) %>% 
    select(-Step)
  
  # Sum migration flows of two times in a year and record all in a tibble (year_twelve)
  year_twelve <- tibble()
  for(i in seq(1, nrow(dataSA), 2)){
    year_one <- colSums(dataSA[i:(i+1), ])
    year_twelve <- bind_rows(year_twelve, year_one)
  }
  colnames(year_twelve) <- colnames(dataSA)
  
  # Transpose 
  year_twelve <- as_tibble(t(year_twelve))
  
  # In-, Out and Net-migration flows of 17 Provinces
  in_sim <- year_twelve[c(1:17), ]
  out_sim <- year_twelve[c(18:34), ]
  net_sim <- in_sim - out_sim
  
  # Update colnames 
  in_name = c()
  for (i in 1:12){
    in_name[i] = paste("In", toString(i + 2004), sep = "")
    in_name[i] = str_replace(in_name[i], "20", "")
  }
  out_name <- in_name %>% str_replace("In", "Out")
  net_name <- in_name %>% str_replace("In", "Net")
  
  colnames(in_sim) <- in_name
  colnames(out_sim) <- out_name
  colnames(net_sim) <- net_name
  
  # Remove 4 provinces in SE region & calculate the average of migration flows
  in_sim %<>% mutate(Province = provinces) %>% 
    filter(Province != "Dong Nai" & Province != "Vung Tau" &
             Province != "Binh Duong" & Province != "Ho Chi Minh city")
  in_sim %<>% mutate(Avg05_10 = round(rowMeans(in_sim %>% select(c(In05:In10))),3),
                     Avg11_16 = round(rowMeans(in_sim %>% select(c(In11:In16))),3),
                     Avg05_16 = round(rowMeans(in_sim %>% select(c(In05:In16))),3)) %>% 
    select(Province, starts_with("Avg"))
  
  out_sim %<>% mutate(Province = provinces) %>% 
    filter(Province != "Dong Nai" & Province != "Vung Tau" &
             Province != "Binh Duong" & Province != "Ho Chi Minh city")
  out_sim %<>% mutate(Avg05_10 = round(rowMeans(out_sim %>% select(c(Out05:Out10))),3),
                      Avg11_16 = round(rowMeans(out_sim %>% select(c(Out11:Out16))),3),
                      Avg05_16 = round(rowMeans(out_sim %>% select(c(Out05:Out16))),3)) %>% 
    select(Province, starts_with("Avg"))
  
  net_sim %<>% mutate(Province = provinces) %>% 
    filter(Province != "Dong Nai" & Province != "Vung Tau" &
             Province != "Binh Duong" & Province != "Ho Chi Minh city")
  net_sim %<>% mutate(Avg05_10 = round(rowMeans(net_sim %>% select(c(Net05:Net10))),3),
                      Avg11_16 = round(rowMeans(net_sim %>% select(c(Net11:Net16))),3),
                      Avg05_16 = round(rowMeans(net_sim %>% select(c(Net05:Net16))),3)) %>% 
    select(Province, starts_with("Avg"))
  
  # Add to the final tibbles _result (Remove Province column)
  if (k == 1) in_result = in_sim[-1]
  else in_result = bind_cols(in_sim[-1], in_result)
  if (k == 1) out_result = out_sim[-1]
  else out_result = bind_cols(out_sim[-1], out_result)
  if (k == 1) net_result = net_sim[-1]
  else net_result = bind_cols(net_sim[-1], net_result)
}

# Update Province column back to final tibbles 
in_result = bind_cols(Province = in_sim$Province, in_result)
out_result = bind_cols(Province = out_sim$Province, out_result)
net_result = bind_cols(Province = net_sim$Province, net_result)

nameResult = c()
nameResult[1] = "Province"
for (k in 1:(nrow(data) / 144)){
  index = (k - 1) * 3
  nameResult[1 + index + 1] = paste("Exp", k, "05_10", sep = "_") 
  nameResult[1 + index + 2] = paste("Exp", k, "11_16", sep = "_") 
  nameResult[1 + index + 3] = paste("Exp", k, "05_16", sep = "_") 
}
colnames(in_result) = colnames(out_result) = colnames(net_result) = nameResult

library(WriteXLS)
WriteXLS(net_result, "result.xls")


