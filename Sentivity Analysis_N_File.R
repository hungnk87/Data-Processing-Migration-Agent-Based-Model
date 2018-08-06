library(tidyverse)
library(magrittr)

remove(list=ls())

# NET-MIGRATION DATA FROM GSO -------------------------------------------------------------------------

data_migration <- read_delim("E02.21-23.csv", delim = ";", skip = 2)

# Update attribute names
colnames(data_migration) <- data_migration %>% 
  colnames() %>% 
  str_replace_all(c("Prel. " = "", " " = "", "-migrationrate20" = "Rate"))

# net-migration and update relevant attributes
net_data <- data_migration %>% 
  select(Province, starts_with("Net")) %>% 
  mutate(NetRate06 = (NetRate05 + NetRate07)/2) %>% 
  select(order(colnames(.)))
net_data %<>% mutate(Avg05_10 = round(rowMeans(net_data %>% select(c(NetRate05:NetRate10))),3), 
                     Avg11_16 = round(rowMeans(net_data %>% select(c(NetRate11:NetRate16))),3), 
                     Avg05_16 = round(rowMeans(net_data %>% select(c(NetRate05:NetRate16))),3)) %>% 
  select(-starts_with("Net"))
net_data <- net_data[-c(1:4), ]

# Sort alphabetically by Province 
net_data %<>% arrange(Province)

# NET-MIGRATION SIMULATION RESULT ---------------------------------------------------------------------

# Province list (ordered from shapefile and ABM model)
provinces = c("Dong Nai", "Dong Thap", "An Giang", "Vung Tau", "Binh Duong", "Bac Lieu", 
              "Ben Tre", "Ca Mau", "Can Tho", "Ho Chi Minh city", "Hau Giang", "Kien Giang", 
              "Long An", "Soc Trang", "Tien Giang", "Tra Vinh", "Vinh Long")

list_files <- list.files(pattern = "AllSteps")
list_results <- lapply(list_files, FUN = function(x) read_delim(x, delim = ";", col_names = FALSE))

# Extract names
row1 <- unlist(list_results[[1]][1,])
names <- c()
# Start from "Step"
for(i in 3:length(row1)){
  if (i %% 2 == 1){ names[(i - 1) / 2] = row1[i] }
}

final_result <- tibble()

for (j in 1:length(list_results)){
  
  # Extract individual result 
  result <- list_results[[j]]
  
  # Create complete individual data with update colnames
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
  
  # Tibbles stored net migration rate results 
  net_result = tibble()
  
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
    net_name = c()
    for (i in 1:12){
      net_name[i] = paste("Net", toString(i + 2004), sep = "")
      net_name[i] = str_replace(net_name[i], "20", "")
    }
    colnames(net_sim) <- net_name
    
    # Remove 4 provinces in SE region 
    # Sort alphabetically by Province name 
    # Calculate the Averages, Diff and Abs_Diff
    # Remove yearly migration data column
    net_sim %<>% mutate(Province = provinces) %>% 
      filter(Province != "Dong Nai" & Province != "Vung Tau" &
               Province != "Binh Duong" & Province != "Ho Chi Minh city") %>% 
      arrange(Province)
    net_sim %<>% mutate(Avg05_16 = round(rowMeans(net_sim %>% select(c(Net05:Net16))),3),
                        Diff = net_data$Avg05_16 - Avg05_16, 
                        Abs_Diff = abs(Diff)) %>% 
      select(-starts_with("Net"))
    
    # Add to the final tibbles _result (Remove Province column)
    if (k == 1) net_result = net_sim[-1]
    else net_result = bind_cols(net_sim[-1], net_result)
  }
  
  # Update Province column back to final tibbles 
  net_result = bind_cols(Province = net_sim$Province, net_result)
  
  sum_result = c()
  for (k in 1:(nrow(data) / 144)){
    # Update the sum of Abs_Diff in net_result 
    sum_result[k] = sum(net_result[, 1 + k * 3], na.rm = TRUE)
  }
  
  if (j == 1) final_result = as.tibble(sum_result)
  else final_result = bind_cols(final_result, as.tibble(sum_result))
}

SA_column = c()
for (k in 1:(nrow(data) / 144)) SA_column[k] = paste0("SA", toString(k))
final_result %<>% mutate(SA = SA_column) %>% select(SA, everything())

final_names = c()
final_names[1] = "SA"
for(i in 1:length(list_files)){
  final_names[i+1] = paste0("File", toString(i))
}
colnames(final_result) <- final_names

final_result %<>% gather(key = "File", value = "Value", File1:File5)
final_result %>% ggplot(aes(SA, File)) + geom_tile(aes(fill = Value))
