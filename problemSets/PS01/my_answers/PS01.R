library(tidyverse)
library(stargazer)
library(tidyr)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#Loading data into dataset
MEP_data <- read_csv("mep_info_EP1.csv")
EP1_data <- read_csv("rcv_ep1.txt")

#Pivoting data frame from wide to long 
EP1_data <- EP1_data %>%
  pivot_longer(
    cols = starts_with("V"),
    names_to = "vote_id",
    values_to = "vote"
  )

#Assigning the numbers to their vote type for future calculations
EP1_data <- EP1_data %>%
  mutate(
    vote = recode(vote,
                  `0` = "Absent",
                  `1` = "Yes",
                  `2` = "No",
                  `3` = "Abstain",
                  `4` = "Present but did not vote",
                  `5` = "Not an MEP"
    )
  )

#Creating a new table with the counts for each vote type
EP1_summary <- EP1_data %>%
  count(vote, name = "n")

#I want to connect the tables through the MEP ID values
#but the column names are different so the code below is 
#renaming the column to match
colnames(MEP_data)
names(MEP_data)[names(MEP_data) == "MEP id"]  <- "MEPID"

#Combining the two data sets
combined_data <- merge(MEP_data, EP1_data)

#Mean yes rates for each EP group
yes_means <- combined_data %>%
  group_by(EPG) %>%
  summarise(
    n_yes = sum(vote == "Yes", na.rm = TRUE),
    n_no = sum(vote == "No", na.rm = TRUE),
    n_abstain = sum(vote == "Abstain", na.rm = TRUE),
    mean_yes_rate = n_yes / (n_yes + n_no + n_abstain),
    .groups = "drop"
  )
print(yes_means)

#Mean abstention rates for each EP Group
abstain_means <- combined_data %>%
  group_by(EPG) %>%
  summarise(
    n_yes = sum(vote == "Yes", na.rm = TRUE),
    n_no = sum(vote == "No", na.rm = TRUE),
    n_abstain = sum(vote == "Abstain", na.rm = TRUE),
    mean_abstain_rate = n_abstain / (n_yes + n_no + n_abstain),
    .groups = "drop"
  )
print(abstain_means)

#Mean vote preferences along the two contested dimensions
#When I first ran the code, a warning came up because the 
#the values were characters, so I converted them to numeric
combined_data <- combined_data %>%
  mutate(
    NOM_D1 = as.numeric(as.character(`NOM-D1`)),
    NOM_D2 = as.numeric(as.character(`NOM-D2`))
  )

NOM_group_means <- combined_data %>%
  group_by(EPG) %>%
  summarise(
    mean_nomd1 = mean(`NOM-D1`, na.rm = TRUE),
    mean_nom2 = mean(`NOM-D2`, na.rm),
    n_MEPS = n(),
    .groups = "drop"
  )
    