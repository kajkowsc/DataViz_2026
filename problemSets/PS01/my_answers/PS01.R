library(tidyverse)
library(stargazer)
library(tidyr)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#MANIPULATION

#Loading data into dataset
MEP_data <- read_csv("mep_info_EP1.csv")
EP1_data <- read_csv("rcv_ep1.txt")

#3

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

#Creating a summary table with the counts for each vote type
EP1_summary <- EP1_data %>%
  count(vote, name = "n")

#wanted to change this to numeric before combining data sets
MEP_data <- MEP_data %>%
  mutate(
    NOM_D1 = as.numeric(as.character(`NOM-D1`)),
    NOM_D2 = as.numeric(as.character(`NOM-D2`))
  ) %>%
  filter(
    !is.na(NOM_D1),
    !is.na(NOM_D2)
  )

#4 

#I want to connect the tables through the MEP ID values and EP Group
#but the column names are different so the code below is 
#renaming the column to match
colnames(MEP_data)
names(MEP_data)[names(MEP_data) == "MEP id"]  <- "MEPID"
names(MEP_data)[names(MEP_data) == "EP Group"]  <- "EPG"

#Combining the two data sets
combined_data <- merge(MEP_data, EP1_data)
sapply(combined_data, function(x) sum(is.na(x))) #checking that theres no NAs

#5

#Mean vote rates for each EP group
means_EPG <- combined_data %>%
  group_by(EPG) %>%
  summarise(
    n_yes = sum(vote == "Yes", na.rm = TRUE),
    n_no = sum(vote == "No", na.rm = TRUE),
    n_abstain = sum(vote == "Abstain", na.rm = TRUE),
    mean_yes_rate = n_yes / (n_yes + n_no + n_abstain),
    mean_abstain_rate = n_abstain / (n_yes + n_no + n_abstain),
    mean_nomd1 = mean(NOM_D1, na.rm = TRUE),
    mean_nom2 = mean(NOM_D2, na.rm = TRUE),
    .groups = "drop"
  )

#VISUALIZATION

#1
pdf("Viz_1")
ggplot(combined_data, aes(x = NOM_D1, fill = EPG)) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  facet_wrap(~EPG, scales = "free_y") +
  labs(
    title = "Distribution of Nominate Dimension 1 by EP Group",
    x = "Nominate Dimension 1",
    y = "Count"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none")
dev.off()

#2
pdf("Viz_2")
ggplot(MEP_data, aes(x = NOM_D1, y = NOM_D2, color = EPG)) +
  geom_point() +
  labs(
    title = "Scatterplot of Nominate Dimensions 1 and 2",
    x = "Nominate Dimension 1",
    y = "Nominate Dimension 2",
    color = "EP Group"
  ) 
dev.off()

#3

#4

#5

