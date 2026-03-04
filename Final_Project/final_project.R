rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}
install.packages("haven")
lapply(c("tidyverse", "ggplot2", "ggrepel", "extrafont", "WDI", "ggtext", "showtext", "readxl", "haven", "scales", "ggridges"),  pkgTest)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#Data
leadership <- read_sav("~/Desktop/Trinity/DataViz_2026/Final_Project/ATP W131.sav") 

#Manipulation
leadership_2 <- leadership %>%
  select(
    F_GENDER, #gender
    F_EDUCCAT2, #education level 
    F_AGECAT, #age category
    F_USR_SELFID, #self identified area (1 = Urban, 2 = Suburban, 3 = Rural) 
    RESPECTW1_W131, #level of respect for female president 
    AMNTWMNBF1_W131, #level of feeling for amount of women in top executive business positions
    AMNTWMNPF1_W131, #level of feeling for amount of women in high political offices
  )  %>%
  rename(
    respect_female = RESPECTW1_W131,
    amount_exect = AMNTWMNBF1_W131,
    amount_polit = AMNTWMNPF1_W131
  ) %>%
  mutate(
    F_GENDER = case_when(
      F_GENDER == 1 ~ "Male",
      F_GENDER == 2 ~ "Female",
      TRUE ~ NA_character_   # everything else becomes NA
    ),
    F_GENDER = factor(F_GENDER)
  ) %>%
  filter(!is.na(F_GENDER))

#Visualization

#1
pdf("bar.pdf")
ggplot(leadership_2, aes(x = factor(amount_polit), fill = F_GENDER)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink")) +
  scale_x_discrete(
    labels = c(
      "1" = "Too Many",
      "2" = "Too Few",
      "3" = "Just Right",
      "99" = "No Answer"
    )) +
  labs(
    title = "Attitudes Towards Number of Women in High Political Office by Gender",
    subtitle = "Answering the survey question: Thinking about the country today, would you say there are… women in high political offices",
    x = "\nAmount of Women",
    y = "Count",
    fill = "Gender",
    caption = "\nData from The American Trends Panel (ATP Wave 131) created by Pew Research Center"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 6),
    panel.border = element_rect(color = "black", fill = NA, size = 0.7),
    panel.background = element_rect(fill = "white", color = NA),
    strip.background = element_rect(fill = "white", color = "black", size = 0.7),
    plot.caption = element_text(face = "italic", size = 5, 
                                color = "grey70", hjust = 0.5)
    )
dev.off()

#2
#organizing the categories to fit the message 
leadership_clean <- leadership_2 %>%
  mutate(respect_female = as_factor(respect_female, levels = "labels")) %>%
  filter(
    F_AGECAT %in% 1:4, 
    !is.na(respect_female),
    respect_female != "Refused"
  ) %>%
  mutate(
    F_AGECAT = factor(F_AGECAT,
                      levels = 1:4,
                      labels = c("18-29", "30-44", "45-59", "60+")),
    respect_female = factor(respect_female, ordered = TRUE),  
    F_GENDER = factor(F_GENDER, levels = c("Male", "Female"))
  )

#calculating the percentages of each respect level based on age category and gender
respect_age_gender <- leadership_clean %>%
    count(F_GENDER, F_AGECAT, respect_female) %>%
    group_by(F_GENDER, F_AGECAT) %>%  
    mutate(pct = n / sum(n)) %>%
    ungroup()

pdf("heat.pdf")  
ggplot(respect_age_gender, aes(x = F_AGECAT, y = respect_female, fill = pct)) +
    geom_tile(color = "white") +
    facet_wrap(~ F_GENDER) +
    scale_fill_viridis_c(labels = percent_format(), option = "C") +
    labs(
      title = "Respect for a Female President by Age and Gender",
      subtitle = "Percentages answering the survey question: Do you think that having a woman as president would make the U.S...\n",
      x = "\nAge Category",
      y = "Level of Respect\n",
      fill = "Percentage",
      caption = "\nData from The American Trends Panel (ATP Wave 131) created by Pew Research Center"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
      plot.subtitle = element_text(hjust = 0.5, size = 6),
      strip.background = element_rect(fill = "white", color = "black", size = 0.7),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.caption = element_text(face = "italic", size = 5, 
                                  color = "grey70", hjust = 0.5)
    )
dev.off()

#3
#renaming the factors for education 
leadership_2 <- leadership_2 %>%
  filter(F_EDUCCAT2 != 99) %>%   # remove 99 first
  mutate(
    F_EDUCCAT2 = factor(
      F_EDUCCAT2,
      levels = sort(unique(F_EDUCCAT2)),
      labels = c("Less than HS", "HS Grad", 
                 "Some College", "Associates", "Bachelors", 
                 "Postgrad")  
    )
  )

#pivioting the data to be long so we can look at all the education levels per gender
leadership_2$amount_exect
leadership_long <- leadership_2 %>%
  pivot_longer(
    cols = c(amount_exect, amount_polit),
    names_to = "attitude_type",
    values_to = "score"
  )

pdf("bar_2.pdf")
ggplot(leadership_long, 
       aes(x = F_EDUCCAT2, y = score, fill = F_GENDER)) +
  scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink")) +
  stat_summary(fun = mean, geom = "bar", 
               position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~attitude_type, 
             scales = "fixed", 
             labeller = as_labeller(c(amount_exect = "Women in Top Executive Business Positions",
                                      amount_polit = "Women in High Political Offices"))) +
  labs(
    title = "Attitudes Toward Women in Leadership by Gender and Education",
    subtitle = "The average values of opinions on the amount of women in these postions (1 = too many) per education category",
    x = "\nEducation Level",
    y = "Average Attitude Score\n",
    fill = "Gender",
    caption = "\nData from The American Trends Panel (ATP Wave 131) created by Pew Research Center"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(face = "italic", size = 5, 
                                color = "grey70", hjust = 0.5)
  )
dev.off()