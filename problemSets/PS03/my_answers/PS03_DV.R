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

lapply(c("tidyverse", "ggplot2", "ggrepel", "extrafont", "WDI", "ggtext", "showtext"),  pkgTest)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#MANIPULATION 
ces2015 <- read_csv("/Users/carolinekajkowski/Desktop/Trinity/DataViz_2026/datasets/CES2015.csv")
ces2015 <- ces2015 |> filter(discard == "Good quality")
unique(ces2015$p_voted)

ces2015 <- filter(ces2015, p_voted %in% c("Yes", "No")) 

table(ces2015$p_voted)
unique(ces2015$p_voted)

unique(ces2015$age)
ces2015 <- ces2015 %>% 
  filter(age != 1000) %>%
  mutate(
    age_cat = case_when(
      age > 1985 ~ "<30",
      age > 1970 ~ "30-45",
      age > 1950 ~"45-64",
      age <= 1950 ~"65+",
    ) 
  ) 

#VISUALIZATIONS

#1

pdf("V1.pdf")
ggplot(ces2015, aes(x = age_cat, fill = p_voted)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c("Yes" = "lightblue", "No" = "lightpink3")
  ) +
  labs(
    x = "Age Group",
    y = "Proportion of Respondents that Voted\n",
    title = "\nTurnout Rate by Age Group",
    fill = "Did they vote?"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  
  )
dev.off()

#2
unique(ces2015$vote_for)
table(ces2015$p_selfplace)
ces2015_party <- ces2015 %>%
  filter(vote_for %in% c("Liberal", "ndp", "Conservatives", "Green Party", "Bloc Quebecois")) %>%
  filter(!is.na(p_selfplace)) %>%  
  mutate(p_selfplace = as.numeric(as.character(p_selfplace)))

pdf("V2.pdf")
ggplot(ces2015_party, aes(x = p_selfplace, fill = vote_for)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~vote_for, ncol = 2) +
  scale_x_continuous(limits = c(0,10), breaks = 0:10) +
  labs(
    x = "\nLeft–right self-placement (0–10 scale)",
    y = "Density\n",
    title = "Density of Left–Right Self-Placement by Party\n",
  ) +
  theme_minimal() +
  theme(
    legend.position = "none") 
dev.off()

#3
unique(ces2015$province)
str(ces2015$income_full)
table(ces2015$income_full)

ces2015_clean <- ces2015 %>%
  filter(!income_full %in% c(".d", ".r"),
         !is.na(income_full)) %>%
  mutate(
    income_full = recode(income_full,
                         "less than $29,999" = "< $30k",
                         "between $30,000 and $59,999" = "$30k–59k",
                         "between $60,000 and $89,999" = "$60k–89k",
                         "between $90,000 and $109,999" = "$90k–109k",
                         "more than $110,000" = "> $110k"
    )
  )

pdf("V3.pdf")
ggplot(ces2015_clean, aes(x = income_full)) +
  geom_bar(fill = "plum4", color = "white") +
  facet_wrap(~province) +
  labs(
    x = "\nIncome Categories",
    y = "Counts of Turnout by Income\n",
    title = "Counts of Turnout by Income per Province"
  ) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
dev.off()


#4
caroline_theme <- function(base_size = 12) {
  theme_minimal(base_family = "sans", base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.5, face = "plain", size = 12),
        plot.caption = element_text(hjust = 0.5, face = "italic", size = 8, color = "grey70"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = rel(1.1), hjust = 0),
        axis.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10), hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 10), hjust = 0.5),
        panel.grid.major = element_line(color = "lightgray", size = 0.5),
        panel.grid.minor = element_line(color = "lightgray", size = 0.25),
        panel.border = element_rect(color = "black", fill = NA, size = 0.7),
        panel.background = element_rect(fill = "white", color = NA),
        strip.background = element_rect(fill = "white", color = "black", size = 0.7))
}

#5
library(ggrepel)
medians <- ces2015_party %>%
  group_by(vote_for) %>%
  summarise(median_selfplace = median(p_selfplace, na.rm = TRUE))

pdf("V4.pdf")
ggplot(ces2015_party, aes(x = p_selfplace, fill = vote_for)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~vote_for, ncol = 2) +
  scale_x_continuous(limits = c(0,10), breaks = 0:10) +
  labs(
    title = "Left–Right Self-Placement by Party",
    subtitle = "The 5 Major Political Party's densities on left to right political scale (0-10)",
    caption = "Data from the Canadian Election Study (CES) in 2015\n1275 observations that stated 1 out of the 5 political parties shown. ",
    x = "Left–right self-placement (0–10 scale)",
    y = "Density"
  ) +
 caroline_theme() +
  theme(
    legend.position = "none") +
  geom_vline(data = medians, 
             aes(xintercept = median_selfplace),
             color = "red", 
             linetype = "dashed") +
  geom_text_repel(data = medians,
                  aes(x = median_selfplace, y = 0.05, label = "Median"),
                  inherit.aes = FALSE,
                  nudge_y = 0.02,
                  color = "red")
dev.off()







 