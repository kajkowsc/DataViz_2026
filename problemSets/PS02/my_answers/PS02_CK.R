library(tidyverse)
library(stargazer)
library(tidyr)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#MANIPULATION

#1
NCSS <- read.csv("NCSS_v1.csv")

NCSS <- NCSS |>
  select(
    CASEID,
    YEAR,
    GDREGION,
    NUMOFFMBR,
    TRAD6,
    TRAD12,
    INCOME
  )

#2
NCSS <- filter(NCSS, TRAD6 %in% c("Chrétiennes", "Juives", "Musulmanes"))

#3
cong_summary <- NCSS |>
  group_by(YEAR, TRAD6) |>
  summarize(
    congregations = n(), 
    mean_income = mean(INCOME, na.rm = TRUE),
    median_income = median(INCOME, na.rm = TRUE),
    n_congs = n(),
    .groups = "drop"  
  )

#4
NCSS <- NCSS |>
  group_by(YEAR, TRAD6) |>
  mutate(
    mean_income = mean(INCOME, na.rm = TRUE),
    AVG_INCOME = case_when(
      is.na(INCOME) ~ NA_integer_,
      INCOME >= mean_income ~ 1L,
      TRUE ~ 0L
    ) 
  ) |>
  ungroup()

#VISUALIZATIONS

#1
pdf("V1.pdf")
ggplot(NCSS, aes(x = TRAD12, fill = factor(AVG_INCOME))) +
  geom_bar(position = "fill") +
  coord_flip() +
  scale_y_continuous(labels = scales:: percent) +
  scale_fill_manual(
    values = c("0" = "deepskyblue2", "1" = "brown2"),
    labels = c( 
      "0" = "Below avaerage income",
      "1" = "Above or average income"
      )
    ) +
  facet_wrap(~YEAR) +
  labs(
    x = "TRAD12",
    y = "Proportion of congregations",
    title = "Proportion of Congregations above and below the Average Income"
    ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
dev.off()

#2
pdf("V2.pdf")
ggplot(NCSS %>%filter(YEAR == 2022), aes(x = TRAD6, y = NUMOFFMBR , fill = TRAD12)) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d() +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(
    title = "Congregational Member Counts (2022)",
    y = "Number of Official Members\n",
    fill = "Congregation"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
dev.off()

#3
install.packages("ggridges") 
library(ggridges)

pdf("V3.pdf")
ggplot(NCSS%>%filter(YEAR == 2022), aes(x = INCOME, y = GDREGION, fill = GDREGION)) + 
  geom_density_ridges(alpha = 0.7, scale = 1.2) +
  theme(legend.position = "none") +
  labs(
    x = "Average yearly Income",
    y = "Region",
    title = "The Distribution of Average Yearly Income in each Region (2022)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
dev.off()

#4
pdf("V4.pdf")
ggplot(NCSS%>%filter(YEAR == 2022), aes(x = TRAD6, y = NUMOFFMBR, fill = TRAD6)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 50000)) + 
  facet_wrap(~ GDREGION) +
  labs(
    x = "Religious Classification",
    y = "Number of Official Members\n",
    fill = "TRAD6",
    title = "Distribution of Official Members per Congregation (2022)",
    subtitle = "by Religious classification and Region"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
dev.off()


