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
lapply(c("tidyverse", "ggplot2", "ggrepel", "extrafont", "WDI", "ggtext", "showtext", "readxl", "haven"),  pkgTest)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#Data
leadership <- read_sav("~/Desktop/Trinity/DataViz_2026/Final_Project/ATP W131.sav")

#Manipulation
leadership_2 <- leadership %>%
  filter(
    F_GENDER, #gender (duh)
    F_EDUCCAT2, #education level 
    F_AGECAT, #age category
    F_USR_SELFID, #self identified area (1 = Urban, 2 = Suburban, 3 = Rural) 
    RESPECTW1_W131, #level of respect for female president 
    AMNTWMNBF1_W131, #level of feeling for amount of women in top executive business positions
    AMNTWMNPF1_W131, #level of feeling for amount of women in high political offices
  )

#Visualization

