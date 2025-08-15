
library(tidyverse)
library(ggplot2)
library(haven)
library(readr)
library(ggplot2)
library(corrplot)
library(stargazer)
library(kableExtra)
library(tidyr)
library(viridis)

#0 Load merged_data
cleaned_data <- readRDS("00_data/cleaned_data.rds")


# 1. DATASET OVERVIEW
glimpse(cleaned_data)

missing_country_names <- cleaned_data %>%
  filter(is.na(country)) %>%
  distinct(country_code) %>%
  nrow()



