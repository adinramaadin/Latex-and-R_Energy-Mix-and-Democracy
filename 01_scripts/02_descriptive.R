
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
library(readxl)

#0 Load merged_data
cleaned_data <- read_csv("00_data/cleaned_data.csv")

# 2 Load kekayaan
kekayaan_raw <- read_excel("00_data/kekayaan.xlsx", 
                           sheet = "Country Analytical History", 
                           skip = 10, 
                           col_names = FALSE)

# 3 Buat nama kolom unik
# Kolom 1 = country, 2 = code, kolom 3 ke atas = tahun
num_years <- ncol(kekayaan_raw) - 2
year_cols <- paste0("year_", 1989:(1989 + num_years - 1))
colnames(kekayaan_raw) <- c("code", "country", year_cols)

# 4️ Pivot long ke format long
kekayaan_long <- kekayaan_raw %>%
  pivot_longer(
    cols = starts_with("year_"),
    names_to = "year",
    values_to = "type"
  ) %>%
  mutate(year = as.numeric(sub("year_", "", year))) %>%
  filter(code != "code") %>%               # hapus baris code= "code"
  distinct(country, year, .keep_all = TRUE)

# 1. DATASET OVERVIEW
# Create year summary data
year_summary <- cleaned_data %>%
  group_by(year) %>%
  summarise(
    countries = n_distinct(country_code),
    total_obs = n(),
    renewable_data = sum(!is.na(renewable_share)),
    crisis_data = sum(!is.na(any_crisis)),
    gdp_data = sum(!is.na(rgpd_pc)),
    democracy_data = sum(!is.na(electoral)),
    .groups = 'drop'
  ) %>%
  arrange(year)

# Create summary by periods
period_summary <- cleaned_data %>%
  mutate(
    period = case_when(
      year < 1980 ~ "Pre-1980",
      year >= 1980 & year < 1990 ~ "1980-1989", 
      year >= 1990 & year < 2000 ~ "1990-1999",
      year >= 2000 & year < 2010 ~ "2000-2009",
      year >= 2010 ~ "2010+"
    )
  ) %>%
  group_by(period) %>%
  summarise(
    years = n_distinct(year),
    countries = n_distinct(country_code),
    total_obs = n(),
    renewable_obs = sum(!is.na(renewable_share)),
    crisis_obs = sum(!is.na(any_crisis)),
    renewable_rate = round(renewable_obs / total_obs * 100, 1),
    crisis_rate = round(crisis_obs / total_obs * 100, 1),
    .groups = 'drop'
  )

cat("\n=== DATA COVERAGE BY PERIOD ===\n")
print(period_summary)

#Seems like 1990 and so is the best
final_cleaned_data <- cleaned_data %>% 
  filter(year >= 1990) %>% 
  filter(!is.na(country_code), !is.na(year)) %>% 
  arrange(country, year)


str(final_cleaned_data)


# 1. PANEL
# FACT 1: THE BIG PICTURE - Global Energy Transition is Real
# Purpose: Establish that energy transition matters globally
cat("=== STYLIZED FACT 1: GLOBAL ENERGY TRANSITION ===\n")

energy_trends <- final_cleaned_data %>%
  group_by(year) %>%
  summarise(
    renewable_avg = mean(renewable_share, na.rm = TRUE),
    n_countries = n(),
    .groups = 'drop'
  )

p1 <- ggplot(energy_trends, aes(x = year, y = renewable_avg)) +
  geom_line(size = 1.2, color = "darkgreen") +
  geom_point(size = 2, color = "darkgreen") +
  labs(
    title = "The Global Energy Transition is Accelerating",
    subtitle = "Average renewable energy share across countries",
    x = "Year", 
    y = "Renewable Energy Share (%)",
    caption = "Data: Own calculations from IEA data"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

print(p1)
cat("→ NARRATIVE: Dunia sedang transisi energi. Tapi kenapa ada yang cepat, ada yang lambat?\n\n")

# FACT 2: THE PUZZLE - Crisis Effects Vary Dramatically
# Purpose: Show confusing/contradictory patterns that need explaining
cat("=== STYLIZED FACT 2: THE CRISIS PUZZLE ===\n")

# Find crisis episodes and their renewable energy changes
crisis_cases <- final_cleaned_data %>%
  # Merge with income classification data
  left_join(kekayaan_long %>% 
              select(country_code = code, year, income_group = type),
            by = c("country_code", "year")) %>%
  filter(!is.na(income_group)) %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    crisis_start = any_crisis == 1 & lag(any_crisis, default = 0) == 0,
    renewable_change = renewable_share - lag(renewable_share, default = NA)
  ) %>%
  filter(crisis_start == TRUE, !is.na(renewable_change)) %>%
  mutate(
    democracy_level = case_when(
      electoral > 0.7 ~ "High Democracy",
      electoral > 0.4 ~ "Medium Democracy", 
      TRUE ~ "Low Democracy"
    ),
    income_group_clean = case_when(
      income_group == "L" ~ "Low Income",
      income_group == "LM" ~ "Lower Middle Income", 
      income_group == "UM" ~ "Upper Middle Income",
      income_group == "H" ~ "High Income",
      TRUE ~ income_group
    )
  ) %>%
  select(country, year, renewable_change, democracy_level, electoral, rgpd_pc, income_group_clean)

# Show extreme cases with plots instead of long tables
cat("PUZZLING CASES - Why do crises have opposite effects?\n")

# Create visualization of crisis effects
crisis_plot_data <- crisis_cases %>%
  mutate(
    effect_type = case_when(
      renewable_change > 1 ~ "Large Increase (>1pp)",
      renewable_change > 0 ~ "Small Increase (0-1pp)",
      renewable_change > -1 ~ "Small Decrease (0-1pp)",
      TRUE ~ "Large Decrease (<-1pp)"
    ),
    democracy_high = electoral > 0.6
  )

# Plot 1: Crisis effects distribution
p_crisis1 <- ggplot(crisis_plot_data, aes(x = renewable_change, fill = democracy_high)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("FALSE" = "red", "TRUE" = "blue"),
                    labels = c("Low Democracy", "High Democracy"),
                    name = "Democracy Level") +
  labs(
    title = "Crisis Effects on Renewable Energy: The Puzzle",
    subtitle = "Some countries increase renewables during crisis, others decrease",
    x = "Change in Renewable Share (percentage points)",
    y = "Number of Crisis Episodes"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_crisis1)

# Plot 2: Crisis effects by democracy and income
p_crisis2 <- crisis_plot_data %>%
  filter(income_group_clean %in% c("High Income", "Low Income")) %>%
  ggplot(aes(x = electoral, y = renewable_change, color = income_group_clean)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("High Income" = "blue", "Low Income" = "red")) +
  labs(
    title = "Crisis Effects Vary by Democracy and Income Level",
    subtitle = "High-income democracies vs Low-income countries show different crisis responses",
    x = "Electoral Democracy Index",
    y = "Change in Renewable Share During Crisis (pp)",
    color = "Income Group"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_crisis2)

# Show just a few illustrative cases in text
cat("\nILLUSTRATIVE CASES:\n")
cat("LARGE INCREASES during crisis:\n")
big_increases <- crisis_cases %>% 
  filter(renewable_change > 2) %>%
  arrange(desc(renewable_change)) %>%
  head(3) %>%
  mutate(
    electoral = round(electoral, 2),
    rgpd_pc = round(rgpd_pc/1000, 0)
  )
print(big_increases)

cat("\nLARGE DECREASES during crisis:\n")
big_decreases <- crisis_cases %>%
  filter(renewable_change < -2) %>%
  arrange(renewable_change) %>%
  head(3) %>%
  mutate(
    electoral = round(electoral, 2),
    rgpd_pc = round(rgpd_pc/1000, 0)
  )
print(big_decreases)

cat("→ NARRATIVE: Krisis punya efek berlawanan! Ada yang malah increase renewables, ada yang drop.\n")
cat("→ SUSPENSE: Apa yang bikin beda? Democracy? Income level? Atau interaksi keduanya?\n\n")

# FACT 3: DEMOCRACY-ENERGY RELATIONSHIP VARIES BY WEALTH
# Purpose: Hint at the triple interaction we'll test
cat("=== STYLIZED FACT 3: DEMOCRACY WORKS DIFFERENTLY ACROSS WEALTH SPECTRUM ===\n")

wealth_democracy <- final%>%
  filter(!is.na(renewable_share), !is.na(electoral), !is.na(rgpd_pc)) %>%
  mutate(
    # Use quartiles based on actual data distribution
    wealth_quartile = ntile(rgpd_pc, 4),
    wealth_group = case_when(
      wealth_quartile == 4 ~ "Highest GDP Quartile",
      wealth_quartile == 1 ~ "Lowest GDP Quartile",
      TRUE ~ "Middle Quartiles"
    )
  ) %>%
  filter(wealth_group %in% c("Highest GDP Quartile", "Lowest GDP Quartile"))

# Calculate correlations
correlations <- wealth_democracy %>%
  group_by(wealth_group) %>%
  summarise(
    correlation = cor(electoral, renewable_share, use = "complete.obs"),
    n_obs = n(),
    mean_gdp = mean(rgpd_pc, na.rm = TRUE),
    .groups = 'drop'
  )

print(correlations)

p2 <- ggplot(wealth_democracy, aes(x = electoral, y = renewable_share, color = wealth_group)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("Highest GDP Quartile" = "blue", "Lowest GDP Quartile" = "red")) +
  labs(
    title = "Democracy-Energy Relationship Varies Across Wealth Spectrum",
    subtitle = "Highest vs Lowest GDP per capita quartiles show different patterns",
    x = "Electoral Democracy Index", 
    y = "Renewable Energy Share (%)",
    color = "GDP Level"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p2)
cat("→ NARRATIVE: Democracy berkorelasi beda di high-GDP vs low-GDP countries!\n")
cat("→ SETUP THEORY: Economic capacity matters for democratic energy policy?\n\n")

# FACT 4: CRISIS TIMING IS QUASI-RANDOM
# Purpose: Justify identification strategy
cat("=== STYLIZED FACT 4: CRISES AS QUASI-EXPERIMENTS ===\n")

crisis_timing <- final_cleaned_data %>%
  filter(any_crisis == 1) %>%
  count(year, name = "n_crisis_countries") %>%
  mutate(crisis_rate = n_crisis_countries / length(unique(final_cleaned_data$country)))

p3 <- ggplot(crisis_timing, aes(x = year, y = n_crisis_countries)) +
  geom_col(fill = "red", alpha = 0.7) +
  labs(
    title = "Economic Crises Hit Different Countries at Different Times",
    subtitle = "Variation in crisis timing enables causal identification",
    x = "Year", 
    y = "Number of Countries in Crisis"
  ) +
  theme_minimal()

print(p3)
cat("→ METHODOLOGY JUSTIFICATION: Crisis timing varies → good for DiD identification\n\n")

