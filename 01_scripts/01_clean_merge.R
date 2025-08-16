
library(tidyverse)
library(ggplot2)
library(haven)
library(readr)
library(haven)
library(readxl)

# 1. IMPORT DATA
globalmacro <- read_dta("00_data/globalmacro.dta") %>% 
  select(country = 'countryname',
         code = 'ISO3',
         year,
         rgpd_pc = 'rGDP_USD',
         population = 'pop',
         export = 'exports_GDP',
         import = 'imports_GDP',
         sovdebtcrisis = 'SovDebtCrisis',
         currencycrisis = 'CurrencyCrisis',
         bangkingcrisis = 'BankingCrisis'
  ) %>% 
  mutate(trade_opennes = export + import)

energy_cons <- read_csv("00_data/energy_cons.csv") %>% 
  select(
    country = Entity,
    code = Code,
    year = Year,
    other_renewables_twh = `Other renewables (including geothermal and biomass) - TWh`,
    biofuels_twh = `Biofuels consumption - TWh`,
    solar_twh = `Solar consumption - TWh`,
    wind_twh = `Wind consumption - TWh`,
    hydro_twh = `Hydro consumption - TWh`,
    nuclear_twh = `Nuclear consumption - TWh`,
    gas_twh = `Gas consumption - TWh`,
    coal_twh = `Coal consumption - TWh`,
    oil_twh = `Oil consumption - TWh`
  )


govrevenue <- read_dta("00_data/govrevenue.dta") %>% 
  select(country,
         code ='iso',
         year,
         resource_rev = 'tot_res_rev')

manufacture <- read_csv("00_data/manufacturing_shar.csv") %>% 
  select(country = 'Entity',
         code = 'Code',
         year = 'Year',
         manu_share = `Manufacturing, value added (% of GDP)`)

service <- read_csv("00_data/service_shar.csv") %>% 
  pivot_longer(cols = `1960`:`2024`, 
               names_to = "year",
               values_to = "service_share"
  ) %>% 
  select(
    country = `Country Name`,
    code = `Country Code`,
    year,
    service_share
  ) 
 
democracy <- readRDS("00_data/vdem_r.rds") %>% 
  select(country = country_name, 
          code = country_text_id,
          year,
          electoral = v2x_polyarchy,
          liberal = v2x_libdem,
          participatory = v2x_partipdem,
          deliberative = v2x_delibdem,
          egalitarian = v2x_egaldem
  )

inc <- read_excel("00_data/kekayaan.xlsx") %>% 
  pivot_longer(cols = 3:ncol(.),
               names_to = "year",
               values_to = "inc_level") %>% 
  mutate(year = as.numeric(year),
         inc_level = recode(
           inc_level,
           "L"  = "low_income",
           "LM" = "lower_middle_income",
           "UM" = "upper_middle_income",
           "H"  = "high_income"
           )
         ) 


glimpse(globalmacro)
glimpse(energy_cons)
glimpse(govrevenue)
glimpse(manufacture)
glimpse(service)
glimpse(democracy)
glimpse(inc)

# 1. Data Cleaning First
globalmacro <- globalmacro %>% 
  mutate(year = as.numeric(year))

democracy <- democracy %>%
  filter(!is.na(code)) 

energy_cons <- energy_cons %>%
  filter(!is.na(code))  

globalmacro <- globalmacro %>% 
  filter(!is.na(code))  

govrevenue <- govrevenue  %>% 
  filter(!is.na(code))  

manufacture <- manufacture %>% 
  filter(!is.na(code))

service <- service %>% 
  filter(!is.na(year), !is.na(code)) %>% 
  mutate(year = as.numeric(year))
  
inc <- inc %>% 
  filter(!is.na(year), !is.na(code))

  # Calculate total electricity generation
  energy_shares <- energy_cons %>%
  # Calculate total electricity generation
  mutate(
    total_electricity = other_renewables_twh + biofuels_twh + solar_twh + 
      wind_twh + hydro_twh + nuclear_twh + 
      gas_twh + coal_twh + oil_twh
  ) %>%
  # Calculate shares (%)
  mutate(
    solar_share    = (solar_twh / total_electricity) * 100,
    wind_share     = (wind_twh / total_electricity) * 100,
    hydro_share    = (hydro_twh / total_electricity) * 100,
    nuclear_share  = (nuclear_twh / total_electricity) * 100,
    coal_share     = (coal_twh / total_electricity) * 100,
    gas_share      = (gas_twh / total_electricity) * 100,
    oil_share      = (oil_twh / total_electricity) * 100,
    renewable_share = ((solar_twh + wind_twh + hydro_twh + other_renewables_twh + biofuels_twh) / total_electricity) * 100,
    lowcarbon_share = ((solar_twh + wind_twh + hydro_twh + nuclear_twh + other_renewables_twh + biofuels_twh) / total_electricity) * 100,
    fosil_share     = ((coal_twh + gas_twh + oil_twh) / total_electricity) * 100
  ) %>%
  # Keep only necessary variables
  select(country, code, year, solar_share, wind_share, hydro_share, nuclear_share, 
         coal_share, gas_share, oil_share, fosil_share,
         renewable_share, lowcarbon_share, total_electricity)

# 4. Create Crisis Variables
crisis_data <- globalmacro %>%
  mutate(
    # Any crisis dummy
    any_crisis = ifelse(
      (sovdebtcrisis == 1 | currencycrisis == 1 | bangkingcrisis == 1) & 
        !is.na(sovdebtcrisis) & !is.na(currencycrisis) & !is.na(bangkingcrisis), 
      1, 0
    ),
    # Individual crisis types
    banking_crisis = ifelse(!is.na(bangkingcrisis), bangkingcrisis, 0),
    currency_crisis = ifelse(!is.na(currencycrisis), currencycrisis, 0),
    debt_crisis = ifelse(!is.na(sovdebtcrisis), sovdebtcrisis, 0)
  ) %>%
  select(country, code, year, rgpd_pc, population, export, import, trade_opennes, 
         any_crisis, banking_crisis, currency_crisis, debt_crisis)

# 5. Sequential Left Joins (Starting with Energy as Base)
# Start with crisis_data 
merged_data <- crisis_data %>%
  # Join macro/crisis data
  left_join(energy_shares, by = c("code", "year")) %>% 

  # Join democracy data  
  left_join(democracy, by = c("code", "year")) %>% 

  # Join government revenue (fossil rents proxy)
  left_join(govrevenue, by = c("code", "year")) %>%
  
  # Join manufacturing share
  left_join(manufacture, by = c("code", "year")) %>%
  
  # Join services share
  left_join(service, by = c("code", "year")) %>% 
  
  #join inc level data
  left_join(inc, by =c("code", "year"))

nrow(crisis_data)
nrow(energy_shares)
nrow(merged_data)

glimpse(merged_data)

#delete repetitive columns
merged_data <- merged_data %>% 
  select(-country) %>%                         # 1. drop plain country
  rename(country = country.x) %>%              # 2. keep country.x
  select(-matches("^country\\..*"))            # 3. drop all suffixed variants

merged_data <- merged_data %>%
  group_by(country) %>%
  arrange(year, .by_group = TRUE) %>%
  fill(inc_level, .direction = "downup") %>%  # fill backward first, then forward
  mutate(inc_level = ifelse(is.na(inc_level), "0", inc_level)) %>%
  ungroup() %>% 
  mutate(inc_level = factor(inc_level, 
                            levels = c("low_income",
                                       "lower_middle_income",
                                       "upper_middle_income",
                                       "high_income"),
                            ordered = TRUE))


df <- merged_data %>%
  filter(year <= 2024) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) 
  

write_csv(df, "00_data/df.csv")
