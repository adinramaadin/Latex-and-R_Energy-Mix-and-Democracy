
library(tidyverse)
library(ggplot2)
library(haven)
library(readr)
library(haven)


# 1. IMPORT DATA
globalmacro <- read_dta("00_data/globalmacro.dta") %>% 
  select(country = 'countryname',
         code = 'ISO3',
         year,
         rgpd_pc = 'rGDP_USD',
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

service <- read_csv("00_data/service_shar.csv", 
                    skip = 3) %>% 
  pivot_longer(cols = 5:ncol(.), 
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


glimpse(globalmacro)
glimpse(energy_cons)
glimpse(govrevenue)
glimpse(manufacture)
glimpse(service)
glimpse(democracy)


# 1. Data Cleaning First
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
  mutate(year = as.numeric(year)) %>% 
  filter(!is.na(year), !is.na(code))
  


# 3. Create Energy Shares (Your Dependent Variables)
energy_shares <- energy_cons %>%
  # Calculate total electricity generation
  mutate(
    total_electricity = other_renewables_twh + biofuels_twh + solar_twh + 
      wind_twh + hydro_twh + nuclear_twh + 
      gas_twh + coal_twh + oil_twh
  ) %>%
  # Calculate renewable shares (%)
  mutate(
    solar_share = (solar_twh / total_electricity) * 100,
    wind_share = (wind_twh / total_electricity) * 100,
    hydro_share = (hydro_twh / total_electricity) * 100,
    nuclear_share = (nuclear_twh / total_electricity) * 100,
    renewable_share = ((solar_twh + wind_twh + hydro_twh + other_renewables_twh + biofuels_twh) / total_electricity) * 100,
    lowcarbon_share = ((solar_twh + wind_twh + hydro_twh + nuclear_twh + other_renewables_twh + biofuels_twh) / total_electricity) * 100
  ) %>%
  # Keep only necessary variables
  select(country, code, year, solar_share, wind_share, hydro_share, nuclear_share, 
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
  select(country, code, year, rgpd_pc, export, import, trade_opennes, 
         any_crisis, banking_crisis, currency_crisis, debt_crisis)

# 5. Sequential Left Joins (Starting with Energy as Base)
# Start with energy data (your main dependent variable)
merged_data <- energy_shares %>%
  # Join macro/crisis data
  left_join(crisis_data, by = c("code", "year")) %>%
  
  # Join democracy data  
  left_join(democracy %>% select(country, code, year, electoral, liberal, participatory, deliberative, egalitarian), 
            by = c("code", "year")) %>%
  
  # Join government revenue (fossil rents proxy)
  left_join(govrevenue %>% select(country, code, year, resource_rev), 
            by = c("code", "year")) %>%
  
  # Join manufacturing share
  left_join(manufacture %>% select(country, code, year, manu_share), 
            by = c("code", "year")) %>%
  
  # Join services share
  left_join(service %>% select(country, code, year, service_share), 
            by = c("code", "year"))


# Create single country name from all the country columns
merged_data <- merged_data %>%
  mutate(
    # Take first non-missing country name from any column
    country = coalesce(country.x, country.y, country.x.x, country.y.y, country.x.x.x, country.y.y.y)
  )

cleaned_data <- merged_data %>%
  select(
    # === IDENTIFIERS ===
    country,
    country_code = code,
    year,
    
    # === DEPENDENT VARIABLES (Energy Shares) ===
    solar_share,
    wind_share, 
    hydro_share,
    nuclear_share,
    renewable_share,
    lowcarbon_share,
    total_electricity,
    
    # === TREATMENT VARIABLES (Crisis) ===
    any_crisis,
    banking_crisis,
    currency_crisis, 
    debt_crisis,
    
    # === KEY INDEPENDENT VARIABLES ===
    # Economic development
    rgpd_pc,
    
    # Democracy measures
    electoral,
    liberal,
    participatory, 
    deliberative,
    egalitarian,
    
    # Economic structure
    manufacturing_share = manu_share,
    services_share = service_share,
    
    # Trade and resources
    trade_openness = trade_opennes,
    exports_gdp = export,
    imports_gdp = import,
    fossil_rents = resource_rev
  )

write_csv(cleaned_data, "00_data/cleaned_data.csv")
