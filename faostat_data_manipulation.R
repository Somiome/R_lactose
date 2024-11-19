# Load necessary libraries
library(readxl)
library(dplyr)
library(countrycode)
library(WDI)
library(ggplot2)
library(ggcorrplot)

# Read the main data
data <- read.csv("./data/FAOSTAT_data_en_11-17-2024.csv")

# Replace whitespace with underscore in 'Element' column
data$Element <- gsub(" ", "_", data$Element)

# Add CountryCode to data
data$CountryCode <- countrycode(data$Area, 'country.name', 'iso3c')

# Create unique lists of countries and elements
unique_countries <- unique(data$Area)
unique_elements <- levels(factor(data$Element))

# Initialize an empty dataframe 'df' with country, CountryCode, and elements
df <- data.frame(matrix(NA, nrow = length(unique_countries), ncol = 2 + length(unique_elements)))
colnames(df) <- c("country", "CountryCode", unique_elements)
df$country <- unique_countries
df$CountryCode <- countrycode(df$country, 'country.name', 'iso3c')

# Extract data and fill 'df'
for (country in unique_countries) {
  country_data <- filter(data, Area == country)
  
  for (element in unique_elements) {
    element_value <- country_data %>%
      filter(Element == element) %>%
      summarize(Value = sum(Value, na.rm = TRUE)) %>%
      pull(Value)
    
    df[df$country == country, element] <- element_value
  }
}

# Read the population data
population <- read.csv("./data/population.csv")

# Filter population data for the year 2022
population_2022 <- population %>%
  filter(Year == 2022)

# Clean country names and get ISO3 country codes
population_2022 <- population_2022 %>%
  rename(Country = Region..subregion..country.or.area..) %>%
  mutate(
    CountryCode = countrycode(Country, 'country.name', 'iso3c'),
    Population = as.numeric(Total.Population..as.of.1.July..thousands.) * 1000  # Convert thousands to actual number
  ) %>%
  filter(!is.na(CountryCode) & !is.na(Population))

# Join 'df' with 'population_2022' on 'CountryCode' and include only 'Population' column
df <- left_join(df, population_2022 %>% select(CountryCode, Population), by = 'CountryCode')

colnames(df)
  
head(df)

df <- df %>%
  mutate(Food_Supply_quantity = as.numeric(`Food_supply_quantity_(tonnes)`),
          Pop_to_supply_ratio = Population / `Food_supply_quantity_(tonnes)`)

head(df)

ggplot(data=df, aes(x=country, y=Ratio))
  + geom_bar()