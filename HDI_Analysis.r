library(readxl)
library(dplyr)
library(countrycode)
library(WDI)
library(ggplot2)
library(ggcorrplot)

# Read the faostat
data <- read.csv("./data/FAOSTAT_data_en_10-29-2024.csv")

# Filter Calories/Year data
Calories_Year_data <- data[data$Element == "Calories/Year", ]

# Read the lactose intolerance data
lactose_data <- read_excel("./data/12862_2009_1252_MOESM1_ESM.XLS", col_names = FALSE)

# Assign column names to lactose_data
colnames(lactose_data) <- c(
  "Frequency_Non_Digestors", "COUNTRY", "POPULATION", "LONGITUDE", "LATITUDE", "N",
  "FREQUENCY_OF_DIGESTORS", "TESTING_METHOD", "REFERENCE", "Extra1", "Extra2", "Extra3", "Extra4", "Extra5"
)

# Keep only the relevant columns
lactose_data <- lactose_data[, 1:9]

# Clean lactose_data
lactose_data <- lactose_data %>%
  filter(!is.na(COUNTRY) & COUNTRY != "" & COUNTRY != "COUNTRY") %>%
  mutate(
    FREQUENCY_OF_DIGESTORS = as.numeric(FREQUENCY_OF_DIGESTORS),
    Lactose_Intolerance_Rate = 1 - FREQUENCY_OF_DIGESTORS
  )

# Aggregate lactose intolerance data by country
lactose_country <- lactose_data %>%
  group_by(COUNTRY) %>%
  summarise(Lactose_Intolerance_Rate = mean(Lactose_Intolerance_Rate, na.rm = TRUE))

# Prepare Calories/Year_data
Calories_Year_country <- Calories_Year_data %>%
  select(Area, Value)

# Standardize country names and get ISO3 country codes
Calories_Year_country <- Calories_Year_country %>%
  mutate(
    CountryCode = countrycode(Area, 'country.name', 'iso3c')
  ) %>%
  filter(!is.na(CountryCode))

lactose_country <- lactose_country %>%
  mutate(
    CountryCode = countrycode(COUNTRY, 'country.name', 'iso3c')
  ) %>%
  filter(!is.na(CountryCode))

# Merge the datasets on CountryCode
merged_data <- merge(Calories_Year_country, lactose_country, by = "CountryCode")

# Get the list of unique CountryCodes
country_codes <- unique(merged_data$CountryCode)

# Retrieve GDP data for the year 2022 (or the most recent available year)
gdp_data <- WDI(country = country_codes, indicator = 'NY.GDP.MKTP.CD', start = 2022, end = 2022)

# Rename columns for clarity
gdp_data <- gdp_data %>%
  rename(
    CountryCode = iso2c,
    GDP = NY.GDP.MKTP.CD
  ) %>%
  select(iso3c, GDP)

# Match the country code
gdp_data <- gdp_data %>%
  rename(
    CountryCode = iso3c
  ) %>%
  filter(!is.na(CountryCode) & !is.na(GDP))

# Merge GDP data with the existing merged_data
merged_data <- merge(merged_data, gdp_data, by = "CountryCode")

# Remove any rows with missing GDP data
merged_data <- merged_data %>%
  filter(!is.na(GDP))

# Since GDP can vary greatly, we can take the logarithm to normalize it
merged_data <- merged_data %>%
  mutate(
    Log_GDP = log(GDP)
  )

# Read the HDI data
HDI_data <- read_excel("./data/HDR23-24_Statistical_Annex_HDI_Table cleaned.xlsx")

# Clean and prepare HDI_data
colnames(HDI_data)[2] <- "Human_Development_Index"

HDI_data <- HDI_data %>%
  mutate(Human_Development_Index = as.numeric(Human_Development_Index)) %>%
  filter(!is.na(Country) & !is.na(Human_Development_Index))

# Map HDI_data$Country to CountryCode
HDI_data <- HDI_data %>%
  mutate(
    CountryCode = countrycode(Country, 'country.name', 'iso3c')
  ) %>%
  filter(!is.na(CountryCode) & !is.na(Human_Development_Index))

# Merge HDI_data with merged_data
merged_data <- merged_data %>%
  left_join(HDI_data[, c('CountryCode', 'Human_Development_Index')], by = 'CountryCode')

# Remove rows with missing Human_Development_Index
merged_data <- merged_data %>%
  filter(!is.na(Human_Development_Index))

# **Add Population Data**

# Read the population data
population <- read.csv("./data/population.csv")

# Filter population data for the year 2023
population_2023 <- population %>%
  filter(Year == 2023)

# Clean country names and get ISO3 country codes
population_2023 <- population_2023 %>%
  rename(Country = Region..subregion..country.or.area..) %>%
  mutate(
    CountryCode = countrycode(Country, 'country.name', 'iso3c'),
    Population = as.numeric(Total.Population..as.of.1.July..thousands.) * 1000  # Convert thousands to actual number
  ) %>%
  filter(!is.na(CountryCode) & !is.na(Population))

# Handle specific country name mismatches if any
# For example, if there are countries that didn't match, you can manually fix them
# population_2023$CountryCode[population_2023$Country == "Bahamas, The"] <- "BHS"

# Merge population data with merged_data
merged_data <- merged_data %>%
  left_join(population_2023[, c('CountryCode', 'Population')], by = 'CountryCode')

colnames(merged_data)

# Remove rows with missing Population
merged_data <- merged_data %>%
  filter(!is.na(Population))

# Calculate Calories/Year per Capita
merged_data <- merged_data %>%
  mutate(
    Calories_Year_per_Capita = Value / Population
  )

# Step 3: Prepare data for regression

# Update regression model to use Calories/Year_per_Capita
model <- lm(Calories_Year_per_Capita ~ Log_GDP + Lactose_Intolerance_Rate + Human_Development_Index, data = merged_data)

# Output the summary of the regression model
print(summary(model))

colnames(merged_data)

# Update correlation matrix to include Calories/Year_per_Capita
correlation_matrix <- cor(merged_data[, c("Calories_Year_per_Capita", "Log_GDP", "Lactose_Intolerance_Rate", "Human_Development_Index")], use = "complete.obs")
print("Correlation Matrix:")
print(correlation_matrix)

# Visualize the correlation matrix
ggcorrplot(correlation_matrix,
           method = "circle",
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           title = "Correlation Matrix",
           ggtheme = theme_minimal())

# Scatter plot of Calories/Year per Capita vs. Human Development Index
ggplot(merged_data, aes(x = Human_Development_Index, y = Calories/Year_per_Capita)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(title = "Calories/Year per Capita vs. Human Development Index",
       x = "Human Development Index",
       y = "Calories/Year per Capita")

# Scatter plot of Calories/Year per Capita vs. Log of GDP
ggplot(merged_data, aes(x = Log_GDP, y = Calories_Year_per_Capita)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Calories/Year per Capita vs. Log of GDP",
       x = "Log of GDP",
       y = "Calories/Year per Capita")

# Scatter plot of Calories/Year per Capita vs. Lactose Intolerance Rate
ggplot(merged_data, aes(x = Lactose_Intolerance_Rate, y = Calories_Year_per_Capita)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Calories/Year per Capita vs. Lactose Intolerance Rate",
       x = "Lactose Intolerance Rate",
       y = "Calories/Year per Capita")