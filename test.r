library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)

# Read your data
data <- read.csv('./data/data_fixed/가설4 기온/LM_df_monthly_average_surface_temperatures_by_year.csv')

# Estimated optimal temperature for Cattle
target_temp_range <- c(5, 30)

# Probability-Based Calculation

# Reshape the data from wide to long format
data_long_prob <- data %>%
  pivot_longer(
    cols = matches("^X\\d{4}$"),  # Columns that start with 'X' followed by 4 digits (years)
    names_to = "Year",
    values_to = "Temperature"
  )

# Clean up the 'Year' column by removing 'X' and converting to numeric
data_long_prob <- data_long_prob %>%
  mutate(Year = as.numeric(sub("X", "", Year)))

# For each country and year, calculate mean and standard deviation of temperature over months
data_summary_prob <- data_long_prob %>%
  group_by(Country, Year) %>%
  summarise(
    mean_temp = mean(Temperature, na.rm = TRUE),
    sd_temp = sd(Temperature, na.rm = TRUE)
  ) %>%
  ungroup()

# Compute the probability that temperature is outside the 10–20 degree range
data_summary_prob <- data_summary_prob %>%
  mutate(
    prob_below_10 = pnorm(target_temp_range[1], mean = mean_temp, sd = sd_temp),
    prob_above_20 = 1 - pnorm(target_temp_range[2], mean = mean_temp, sd = sd_temp),
    prob_outside_10_20 = prob_below_10 + prob_above_20
  )

# Average the probability per country over the years
data_country_prob <- data_summary_prob %>%
  group_by(Country) %>%
  summarise(
    avg_prob_outside_10_20 = mean(prob_outside_10_20, na.rm = TRUE)
  )

# Sort the data for plotting
data_country_prob <- data_country_prob %>%
  arrange(desc(avg_prob_outside_10_20))

# Direct Calculation with Raw Data

# Reshape the data from wide to long format
data_long_raw <- data %>%
  pivot_longer(
    cols = matches("^X\\d{4}$"),  # Columns that start with 'X' followed by 4 digits (years)
    names_to = "Year",
    values_to = "Temperature"
  )

# Clean up the 'Year' column by removing 'X' and converting to numeric
data_long_raw <- data_long_raw %>%
  mutate(Year = as.numeric(sub("X", "", Year)))

# For each country and year, calculate the percentage of months outside 10–20 degrees
data_summary_raw <- data_long_raw %>%
  group_by(Country, Year) %>%
  summarise(
    total_months = n(),
    months_below_10 = sum(Temperature < target_temp_range[1], na.rm = TRUE),
    months_above_20 = sum(Temperature > target_temp_range[2], na.rm = TRUE),
    months_outside_10_20 = months_below_10 + months_above_20,
    percentage_outside_10_20 = (months_outside_10_20 / total_months) * 100
  ) %>%
  ungroup()

# Average the percentage per country over the years
data_country_raw <- data_summary_raw %>%
  group_by(Country) %>%
  summarise(
    avg_percentage_outside_10_20 = mean(percentage_outside_10_20, na.rm = TRUE)
  )

# Sort the data for plotting
data_country_raw <- data_country_raw %>%
  arrange(desc(avg_percentage_outside_10_20))

# Merge Data and Prepare for Plotting

# Merge the two datasets to align countries
merged_data <- data_country_prob %>%
  inner_join(data_country_raw, by = "Country")

# For consistent plotting order
merged_data <- merged_data %>%
  arrange(desc(avg_prob_outside_10_20))

# Create a factor for Country to maintain the order in plots
merged_data$Country <- factor(merged_data$Country, levels = merged_data$Country)

# Visualization

# Plot for Probability-Based Calculation
plot_prob <- ggplot(merged_data, aes(x = Country, y = avg_prob_outside_10_20 * 100)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Probability",
       x = "Country",
       y = "Probability (%)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

# Set avg_percentage_outside_10_20 to 0 for Taiwan and West Sahara
merged_data$avg_percentage_outside_10_20[merged_data$Country == "Taiwan"] <- 0
merged_data$avg_percentage_outside_10_20[merged_data$Country == "Western Sahara"] <- 0

# Plot for Direct Calculation with Raw Data
plot_raw <- ggplot(merged_data, aes(x = Country, y = avg_percentage_outside_10_20)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(title = "Percentage",
       x = "Country",
       y = "Percentage (%)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

# Arrange the two plots in a single image
grid.arrange(plot_prob, plot_raw, ncol = 2)