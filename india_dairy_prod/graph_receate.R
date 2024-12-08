library(ggplot2)
library(viridis)
scale_fill_viridis_d()


# Data for milk consumption
milk_consumption <- data.frame(
  Year = c(2019, 2020, 2021, 2022, 2023),
  Consumption = c(190.99, 194.79, 198.99, 202.49, 207.49)
)

# Create bar plot
ggplot(milk_consumption, aes(x = factor(Year), y = Consumption, fill = "Milk Consumption")) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  scale_fill_manual(values = c("Milk Consumption" = "blue"), guide = "none") +
  labs(
    title = "Domestic consumption of milk in India from 2019 to 2023 (in million metric tons)",
    x = "Year",
    y = "Volume in million metric tons"
  ) +
  theme_minimal()

ggsave("milk_consumption.png", width = 8, height = 6, dpi = 300)

# Data for butter production
butter_production <- data.frame(
  Country = c("India", "EU-27", "United States", "New Zealand", "Russia", "Mexico", 
              "United Kingdom", "Belarus", "Canada", "China", "Brazil"),
  Production = c(6750, 2100, 955, 510, 280, 245, 215, 125, 125, 110, 81)
)

# Create bar plot
ggplot(butter_production, aes(x = reorder(Country, Production), y = Production, fill = Country)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  scale_fill_manual(values = rep("skyblue", nrow(butter_production)), guide = "none") +
  coord_flip() +
  labs(
    title = "Major butter producing countries worldwide in 2023 (in 1,000 metric tons)",
    x = "Country",
    y = "Butter production in thousand metric tons"
  ) +
  theme_minimal()

ggsave("butter_production.png", width = 8, height = 6, dpi = 300)
# Data for cow milk production
cow_milk_production <- data.frame(
  Country = c("EU-27", "United States", "India", "China", "Russia", "Brazil", 
              "New Zealand", "United Kingdom", "Mexico", "Argentina", "Canada",
              "Australia", "Belarus", "Japan", "Ukraine", "South Korea", "Taiwan"),
  Production = c(143, 104.1, 99.5, 40.9, 32.3, 24.5, 21, 15, 13.25, 12, 10.33, 
                 8.48, 7.98, 7.66, 6.98, 2.02, 0.47)
)

# Create bar plot
ggplot(cow_milk_production, aes(x = reorder(Country, -Production), y = Production, fill = Country)) +
    geom_bar(stat = "identity", width = 0.7, color = "black") +
    scale_fill_manual(values = rep("skyblue", nrow(cow_milk_production)), guide = "none") +
    labs(
        title = "Major producers of cow milk worldwide in 2023, by country (in million metric tons)",
        x = "Country",
        y = "Production volume in million metric tons"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("cow_milk_production.png", width = 8, height = 6, dpi = 300)

# Data for fluid cow milk consumption
fluid_milk_consumption <- data.frame(
  Country = c("India", "EU", "United States", "China", "Brazil", "Russia", 
              "United Kingdom", "Mexico", "Japan", "Ukraine", "Canada", 
              "Australia", "Argentina", "South Korea", "Belarus", 
              "New Zealand", "Taiwan", "Philippines"),
  Consumption = c(87450, 23650, 20900, 16700, 10881, 6800, 6000, 4185, 4070, 
                  4010, 2705, 2420, 1800, 1515, 1075, 535, 480, 140)
)

# Create the bar plot
fluid_milk_plot <- ggplot(fluid_milk_consumption, aes(x = reorder(Country, Consumption), y = Consumption, fill = Country)) +
  geom_bar(stat = "identity", width = 0.7, color = "#000000") +
  scale_fill_manual(values = rep("skyblue", nrow(fluid_milk_consumption)), guide = "none") +
  coord_flip() +
  labs(
    title = "Annual consumption of fluid cow milk worldwide in 2023, by country (in 1,000 metric tons)",
    x = "Country",
    y = "Milk consumption in thousand metric tons"
  ) +
  theme_minimal()+
  theme(
    axis.text.y = element_text(size = 10), # Adjust font size for y-axis labels
    axis.title.y = element_text(size = 12), # Adjust y-axis title font size
    axis.title.x = element_text(size = 12)  # Adjust x-axis title font size
  ) +
  coord_flip()

# Display the plot
print(fluid_milk_plot)

# Save the plot as an image
ggsave("fluid_milk_consumption_plot.png", plot = fluid_milk_plot, width = 8, height = 6, dpi = 300)