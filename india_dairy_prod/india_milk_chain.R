library(networkD3)
library(dplyr)

# Define the data
nodes <- data.frame(
  name = c("Cow Milk Production", "Buffalo Milk Production", "Other Milk Production",
           "Imports", "Domestic Consumption", "Factory Use", "Exports")
)

# Define the flows (links)
links <- data.frame(
  source = c(0, 1, 2, 3, 0, 1, 2, 3),  # source nodes
  target = c(4, 4, 4, 4, 5, 5, 5, 6),  # target nodes
  value = c(102000, 110700, 0, 0, 90000, 122680, 20, 0)  # corresponding values in MMT
)

# Normalize the values (adjust as needed)
links$value <- links$value / 1000  # Dividing by 1000 to scale the values

# Remove links with zero or negligible values
links <- links %>% filter(value > 1) 

# Define sankey
sankey <- sankeyNetwork(Links = links, Nodes = nodes,
                        Source = "source", Target = "target",
                        Value = "value", NodeID = "name",
                        units = "Thousand MMT", fontSize = 14, nodeWidth = 40)

sankey
