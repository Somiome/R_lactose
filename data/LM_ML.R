library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggfortify)
library(umap)
library(maps)
library(plotly)
library(randomForest)
library(rpart.plot)

# 1. Data Preprocessing for Clustering

LMP <- read.csv('LM_processed.csv', sep = ',', header = T)

tmp_cm_cal <- read.csv('CM_cal_processed.csv', sep = ',', header = T)
tmp_cm_fsq <- read.csv('CM_fsq_processed.csv', sep = ',', header = T)

LM_data_full <- cbind(LMP[,-1], tmp_cm_cal[,5:18], tmp_cm_fsq[,5:18])
colnames(LM_data_full) <- c('Country', 'Group', 'LMP', 'LMP_95%_CI_min', 'LMP_95%_CI_max',
                            paste0('CM_cal_', 2010:2022), 'CM_cal_mean',
                            paste0('CM_fsq_', 2010:2022), 'CM_fsq_mean')

GD <- read.csv('genetic_distance_from_danish.csv', sep = ',', header = T)
GD <- GD[-c(88:65534),]

LMP_group <- LM_data_full$Group
names(LMP_group) <- LM_data_full$Country
#LMP_group <- LMP_group[-74]

LM_data_full$Group <- GD$FST

world_gd <- ne_countries(scale = "medium", returnclass = "sf")
map_data_gd <- world_gd %>%
  left_join(LM_data_full, by = c("name" = "Country"))
ggplot(data = map_data_gd) +
  geom_sf(aes(fill = Group), color = "black", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", na.value = "white", name = "0-1") +
  labs(title = "Genetic distance from dannish") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

LM_corr <- read.csv('LM_corr.csv', sep = ',', header = T)

LM_data_full <- LM_data_full[,-which(colnames(LM_data_full) == 'CM_cal_mean' | colnames(LM_data_full) == 'CM_fsq_mean')]
LM_data_full <- cbind(LM_data_full, LM_corr[,7:27])

#LM_data_full$LM_minmax <- LM_data_full$`LMP_95%_CI_max` - LM_data_full$`LMP_95%_CI_min`
#LM_data_full$CM_cal_mean <- rowMeans(LM_data_full[,6:18], na.rm = T)
#LM_data_full$CM_fsq_mean <- rowMeans(LM_data_full[,19:31], na.rm = T)
#LM_data_full$temp_minmax <- c(apply(LM_data_full[,41:52], 1, max) - apply(LM_data_full[,41:52], 1, min))

#LM_data_full <- LM_data_full[,c(1:3, 53, 54, 55, 56, 32:40)]

LM_data_full <- na.omit(LM_data_full) 
str(LM_data_full)

setdiff(names(LMP_group), LM_data_full$Country)
LMP_group <- LMP_group[-c(which(names(LMP_group) == "Zambia" | names(LMP_group) == "Belgium"))]

# 2. PCA
#pca <- prcomp(LM_data_full[, c("LMP", "LM_minmax", "CM_cal_mean", "CM_fsq_mean", "temp_minmax", "pop_under15", "pop_under65", "pop_above65_rate", "agr_land_perc", "docs_per_1000", "GDP", "pop_hist", "health_exp", "life_exp")], scale. = TRUE)
pca <- prcomp(LM_data_full[,-1], scale. = TRUE)
pca_data <- data.frame(pca$x, Group = LMP_group)
ggplot(pca_data, aes(x = PC1, y = PC2, color = as.factor(Group))) +
  geom_point() +
  stat_ellipse(aes(group = Group), level = 0.8) +
  labs(title = "PCA of Variables by Group", x = "PC1", y = "PC2", color = "Group") +
  theme_minimal()

explained_variance <- (pca$sdev^2) / sum(pca$sdev^2)
cumulative_variance <- cumsum(explained_variance)

data.frame(Principal_Component = 1:length(explained_variance), 
           Explained_Variance = explained_variance, 
           Cumulative_Variance = cumulative_variance)

ggplot(data.frame(PC = 1:length(explained_variance), Variance = explained_variance), aes(x = PC, y = Variance)) +
  geom_point() +
  geom_line() +
  labs(title = "Scree Plot", x = "Principal Component", y = "Explained Variance") +
  theme_minimal()

ggplot(data.frame(PC = 1:length(cumulative_variance), Cumulative_Variance = cumulative_variance), aes(x = PC, y = Cumulative_Variance)) +
  geom_point() +
  geom_line() +
  labs(title = "Cumulative Explained Variance", x = "Principal Component", y = "Cumulative Explained Variance") +
  theme_minimal()



# 3-1. K-means screening
set.seed(42)
#wss <- sapply(1:10, function(k) {
#  kmeans(LM_data_full[, c("LMP", "LM_minmax", "CM_cal_mean", "CM_fsq_mean", "temp_minmax", "pop_under15", "pop_under65", "pop_above65_rate", "agr_land_perc", "docs_per_1000", "GDP", "pop_hist", "health_exp", "life_exp")], centers = k, nstart = 25)$tot.withinss
#})
wss <- sapply(1:10, function(k) {
  kmeans(LM_data_full[,-1], centers = k, nstart = 25)$tot.withinss
})


## Elbow plot
ggplot(data.frame(k = 1:10, wss = wss), aes(x = k, y = wss)) +
  geom_line() +
  geom_point() +
  labs(title = "Elbow Plot for Optimal K", x = "Number of Clusters", y = "Total Within Sum of Squares") +
  theme_minimal()

## UMAP 2D
#umap_result <- umap(LM_data_full[, c("LMP", "LM_minmax", "CM_cal_mean", "CM_fsq_mean", "temp_minmax", "pop_under15", "pop_under65", "pop_above65_rate", "agr_land_perc", "docs_per_1000", "GDP", "pop_hist", "health_exp", "life_exp")],
#                    n_components = 3)
umap_result <- umap(LM_data_full[,-1],
                    n_components = 3)

umap_data <- data.frame(UMAP1 = umap_result$layout[,1], UMAP2 = umap_result$layout[,2], Group = LMP_group)
ggplot(umap_data, aes(x = UMAP1, y = UMAP2, color = as.factor(Group))) +
  geom_point() +
  labs(title = "UMAP (2D) by Group", x = "UMAP1", y = "UMAP2", color = "Group") +
  theme_minimal()

## UMAP 3D
umap3d_data <- data.frame(UMAP1 = umap_result$layout[,1], UMAP2 = umap_result$layout[,2], UMAP3 = umap_result$layout[,3], Group = LMP_group)

plot_ly(umap3d_data, x = ~UMAP1, y = ~UMAP2, z = ~UMAP3, color = ~Group, type = 'scatter3d', mode = 'markers') %>%
  layout(title = "UMAP (3D) by Group", scene = list(xaxis = list(title = 'UMAP1'),
                                                    yaxis = list(title = 'UMAP2'),
                                                    zaxis = list(title = 'UMAP3')))



# 4. Random Forest
rf_model <- randomForest(LMP ~ Group + LM_minmax + CM_cal_mean + CM_fsq_mean + temp_minmax + pop_under15 + pop_under65 + pop_above65_rate + agr_land_perc + docs_per_1000 + GDP + pop_hist + health_exp + life_exp, 
                         data = LM_data_full, importance = TRUE)
importance_data <- data.frame(Variable = rownames(importance(rf_model)), Importance = importance(rf_model)[, 1])

ggplot(importance_data, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Random Forest Variable Importance", x = "Variable", y = "Importance") +
  theme_minimal()

tree_model <- rpart(LMP ~ Group + LM_minmax + CM_cal_mean + CM_fsq_mean + 
                      temp_minmax + pop_under15 + pop_under65 + pop_above65_rate + 
                      agr_land_perc + docs_per_1000 + GDP + pop_hist + 
                      health_exp + life_exp, data = LM_data_full)
rpart.plot(tree_model, main = "Decision Tree")












set.seed(123)
wcss <- vector()
for (k in 1:15){
  kmeans_result <- kmeans(LM_data_full, centers = k, nstart = 25)
  wcss[k] <- kmeans_result$tot.withinss
}
elbow_df <- data.frame(Clusters = 1:15, WCSS = wcss)
ggplot(elbow_df, aes(x = Clusters, y = WCSS)) +
  geom_line() +
  geom_point(size = 3, color = "blue") +
  labs(title = "Elbow Method for Optimal K", x = "Number of Clusters", y = "WCSS") +
  theme_minimal()

optimal_k <- 10
kmeans_final <- kmeans(LM_data_full, centers = optimal_k, nstart = 25)
LM_data_full$Cluster <- as.factor(kmeans_final$cluster)

umap_result <- umap(LM_data_full[, -ncol(LM_data_full)])
umap_df <- as.data.frame(umap_result$layout)
colnames(umap_df) <- c("UMAP1", "UMAP2")
umap_df$Country <- rownames(LM_data_full)
umap_df$Cluster <- as.factor(kmeans_final$cluster)

ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "UMAP Visualization with Clustering", x = "UMAP Dimension 1", y = "UMAP Dimension 2") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = Cluster, label = Country)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, size = 3) +
  labs(title = "UMAP Visualization with Country Labels", 
       x = "UMAP Dimension 1", 
       y = "UMAP Dimension 2") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))




map_data_world <- map_data("world")
map_data_world$Cluster <- umap_df$Cluster[match(map_data_world$region, umap_df$Country)]

ggplot(map_data_world, aes(x = long, y = lat, group = group, fill = Cluster)) +
  geom_polygon(color = "black", size = 0.2) +
  scale_fill_brewer(palette = "Set3", na.value = "gray90") +  
  labs(title = "World Map with Clustering by Country", 
       fill = "Cluster") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))





umap_3d <- umap(LM_data_full[, -ncol(LM_data_full)], n_components = 3)

umap_3d_df <- as.data.frame(umap_3d$layout)
colnames(umap_3d_df) <- c("UMAP1", "UMAP2", "UMAP3")
umap_3d_df$Country <- rownames(LM_data_full)
umap_3d_df$Cluster <- as.factor(kmeans_final$cluster)

plot_ly(
  data = umap_3d_df,
  x = ~UMAP1, y = ~UMAP2, z = ~UMAP3,
  color = ~Cluster,
  text = ~Country,
  type = "scatter3d",
  mode = "markers"
) %>%
  layout(
    title = "3D UMAP Visualization with Country Labels",
    scene = list(
      xaxis = list(title = "UMAP Dimension 1"),
      yaxis = list(title = "UMAP Dimension 2"),
      zaxis = list(title = "UMAP Dimension 3")
    )
  )

umap_df <- as.data.frame(umap_result$layout)
colnames(umap_df) <- c("UMAP1", "UMAP2")
combined_data <- cbind(LM_data_full, umap_df)

correlations <- data.frame(
  Variable = colnames(LM_data_full[, -ncol(LM_data_full)]),
  Correlation_with_UMAP1 = apply(LM_data_full[, -ncol(LM_data_full)], 2, function(x) cor(x, umap_df$UMAP1)),
  Correlation_with_UMAP2 = apply(LM_data_full[, -ncol(LM_data_full)], 2, function(x) cor(x, umap_df$UMAP2))
)

correlations[order(abs(correlations$Correlation_with_UMAP1), decreasing = TRUE), ]
correlations[order(abs(correlations$Correlation_with_UMAP2), decreasing = TRUE), ]

formula <- as.formula(
  paste("Cluster ~", paste(colnames(combined_data[, !(colnames(combined_data) %in% c("Cluster", "UMAP1", "UMAP2"))]), collapse = " + "))
)





rf_model <- randomForest(Cluster ~ ., data = combined_data, importance = TRUE)
importance_vals <- importance(rf_model)
varImpPlot(rf_model, type = 2, main = "Variable Importance")