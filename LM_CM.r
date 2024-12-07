library(ggplot2)
library(dplyr)
library(tidyr)
library(ggbreak)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(nlme)
library(ggcorrplot)
library(Hmisc)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ggrepel)
library(ggparty)

##############################################################################
#                          1. Data Pre-processing                            #
##############################################################################
# A. Load LM data
## LM.csv: lactose malabsorption prevalence data per country
## LMP: Lactose Malabsorption Prevalence
## Columns info.
##  Country: country names
##  Group: groups divided by researchers (based on genetic relations)
##  Preval_primary: LMP from primary sources
##  95_CI_min_primary: min value of 95% confidence interval for primary data
##  95_CI_max_primary: max value of 95% confidence interval for primary data
##  etc.: the same as the ones for primary sources respectively 
##    but for secondary sources -> dropped (no need for analysis)

LM <- read.csv('LM.csv', header = T, sep = ',')
names(LM) = c('Country','Group',
              'Preval_primary','95_CI_min_primary','95_CI_max_primary',
              'Preval_secondary','95_CI_min_secondary','95_CI_max_secondary')
LM <- LM[,-grep("_secondary$", colnames(LM))] # extract col.s about secondary

# *****************************************************************************
# B. Load CM data
## FAOSTAT_data_en_11-8-2024 (1).csv: cattle milk data by country
## Columns info.
##  Area: name of each country
##  Element: Production, Import quantity, Food supply quantity, Calories/Year
##    Food supply quantity: average daily quantity of food available per person
##      includes all food produced within a country or imported for consumption, 
##      adjusted for losses during storage, transportation, and processing
##    Calories/Year: total calorie content of the food supply available 
##      in a country, measured in kilocalories per capita per year
##  Item: Raw milk of cattle
##  Year: 2010-2022
##  Unit: t, g/cap/d, Kcal
##  Value: value of each property 
##  etc.: not important here

cattle_milk <- read.csv('FAOSTAT_data_en_11-8-2024 (1).csv', header = T, sep = ',')
CM <- cattle_milk[,c(4,6,10,11,12)] # exclude columns that are out of interest

CM_tidy <- CM %>%
  pivot_wider(
    names_from = Year,
    values_from = Value
  )
CM_tidy_tmp <- split(CM_tidy, CM_tidy$Element)

CM_cal <- as.data.frame(CM_tidy_tmp$`Calories/Year`)
CM_fsq <- as.data.frame(CM_tidy_tmp$`Food supply quantity (g/capita/day)`)


# *****************************************************************************
# C. Unify each country name
## CM_cal first
setdiff(levels(factor(CM_cal$Area)), levels(factor(LM$Country)))

CM_cal$Area[CM_cal$Area == 'China, Taiwan Province of'] = "Taiwan"
CM_cal$Area[CM_cal$Area == 'Czechia'] = 'Czech Republic'
CM_cal$Area[CM_cal$Area == 'Iran (Islamic Republic of)'] = 'Iran'
CM_cal$Area[CM_cal$Area == 'Netherlands (Kingdom of the)'] = 'Netherlands'
CM_cal$Area[CM_cal$Area == 'Republic of Korea'] = 'South Korea'
CM_cal$Area[CM_cal$Area == 'Russian Federation'] = 'Russia'
CM_cal$Area[CM_cal$Area == 'Syrian Arab Republic'] = 'Syria'
CM_cal$Area[CM_cal$Area == 'Türkiye'] = 'Turkey'
CM_cal$Area[CM_cal$Area == 'United Kingdom of Great Britain and Northern Ireland'] = 'United Kingdom'
CM_cal$Area[CM_cal$Area == 'United Republic of Tanzania'] = 'Tanzania'
CM_cal$Area[CM_cal$Area == 'United States of America'] = 'United States'
CM_cal$Area[CM_cal$Area == 'Viet Nam'] = 'Vietnam'
CM_cal = CM_cal[-which(CM_cal$Area == 'French Polynesia'),]
CM_cal = CM_cal[-which(CM_cal$Area == 'China, mainland'),]

nrow(LM); nrow(CM_cal)
setdiff(levels(factor(LM$Country)), levels(factor(CM_cal$Area)))
LM = LM[-which(LM$Country == 'Republic of Congo'),]
LM = LM[-which(LM$Country == 'Western Sahara'),]

which(sort(CM_cal$Area) != sort(LM$Country))

## CM_fsq second
setdiff(levels(factor(CM_fsq$Area)), levels(factor(LM$Country)))

CM_fsq$Area[CM_fsq$Area == 'China, Taiwan Province of'] = "Taiwan"
CM_fsq$Area[CM_fsq$Area == 'Czechia'] = 'Czech Republic'
CM_fsq$Area[CM_fsq$Area == 'Iran (Islamic Republic of)'] = 'Iran'
CM_fsq$Area[CM_fsq$Area == 'Netherlands (Kingdom of the)'] = 'Netherlands'
CM_fsq$Area[CM_fsq$Area == 'Republic of Korea'] = 'South Korea'
CM_fsq$Area[CM_fsq$Area == 'Russian Federation'] = 'Russia'
CM_fsq$Area[CM_fsq$Area == 'Syrian Arab Republic'] = 'Syria'
CM_fsq$Area[CM_fsq$Area == 'Türkiye'] = 'Turkey'
CM_fsq$Area[CM_fsq$Area == 'United Kingdom of Great Britain and Northern Ireland'] = 'United Kingdom'
CM_fsq$Area[CM_fsq$Area == 'United Republic of Tanzania'] = 'Tanzania'
CM_fsq$Area[CM_fsq$Area == 'United States of America'] = 'United States'
CM_fsq$Area[CM_fsq$Area == 'Viet Nam'] = 'Vietnam'
CM_fsq = CM_fsq[-which(CM_fsq$Area == 'French Polynesia'),]
CM_fsq = CM_fsq[-which(CM_fsq$Area == 'China, mainland'),]

nrow(LM); nrow(CM_fsq)
setdiff(levels(factor(LM$Country)), levels(factor(CM_fsq$Area)))
which(sort(CM_fsq$Area) != sort(LM$Country))

# *****************************************************************************
# D. Add 'Mean' column
ncol(CM_cal)
CM_cal[,17] = apply(CM_cal[,c(4:16)], 1, function(x){mean(x, na.rm=T)})
colnames(CM_cal)[17] = 'Mean'

ncol(CM_fsq)
CM_fsq[,17] = apply(CM_fsq[,c(4:16)], 1, function(x){mean(x, na.rm=T)})
colnames(CM_fsq)[17] = 'Mean'

# write.csv(LM, 'LM_processed.csv')
# write.csv(CM_cal, 'CM_cal_processed.csv')
# write.csv(CM_fsq, 'CM_fsq_processed.csv')







##############################################################################
#                         2. Exploratory data analysis                       #
##############################################################################
# A. LM data
## Fig 1. (a) boxplot of LMP in total
boxplot(LM$Preval_primary, col = 'cornsilk', main = 'Boxplot of LMP')
text(x = 1.2, y = median(LM$Preval_primary), 
     labels = paste0("← median: ",round(median(LM$Preval_primary), 2)), col = "black", pos = 4)

## Fig 1. (b) histogram of LMP in total
hist(LM$Preval_primary, col = hcl.colors(10, palette = "Fall"),
     main = 'Histogram of LMP', breaks = 10, xlab = 'Lactose Malabsorption Prevalence')

## Fig 1. (c) boxplots of LMP per group
##  Africa (southern, eastern, western), America, Asia, Eastern Europe, 
##  Europe (western, southern, northern), Former Soviet Republics, 
##  Middle East, Northern Africa, Oceania, respectively
cols <- hcl.colors(9, palette = "Fall")
boxplot(LM$Preval_primary ~ LM$Group, 
        names = c('AFR', 'AME', 'ASI', 'EEU', 'EUR', 'FSR', 'MIE', 'NAF', 'OCE'),
        xlab = 'Group', ylab = 'LMP', col = cols, main = 'LMP by Groups')

## Fig 1. (d) dumbbell chart for LMP of each country by groups with 95% confidence interval
LM_tmp <- LM
LM_tmp$Group[LM_tmp$Group == 'Asia' | LM_tmp$Group == 'Oceania'] <- 'Asia and Oceania'
LM_tmp$Group[LM_tmp$Group == 'Middle East' | LM_tmp$Group == 'Northern Africa'] <- 'Middle East and north Africa'
LM_tmp$Group[LM_tmp$Group == 'Eastern Europe' | LM_tmp$Group == 'Former Soviet Republics' | LM_tmp$Group == 'Europe (western, southern, northern)'] <- 'Europe'
LM_tmp$Group[LM_tmp$Group == 'Africa (southern, eastern, western)'] <- 'Sub-Saharan Africa'

LM_tmp <- LM_tmp %>%
  group_by(Group) %>%
  mutate(
    Group_Color = hcl.colors(n = n(), palette = 'Fall')
  ) %>%
  ungroup()

ggplot(LM_tmp, aes(
  y = reorder(Country, Preval_primary),
  x = Preval_primary,
  color = Group_Color
)) +
  geom_pointrange(aes(xmin = `95_CI_min_primary`, xmax = `95_CI_max_primary`), size = 0.8) +
  geom_point(size = 0.5, shape = 16) +
  facet_wrap(~Group, nrow = 1, ncol = 5, scales = 'free_y') +
  scale_color_identity() + 
  theme_minimal() +
  labs(
    x = 'Prevalence (95% CI)',
    y = '',
    title = 'Regional Lactose Malabsorption Prevalence'
  ) +
  theme(
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

## Fig 1. (e) LMP of each country on map
LM_CM_cal <- LM %>%
  left_join(CM_cal, by = c("Country" = "Area"))

world <- ne_countries(scale = "medium", returnclass = "sf")
map_data <- world %>%
  left_join(LM_CM_cal, by = c("name" = "Country"))
ggplot(data = map_data) +
  geom_sf(aes(fill = Preval_primary), color = "black", size = 0.1) +
  scale_fill_viridis_c(option = "viridis", na.value = "white", name = "LMP") +
  labs(title = "LMP World Map") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# *****************************************************************************
# B. CM data
## Fig 2. (a) boxplot of CM_cal in total
boxplot(CM_cal$Mean, col = 'cornsilk2', main = 'Boxplot of Cattle Milk Calories/Year')
text(x = 1.2, y = median(CM_cal$Mean), 
     labels = paste0("← median: ",round(median(CM_cal$Mean), 2)), col = "black", pos = 4)

## Fig 2. (b) histogram of CM_cal in total
hist(CM_cal$Mean, col = hcl.colors(15, palette = "Fall"),
     main = 'Histogram of CM cal.', breaks = 15, xlab = 'Mean of Kcal')

## Fig 2. (c) boxplots of CM_cal per group
ggplot(LM_CM_cal, aes(x = Group, y = Mean, fill = Group)) +
  geom_boxplot() +
  scale_fill_manual(values = cols) +
  scale_x_discrete(labels = c('AFR', 'AME', 'ASI', 'EEU', 'EUR', 'FSR', 'MIE', 'NAF', 'OCE')) +
  scale_y_break(c(12500000, 50000000)) +
  labs(
    title = "Mean of Cattle Milk Calories per Year by Groups",
    x = "Group",
    y = "Mean"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.4)
  )

## Fig 2. (d) boxplot of CM_fsq in total
boxplot(CM_fsq$Mean, col = 'cornsilk3', main = 'Boxplot of Cattle Milk Food Supply Quantity')
text(x = 1.2, y = median(CM_fsq$Mean), 
     labels = paste0("← median: ",round(median(CM_fsq$Mean), 2)), col = "black", pos = 4)

## Fig 2. (e) histogram of CM_fsq in total
hist(CM_fsq$Mean, col = hcl.colors(15, palette = "Fall"),
     main = 'Histogram of CM fsq', breaks = 15, xlab = 'Mean of g')

## Fig 2. (f) boxplots of CM_fsq per group
LM_CM_fsq <- LM %>%
  left_join(CM_fsq, by = c("Country" = "Area"))

ggplot(LM_CM_fsq, aes(x = Group, y = Mean, fill = Group)) +
  geom_boxplot() +
  scale_fill_manual(values = cols) +
  scale_x_discrete(labels = c('AFR', 'AME', 'ASI', 'EEU', 'EUR', 'FSR', 'MIE', 'NAF', 'OCE')) +
  labs(
    title = "Mean of Cattle Milk FSQ per Year by Groups",
    x = "Group",
    y = "Mean"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.4)
  )

## Fig 2. (g) CM_cal of each country on map
world_cm_cal <- ne_countries(scale = "medium", returnclass = "sf")
map_data_cm_cal <- world_cm_cal %>%
  left_join(LM_CM_cal, by = c("name" = "Country"))
ggplot(data = map_data_cm_cal) +
  geom_sf(aes(fill = Mean), color = "black", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", na.value = "white", name = "Kcal") +
  labs(title = "Cattle Milk Calories/Year World Map") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

## Fig 2. (h) CM_fsq of each country on map
world_cm_fsq <- ne_countries(scale = "medium", returnclass = "sf")
map_data_cm_fsq <- world_cm_fsq %>%
  left_join(LM_CM_fsq, by = c("name" = "Country"))
ggplot(data = map_data_cm_fsq) +
  geom_sf(aes(fill = Mean), color = "black", size = 0.1) +
  scale_fill_viridis_c(option = "cividis", na.value = "white", name = "g") +
  labs(title = "Cattle Milk Food Supply Quantity World Map") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )







##############################################################################
#                              3. Regression                                 #
##############################################################################
# A. LMP & CM_cal
LM_CM_cal_long <- LM_CM_cal %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "Year",
               values_to = "CM_cal") %>%
  mutate(Year = as.integer(Year))

data_for_regression <- LM_CM_cal_long %>%
  select(Year, CM_cal, Preval_primary) %>%
  filter(!is.na(CM_cal) & !is.na(Preval_primary))

data_for_regression <- data_for_regression %>%
  filter(CM_cal > 0)

data_for_regression$log_CM_cal <- log(data_for_regression$CM_cal)

lm_model_log <- lm(log_CM_cal ~ Year + Preval_primary, data = data_for_regression)

summary(lm_model_log)

## Fig 3. (a) 3D scatter: CM year, LMP, CM calories(log)
plot_ly(data_for_regression, x = ~Year, y = ~Preval_primary, z = ~log_CM_cal,
        type = "scatter3d", mode = "markers", color = ~Preval_primary) %>%
  layout(title = "3D Scatter: Year, Prevalence, and Log of Cattle Milk Calories",
         scene = list(xaxis = list(title = "Year"),
                      yaxis = list(title = "Prevalence of Lactose Malabsorption"),
                      zaxis = list(title = "Log of Cattle Milk Calories")))

## Fig 3. (b) Linear regression between CM year and CM calories(log)
ggplot(data_for_regression, aes(x = Year, y = log_CM_cal)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Year vs Log of Cattle Milk Calories (log_CM_cal)", 
       x = "Year", 
       y = "Log of Cattle Milk Calories (log_CM_cal)") +
  theme_minimal()

## Fig 3. (c) Linear regression between LMP and CM calories(log)
ggplot(data_for_regression, aes(x = Preval_primary, y = log_CM_cal)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Prevalence of Lactose Malabsorption vs Log of Cattle Milk Calories (log_CM_cal)", 
       x = "Prevalence of Lactose Malabsorption (Preval_primary)", 
       y = "Log of Cattle Milk Calories (log_CM_cal)") +
  theme_minimal()

# *****************************************************************************
# B. LMP & CM_fsq
LM_CM_fsq_long <- LM_CM_fsq %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "Year",
               values_to = "CM_fsq") %>%
  mutate(Year = as.integer(Year))

data_for_regression_fsq <- LM_CM_fsq_long %>%
  select(Year, CM_fsq, Preval_primary) %>%
  filter(!is.na(CM_fsq) & !is.na(Preval_primary))

data_for_regression_fsq <- data_for_regression_fsq %>%
  filter(CM_fsq > 0)

lm_model_fsq <- lm(CM_fsq ~ Year + Preval_primary, data = data_for_regression_fsq)

summary(lm_model_fsq)

## Fig 3. (d) 3D scatter: CM year, LMP, CM Food Supply Quantity
plot_ly(data_for_regression_fsq, x = ~Year, y = ~Preval_primary, z = ~CM_fsq,
        type = "scatter3d", mode = "markers", color = ~Preval_primary) %>%
  layout(title = "3D Scatter: Year, Prevalence, and Cattle Milk Food Supply (CM_fsq)",
         scene = list(xaxis = list(title = "Year"),
                      yaxis = list(title = "Prevalence of Lactose Malabsorption"),
                      zaxis = list(title = "Cattle Milk Food Supply (CM_fsq)")))

## Fig 3. (e) Linear regression between CM year and CM Food Supply Quantity
ggplot(data_for_regression_fsq, aes(x = Year, y = CM_fsq)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Year vs Cattle Milk Food Supply (CM_fsq)", 
       x = "Year", 
       y = "Cattle Milk Food Supply (CM_fsq)") +
  theme_minimal()

## Fig 3. (f) Linear regression between LMP and Food Supply Quantity
ggplot(data_for_regression_fsq, aes(x = Preval_primary, y = CM_fsq)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Prevalence of Lactose Malabsorption vs Cattle Milk Food Supply (CM_fsq)", 
       x = "Prevalence of Lactose Malabsorption (Preval_primary)", 
       y = "Cattle Milk Food Supply (CM_fsq)") +
  theme_minimal()






##############################################################################
#                              4. Hypothesis                                 #
##############################################################################
# A. Load data
## pop_under15: population of 0-14 years old
## pop_under65: population of 15-65 years old
## pop_above65_rate: population rate of >= 65 years old
## agr_land_perc: percentage of agricultural land use
## doc_and_gdp: # of physicians and GDP
## health_exp: health expenditure
## life_exp: life expectancy
## mon_temper: monthly average surface temperature by year

# Hypothesis 1.
pop_under15 <- read.csv('./data_fixed/가설1 인구수/LM_df_0_15_population.csv', header = T, sep = ',')
pop_under15 <- pop_under15[,c('Country',paste0('X',1960:1998))]

pop_under65 <- read.csv('./data_fixed/가설1 인구수/LM_df_15_65_population.csv', header = T, sep = ',')
pop_under65 <- pop_under65[,c('Country',paste0('X',1960:1998))]

year_columns <- colnames(pop_under15)[2:ncol(pop_under15)]
pop_ratio <- data.frame(Country = pop_under15$Country)
for (year in year_columns) {
  under15 <- pop_under15[[year]]
  under65 <- pop_under65[[year]]
  
  pop_ratio[[year]] <- (under15 / under65) * 100
}
head(pop_ratio)

pop_above65_rate <- read.csv('./data_fixed/가설1 인구수/LM_df_65_ratio_population.csv', header = T, sep = ',')
pop_above65_rate <- pop_above65_rate[,c('Country',paste0('X',1960:1998))]

# Hypothesis 2.
agr_land_perc <- read.csv('./data_fixed/가설2 토지이용도/LM_df_Agricultural_Land.csv', header = T, sep = ',')
agr_land_perc <- agr_land_perc[,c('Country','Value')]

# Hypothesis 3.
doc_and_gdp <- read.csv('./data_fixed/가설3 의료접근성/LM_df_medical_doctors_per_1000_people_vs_gdp_per_capita.csv',
                        header = T, sep = ',')
doc_and_gdp <- doc_and_gdp[,c(1,11:12)]

health_exp <- read.csv('./data_fixed/가설3 의료접근성/wide_format_file_healthcare_expenditure_vs_GDP.csv',
                       header = T, sep = ',')
health_exp <- health_exp[,c(1,10:31)]

life_exp <- read.csv('./data_fixed/가설3 의료접근성/wide_LM_df_life_expectancy.csv', header = T, sep = ',')
life_exp <- life_exp[,c(1,10:50)]

# Hypothesis 4.
mon_temper <- read.csv('./data_fixed/가설4 기온/LM_df_monthly_average_surface_temperatures_by_year.csv',
                       header = T, sep = ',')
mon_temper <- mon_temper[,c(1,10:50)]
year_temp_per_mon <- split(mon_temper, mon_temper$Month)
year_temp_per_con <- split(mon_temper, mon_temper$Country)


# *****************************************************************************
# B. Pre-processing
pop_under15 = pop_under15[-which(pop_under15$Country == 'Republic of Congo' | 
                                   pop_under15$Country == 'Western Sahara'),]
pop_under65 = pop_under65[-which(pop_under65$Country == 'Republic of Congo' | 
                                   pop_under65$Country == 'Western Sahara'),]
pop_above65_rate = pop_above65_rate[-which(pop_above65_rate$Country == 'Republic of Congo' | 
                                             pop_above65_rate$Country == 'Western Sahara'),]
agr_land_perc = agr_land_perc[-which(agr_land_perc$Country == 'Republic of Congo' | 
                                       agr_land_perc$Country == 'Western Sahara'),]
doc_and_gdp = doc_and_gdp[-which(doc_and_gdp$Country == 'Republic of Congo' | 
                                   doc_and_gdp$Country == 'Western Sahara'),]
health_exp = health_exp[-which(health_exp$Country == 'Republic of Congo' | 
                                 health_exp$Country == 'Western Sahara'),]
health_exp = data.frame(
  Country = health_exp$Country,
  X2000 = health_exp$X2000, X2001 = health_exp$X2001, X2002 = health_exp$X2002,
  X2003 = health_exp$X2003, X2004 = health_exp$X2004, X2005 = health_exp$X2005,
  X2006 = health_exp$X2006, X2007 = health_exp$X2007, X2008 = health_exp$X2008,
  X2009 = health_exp$X2009, X2010 = health_exp$X2010, X2011 = health_exp$X2011,
  X2012 = health_exp$X2012, X2013 = health_exp$X2013, X2014 = health_exp$X2014,
  X2015 = health_exp$X2015, X2016 = health_exp$X2016, X2017 = health_exp$X2017,
  X2018 = health_exp$X2018, X2019 = health_exp$X2019, X2020 = health_exp$X2020,
  X2021 = health_exp$X2021
)
life_exp = life_exp[-which(life_exp$Country == 'Republic of Congo' | 
                             life_exp$Country == 'Western Sahara'),]

# *****************************************************************************
# C. EDA
## Fig 4. (a) Child dependency ratio (pop0-15/pop15-65) PCA
pop_data <- pop_ratio[, -1]
row.names(pop_data) <- pop_ratio$Country
pop_data <- na.omit(pop_data) 
pca_res <- prcomp(pop_data, scale. = TRUE)

scree_data <- data.frame(
  PC = paste0("PC", 1:length(pca_res$sdev)),
  Variance = (pca_res$sdev^2) / sum(pca_res$sdev^2) * 100
)

ggplot(scree_data, aes(x = PC, y = Variance)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(Variance, 1)), vjust = -0.5) +
  labs(title = "Scree Plot",
       x = "Principal Components",
       y = "Explained Variance (%)") +
  theme_minimal()

pca_df <- data.frame(
  Country = row.names(pop_data),
  Group = LM$Group,
  PC1 = pca_res$x[, 1],
  PC2 = pca_res$x[, 2]
)

ggplot(pca_df, aes(x = PC1, y = PC2, color = Group, label = Country)) +
  geom_point(size = 3) + 
  geom_text_repel(max.overlaps = 20) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "PCA Scatter Plot: PC1 vs. PC2",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal() +
  theme(legend.position = "bottom")

## Fig 4. (b) Population 65 ratio PCA
pop65_data <- pop_above65_rate[, -1]
row.names(pop65_data) <- pop_above65_rate$Country
pop65_data <- na.omit(pop65_data) 
pca_res2 <- prcomp(pop65_data, scale. = TRUE)

scree_data2 <- data.frame(
  PC = paste0("PC", 1:length(pca_res$sdev)),
  Variance = (pca_res$sdev^2) / sum(pca_res$sdev^2) * 100
)

ggplot(scree_data2, aes(x = PC, y = Variance)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(Variance, 1)), vjust = -0.5) +
  labs(title = "Scree Plot",
       x = "Principal Components",
       y = "Explained Variance (%)") +
  theme_minimal()

pca_df2 <- data.frame(
  Country = row.names(pop65_data),
  Group = LM$Group,
  PC1 = pca_res2$x[, 1],
  PC2 = pca_res2$x[, 2]
)

ggplot(pca_df2, aes(x = PC1, y = PC2, color = Group, label = Country)) +
  geom_point(size = 3) + 
  geom_text_repel(max.overlaps = 20) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "PCA Scatter Plot: PC1 vs. PC2",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal() +
  theme(legend.position = "bottom")

## Fig 4. (c) Percentage of agricultural land use
agr_land_data <- data.frame(
   Country = agr_land_perc$Country,
   Group = LM$Group,
   Value = agr_land_perc$Value
)
ggplot(agr_land_data, aes(x = Group, y = Value, fill = Group)) +
  geom_boxplot() +
  scale_fill_manual(values = cols) +
  scale_x_discrete(labels = c('AFR', 'AME', 'ASI', 'EEU', 'EUR', 'FSR', 'MIE', 'NAF', 'OCE')) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.4)
  )

## Fig 4. (d) & (e) # of physicians and GDP per capita
doc_gdp_data <- data.frame(
  Country = doc_and_gdp$Country,
  Group = LM$Group,
  Doc_per_1000 = doc_and_gdp$Physicians..per.1.000.people.,
  GDP_per_capita = doc_and_gdp$GDP.per.capita..PPP..constant.2017.international...
)
ggplot(doc_gdp_data, aes(x = Group, y = Doc_per_1000, fill = Group)) +
  geom_boxplot() +
  scale_fill_manual(values = cols) +
  scale_x_discrete(labels = c('AFR', 'AME', 'ASI', 'EEU', 'EUR', 'FSR', 'MIE', 'NAF', 'OCE')) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.4)
  )
ggplot(doc_gdp_data, aes(x = Group, y = GDP_per_capita, fill = Group)) +
  geom_boxplot() +
  scale_fill_manual(values = cols) +
  scale_x_discrete(labels = c('AFR', 'AME', 'ASI', 'EEU', 'EUR', 'FSR', 'MIE', 'NAF', 'OCE')) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.4)
  )

## Fig 4. (f)  healthcare expenditure PCA
health_exp_data <- health_exp[,-1]
rownames(health_exp_data) <- health_exp$Country
temp <- is.na(health_exp_data)
temp_names <- names(which(rowSums(temp) > 0))
health_exp_data <- na.omit(health_exp_data)

pca_res3 <- prcomp(health_exp_data, scale. = TRUE)

scree_data3 <- data.frame(
  PC = paste0("PC", 1:length(pca_res3$sdev)),
  Variance = (pca_res3$sdev^2) / sum(pca_res3$sdev^2) * 100
)

ggplot(scree_data3, aes(x = PC, y = Variance)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(Variance, 1)), vjust = -0.5) +
  labs(title = "Scree Plot",
       x = "Principal Components",
       y = "Explained Variance (%)") +
  theme_minimal()

pca_df3 <- data.frame(
  Country = row.names(health_exp_data),
  Group = LM[-which(LM$Country %in% temp_names),]$Group,
  PC1 = pca_res3$x[, 1],
  PC2 = pca_res3$x[, 2]
)

ggplot(pca_df3, aes(x = PC1, y = PC2, color = Group, label = Country)) +
  geom_point(size = 3) + 
  geom_text_repel(max.overlaps = 20) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "PCA Scatter Plot: PC1 vs. PC2",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal() +
  theme(legend.position = "bottom")

## Fig 4. (g) life expectancy PCA
life_exp_data <- life_exp[,-1]
rownames(life_exp_data) <- life_exp$Country
life_exp_data <- na.omit(life_exp_data)

pca_res4 <- prcomp(life_exp_data, scale. = TRUE)

scree_data4 <- data.frame(
  PC = paste0("PC", 1:length(pca_res4$sdev)),
  Variance = (pca_res4$sdev^2) / sum(pca_res4$sdev^2) * 100
)

ggplot(scree_data4, aes(x = PC, y = Variance)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(Variance, 1)), vjust = -0.5) +
  labs(title = "Scree Plot",
       x = "Principal Components",
       y = "Explained Variance (%)") +
  theme_minimal()

pca_df4 <- data.frame(
  Country = row.names(life_exp_data),
  Group = LM$Group,
  PC1 = pca_res4$x[, 1],
  PC2 = pca_res4$x[, 2]
)

ggplot(pca_df4, aes(x = PC1, y = PC2, color = Group, label = Country)) +
  geom_point(size = 3) + 
  geom_text_repel(max.overlaps = 20) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "PCA Scatter Plot: PC1 vs. PC2",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal() +
  theme(legend.position = "bottom")

## Fig 4. (h) yearly temperature changes by country 
avg_year_temp <- t(as.data.frame(lapply(year_temp_per_con, function(x) colMeans(x[,-c(1:2)]))))
avg_year_temp <- avg_year_temp[,-1]

avg_jan_temp <- select(year_temp_per_mon$`1`, -Month)
avg_feb_temp <- select(year_temp_per_mon$`2`, -Month)
avg_mar_temp <- select(year_temp_per_mon$`3`, -Month)
avg_apr_temp <- select(year_temp_per_mon$`4`, -Month)
avg_may_temp <- select(year_temp_per_mon$`5`, -Month)
avg_jun_temp <- select(year_temp_per_mon$`6`, -Month)
avg_jul_temp <- select(year_temp_per_mon$`7`, -Month)
avg_agu_temp <- select(year_temp_per_mon$`8`, -Month)
avg_sep_temp <- select(year_temp_per_mon$`9`, -Month)
avg_oct_temp <- select(year_temp_per_mon$`10`, -Month)
avg_nov_temp <- select(year_temp_per_mon$`11`, -Month)
avg_dec_temp <- select(year_temp_per_mon$`12`, -Month)

avg_mon_temp <- data.frame(
  Country = avg_jan_temp$Country,
  Jan = rowMeans(avg_jan_temp[,-1], na.rm = TRUE),
  Feb = rowMeans(avg_feb_temp[,-1], na.rm = TRUE),
  Mar = rowMeans(avg_mar_temp[,-1], na.rm = TRUE),
  Apr = rowMeans(avg_apr_temp[,-1], na.rm = TRUE),
  May = rowMeans(avg_may_temp[,-1], na.rm = TRUE),
  Jun = rowMeans(avg_jun_temp[,-1], na.rm = TRUE),
  Jul = rowMeans(avg_jul_temp[,-1], na.rm = TRUE),
  Aug = rowMeans(avg_agu_temp[,-1], na.rm = TRUE),
  Sep = rowMeans(avg_sep_temp[,-1], na.rm = TRUE),
  Oct = rowMeans(avg_oct_temp[,-1], na.rm = TRUE),
  Nov = rowMeans(avg_nov_temp[,-1], na.rm = TRUE),
  Dec = rowMeans(avg_dec_temp[,-1], na.rm = TRUE)
)

avg_year_temp_long <- avg_year_temp %>%
  as.data.frame() %>% 
  mutate(Country = rownames(avg_year_temp)) %>% 
  pivot_longer(cols = starts_with("X"), names_to = "Year", values_to = "Temperature") %>%
  mutate(Year = as.numeric(gsub("X", "", Year)))

ggplot(avg_year_temp_long, aes(x = Year, y = Temperature, group = Country, color = Country)) +
  geom_line() + 
  geom_point(data = avg_year_temp_long %>% group_by(Country) %>% filter(Year == max(Year)), 
             aes(x = Year, y = Temperature), size = 2) +  
  geom_text(data = avg_year_temp_long %>% group_by(Country) %>% filter(Year == max(Year)), 
            aes(x = Year, y = Temperature, label = Country), 
            hjust = -0.1, size = 3, check_overlap = TRUE) +  
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 10, ymax = 20), fill = "gray", alpha = 0.01, inherit.aes = FALSE) +
  theme_minimal() +
  labs(title = "Yearly Temperature Changes by Country",
       x = "Year",
       y = "Temperature") +
  theme(legend.position = "none")

## Fig 4. (i) Average temperature per year  by country (distance from 10-20)
avg_year_temp <- cbind(avg_year_temp, rowMeans(avg_year_temp))
colnames(avg_year_temp)[40] <- "distance"

avg_year_temp <- as.data.frame(avg_year_temp)
avg_year_temp$distance[which(avg_year_temp$distance < 10)] <- abs(avg_year_temp$distance[which(avg_year_temp$distance < 10)] - 10)
avg_year_temp$distance[which(avg_year_temp$distance >= 10 & avg_year_temp$distance <= 20)] <- 0
avg_year_temp$distance[which(avg_year_temp$distance > 20)] <- abs(avg_year_temp$distance[which(avg_year_temp$distance > 20)] - 20)

temp_countries <- rownames(avg_year_temp)
temp_countries_updated <- gsub("\\.", " ", temp_countries)
non_matching <- setdiff(temp_countries_updated, LM$Country)

avg_year_temp <- avg_year_temp[!rownames(avg_year_temp) %in% c("Republic.of.Congo", "Western.Sahara"), ]

avg_year_temp_data <- data.frame(
  Country = rownames(avg_year_temp),
  Group = LM$Group,
  distance = avg_year_temp$distance
)
ggplot(avg_year_temp_data, aes(x = Group, y = distance, fill = Group)) +
  geom_boxplot() +
  scale_fill_manual(values = cols) +
  scale_x_discrete(labels = c('AFR', 'AME', 'ASI', 'EEU', 'EUR', 'FSR', 'MIE', 'NAF', 'OCE')) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.4)
  )







##############################################################################
#                             5. Correlation                                 #
##############################################################################
# Data
CM_cal_mean = data.frame(Country = CM_cal$Area, CM_cal_mean = CM_cal$Mean)
CM_fsq_mean = data.frame(Country = CM_fsq$Area, CM_fsq_mean = CM_fsq$Mean)
LMP = data.frame(Country = LM$Country, LMP = LM$Preval_primary)
GD <- read.csv('genetic_distance_from_danish.csv', sep = ',', header = T)
GD <- GD[-c(88:65534),]
Genetic_distance = data.frame(Country = GD$Country, Genetic_distance = GD$FST)
pop_ratio_mean = data.frame(
  Country = pop_ratio$Country, 
  pop_ratio_mean = rowMeans(pop_ratio[,-1], na.rm = T)
)
pop_above65_rate_mean = data.frame(
  Country = pop_above65_rate$Country,
  pop_above65_rate_mean = rowMeans(pop_above65_rate[,-1], na.rm = T)
)
agr_land = data.frame(Country = agr_land_perc$Country, agr_land = agr_land_perc$Value)
doctors = data.frame(Country = doc_and_gdp$Country, doctors = doc_and_gdp$Physicians..per.1.000.people.)
gdp = data.frame(Country = doc_and_gdp$Country, gdp = doc_and_gdp$GDP.per.capita..PPP..constant.2017.international...)
ht_exp = data.frame(Country = health_exp$Country, ht_exp = rowMeans(health_exp[,-1], na.rm = T))
lf_exp = data.frame(Country = life_exp$Country, lf_exp = rowMeans(life_exp[,-1], na.rm = T))
year_temp = data.frame(Country = rownames(avg_year_temp), year_temp = avg_year_temp$distance)
year_temp$Country <- gsub("\\.", " ", year_temp$Country)

# outer join
outer_joined_df <- Reduce(function(x, y) merge(x, y, by = "Country", all = TRUE),
                          list(CM_cal_mean, CM_fsq_mean, LMP, Genetic_distance, pop_ratio_mean, pop_above65_rate_mean, 
                               agr_land, doctors, gdp, ht_exp, lf_exp, year_temp))
outer_joined_df <- outer_joined_df[-c(75,87),]
outer_joined_df$CM_cal_mean_log <- log(outer_joined_df$CM_cal_mean)

## Fig 5. (b) Correlation Plot & (c) correlation plot with p-value
ggcorrplot(cor(outer_joined_df[,-1], use = "complete.obs"), 
           hc.order = T,
           method = 'circle',
           outline.color = 'white',
           colors = hcl.colors(3, palette = 'Fall'))

ggcorrplot(cor(outer_joined_df[,-1], use = "complete.obs"), 
           hc.order = T,
           lab = T,
           lab_size = 3,
           outline.color = 'white',
           type = 'lower',
           p.mat = cor_pmat(outer_joined_df[,-1], use = 'complete.obs'),
           colors = hcl.colors(3, palette = 'Fall'))







##############################################################################
#                              6. Random forest                              #
##############################################################################
set.seed(123)
data <- na.omit(outer_joined_df)

# 1. LM
rf_model_LM <- randomForest(LMP ~ Genetic_distance + pop_ratio_mean + 
                              CM_cal_mean_log + ht_exp + gdp + lf_exp +
                              pop_above65_rate_mean + doctors, data = data[,-1], importance = TRUE)
varImpPlot(rf_model_LM)

importance_df <- data.frame(Variable = rownames(rf_model_LM$importance),
                            Importance = rf_model_LM$importance[,"%IncMSE"])
importance_df <- importance_df[order(-importance_df$Importance),]

ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "wheat", width = 0.7) +
  coord_flip() +
  labs(title = "Variable Importance (%IncMSE)",
       x = "Variables",
       y = "Importance") +
  theme_minimal()

tree_model_LM <- rpart(LMP ~ Genetic_distance + pop_ratio_mean + 
                      CM_cal_mean_log + ht_exp + gdp + lf_exp +
                      pop_above65_rate_mean + doctors, data = data[,-1])
rpart.plot(tree_model_LM, type = 2, extra = 101, 
           box.palette = c("wheat1","wheat2","wheat3"),
           col = "gray4")

# 2. CM
## cal_log
rf_model_CM_cal_log <- randomForest(CM_cal_mean_log ~ year_temp + Genetic_distance +
                                      LMP + pop_ratio_mean + agr_land, data = data[,-1], importance = TRUE)
varImpPlot(rf_model_CM_cal_log)
importance_df_CM_cal_log <- data.frame(Variable = rownames(rf_model_CM_cal_log$importance),
                                       Importance = rf_model_CM_cal_log$importance[,"%IncMSE"])
importance_df_CM_cal_log <- importance_df_CM_cal_log[order(-importance_df_CM_cal_log$Importance),]

ggplot(importance_df_CM_cal_log, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "slategray1", width = 0.7) +
  coord_flip() +
  labs(title = "Variable Importance (%IncMSE)",
       x = "Variables",
       y = "Importance") +
  theme_minimal()

tree_model_CM_cal <- rpart(CM_cal_mean_log ~ year_temp + Genetic_distance +
                             LMP + pop_ratio_mean + agr_land, data = data[,-1])
rpart.plot(tree_model_CM_cal, type = 2, extra = 101, 
           box.palette = c("slategray1","slategray2","slategray3"),
           col = "gray4")

## fsq
rf_model_CM_fsq <- randomForest(CM_fsq_mean ~ year_temp + Genetic_distance + pop_ratio_mean +
                                  lf_exp + pop_above65_rate_mean + doctors, data = data[,-1], importance = TRUE)
varImpPlot(rf_model_CM_fsq)
importance_df_CM_fsq <- data.frame(Variable = rownames(rf_model_CM_fsq$importance),
                                       Importance = rf_model_CM_fsq$importance[,"%IncMSE"])
importance_df_CM_fsq <- importance_df_CM_fsq[order(-importance_df_CM_fsq$Importance),]

ggplot(importance_df_CM_fsq, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "paleturquoise", width = 0.7) +
  coord_flip() +
  labs(title = "Variable Importance (%IncMSE)",
       x = "Variables",
       y = "Importance") +
  theme_minimal()

tree_model_CM_fsq <- rpart(CM_fsq_mean ~ year_temp + Genetic_distance + pop_ratio_mean +
                             lf_exp + pop_above65_rate_mean + doctors, data = data[,-1])
rpart.plot(tree_model_CM_fsq, type = 2, extra = 101, 
           box.palette =c("paleturquoise","paleturquoise3","paleturquoise4"))
