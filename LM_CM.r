# install.package('ggplot2')
# install.package('dplyr')
# install.package('tidyr')
# install.packages('ggbreak')
# install.packages('sf')
# install.packages('rnaturalearth')
# install.packages('rnaturalearthdata')
# install.packages('ggcorrplot')
# install.packages('Hmisc')
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("randomForest")

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggbreak)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggcorrplot)
library(Hmisc)
library(rpart)
library(rpart.plot)
library(randomForest)

##############################################################################
#                          1. Data Pre-processing                            #
##############################################################################

# before loading the data, set the working directory
# setwd()

# *****************************************************************************
# A. Load LM data
## LM: lactose malabsorption prevalence data per country
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
head(LM)

# *****************************************************************************
# B. Load CM data
## CM: cattle milk data per country
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
head(CM_cal)
head(CM_fsq)

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

# *****************************************************************************
# E. Load data for correlation/regression analysis later
## pop_under15: population of 0-14 years old
## pop_under65: population of 15-65 years old
## pop_above65_rate: population rate of >= 65 years old
## agr_land_perc: percentage of agricultural land use
## doc_and_gdp: # of physicians and GDP
## health_exp: health expenditure
## life_exp: life expectancy
## mon_temper: monthly average surface temperature by year
## soy_prod: soybean production

pop_under15 <- read.csv('LM_df_0_15_population.csv', header = T, sep = ',')
pop_under65 <- read.csv('LM_df_15_65_population.csv', header = T, sep = ',')
pop_above65_rate <- read.csv('LM_df_65_ratio_population.csv', header = T, sep = ',')
agr_land_perc <- read.csv('LM_df_Agricultural_Land.csv', header = T, sep = ',')
doc_and_gdp <- read.csv('LM_df_medical_doctors_per_1000_people_vs_gdp_per_capita.csv',
                        header = T, sep = ',')
health_exp <- read.csv('wide_format_file_health_expenditure.csv',
                            header = T, sep = ',')
life_exp <- read.csv('wide_LM_df_life_expectancy.csv', header = T, sep = ',')
mon_temper <- read.csv('LM_df_monthly_average_surface_temperatures_by_year.csv',
                       header = T, sep = ',')
soy_prod <- read.csv('LM_df_soybean_production_by_country_2024.csv',
                     header = T, sep = ',')


##############################################################################
#                         2. Exploratory data analysis                       #
##############################################################################
# A. LM data
## Fig 1: boxplot of LMP in total
boxplot(LM$Preval_primary, col = 'cornsilk', main = 'Boxplot of LMP')
text(x = 1.2, y = median(LM$Preval_primary), 
     labels = paste0("← median: ",round(median(LM$Preval_primary), 2)), col = "black", pos = 4)

## Fig 2: histogram of LMP in total
hist(LM$Preval_primary, col = hcl.colors(10, palette = "Fall"),
     main = 'Histogram of LMP', breaks = 10, xlab = 'Lactose Malabsorption Prevalence')

## Fig 3: boxplots of LMP per group
##  Africa (southern, eastern, western), America, Asia, Eastern Europe, 
##  Europe (western, southern, northern), Former Soviet Republics, 
##  Middle East, Northern Africa, Oceania, respectively
cols <- hcl.colors(9, palette = "Fall")
boxplot(LM$Preval_primary ~ LM$Group, 
        names = c('AFR', 'AME', 'ASI', 'EEU', 'EUR', 'FSR', 'MIE', 'NAF', 'OCE'),
        xlab = 'Group', ylab = 'LMP', col = cols, main = 'LMP by Groups')

## Fig 4: dumbbell chart for LMP of each country by groups with 95% confidence interval
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

windows()
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

## Fig 5: LMP of each country on map
 <- LM %>%
  left_join(CM_cal, by = c("Country" = "Area"))

world <- ne_countries(scale = "medium", returnclass = "sf")
map_data <- world %>%
  left_join(, by = c("name" = "Country"))
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
## Fig 5: boxplot of CM_cal / CM_fsq in total
boxplot(CM_cal$Mean, col = 'cornsilk2', main = 'Boxplot of Cattle Milk Calories/Year')
text(x = 1.2, y = median(CM_cal$Mean), 
     labels = paste0("← median: ",round(median(CM_cal$Mean), 2)), col = "black", pos = 4)

ggplot(CM_cal, aes(x = 1, y = Mean)) +
  geom_boxplot(fill = 'cornsilk2') +
  scale_y_break(c(1e+07, 5e+07), scales = 0.1) +
  labs(
    title = 'Boxplot of Cattle Milk Calories/Year',
    x = 'CM cal'
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

boxplot(CM_fsq$Mean, col = 'cornsilk3', main = 'Boxplot of Cattle Milk Food Supply Quantity')
text(x = 1.2, y = median(CM_fsq$Mean), 
     labels = paste0("← median: ",round(median(CM_fsq$Mean), 2)), col = "black", pos = 4)

ggplot(CM_fsq, aes(x = 1, y = Mean)) +
  geom_boxplot(fill = 'cornsilk3') +
  labs(
    title = 'Boxplot of Cattle Milk Food Supply Quantity',
    x = 'CM fsq'
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

## Fig 6: histogram of CM_cal in total
hist(CM_cal$Mean, col = hcl.colors(15, palette = "Fall"),
     main = 'Histogram of CM cal.', breaks = 15, xlab = 'Mean of Kcal')

ggplot(CM_cal, aes(x = Mean)) +
  geom_histogram(fill = 'cornsilk2', bins = 15) +
  scale_y_break(c(15,100), scales = 0.5) +
  labs(
    title = 'Histogram of Cattle Milk Calories/Year',
    x = 'CM cal'
  ) + theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

hist(CM_fsq$Mean, col = hcl.colors(15, palette = "Fall"),
     main = 'Histogram of CM fsq', breaks = 15, xlab = 'Mean of g')

ggplot(CM_fsq, aes(x = Mean)) +
  geom_histogram(fill = 'cornsilk2', bins = 15) +
  scale_y_break(c(10,100), scales = 0.1) +
  labs(
    title = 'Histogram of Cattle Milk Food Supply Quantity',
    x = 'CM fsq'
  ) + theme_minimal() + theme(plot.title = element_text(hjust = 0.5))


## Fig 7: boxplots of CM_cal per group
ggplot(, aes(x = Group, y = Mean, fill = Group)) +
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

## Fig 8: boxplots of CM_fsq per group
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

## Fig 9: CM of each country on map
world_cm_cal <- ne_countries(scale = "medium", returnclass = "sf")
map_data_cm_cal <- world_cm_cal %>%
  left_join(, by = c("name" = "Country"))
ggplot(data = map_data_cm_cal) +
  geom_sf(aes(fill = Mean), color = "black", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", na.value = "white", name = "Kcal") +
  labs(title = "Cattle Milk Calories/Year World Map") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

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
#                              3. Correlation test                           #
##############################################################################

# A. LMP & CM_cal(Mean)
lm($Mean ~ $Preval_primary)
cor($Mean, $Preval_primary) #-0.05720739

ggplot(, aes(x = Mean, y = Preval_primary)) +
  geom_point(aes(color = Mean)) +
  geom_smooth(method = "lm", color = "violetred2", se = FALSE) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "Correlation b/n LMP and CM calories/year",
    x = "Mean of Cattle Milk Calories/Year",
    y = "LMP"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(, aes(x = Mean, y = Preval_primary)) +
  geom_point(aes(color = Mean)) +
  geom_smooth(method = "lm", color = "violetred2", se = FALSE) +
  scale_x_break(c(5e+06, 6e+07), scales = 0.1) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "Correlation b/n LMP and CM calories/year",
    x = "Mean of Cattle Milk Calories/Year",
    y = "LMP"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# *****************************************************************************
# B. LMP & CM_fsq(Mean)
lm(LM_CM_fsq$Mean ~ LM_CM_fsq$Preval_primary)
cor(LM_CM_fsq$Mean, LM_CM_fsq$Preval_primary) #-0.1940693

ggplot(LM_CM_fsq, aes(x = Mean, y = Preval_primary)) +
  geom_point(aes(color = Mean)) +
  geom_smooth(method = "lm", color = "royalblue", se = FALSE) +
  scale_color_viridis_c(option = "cividis") +
  labs(
    title = "Correlation b/n LMP and CM Food Supply Quantity",
    x = "Mean of Cattle Milk Food Supply Quantity",
    y = "LMP"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(LM_CM_fsq, aes(x = Mean, y = Preval_primary)) +
  geom_point(aes(color = Mean)) +
  geom_smooth(method = "lm", color = "royalblue", se = FALSE) +
  scale_x_break(c(400, 600), scales = 0.1) +
  scale_color_viridis_c(option = "cividis") +
  labs(
    title = "Correlation b/n LMP and CM Food Supply Quantity",
    x = "Mean of Cattle Milk Food Supply Quantity",
    y = "LMP"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# *****************************************************************************
# C. LMP & Other factors
## pre-processing
LM_corr <- LM

pop_under15 = pop_under15[-which(pop_under15$Country == 'Republic of Congo' | 
                                   pop_under15$Country == 'Western Sahara'),]
LM_corr$'pop_under15' <- apply(pop_under15[,12:75], 1, function(x){mean(x, na.rm = T)})

pop_under65 = pop_under65[-which(pop_under65$Country == 'Republic of Congo' | 
                                   pop_under65$Country == 'Western Sahara'),]
LM_corr$'pop_under65' <- apply(pop_under65[,12:75], 1, function(x){mean(x, na.rm = T)})

pop_above65_rate = pop_above65_rate[-which(pop_above65_rate$Country == 'Republic of Congo' | 
                                             pop_above65_rate$Country == 'Western Sahara'),]
LM_corr$'pop_above65_rate' <- apply(pop_above65_rate[,12:75], 1, 
                                    function(x){mean(x, na.rm = T)})

agr_land_perc = agr_land_perc[-which(agr_land_perc$Country == 'Republic of Congo' | 
                                       agr_land_perc$Country == 'Western Sahara'),]
LM_corr$'agr_land_perc' <- agr_land_perc$Value

doc_and_gdp = doc_and_gdp[-which(doc_and_gdp$Country == 'Republic of Congo' | 
                                   doc_and_gdp$Country == 'Western Sahara'),]
LM_corr$'docs_per_1000' <- doc_and_gdp$Physicians..per.1.000.people.
LM_corr$'GDP' <- doc_and_gdp$GDP.per.capita..PPP..constant.2017.international...
LM_corr$'pop_hist' <- doc_and_gdp$Population..historical.

health_exp = health_exp[-which(health_exp$Country == 'Republic of Congo' | 
                                 health_exp$Country == 'Western Sahara'),]
LM_corr$'health_exp' <- apply(health_exp[,10:31], 1, function(x){mean(x, na.rm = T)})

life_exp = life_exp[-which(life_exp$Country == 'Republic of Congo' | 
                             life_exp$Country == 'Western Sahara'),]
LM_corr$'life_exp' <- apply(life_exp[,10:71], 1, function(x){mean(x, na.rm = T)})

temp <- split(mon_temper, mon_temper$Month)
for (month in names(temp)[-1]) {
  current_data <- temp[[month]]
  current_data$Mean <- apply(current_data[, 11:85], 1, function(x) mean(x, na.rm = TRUE))
  month_name <- paste0(tolower(month.abb[as.numeric(month)]), "_tmp")
  
  merged_data <- merge(LM_corr, current_data[, c("Country", "Mean")], 
                       by = "Country", all.x = TRUE)

  colnames(merged_data)[ncol(merged_data)] <- paste0(tolower(month.abb[as.numeric(month)]), "_temp")

  LM_corr <- merged_data
}

LM_corr$'avg_temp' <- apply(LM_corr[, 15:26], 1, function(x) mean(x, na.rm = TRUE))
head(LM_corr)

## soy_prod -> 37/89 (41%, 0)

## corr.
LM_corr$'CM_cal_mean' <- LM_CM_cal$Mean
LM_corr$'CM_fsq_mean' <- LM_CM_fsq$Mean

corr_vec_CM_cal = c()
corr_vec_CM_fsq = c()
for (i in 3:29){
  tmp1 = cor(LM_corr$CM_cal_mean, LM_corr[,i], use = 'complete.obs')
  names(tmp1) = colnames(LM_corr)[i]
  
  tmp2 = cor(LM_corr$CM_fsq_mean, LM_corr[,i], use = 'complete.obs')
  names(tmp2) = colnames(LM_corr)[i]

  corr_vec_CM_cal = c(corr_vec_CM_cal, tmp1)
  corr_vec_CM_fsq = c(corr_vec_CM_fsq, tmp2)
}

sort(corr_vec_CM_cal)
sort(corr_vec_CM_fsq)

cor_mat_LM = LM_corr[,c('Preval_primary','pop_under15','pop_under65','pop_above65_rate',
                        'agr_land_perc','docs_per_1000','GDP','health_exp','life_exp',
                        'avg_temp','CM_cal_mean','CM_fsq_mean')]
plot(cor_mat_LM)
round(cor(cor_mat_LM, use = "complete.obs"), 2)
cor(cor_mat_LM, use = "complete.obs")
rcorr(as.matrix(cor_mat_LM))$P

ggcorrplot(cor(cor_mat_LM, use = "complete.obs"), 
           hc.order = T,
           method = 'circle',
           outline.color = 'white',
           colors = hcl.colors(3, palette = 'Fall'))

ggcorrplot(cor(cor_mat_LM, use = "complete.obs"), 
           hc.order = T,
           lab = T,
           lab_size = 3,
           outline.color = 'white',
           type = 'lower',
           p.mat = rcorr(as.matrix(cor_mat_LM))$P,
           colors = hcl.colors(3, palette = 'Fall'))

cor_pmat(cor_mat_LM, use = 'complete.obs')
ggcorrplot(cor(cor_mat_LM, use = "complete.obs"), 
          hc.order = T,
          lab = T,
          lab_size = 3,
          outline.color = 'white',
          type = 'lower',
          p.mat = cor_pmat(cor_mat_LM, use = 'complete.obs'),
          colors = hcl.colors(3, palette = 'Fall'))

cor(cor_mat_LM, use = "complete.obs")[,c('CM_cal_mean', 'CM_fsq_mean')]


fit1 <- lm(CM_cal_mean ~ Preval_primary + pop_under15 + pop_under65 + pop_above65_rate + agr_land_perc 
           + docs_per_1000 + GDP + health_exp + life_exp + avg_temp, data = cor_mat_LM, na.action = na.omit)
summary(fit1)

fit2 <- lm(CM_fsq_mean ~ Preval_primary + pop_under15 + pop_under65 + pop_above65_rate + agr_land_perc 
           + docs_per_1000 + GDP + health_exp + life_exp + avg_temp, data = cor_mat_LM, na.action = na.omit)
summary(fit2)

tree_fit1 <- rpart(CM_cal_mean ~ Preval_primary + pop_under15 + pop_under65 + pop_above65_rate + agr_land_perc 
                   + docs_per_1000 + GDP + health_exp + life_exp + avg_temp, data = cor_mat_LM, na.action = na.omit)
plot(tree_fit1)
text(tree_fit1)
summary(tree_fit1)

tree_fit2 <- rpart(CM_fsq_mean ~ Preval_primary + pop_under15 + pop_under65 + pop_above65_rate + agr_land_perc 
                   + docs_per_1000 + GDP + health_exp + life_exp + avg_temp, data = cor_mat_LM, na.action = na.omit)
plot(tree_fit2)
text(tree_fit2)
summary(tree_fit2)



mon_temper <- read.csv('LM_df_monthly_average_surface_temperatures_by_year.csv',
                       header = T, sep = ',')
soy_prod <- read.csv('LM_df_soybean_production_by_country_2024.csv',
                     header = T, sep = ',')
