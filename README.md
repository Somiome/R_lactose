# Lactose Intolerance and Cattle Milk
2024 KHU R Programming Project 

[Presentation Link](https://bit.ly/lactoseintolerance_R_KHU)(Only accessible for KHU members)

Dongmin Kim (2021102277), JoonYeong Yu (2018103608), Somi Lim (2020102770)

## Table of Contents
[1. **Lactose Malabsorption and Cattle Milk**](#lactose-intolerance-and-cattle-milk)

[2. **Case Study: India**](#case-study-india)

[3. **Case Study: South Korea**](#case-study-south-korea)

## Lactose Malabsorption and Cattle Milk
- Lactose Malabsorption Prevalence (LMP)
  - similar concept with lactose intolerance with values from 0 to 1
  - `/data/LM.csv` lactose malabsorption prevalence data per country
    - C. Storhaug, et al., *Country, regional, and global estimates for lactose malabsorption in adults: a systematic review and meta-analysis*, The Lancet Gastroenterology & Hepatology, Volume 2, Issue 10, 2017, Pages 738-746, ISSN 2468-1253, https://doi.org/10.1016/S2468-1253(17)30154-1.
- Cattle Milk (CM)
  - Calories/Year (KCal)
     - total calorie content of the food supply available
     - in a country, measured in kilocalories per capita per year
  - Food Supply Quantity (g/cap/d)
     - average daily quantity of food available per person
     - includes all food produced within a country or imported for consumption,
     - adjusted for losses during storage, transportation, and processing
  - `/data/FAOSTAT_data_en_11-8-2024 (1).csv` cattle milk data per country
    - *Supply Utilization Accounts (2010-)*, FAOSTAT, Food and Agriculture Organization of the United Nations, https://www.fao.org/faostat/en/#data/SCL

### Exploratory Data Analysis `/data/LM_CM.r`
1. **LMP**

![Fig 1. (a) Distribution of LMP, (b) Frequency Distribution of LMP, (c) Comparison of LMP Across Regions](/LM_CM/image-3.png)
![Fig 1. (d) Regional LMP by Countries](/LM_CM/image-4.png)
![Fig 1. (e) Global Distribution of LMP](/LM_CM/image-5.png)

- The fact that the median value of LMP is 0.73 **(a)** and there are higher frequency when LMP is closer to 1 **(b)** indicates that in almost all countries people can't digest lactose properly.
- Countries in Europe have the lowest median value of LMP. **(c)**
- In Europe, there are lots of countries that have LMP values closer to 0, meaning that the majority of residents in those countries can consume dairy products without any metabolic issues. **(d)** Some countries in Asia and Oceania show an extreme LMP value (1) and without any distribution, such as South Korea and Vietnam.
- The darker blue **(e)** represents lower LMP (closer to 0) around northern Europe such as Sweden, Norway, and United Kingdom.
-----
2. **CM**

![Fig 2. (a) Distribution of CM Calories/Year, (b) Frequency Distribution of CM Calories/Year, (c) Comparison of CM Calories/Year Across Regions](/LM_CM/image-6.png)
![Fig 2. (d) Distribution of CM Food Supply Quantity, (e) Frequency Distribution of CM Food Supply Quantity, (f) Comparison of CM Food Supply Quantity Across Regions](/LM_CM/image-8.png)
![Fig 2. (g) Global Distribution of CM Calories/Year](/LM_CM/image-9.png)
![Fig 2. (h) Global Distribution of CM Food Supply Quantity](/LM_CM/image-10.png)

- CM Calories/Year distribution **(a) and (b)** has many outliers.
- There is one extreme outlier **(c)** for CM Calories/Year in Asia.
- CM Food Supply Quantity distribution **(d) and (e)** shows broad range of values and some outliers.
- The group that has the highest median value in both indicators **(c) and (f)** is Eastern Europe.
- One of outliers in CM Calories/Year data **(g)** is confirmed as India. 
- One of outliers in CM Food Supply Quantity data **(h)** is confirmed as Kazakhstan.

### Regression between LMP and CM
![Fig 3. (a) 3D scatter: CM year, LMP, CM calories(log)](/LM_CM/image-21.png)
![Fig 3. (b) Linear regression between CM year and CM calories(log), (c) Linear regression between LMP and CM calories(log)](/LM_CM/image-22.png)
![Fig 3. (d) 3D scatter: CM year, LMP, CM Food Supply Quantity](/LM_CM/image-23.png)
![Fig 3. (e) Linear regression between CM year and CM Food Supply Quantity, (f) Linear regression between LMP and CM Food Supply Quantity](/LM_CM/image-24.png)
- Linear regression model for (a) is `log_CM_cal ~ Year + Preval_primary`.
  - `Year` CM year, `Preval_primary` LMP
    ```
    Coefficients:
                    Estimate Std. Error t value Pr(>|t|)    
    (Intercept)     5.527872  32.052828   0.172    0.863    
    Year            0.004049   0.015899   0.255    0.799    
    Preval_primary -1.513810   0.201575  -7.510  1.2e-13 ***
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

    Residual standard error: 1.995 on 1123 degrees of freedom
    Multiple R-squared:  0.04788,	Adjusted R-squared:  0.04618 
    F-statistic: 28.23 on 2 and 1123 DF,  p-value: 1.087e-12
    ```
  - `Preval_primary = -1.513810`
    - This shows that for each 1-unit increase in `Preval_primary`, `log_CM_cal` is expected to decrease by about 1.51.
    - This coefficient is highly significant with a p-value of 1.2e-13, indicating that Preval_primary has a strong and statistically significant impact on `log_CM_cal`.
  - `Multiple R-squared = 0.04788`
    - This value indicates that the model explains about 4.8% of the variation in `log_CM_cal`, suggesting that Year and `Preval_primary` alone do not explain much of the variability in `log_CM_cal`.

- Linear regression model for (d) was `CM_fsq ~ Year + Preval_primary`.
  - `Year` CM year, `Preval_primary` LMP
    ```
    Coefficients:
                    Estimate Std. Error t value Pr(>|t|)    
    (Intercept)     254.85161 1999.87141   0.127    0.899    
    Year             -0.04022    0.99198  -0.041    0.968    
    Preval_primary  -82.42064   12.57687  -6.553 8.55e-11 ***
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

    Residual standard error: 124.5 on 1123 degrees of freedom
    Multiple R-squared:  0.03683,	Adjusted R-squared:  0.03512 
    F-statistic: 21.47 on 2 and 1123 DF,  p-value: 7.047e-10
    ```
  - `Preval_primary = -82.42064`
    - This shows that for each 1-unit increase in `Preval_primary`, `CM_fsq` is expected to decrease by 82.42. 
    - This coefficient is highly significant with a p-value of 8.55e-11, indicating that `Preval_primary` has a strong and statistically significant negative impact on `CM_fsq`.
  - `Multiple R-squared = 0.03683`
    - This value indicates that the model explains only about 3.7% of the variation in `CM_fsq`, suggesting that the independent variables Year and Preval_primary do not explain much of the variability in `CM_fsq`.


---

> **Conclusion**: Lactose Malabsorption Prevalence can affect on Cattle Milk (Calories/Year and Food Supply Quantity) but not enough alone.
>
> **Quenstion**: What else variables could be related to Cattle Milk consumption?


### Hypothesis
1. **Countries with a higher youth dependency ratio** will have **higher dairy consumption and production.**
   - Lactase production declines with age; thus, countries with a higher proportion of children may have higher dairy consumption and sales.
   - https://data.worldbank.org/indicator/SP.POP.0014.TO?view=map
   - `pop_under15`: population of 0-14 years old 
     - `./data_fixed/가설1 인구수/LM_df_0_15_population.csv`
   - `pop_under65`: population of 15-65 years old
     - `./data_fixed/가설1 인구수/LM_df_15_65_population.csv`
   - `pop_above65_rate`: population rate of >= 65 years old
     - `./data_fixed/가설1 인구수/LM_df_65_ratio_population.csv`
2. **Countries with higher land utilization** will have **higher dairy consumption and production.**
    - Efficient land use can support dairy farming, leading to increased dairy production and consumption.
    - https://www.fao.org/faostat/en/#data/RL/visualize
    - `agr_land_perc`: percentage of land use
      - `./data_fixed/가설2 토지이용도/LM_df_Agricultural_Land.csv`
3. **Countries with higher dairy consumption** are more likely to be **economically affluent (or have better healthcare access)**.
   - Economically affluent countries have better access to digestive aids and are more health-conscious, leading to higher dairy consumption. Additionally, better healthcare systems can manage lactose intolerance effectively.
   - https://ourworldindata.org/data?topics=Health~Global+Health
   - `doc_and_gdp`: # of physicians and GDP per capita
     - `'./data_fixed/가설3 의료접근성/LM_df_medical_doctors_per_1000_people_vs_gdp_per_capita.csv'`
   - `health_exp`: health expenditure
     - `./data_fixed/가설3 의료접근성/wide_format_file_healthcare_expenditure_vs_GDP.csv'`
   - `life_exp`: life expectancy
     - `./data_fixed/가설3 의료접근성/wide_LM_df_life_expectancy.csv`
4. **Countries with an average annual temperature close to 10-20°C and lower monthly temperature variance** will have **higher dairy production**.
   - The optimal temperature range for dairy farming is between 10-20°C. Stable temperatures within this range support dairy industry development.
   - https://ourworldindata.org/grapher/monthly-average-surface-temperatures-by-year?country=~KOR
   - `mon_temper`: monthly average surface temperature by year
   - `./data_fixed/가설4 기온/LM_df_monthly_average_surface_temperatures_by_year.csv`


**EDA**
to examine overall tendency of data
![Fig 4. (a) PCA plot of Child dependency ratio](/LM_CM/image-2.png)
- It shows that the original groups, especially `Europe (western, southern, northern)` and `Africa (southern, eastern, western)` tend to cluster together along the first two principal components (PC1, PC2). 
  
![Fig 4. (b) PCA plot of ratio of population over 65 years old to all](/LM_CM/image-14.png)
- It shows that almost all original groups tend to cluster together along the first two principal components (PC1, PC2). 

![Fig 4. (c) Boxplot of percentage of agricultural land use](/LM_CM/image-15.png)
- Group which has the **highest** median value that indicates how much lands are used is **Former Soviet Republics**.
- Group which has the **lowest** median value is **Oceania**.

![Fig 4. (d) Boxplot of number of physicians per 1000 people, (e) GDP per capita](/LM_CM/image-25.png)
- Group which has the **highest** median value that indicates the number of physicians per 1000 people **(d)** is **Europe (western, eastern, northern)**.
- Group which has the **lowest** median value **(d)** is **Africa (southern, estern, western)**.
- Group which has the **highest** median value that indicates GDP per capita **(e)** is also **Europe (western, eastern, northern)**.
- Group which has the **lowest** median value **(e)** is also **Africa (southern, estern, western)**.


![Fig 4. (f) PCA plot of healthcare expenditure](/LM_CM/image-30.png)
- It shows that some original groups (`Europe (western, southern, northern)`, `Africa (southern, eastern, western)`) tend to cluster together along the first two principal components (PC1, PC2) slightly.

![Fig 4. (g) PCA plot of life expectancy](/LM_CM/image-27.png)
- It shows that some original groups (`Europe (western, southern, northern)`, `Africa (southern, eastern, western)`) tend to cluster together along the first two principal components (PC1, PC2) slightly.

![Fig 4. (h) Yearly temperature changes by country](/LM_CM/image-28.png)
- Grey box means a boundary of y-axis (temperature) between 10 to 20.

![Fig 4. (i) Boxplot of average temperature per by country](/LM_CM/image-29.png)
- Group which has the **highest** median value that indicates an average temperature per year by country after scaling it as a distance from 10-20 is **Africa (southern, estern, western)**.
- Group which has the **lowest** median value is **America**.

### Correlation and Random Forest
- `CM_cal_mean` Mean of CM Calories/Year
- `CM_fsq_mean` Mean of CM Food Supply Quantity
- `LMP` Lactose Malabsorption Prevalence
- `Genetic_distance` Genetic distance Fixation index from Dannish
  - Dannish: the lowest LMP
- `pop_ratio_mean` Mean of population between 0 and 15 years old divided by population between 15 and 65 years old
  - same as child dependency ratio
- `pop_above65_rate_mean` Mean of ratio of population above 65 years old
- `agr_land` Percentage of land use
- `doctors` Number of physicians per 1,000 people
- `gdp` GDP per capita
- `ht_exp` Mean of healthcare expenditure
- `lf_exp` Mean of life expectancy
- `year_temp` Distance from 10-20℃ to mean temperature per year
  - 10-20℃: optimal temperature range for dairy farming
    ```
    avg_year_temp$distance[which(avg_year_temp$distance < 10)] <- abs(avg_year_temp$distance[which(avg_year_temp$distance < 10)] - 10)
    avg_year_temp$distance[which(avg_year_temp$distance >= 10 & avg_year_temp$distance <= 20)] <- 0
    avg_year_temp$distance[which(avg_year_temp$distance > 20)] <- abs(avg_year_temp$distance[which(avg_year_temp$distance > 20)] - 20)
    ```

```
> outer_joined_df |> head()
      Country CM_cal_mean CM_fsq_mean  LMP Genetic_distance pop_ratio_mean pop_above65_rate_mean agr_land doctors       gdp    ht_exp   lf_exp year_temp
1 Afghanistan   711198.76    79.37846 0.82             0.02       87.71988              2.505274    12.19   0.254  1968.341 11.153000 41.40616  0.000000
2     Algeria  1682856.25   162.66538 0.62             0.03       87.04982              3.348474     3.27   1.732 11725.878  5.053182 55.04647  3.731889
3      Angola   112873.54    15.01231 0.94             0.17       89.48066              2.563203     2.99   0.211  6878.590  2.962727 41.64690  2.196001
4     Armenia   302628.71   408.22308 0.98             0.02       56.52653              5.653344    17.57   0.000     0.000  8.483636 65.87173  3.503857
5   Australia  1822606.85   290.68308 0.44             0.14       40.01963              9.778728     2.85   4.102 48651.734  8.909545 74.38675  2.143056
6     Austria    54105.58    24.09615 0.22             0.03       32.15997             14.360906    18.58   5.459 53817.305 10.105000 73.10150  3.241334
```
![Fig 5. (a) Global Distribution of Genetic Distance from Dannish](/LM_CM/image-13.png)
![Fig 5. (b) Correlation Plot, (c) Correlation plot with p-value](/LM_CM/image-31.png)
- When `LMP` is focused on, variables such as `Genetic_distance`, `pop_ratio_mean`, `CM_cal_mean_log`, `ht_exp`, `gdp`, `lf_exp`, `pop_above65_rate_mean`, and `doctors.` appears correlations with <=0.05 p-value.
- When `CM_cal_mean_log` is focused on, variables such as `year_temp`, `Genetic_distance`, `LMP`, `pop_ratio_mean`, and `agr_land` appears correlations with <= 0.05 p-value.
- When `CM_fsq_mean` is focused on, variables such as `year_temp`, `Genetic_distance`, `pop_ratio_mean`, `lf_exp`, `pop_above65_rate_mean`, and `doctors` appears correlations with <= 0.05 p-value.
![Fig 5. (d)-(i) Importance plot and tree plot](/LM_CM/image-32.png)
- In terms of `LMP`, `gdp`, `pop_above65_rate_mean`, `pop_ratio_mean`, `lf_exp`, `doctors`, `ht_exp`, `Genetic_distance`, and `CM_cal_mean_log` are important in descending order.
- In terms of `CM cal (mean log)`, `Genetic distance`, `LMP`, `agr_land`, `year_temp`, and `pop_ratio_mean` are important in descending order.
- In terms of `CM_fsq`, `pop_above65_rate_mean`, `lf_exp`, `doctors`, `pop_ratio_mean`, `Genetic_distance`, and `year_temp` are important in descending order.
- Variables that don't have a significant p-value in the correlation test are excluded for this step.

---
> **Conclusion**
> 1. The analysis shows that while LMP is a key determinant in Cattle Milk consumption, other socio-economic and environmental variables, such as GDP, child dependency ratio, elderly population, life expectancy, and etc., contribute significantly. 
> 2. LMP affects both the Calories per year and Food Supply Quantity from cattle milk consumption, while cattle milk consumption is also correlated with LMP. This demonstrates that they are part of a dynamic feedback loop influenced by health, demographic trends, economic conditions, and other factors.
> 3. Addressing LMP has critical implications for food security. Failure to mitigate LMP could lead to restrictions in cattle milk consumption and impact the global supply chain. Therefore, a deeper understanding of LMP and its effects is es sential for improving food security.
> 4. Policies targeting LMP should incorporate not only health-related interventions but also economic factors, demographic trends, climate change, and other underlying drivers. Coordinated, multi-sectoral responses are crucial for addressing LMP comprehensively.
> 

---
## Case Study: India

### India

India have about 65% of lactose intolerance, however it has the largest domestic consumption of fluid milk of 87MMT from 2023. The trend is everso growing since.

Northern Indians have lower lactose intolerance rate, averaging about 1/3 of population while southern indians have higher rate with 2/3 of population being affected.

![India Domestic Consumption from 2019-2023](./india_dairy_prod/domestic_consump.png)

And it is 3rd most larest producer of cow milk(Emphasis on cow milk), with 99.5MMT in 2023.

![Global Production 2023](./india_dairy_prod/global_prod_2023.png)

In 2023, India produced 99.5MMT of cow milk, and approximately 92MNT of Buffalo milk, with the percentile of buffalo milk in total milk production growing.

Despite the large production, India is not a major exporter of milk, or considering it's consumption, it is not a major importer either. The country is near self-sufficient in milk production.

### Fluid Milk Chain

Comparing the numbers, the domestic consumption of dairy milk is almost identical to production of cow milk, but accountign for all milk source, domestic consumption is approximately half of total consumption. The rest is processed into other dairy products including ghee, cheese, ...ect.

![India Milk Chain](./india_dairy_prod/Rplot.png)

### Secondary Products

India is also the largest producer in butter industry, accounting for 6750000MT in 2023

![Global Production 2023](./india_dairy_prod/global_butter_prod.png)

Per New Delpi India, in 2023, India alone produced total of 6750(1000 Metric Tonne) of Butter, but total export accounted for only 18(1000 MMT), which is less than 1% of total production. This highly implies that most of butter is domestically consumed.

### Actual population consumption

India, while it's domestic consumption and calory consumption is high, is not the highest in (fluid) milk per capita. In fact, per 2013 study it ranks at 99th in the world.

![Fluid Milk Consumption](./india_dairy_prod/milk_capita.png)

And study(Gallego Romero et al., 2012) Suggest that one of factor in lactase persistence, -13910C>T is distributed mostly in northern india, whereas southern india has higher lactose intolerance rate. The study suggest a shared origin for the mutation in India and Europe, and that due to sociological factor, there were positive selection toward lactose persistance in northern India.

![Meat Consumption per region](./india_dairy_prod/India_meat_consumption_rural.png)
![Dairy Consumption per region](./india_dairy_prod/India_dairy_consumption_rural.png)
![-13910C>T distribution](./india_dairy_prod/13910.png)

Which would suggest that people consumed milk in place for animal protein, hence lower positive selection towards eastern/southern regions in India.

Also, India’s substantial dairy consumption and production, particularly of cow and buffalo milk, are deeply integrated into culinary traditions and cultural practices. Even in areas with higher lactose intolerance rates, dairy remains a staple—whether consumed directly as fluid milk or processed into forms like butter, ghee, and cheese, which often reduce the lactose content through fermentation or clarifying processes.

### Conclusion

India's dairy consumption might seems odd considering i's high domestic consumption rate and LMP rate, it is result of historic dietary practice, sociological factors and regional genetic diversity. Given India's large butter industry and widespread vegeanism it is not that odd that India have such a high domestic consumption rate of dairy product.

---

## Case Study: South Korea

The rate of lactose intolerance in Korea is very high, but milk consumption is also high.

### consumption rate of Koreans by dairy products

According to a survey of the per capita consumption rate of Koreans by dairy products, the proportion of products high in lactose was high.

![consumption rate of Koreans by dairy products](/dairy_production_korea/유당별분류.png)

According to the paper, Koreans' awareness of lactose seems low, but there is a high perception that milk is good for health.

![recognition of lactose and milk](/dairy_production_korea/milklactose.png)

#### Conclusion of high CM

The pervasive perception that Korean milk is healthy leads to high dairy consumption rates regardless of lactose.

### Causes of high LMP

DNA of mummified brains and femurs of the tombs of the Joseon Dynasty was extracted and analyzed, indicating that people of the Joseon Dynasty also suffer from lactose intolerance.

![Lactose intolerance of Joseon](/dairy_production_korea/Joseon.png)

#### Conclusion of high LMP

Koreans also suffered from lactose intolerance, but the evolutionary selection pressure on milk was weak, so the lactase persistence phenotype was not transmitted to later people.

### Conclusion

By Conclusion of high CM and  Conclusion of high LMP, Koreans consume high milk despite high lactose intolerance.
