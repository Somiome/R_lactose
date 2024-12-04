library(dplyr)


# CSV 파일 불러오기
df_LM <- read.csv("./data/LM.csv") #기준이 되는 데이터

#조인할 데이터
df_0_15_population <- read.csv("./data/data_pre_processing/인구수/API_SP.POP.0014.TO_DS2_en_csv_v2_24206.csv", header =TRUE) 
df_15_65_population <- read.csv("./data/data_pre_processing/인구수/API_SP.POP.1564.TO_DS2_en_csv_v2_12838.csv", header =TRUE)
df_65_population <- read.csv("./data/data_pre_processing/인구수/API_SP.POP.65UP.TO.ZS_DS2_en_csv_v2_20080.csv", header =TRUE)
df_temperature <- read.csv("./data/data_pre_processing/인구수/monthly-average-surface-temperatures-by-year.csv", header =TRUE)
df_life_expectancy <- read.csv("./data/data_pre_processing/인구수/life-expectancy.csv", header =TRUE)
df_doctor <- read.csv("./data/data_pre_processing/인구수/medical-doctors-per-1000-people-vs-gdp-per-capita.csv", header =TRUE)
df_healthcare <- read.csv("./data/data_pre_processing/인구수/total-healthcare-expenditure-gdp.csv", header =TRUE)
df_soybean <- read.csv("./data/data_pre_processing/인구수/soybean-production-by-country-2024.csv", header =TRUE)
df_landuse <- read.csv("./data/data_pre_processing/인구수/FAOSTAT_data_en_11-6-2024.csv", header =TRUE)




# 국가별 병합
merged_df1 <- df_LM %>% left_join(df_0_15_population, by = "Country")
merged_df2 <- df_LM %>% left_join(df_15_65_population, by = "Country")
merged_df3 <- df_LM %>% left_join(df_65_population, by = "Country")
merged_df4 <- df_LM %>% left_join(df_0_15_population, by = "Country")
merged_df5 <- df_LM %>% left_join(df_0_15_population, by = "Country")
merged_df6 <- df_LM %>% left_join(df_0_15_population, by = "Country")
merged_df7 <- df_LM %>% left_join(df_0_15_population, by = "Country")
merged_df8 <- df_LM %>% left_join(df_0_15_population, by = "Country")



# 필요 없는 열 선택적으로 제거하고, 결측값을 0으로 대체
LM_df_soybean_product <- merged_df %>%
  mutate_all(~replace(., is.na(.), 0))

# 전처리 결과 확인 및 저장
print(LM_df_soybean_product)
write.csv(LM_df_soybean_product, "./data/data_pre_processing/LM_df_soybean_product.csv", row.names = FALSE)

