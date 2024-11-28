library(ggplot2)
library(tidyverse)

data <- read.csv("C:/Users/82104/OneDrive/바탕 화면/R프로그래밍/한국 우유 소비 비율/일반개황.csv", 
                 fileEncoding = "EUC-KR")

# 데이터 전치
data_transposed <- t(data)

#데이터프레임으로 변환
data_transposed <- as.data.frame(data_transposed)

head(data_transposed)

# 첫 번째 행을 열 이름으로 설정
colnames(data_transposed) <- data_transposed[1, ]

# 첫 번째 행은 더 이상 필요하므로 제거
data_transposed <- data_transposed[-1, ]

# 'Year' 열을 다시 데이터에 추가하기
data_transposed <- cbind(Year = rownames(data_transposed), data_transposed)

head(data_transposed)



# 필요한 열만 추출
data_selected <- data_transposed %>%
  select("Year", "백색시유 소비량", "가공시유 소비량", "액상발효유 소비량", "호상발효유 소비량", "자연치즈 소비량", "가공치즈 소비량")

colnames(data_transposed)

# 연도를 기준으로 데이터를 긴 형식(long format)으로 변환
data_long <- data_selected %>%
  gather(key = "Product", value = "Consumption", -Year)

head(data_long)


# Year 열에서 'X랑 년' 제거하고, 숫자형으로 변환
data_long$Year <- gsub("X|년", "", data_long$Year)  # X 제거
data_long$Year <- as.numeric(data_long$Year)  # 숫자형으로 변환
data_long$Consumption <- as.numeric(data_long$Consumption)

# 변환 후 확인
str(data_long)


# 연도별로 총 소비량을 합산
data_total <- data_long %>%
  group_by(Year, Product) %>%
  summarize(Total_Consumption = sum(Consumption, na.rm = TRUE)) %>%
  ungroup()

data_total$Year <- factor(data_total$Year)

# 총 소비량을 이용한 축적 막대그래프 그리기
ggplot(data_total, aes(x = Year, y = Total_Consumption, fill = Product)) +
  geom_bar(stat = "identity", position = "stack") +  # stat = "identity"로 y값을 그대로 사용하고, position = "stack"으로 누적 막대 그래프를 만듬
  labs(title = "Total Dairy Product Consumption by Year",
       x = "Year",
       y = "Total Consumption",
       fill = "Product Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # x축 텍스트 기울이기



#유당함량이 높은 시유 그룹과 유당함량이 적은 발효유 및 자연치즈 그룹룹 그리고 
#제품에 따라 상이한 가공치즈끼리 묶은 그래프



library(dplyr)

# 새로운 그룹 생성
data_long1 <- data_long %>%
  mutate(Group = case_when(
    Product %in% c("백색시유 소비량", "가공시유 소비량") ~ "시유",
    Product %in% c("액상발효유 소비량", "호상발효유 소비량", "자연치즈 소비량") ~ "발효유 및 자연치즈",
    Product == "가공치즈 소비량" ~ "가공치즈",
    TRUE ~ "기타" # 예외 처리용
  ))


# 그룹별 연도별 소비량 합산
data_grouped <- data_long1 %>%
  group_by(Year, Group) %>%
  summarize(Total_Consumption = sum(Consumption, na.rm = TRUE)) %>%
  ungroup()

# 결과 확인
head(data_grouped)



# 그룹별 축적 막대그래프 그리기
ggplot(data_grouped, aes(x = Year, y = Total_Consumption, fill = Group)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Grouped Dairy Product Consumption by Year",
       x = "Year",
       y = "Total Consumption",
       fill = "Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # x축 텍스트 기울이기













