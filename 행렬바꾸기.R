install.packages("tidyr")

library(tidyr)
library(dplyr)


df <- read.csv("C:\\Users\\82104\\OneDrive\\바탕 화면\\R프로그래밍\\한국 우유 소비 비율\\OECD.WISE.INE,DSD_TIME_USE@DF_TIME_USE,1.0+.._T.csv")

colnames(df)
head(df)

# 'years'를 열로 변환
df_wide <- df %>%
  pivot_wider(names_from = Measure,   # 'year' 컬럼을 컬럼명으로 사용
              values_from = OBS_VALUE)  # 각 값들을 'value' 컬럼에서 가져와 채움
head(df_wide)

write.csv(df_wide, "C:\\Users\\82104\\OneDrive\\바탕 화면\\R프로그래밍\\한국 우유 소비 비율\\OOECD.WISE.INE,DSD_TIME_USE@DF_TIME_USE,1.0+.._T.csv", row.names = FALSE)


Period.life.expectancy.at.birth.-.Sex:all.-.Age:0
