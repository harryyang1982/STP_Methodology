library(tidyverse)
library(readxl)
# BUK
edustat <- read_excel("datasets/edustat_부울경.xlsx")

edustat_seoul <- edustat %>% 
  filter(시도 == "서울" & 대계열 == "공학계열")

edustat_gn <- edustat %>% 
  filter(시도 == "경남" & 대계열 == "공학계열")
edustat_bsn <- edustat %>% 
  filter(시도 == "부산" & 대계열 == "공학계열")
edustat_usn <- edustat %>% 
  filter(시도 == "울산" & 대계열 == "공학계열")

edustat_sum_seoul <- edustat_seoul %>% 
  group_by(year, 중계열, 과정구분) %>% 
  summarize(졸업자_남자_비중 = mean(졸업자중남자, na.rm = TRUE),
            졸업자_여자_비중 = mean(졸업자중여자, na.rm = TRUE),
            취업자_남자_비중 = mean(취업자중남자, na.rm = TRUE),
            취업자_여자_비중 = mean(취업자중여자, na.rm = TRUE),
            전체_취업률 = mean(취업률_계, na.rm = TRUE),
            남자_취업률 = mean(남자취업률, na.rm = TRUE),
            여자_취업률 = mean(여자취업률, na.rm = TRUE))

edustat_sum_gn <- edustat_gn %>% 
  group_by(year, 중계열, 과정구분) %>% 
  summarize(졸업자_남자_비중 = mean(졸업자중남자, na.rm = TRUE),
            졸업자_여자_비중 = mean(졸업자중여자, na.rm = TRUE),
            취업자_남자_비중 = mean(취업자중남자, na.rm = TRUE),
            취업자_여자_비중 = mean(취업자중여자, na.rm = TRUE),
            전체_취업률 = mean(취업률_계, na.rm = TRUE),
            남자_취업률 = mean(남자취업률, na.rm = TRUE),
            여자_취업률 = mean(여자취업률, na.rm = TRUE))

edustat_sum_bsn <- edustat_bsn %>% 
  group_by(year, 중계열, 과정구분) %>% 
  summarize(졸업자_남자_비중 = mean(졸업자중남자, na.rm = TRUE),
            졸업자_여자_비중 = mean(졸업자중여자, na.rm = TRUE),
            취업자_남자_비중 = mean(취업자중남자, na.rm = TRUE),
            취업자_여자_비중 = mean(취업자중여자, na.rm = TRUE),
            전체_취업률 = mean(취업률_계, na.rm = TRUE),
            남자_취업률 = mean(남자취업률, na.rm = TRUE),
            여자_취업률 = mean(여자취업률, na.rm = TRUE))

edustat_sum_usn <- edustat_usn %>% 
  group_by(year, 중계열, 과정구분) %>% 
  summarize(졸업자_남자_비중 = mean(졸업자중남자, na.rm = TRUE),
            졸업자_여자_비중 = mean(졸업자중여자, na.rm = TRUE),
            취업자_남자_비중 = mean(취업자중남자, na.rm = TRUE),
            취업자_여자_비중 = mean(취업자중여자, na.rm = TRUE),
            전체_취업률 = mean(취업률_계, na.rm = TRUE),
            남자_취업률 = mean(남자취업률, na.rm = TRUE),
            여자_취업률 = mean(여자취업률, na.rm = TRUE))

# Industrial Talents

files_list <- list.files(path = "datasets", pattern = "*.xlsx")[-1]
df_list <- map(str_c("datasets/", files_list), read_excel, skip = 1)
df <- bind_rows(df_list)

industry <- df %>% 
  fill(시점) %>% 
  mutate(지역 = c(rep("부산", 316), rep("경남", 320), rep("서울", 302), rep("울산", 294))) %>% 
  mutate_at(3:44, as.numeric)


industry_subset <- industry %>% 
  mutate(여성비율 = `학사(여)` / 학사) %>% 
  rename(산업별 = `산업별(1)`) %>% 
  filter(산업별 %in% c("기계", "디스플레이", "반도체", "섬유", "자동차", "전자", "철강", "화학", "바이오ㆍ헬스"))

industry_subset %>% 
  group_by(지역, 산업별) %>% 
  summarize(여성비율 = mean(여성비율))

edustat_sum_gn %>% filter(과정구분 == "대학과정") %>% 
  ggplot(aes(x = year, y = 여자_취업률)) +
  geom_line(aes(color = 중계열, group = 중계열))

