# 필요한 패키지 장착
library(dplyr)
library(ggplot2)
library(showtext)
library(readxl)


# 구매데이터 불러오기
Req = read.csv("C:/k_digital/source/r_source/project/req_data.csv")
View(Req)



# 50대 남성과 여성 구매 테이블

## 50대 남성
Req_male <- Req %>% filter(person_gender == 1 & age_group >= 50 & age_group < 60)
View(Req_male)

## 50대 여성
Req_female <- Req %>% filter(person_gender == 0 & age_group >= 50 & age_group < 60)
View(Req_female)




# 50대 남성의 카테고리의 월별 구매 건수추이

## 날짜를 '-'단위로 자르기
date <- data.frame(do.call(rbind, strsplit(Req_male$pay_date, split = '-', fixed =T)))
View(date)

## 연도와 월을 붙이기
date <- data.frame(date = paste(date$X1, date$X2, sep = '-'))
View(date)

## 필요한 열의 정보만 가져와서 연결하기기
Req_male_50 <- data.frame(Req_male$age_group, date, Req_male$amount, 
                Req_male$pay_place_name, Req_male$large_category, Req_male$mid_category)
names(Req_male_50) <- c('age_group', 'date', 'amount', 'pay_place', 'large', 'mid')

Req_male_50 <- Req_male_50 %>% arrange(age_group, date, large)

View(Req_male_50)

# 3개월 단위 카테고리별 구매 건수 현황
Req_male_50 %>% filter(date >= '2021-01' & date <= '2021-03') %>%
  ggplot() + geom_bar(aes(date, fill = large),color = 'purple', position = 'dodge')

Req_male_50 %>% filter(date >= '2021-04' & date <= '2021-06') %>%
  ggplot() + geom_bar(aes(date, fill = large),color = 'purple', position = 'dodge')

Req_male_50 %>% filter(date >= '2021-07' & date <= '2021-09') %>%
  ggplot() + geom_bar(aes(date, fill = large),color = 'purple', position = 'dodge')

Req_male_50 %>% filter(date >= '2021-10' & date <= '2021-12') %>%
  ggplot() + geom_bar(aes(date, fill = large),color = 'purple', position = 'dodge')

Req_male_50 %>% filter(date >= '2022-01' & date <= '2022-03') %>%
  ggplot() + geom_bar(aes(date, fill = large),color = 'purple', position = 'dodge')

Req_male_50 %>% filter(date >= '2021-04' & date <= '2021-06') %>%
  ggplot() + geom_bar(aes(date, fill = large),color = 'purple', position = 'dodge')



# 카테고리별 구매 건수 변화 추이
Req_male_50_cate <- Req_male_50 %>% group_by(date, large) %>%
  summarise(n = length(large))
View(Req_male_50_cate)
class(Req_male_50_cate$date)



Req_male_50_cate %>% filter(large == '경조사/회비')%>% ggplot() + 
  labs(title = '경조사/회비') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 150)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")


Req_male_50_cate %>% filter(large == '교통/차량')%>% ggplot() + 
  labs(title = '교통/차량') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 7000)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")


Req_male_50_cate %>% filter(large == '금융/보험')%>% ggplot() + 
  labs(title = '금융/보험') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 1700)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")


Req_male_50_cate %>% filter(large == '문화/예술')%>% ggplot() + 
  labs(title = '문화/예술') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 800)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")


Req_male_50_cate %>% filter(large == '백화점/패션')%>% ggplot() + 
  labs(title = '백화점/패션') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 1600)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")


Req_male_50_cate %>% filter(large == '뷰티/미용')%>% ggplot() + 
  labs(title = '뷰티/미용') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 500)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")


Req_male_50_cate %>% filter(large == '생활/마트')%>% ggplot() + 
  labs(title = '생활/마트') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 16000)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")


Req_male_50_cate %>% filter(large == '술/유흥')%>% ggplot() + 
  labs(title = '술/유흥') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 400)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")


Req_male_50_cate %>% filter(large == '스포츠/레저')%>% ggplot() + 
  labs(title = '스포츠/레저') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 700)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")


Req_male_50_cate %>% filter(large == '식사')%>% ggplot() + 
  labs(title = '식사') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 10000)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")


Req_male_50_cate %>% filter(large == '여행/숙박')%>% ggplot() + 
  labs(title = '여행/숙박') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 500)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")


Req_male_50_cate %>% filter(large == '온라인쇼핑')%>% ggplot() + 
  labs(title = '온라인/쇼핑') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 7000)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")


Req_male_50_cate %>% filter(large == '의료/건강')%>% ggplot() + 
  labs(title = '의료/건강') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 4000)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")


Req_male_50_cate %>% filter(large == '주거/통신')%>% ggplot() + 
  labs(title = '주거/통신') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 2000)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")


Req_male_50_cate %>% filter(large == '카페/간식')%>% ggplot() +
  labs(title = '카페/간식') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#dcb7b7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -2.0) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 4500)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")



Req_male_50_cate %>% filter(large == '학습/교육')%>% ggplot() + 
  labs(title = '학습/교육') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#dcb7b7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -2.0) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 350)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")

font_families()




Req_male_50_cate %>% ggplot() + 
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n, group = large, col = large)) +
  geom_line(aes(x = date, y = n, group = large, col = large), size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(2000, 17000))





