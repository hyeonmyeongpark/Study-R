View(Req)
카페/간식
Req = read.csv("C:/k_digital/source/r_source/project/req_data.csv")
View(Req)

# 월별 구매 건수추이
## 날짜를 '-'단위로 자르기
date <- data.frame(do.call(rbind, strsplit(Req$pay_date, split = '-', fixed =T)))
View(date)

## 연도와 월을 붙이기
date <- data.frame(date = paste(date$X1, date$X2, sep = '-'))
View(date)

## 필요한 열의 정보만 가져와서 연결하기기
Req_1 <- data.frame(Req$age_group,
                    date, 
                    Req$person_,
                    Req$pay_place_name,
                    Req$large_category,
                    Req$mid_category)
names(Req_1) <- c('age_group', 'date', 'person', 'pay_place', 'large', 'mid')


# 3040 데이터셋
Req_3040 <- Req_1 %>%
  filter(age_group >= 30 & age_group < 50 & date >= '2021-01' & date <='2021-12') %>%
  arrange(person,large, mid)
View(Req_3040)



# 3040 전체 사람 수 -> 17,758
person_3040 <- distinct(Req_3040 %>% select(person))
View(person_3040)

# 카페/간식 이용 사람 -> 17278명
Req_3040_cafe <- distinct(Req_3040 %>% filter(large=='카페/간식') %>% select(person))
View(Req_3040_cafe)

# 카페/간식 이용 안한 사람 -> 480명
Req_3040_cafe_x <- anti_join(person_3040, Req_3040_cafe, by='person')
View(Req_3040_cafe_x)



#-------------------------------------------------------------------------------
# 카페/간식을 이용한 사람 데이터셋 -> 17278명
person_cafe <- Req_3040 %>% filter(person %in% Req_3040_cafe$person)
View(person_cafe)

#-------------------------------------------------------------------------------

# 카페/간식을 이용한 사람 중 도서를 이용한 사람 -> 10,043
person_cafe_bk <- distinct(person_cafe %>% filter(mid=='도서') %>% select(person))
View(person_cafe_bk)
bk_cnt <- c(10043/17278*100)



# 카페/간식을 이용한 사람 중 도서를 이용하지 않은 사람
bk_x_cnt <- c(7235/17278*100)


# 도서 비율 데이터셋
bk <- c(bk_cnt, bk_x_cnt)
bk_name <- c('사용', '미사용')
names(bk) <- bk_name
bk <- data.frame(bk)
bk

# 도서 비율 그래프
# 41.87406 %, 58.12594 %
ggplot(bk, aes(x=bk_name, y=bk, fill=bk_name)) + 
  geom_bar(stat = 'identity') +
  labs(fill='도서 사용') +
  scale_fill_brewer(palette = "BuPu") +
  theme_void()






######################### 상관분석 ###########################




### 3040 카페/간식 이용 건수 ###

Req_3040_cafe <- Req %>%
  filter(age_group >= 30 & age_group <50 & large_category == '카페/간식')%>% 
  group_by(pay_date) %>%
  arrange(pay_date) %>%
  summarise(n = length(pay_date))


Req_3040_cafe <- Req_3040_cafe %>%
  filter(pay_date >= '2021-01-01' & pay_date < '2022-01-01')


View(Req_3040_cafe)





### 3040 카페/간식 - 도서 이용 건수 ###

Req_3040_cafe_book <- Req %>%
  filter(age_group >= 30 & age_group <50 & 
           (person_ %in% person_cafe$person) & mid_category == '도서') %>%
  group_by(pay_date) %>% 
  summarise(n = length(pay_date))

Req_3040_cafe_book <- Req_3040_cafe_book %>%
  filter(pay_date >= '2021-01-01' & pay_date < '2022-01-01')

View(Req_3040_cafe_book)




### 3040 카페/간식 - 주유소 이용 건수 ###

Req_3040_cafe_gas <- Req %>%
  filter(age_group >= 30 & age_group <50 & 
           (person_ %in% person_cafe$person) & mid_category == '주유소') %>%
  group_by(pay_date) %>%
  summarise(n = length(pay_date))

Req_3040_cafe_gas <- Req_3040_cafe_gas %>%
  filter(pay_date >= '2021-01-01' & pay_date < '2022-01-01')

View(Req_3040_cafe_gas)




### 3040 카페/간식 - 패션 이용 건수 ###

Req_3040_cafe_fasion <- Req %>%
  filter(age_group >= 30 & age_group <50 & 
           (person_ %in% person_cafe$person) & mid_category == '패션') %>%
  group_by(pay_date) %>%
  summarise(n = length(pay_date))

Req_3040_cafe_fasion <- Req_3040_cafe_fasion %>%
  filter(pay_date >= '2021-01-01' & pay_date < '2022-01-01')


View(Req_3040_cafe_fasion)





### 3040 카페/간식 - 생활서비스 이용 건수 ###

Req_3040_cafe_life <- Req %>%
  filter(age_group >= 30 & age_group <50 & 
           (person_ %in% person_cafe$person) & mid_category == '생활서비스') %>%
  group_by(pay_date) %>%
  summarise(n = length(pay_date))

Req_3040_cafe_life <- Req_3040_cafe_life %>%
  filter(pay_date >= '2021-01-01' & pay_date < '2022-01-01')

View(Req_3040_cafe_life)




### 3040 카페/간식 -  육아 이용 건수 ###

Req_3040_cafe_baby <- Req %>%
  filter(age_group >= 30 & age_group <50 & 
           (person_ %in% person_cafe$person) & mid_category == '육아') %>%
  group_by(pay_date) %>%
  summarise(n = length(pay_date))

Req_3040_cafe_baby <- Req_3040_cafe_baby %>%
  filter(pay_date >= '2021-01-01' & pay_date < '2022-01-01')

View(Req_3040_cafe_baby)




### 3040 카페/간식 -  반려동물 이용 건수 ###

Req_3040_cafe_animal <- Req %>%
  filter(age_group >= 30 & age_group <50 & 
           (person_ %in% person_cafe$person) & mid_category == '반려동물') %>%
  group_by(pay_date) %>%
  summarise(n = length(pay_date))

Req_3040_cafe_animal <- Req_3040_cafe_animal %>%
  filter(pay_date >= '2021-01-01' & pay_date < '2022-01-01')

View(Req_3040_cafe_animal)







### 3040 카페 그래프 ###

Req_3040_cafe %>% ggplot() + geom_point(aes(pay_date, n))


### 3040 카페-도서 그래프 ###
Req_3040_cafe_book %>% ggplot() + geom_point(aes(pay_date, n))


### 3040 카페-주유 그래프 ###
Req_3040_cafe_gas %>% ggplot() + geom_point(aes(pay_date, n))


### 3040 카페-패션 그래프 ###
Req_3040_cafe_fasion %>% ggplot() + geom_point(aes(pay_date, n))


### 3040 카페-생활서비스 그래프 ###
Req_3040_cafe_life %>% ggplot() + geom_point(aes(pay_date, n))


### 3040 카페-육아 그래프 ###
Req_3040_cafe_baby %>% ggplot() + geom_point(aes(pay_date, n))


### 3040 카페-반려동물 그래프 ###
Req_3040_cafe_animal %>% ggplot() + geom_point(aes(pay_date, n))







### 상관관계 분석 ###


# 카페/간식 , 카페/간식-도서
cor(Req_3040_cafe$n, Req_3040_cafe_book$n)

cor.test(Req_3040_cafe$n, Req_3040_cafe_book$n)

# p-value = 1.807e-15  =>  귀무가설 기각, 대립가설 채택 => 상관관계가 있다. 





# 카페/간식 , 카페/간식-주유소
cor(Req_3040_cafe$n, Req_3040_cafe_gas$n)

cor.test(Req_3040_cafe$n, Req_3040_cafe_gas$n)

# p-value = 2.2e-16  =>  귀무가설 기각, 대립가설 채택 => 상관관계가 있다. 




# 카페/간식 , 카페/간식-패션
cor(Req_3040_cafe$n, Req_3040_cafe_fasion$n)

cor.test(Req_3040_cafe$n, Req_3040_cafe_fasion$n)

# p-value = 3.41e-08  =>  귀무가설 기각, 대립가설 채택 => 상관관계가 있다. 




# 카페/간식 , 카페/간식-생활서비스
cor(Req_3040_cafe$n, Req_3040_cafe_life$n)

cor.test(Req_3040_cafe$n, Req_3040_cafe_life$n)

# p-value = 0.0004015  =>  귀무가설 기각, 대립가설 채택 => 상관관계가 있다. 




# 카페/간식 , 카페/간식-육아
cor(Req_3040_cafe$n, Req_3040_cafe_baby$n)

cor.test(Req_3040_cafe$n, Req_3040_cafe_baby$n)

# p-value = 0.8199  =>  귀무가설 채택 => 상관관계가 없다. 




# 카페/간식 , 카페/간식-반려동물
cor(Req_3040_cafe$n, Req_3040_cafe_animal$n)

cor.test(Req_3040_cafe$n, Req_3040_cafe_animal$n)

# p-value = 0.01531  =>  귀무가설 기각, 대립가설 채택 => 상관관계가 있다. 




# 상관관계 시각화

library(corrplot)

Req_3040_corr <- data.frame(Req_3040_cafe, Req_3040_cafe_book$n, Req_3040_cafe_gas$n,
                            Req_3040_cafe_fasion$n, Req_3040_cafe_life$n, 
                            Req_3040_cafe_baby$n, Req_3040_cafe_animal$n)


names(Req_3040_corr) <- c('date', 'cafe', 'cafe_book', 'cafe_gas', 'cafe_fasion', 'cafe_life',
                          'cafe_baby', 'cafe_animal')


View(Req_3040_corr)

X <- cor(Req_3040_corr[, 2:8])

corrplot(X, type = 'upper', method = 'circle', tl.pos = "d")
corrplot(X, add=TRUE, method="num", type = "lower",
         diag = FALSE, tl.pos = "n", cl.pos = "n")
