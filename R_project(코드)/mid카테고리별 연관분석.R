# 결제데이터를 이용한 연관분석
Req_cafe <- Req %>% filter(large_category == '카페/간식') %>%
  arrange(pay_date, large_category, mid_category)
table(Req_cafe$mid_category)

Req_day1 <- Req %>% filter(pay_date == '2021-01-01') %>% 
  arrange(person_)
View(Req_day1)




martTable <- table(Req %>% 
                     filter(large_category == '생활/마트') %>% 
                     select(pay_place_name))

martTable <- sort(martTable)
tail(martTable, 50)




cafeTable <- table(Req %>% 
                     filter(large_category == '카페/간식') %>% 
                     select(pay_place_name))

cafeTable <- sort(cafeTable)
tail(cafeTable, 50)



date <- data.frame(do.call(rbind, 
                           strsplit(Req$pay_date, split = '-', fixed = T)))
View(date)

date <- data.frame(date = paste(date$X1, date$X2, sep = '-'))

rm(Req_cafe)

Req_pay <- data.frame(Req$person_, date, Req$amount, Req$pay_place_name,
                       Req$large_category, Req$mid_category)

names(Req_pay) <- c('person', 'date', 'amount', 'place', 'large', 'mid')


################# 테마파크 ###################

Req_park <- Req_pay %>% filter(mid == '테마파크') %>%
  group_by(date, place) %>% summarise(n = length(place)) %>% 
  arrange(n)

sort(table(Req_pay %>% filter(mid == '테마파크') %>% 
             select(place)))

Req_park %>%
  filter(place == '에버랜드' | place == '서울랜드' | 
         place == '롯데월드' | place == '삼봉개발경주지점' |
         place == '경주월드')%>%
  ggplot() +
  geom_point(aes(date, n, group = place, col = place)) + 
  geom_line(aes(date, n, group = place, col = place))



################## 취미 ###################

Req_leisure <- Req_pay %>%
  filter(mid == '여가시설') %>% group_by(date, place) %>%
  summarise(n = length(place)) %>% arrange(n)

View(Req_leisure)

sort(table(Req_pay %>% filter(mid == '여가시설') %>% 
             select(place)))


Req_leisure %>%
  filter(place == 'PC방' | place == '노래방' | 
           place == '여가시설' | place == '쓰리팝PC방' |
           place == '아이비스PC방' | place == '놀숲만화카페' |
           place == '맥스피드PC방' | place == '아이센스리그PC방' |
           place == '비디오'| place == '탑플레이스')%>%
  ggplot() +
  geom_point(aes(date, n, group = place, col = place)) + 
  geom_line(aes(date, n, group = place, col = place))


################## 패션 #####################
Req_fasion <- Req_pay %>%
  filter(mid == '패션') %>% group_by(date, place) %>%
  summarise(n = length(place)) %>% arrange(n)

sort(table(Req_pay %>% filter(mid == '패션') %>% 
             select(place)))

Req_fasion %>%
  filter(place == '엘칸토' | place == '98도씨' | 
           place == '이랜드후아유' | place == '타미힐피거' |
           place == '여성크로커다일 ' | place == '쌤소나이트' |
           place == '에이티브' | place == '룩옵티컬' |
           place == '뉴에라'| place == '무극안경' |
           place == '샤넬부띠끄' | place == '탠디')%>%
  ggplot() +
  geom_point(aes(date, n, group = place, col = place)) + 
  geom_line(aes(date, n, group = place, col = place))


################## 뷰티제품 ##################
Req_beauty <- Req_pay %>%
  filter(mid == '뷰티제품') %>% group_by(date, place) %>%
  summarise(n = length(place)) %>% arrange(n) 

sort(table(Req_pay %>% filter(mid == '뷰티제품') %>% 
             select(place)))

Req_beauty %>%
  filter(place == '이니스프리' | 
           place == '아모레퍼시픽' | place == '롯데쇼핑롭스' |
           place == '뉴스킨코리아' | place == '아리따움' |
           place == '네이처리퍼블릭' | place == '코리아나화장품' |
           place == '버드뷰'| place == '시드물' |
           place == '뷰티플렉스' | place == '러쉬' |
           place == '랄라블라' | place == '토니모리') %>%
  ggplot() +
  labs(title = '뷰티제품')+
  geom_point(aes(date, n, group = place, col = place)) + 
  geom_line(aes(date, n, group = place, col = place))


################# 가전/가구 ##################
Req_home <- Req_pay %>%
  filter(mid == '가전/가구') %>% group_by(date, place) %>%
  summarise(n = length(place)) %>% arrange(n) 


View(Req_home)
sort(table(Req_pay %>% filter(mid == '가전/가구') %>% 
             select(place)))


Req_home %>%
  filter(place == '삼성전자' | 
           place == 'LG전자' | place == '전자제품' |
           place == '이케아' | place == '코웨이' |
           place == 'SK매직' | place == '버킷플레이스' |
           place == '쿠쿠전자'| place == '가전/가구' |
           place == '하이마트' | place == '한샘' |
           place == '렌탈' | place == '인테리어') %>%
  ggplot() +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  labs(title = '가전/가구') +
  geom_point(aes(date, n, group = place, col = place)) + 
  geom_line(aes(date, n, group = place, col = place))




################# 스포츠 ##################

Req_sports <- Req_pay %>%
    filter(mid == '스포츠') %>% group_by(date, place) %>%
    summarise(n = length(place)) %>% arrange(n) 
  
sort(table(Req_pay %>% filter(mid == '스포츠') %>% 
               select(place)))
  
Req_sports %>%
    filter(
             place == '골프' | place == '바이크' |
             place == '당구' | place == '볼링' |
             place == '등산' | place == '낚시' |
             place == '캠핑'| place == '체육관' |
             place == '골프존마켓' | place == '캠핑장' |
             place == '수영' | place == '삼천리자전거') %>%
    ggplot() +
    geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
    labs(title = '스포츠') +
    geom_point(aes(date, n, group = place, col = place)) + 
    geom_line(aes(date, n, group = place, col = place))
  





########## 연관분석 ##########
View(Req_day1)



length(table(Req_day1$mid_category))

mid <- distinct(data.frame(Req_day1$mid_category))
mid

dd = data.frame()
for(i in 1:3){
  for(j in 2:4){
  dd <- rbind(dd, i*j)
  }
}






Req_user <- Req %>% 
  filter(mid_category == '도서' | mid_category == '여가시설' |
        mid_category == '영화' | mid_category == '백화점' |
        mid_category == '스포츠의류'|  mid_category == '아울렛/몰' |
        mid_category == '패션'|  mid_category == '뷰티제품' |
        mid_category == '헤어샵'|  mid_category == '가전/가구' |
        mid_category == '기프트샵/꽃'|  mid_category == '마트' |
        mid_category == '반려동물'|  mid_category == '생활서비스' |
        mid_category == '육아'|  mid_category == '일상용품' |
        mid_category == '편의점'|  mid_category == '레저' |
        mid_category == '스포츠'|  mid_category == '테마파크' |
        mid_category == '숙박'|  mid_category == '여행' |
        mid_category == '항공'|  mid_category == '의료기기' |
        mid_category == '보조식품'|  mid_category == '도넛/아이스크림' |
        mid_category == '디저트/떡'|  mid_category == '베이커리' |
        mid_category == '샌드위치/핫도그'|  mid_category == '차/주스/빙수' |
        mid_category == '커피') %>%
  select(person_, age_group, person_gender, pay_date, 
        pay_place_name, large_category, mid_category) %>%
  arrange(person_, mid_category)








