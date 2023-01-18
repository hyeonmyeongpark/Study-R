# 1. 결측지, 이상치 제거
purchase_table$Sales_Unit <- as.numeric(purchase_table$Sales_Unit)

purchase_1 <-  purchase_table %>% filter(MasterCategoryFullName != 'Unknown' & Price > 100 )
View(purchase_1)


# 2. 월별 구매 건수추이
## 날짜를 '-'단위로 자르기
class(purchase_1$Purchase_Date)
purchase_1$Purchase_Date <- as.character(purchase_1$Purchase_Date)
date <- data.frame(do.call(rbind, strsplit(purchase_1$Purchase_Date, split = '-', fixed =T)))
View(date)

## 연도와 월을 붙이기
date <- data.frame(date = paste(date$X1, date$X2, sep = '-'))
View(date)



# 3. Tot_price 변수 생성
Tot_price <- data.frame(Tot_price = purchase_1$Sales_Unit * purchase_1$Price)
View(Tot_price)



# 4. 카테고리 분류
# 카테고리를 s1 변수에 담기
s1 = data.frame(category = purchase_1$MasterCategoryFullName)
View(s1)
class(s1$category)

## 카테고리를 '->' 기준으로 자르기
s1_split <- data.frame(do.call(rbind, strsplit(s1$category,split = '->',fixed = T)))
View(s1_split)


## 5. 데이터셋 생성
## 필요한 열의 정보만 가져와서 연결하기기
purchase <- data.frame(date,
                       purchase_1$Sales_Unit,
                       purchase_1$Price,
                       Tot_price,
                       s1_split)
names(purchase) <- c('date', 'sales_Unit', 'price', 'tot', 'large', 'mid', 'small')
purchase <- purchase %>% arrange(date, large, mid, small)
View(purchase)


purchase_large <- purchase %>% group_by(date, large) %>% summarise(n = length(large)) %>%
  arrange(date)

View(purchase_large)

purchase_large %>% ggplot() + 
  geom_point(aes(date, n, group = large, col = large)) +
  geom_line(aes(date, n, group = large, col = large))

rm(Req_male_50)

Req_day <- data.frame(Req$pay_day_of_week, Req$large_category)
names(Req_day) <- c('day', 'large')
Req_day <- Req_day %>% group_by(day, large) %>% summarise(n = length(large))
View(Req_day)


Req_day %>% ggplot() +
  geom_text(aes(x= day, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(day, n, group = large, col = large)) +
  geom_line(aes(day, n, group = large, col = large)) + 
  scale_y_continuous(expand = expansion(mult = 0), limits = c(500000, 1050000))


Req_day %>% ggplot() +
  geom_text(aes(x= day, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(day, n, group = large, col = large)) +
  geom_line(aes(day, n, group = large, col = large)) + 
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 400000))

