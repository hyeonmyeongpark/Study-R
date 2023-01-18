# 유통데이터 가져오기
dis = read.csv("C:/k_digital/source/r_source/project/[유통데이터 활용 경진대회] 상품정보 데이터.xlsx - 마스터상품.csv")
dis
View(dis)
str(dis)




# 상품분류명을 중복을 제거하고 가져오기
disvalue <- dis %>% select('상품분류명')
disvalue = distinct(disvalue) %>% arrange("상품분류명")
View(disvalue)
str(disvalue)



# 상품분류명을 | 기준으로 자르기

disvalue_split <- data.frame(do.call(rbind, 
                                     strsplit(disvalue$상품분류명, split = '|',
                                              fixed = T)))
View(disvalue_split)




# 각 대분류의 분포를 막대그래프로로 확인
qplot(data = disvalue_split, x = X1, geom = 'bar')


a1 <- disvalue_split %>% filter(X1 == "가공식품") %>% select(X1, X2)
a1

a1 %>% ggplot() + geom_bar(aes(X1, fill = X2), position = "dodge")

a2 <- disvalue_split %>% filter(X1 == "교육/문화용품") %>% select(X1, X2)
a2

a2 %>% ggplot() + geom_bar(aes(X1, fill = X2), position = "dodge")

a3 <- disvalue_split %>% filter(X1 == "기타상품") %>% select(X1, X2)
a3

a3 %>% ggplot() + geom_bar(aes(X1, fill = X2), position = "dodge")

a4 <- disvalue_split %>% filter(X1 == "디지털/가전") %>% select(X1, X2)
a4

a4 %>% ggplot() + geom_bar(aes(X1, fill = X2), position = "dodge")

a5 <- disvalue_split %>% filter(X1 == "신선식품") %>% select(X1, X2)
a5

a5 %>% ggplot() + geom_bar(aes(X1, fill = X2), position = "dodge")

a6 <- disvalue_split %>% filter(X1 == "의류/패션잡화") %>% select(X1, X2)
a6

a6 %>% ggplot() + geom_bar(aes(X1, fill = X2), position = "dodge")

a7 <- disvalue_split %>% filter(X1 == "의약품/의료기기") %>% select(X1, X2)
a7

a7 %>% ggplot() + geom_bar(aes(X1, fill = X2), position = "dodge")

a8 <- disvalue_split %>% filter(X1 == "인테리어") %>% select(X1, X2)
a8

a8 %>% ggplot() + geom_bar(aes(X1, fill = X2), position = "dodge")

a9 <- disvalue_split %>% filter(X1 == "일상용품") %>% select(X1, X2)
a9

a9 %>% ggplot() + geom_bar(aes(X1, fill = X2), position = "dodge")

a10 <- disvalue_split %>% filter(X1 == "전문스포츠/레저") %>% select(X1, X2)
a10

a10 %>% ggplot() + geom_bar(aes(X1, fill = X2), position = "dodge")



# 구매데이터 가져오기

Req = read.csv("C:/k_digital/source/r_source/project/req_data.csv")
View(Req)

str(Req)

Req_date <- Req %>% filter(age_group >= 40) %>% arrange(pay_date)
View(Req_date)

Req_old <- Req %>% filter(age_group >= 40) %>% arrange(age_group)
View(Req_old)

# 구매데이터 상세
install.packages('readxl')
library(readxl)
Req_info = read_excel("C:/k_digital/source/r_source/project/card_transaction_명세.xlsx")
View(Req_info)

# 결제데이터 상세
purchase_info = read_excel("C:/k_digital/source/r_source/project/purchase_transaction.xlsx", 
                           sheet = '테이블 명세')

View(purchase_info)

# 결제데이터
purchase_table = read_excel("C:/k_digital/source/r_source/project/purchase_transaction.xlsx", 
                            sheet = 'transaction')
View(purchase_table)



# 카테고리를 s1 변수에 담기
s1 = table(purchase_table$MasterCategoryFullName)
str(s1)
s1 = as.data.frame(s1)
s1$Var1 = as.vector(s1$Var1)
str(s1)
View(s1)


## 카테고리를 '->' 기준으로 자르기
s1_split <- data.frame(do.call(rbind, strsplit(s1$Var1, split = '->',
                                              fixed = T)))


View(s1_split)
s1_split <- s1_split %>% arrange(X1, X2, X3)



## 중위 카테고리 중복을 없애기
s1_split_mid <- s1_split %>% distinct(X2)
View(s1_split_mid)

s1_split_large <- s1_split %>% distinct(X1)
View(s1_split_large)
