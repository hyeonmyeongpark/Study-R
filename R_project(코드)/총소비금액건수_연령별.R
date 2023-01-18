#연령별 소비금액

# 20대
## 총 소비금액 168544899000원
amount_20 <- sum(Req %>% filter(age_group >= 20 & age_group < 30) %>% select(amount))
amount_20 

# 온라인쇼핑 소비금액 34336612000원
online_amount_20 <- sum(Req %>% filter(age_group >= 20 & age_group < 30 & large_category=='온라인쇼핑') %>% select(amount))
online_amount_20 

## 총 소비금액 중 온라인 결제금액 비율 20.37238 %
percent_20 <- online_amount_20/amount_20*100
percent_20 


# 30대 
## 총 소비금액 313645443000원
amount_30 <- sum(Req %>% filter(age_group >= 30 & age_group < 40) %>% select(amount))
amount_30 

# 온라인쇼핑 소비금액 66697292000원
online_amount_30 <- sum(Req %>% filter(age_group >= 30 & age_group < 40 & large_category=='온라인쇼핑') %>% select(amount))
online_amount_30 

## 총 소비금액 중 온라인 결제금액 건수 비율 21.26519 %
percent_30 <- online_amount_30/amount_30*100
percent_30 


# 40대 
## 총 소비금액 413855904000원
amount_40 <- sum(Req %>% filter(age_group >= 40 & age_group < 50) %>% select(amount))
amount_40 
# 온라인쇼핑 소비금액 71703951000원
online_amount_40 <- sum(Req %>% filter(age_group >= 40 & age_group < 50 & large_category=='온라인쇼핑') %>% select(amount))
online_amount_40 

## 총 소비금액 중 온라인 결제금액 비율 17.32583 %
percent_40 <- online_amount_40/amount_40*100
percent_40 


# 50대 
## 총 소비금액 89959436000원
amount_50 <- sum(Req %>% filter(age_group >= 50 & age_group < 60) %>% select(amount))
amount_50

# 온라인쇼핑 소비금액 9823405000원
online_amount_50 <- sum(Req %>% filter(age_group >= 50 & age_group < 60 & large_category=='온라인쇼핑') %>% select(amount))
online_amount_50

## 총 소비금액 중 온라인 결제금액 비율 10.91982 %
percent_50 <- online_amount_50/amount_50*100
percent_50 


# 60대 
## 총 소비금액 21829614000원
amount_60 <- sum(Req %>% filter(age_group >= 60 & age_group < 70) %>% select(amount))
amount_60 

# 온라인쇼핑 소비금액 2173236000원
online_amount_60 <- sum(Req %>% filter(age_group >= 60 & age_group < 70 & large_category=='온라인쇼핑') %>% select(amount))
online_amount_60 

## 총 소비금액 중 온라인 결제금액 비율 9.955449 %
percent_60 <- online_amount_60/amount_60*100
percent_60 


#---------------------------------------------------------------------------------

age_all <- c('20대','30대','40대','50대','60대')
age_all <- factor(age_all)


# 연령대별 총소비금액 데이터프레임 생성
amount_age_all <- c(amount_20, amount_30, amount_40, amount_50, amount_60)
names(amount_age_all) <- age_all # 행 변경
amount_df <- data.frame(amount_age_all)
amount_df

# 연령대별 총소비금액 막대그래프 생성
barplot(amount_age_all)

ggplot(n_df, aes(x=age_all, y=amount_age_all, fill=age_all)) + 
  geom_bar(stat = 'identity') +
  labs(title='연령대별 총 소비 금액',
       x='연령대',
       y='금액',
       fill='연령')


# 연령대별 온라인쇼핑 소비금액 데이터프레임 생성
online_amount <- c(online_amount_20, online_amount_30, online_amount_40, online_amount_50, online_amount_60)
names(online_amount) <- age_all # 행 변경
online_amount_df <- data.frame(online_amount)
online_amount_df

# 연령대별 온라인쇼핑 소비금액 막대그래프 생성
barplot(online_amount)

ggplot(n_df, aes(x=age_all, y=online_amount, fill=age_all)) + 
  geom_bar(stat = 'identity') +
  labs(title='연령대별 온라인 결제 금액',
       x='연령대',
       y='금액',
       fill='연령')

# 연령대별 총 소비금액 중 온라인 결제 금액 비율 데이터프레임 생성
amount_percent <- c(percent_20, percent_30, percent_40, percent_50, percent_60)
names(amount_percent) <- age_all # 행 변경
amount_percent_df <- data.frame(amount_percent)
amount_percent_df

# 연령대별 총 소비금액 중 온라인 결제 금액 비율 막대그래프 생성
barplot(amount_percent)

ggplot(n_df, aes(x=age_all, y=amount_percent, fill=age_all)) + 
  geom_bar(stat = 'identity') +
  labs(title='연령별 총 소비금액 중 온라인 결제 금액 비율',
       x='연령대',
       y='비율',
       fill='연령')

#---------------------------------------------------------------------------


# 연령별 소비 건수 ## amount -값 제외해야 하나?
# 20대
## 총 소비 건수 5483596
all_n_20 <- nrow(Req %>% filter(age_group >= 20 & age_group < 30) %>% select(amount)) 

## 온라인 결제 건수 986309 
online_n_20 <- nrow(Req %>% filter(age_group >= 20 & age_group < 30 & large_category=='온라인쇼핑') %>% select(amount)) 

## 총 소비건수 중 온라인 결제 건수 비율 17.98654 %
percent_n_20 <- online_n_20/all_n_20*100
percent_20 


# 30대
## 총 소비 건수 8084964
all_n_30 <- nrow(Req %>% filter(age_group >= 30 & age_group < 40) %>% select(amount))

## 온라인 결제 건수 1669934
online_n_30 <- nrow(Req %>% filter(age_group >= 30 & age_group < 40 & large_category=='온라인쇼핑') %>% select(amount))

## 총 소비건수 중 온라인 결제 건수 비율 20.65481 %
percent_n_30 <- online_n_30/all_n_30*100
percent_n_30


# 40대
## 총 소비 건수 9360801
all_n_40 <- nrow(Req %>% filter(age_group >= 40 & age_group < 50) %>% select(amount))

## 온라인 결제 건수 1631039
online_n_40 <- nrow(Req %>% filter(age_group >= 40 & age_group < 50 & large_category=='온라인쇼핑') %>% select(amount))

## 총 소비건수 중 온라인 결제 건수 비율 17.42414 %
percent_n_40 <- online_n_40/all_n_40*100
percent_n_40 


# 50대
## 총 소비 건수 # 1501621
all_n_50 <- nrow(Req %>% filter(age_group >= 50 & age_group < 60) %>% select(amount))

## 온라인 결제 건수 # 208791
online_n_50 <- nrow(Req %>% filter(age_group >= 50 & age_group < 60 & large_category=='온라인쇼핑') %>% select(amount))

## 총 소비건수 중 온라인 결제 건수 비율 13.90437 %
percent_n_50 <- online_n_50/all_n_50*100
percent_n_50


# 60대
## 총 소비 건수 381437
all_n_60 <- nrow(Req %>% filter(age_group >= 60 & age_group < 70) %>% select(amount))

## 온라인 결제 건수 50620
online_n_60 <- nrow(Req %>% filter(age_group >= 60 & age_group < 70 & large_category=='온라인쇼핑') %>% select(amount))

## 총 소비건수 중 온라인 결제 건수 비율 13.27087
percent_n_60 <- online_n_60/all_n_60*100
percent_n_60 



#----------------------------------------------------------------------------------

# 연령대별 총소비금액 데이터프레임 생성
# amount_age_all <- c(amount_20, amount_30, amount_40, amount_50, amount_60)
# amount_df <- data.frame(age_all, amount_age_all)
# names(amount_df) <- c('연령대','총금액') # 컬럼명 변경
# amount_df



# 연령대별 총 소비건수 데이터프레임 생성
n_age_all <- c(all_n_20, all_n_30, all_n_40, all_n_50, all_n_60)
names(n_age_all) <- age_all # 행 변경
n_df <- data.frame(n_age_all)
n_df



# 연령대별 총 소비건수 막대그래프 생성
barplot(n_age_all)

ggplot(n_df, aes(x=age_all, y=n_age_all, fill=age_all)) + 
  geom_bar(stat = 'identity') +
  labs(title='연령대별 총 소비 건수',
       x='연령대',
       y='금액',
       fill='연령')

ggplot(n_df, aes(x=age_all, y=n_age_all, fill=n_age_online)) + 
  geom_bar(stat = 'identity',
           position='dodge') +
  labs(title='연령대별 총 소비 건수',
       x='연령대',
       y='금액',
       fill='연령')


# 연령대별 온라인 소비건수 데이터프레임 생성
n_age_online <- c(online_n_20, online_n_30, online_n_40, online_n_50, online_n_60)
names(n_age_online) <- age_all # 행 변경
n_online_df <- data.frame(n_age_online)
n_online_df

# 연령대별 온라인 소비건수 막대그래프 생성
barplot(n_age_online)

ggplot(n_df, aes(x=age_all, y=n_age_online, fill=age_all)) + 
  geom_bar(stat = 'identity') +
  labs(title='연령대별 온라인 결제 건수',
       x='연령대',
       y='금액',
       fill='연령')

# 연령대별 총 소비건수 중 온라인 결제 건수 비율 데이터프레임 생성
online_percent <- c(percent_n_20, percent_n_30, percent_n_40, percent_n_50, percent_n_60)
names(online_percent) <- age_all # 행 변경
online_percent_df <- data.frame(online_percent)
online_percent_df


# 연령대별 총 소비건수 중 온라인 결제 건수 비율 막대그래프 생성
barplot(online_percent)

ggplot(n_df, aes(x=age_all, y=online_percent, fill=age_all)) + 
  geom_bar(stat = 'identity') +
  labs(title='연령대별 총 소비건수 중 온라인 결제 건수 비율',
       x='연령대',
       y='비율',
       fill='연령')