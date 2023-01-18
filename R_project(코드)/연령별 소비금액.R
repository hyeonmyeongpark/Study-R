# 최근소비 상승하강추이(연령대별), 최근 소비 추세 파악

## dplyr 패키지 적용 
library(dplyr)
library(ggplot2)
## 나이, 날짜, 금액을 선택해 해로운 데이터셋을 만듬
resent_consu <- Req %>% select(age_group, pay_date, amount)
View(resent_consu)

## 날짜를 '-'을 기준으로 자르고 년, 월, 일을 컬럼명으로 부여
new <- resent_consu %>% 
  data.frame(do.call(rbind, strsplit(resent_consu$pay_date, split = '-', fixed = T)))
View(new)

# 2행(pay_date)를 제외하고 가져오기
new <- new[, -2]
View(new)

# 모든 행에 이름 붙이기
names(new) <- c('age_group', 'amount','year', 'month', 'day')
View(new)

# 2행과 5행(amount, day)제외하고 가져오기
new <- new[, -c(2, 5)]
View(new)

# cbind를 이용해서 amount붙이기
new <- cbind(new, resent_consu$amount)
names(new) <- c('age_group', 'year', 'month', 'amount')
View(new)


# 연령, 년도, 달로 그룹을 지어 연령별 지출 총합 데이터셋
consu_flow <- new %>% group_by(age_group, year, month) %>%
  summarize(Sum_amount = sum(amount))
View(consu_flow)
date <- data.frame(date = (paste(consu_flow$year, consu_flow$month, sep = '-')))
View(date)
consu_age <- cbind(consu_flow$age_group, date)
consu_flow <- cbind(consu_age, consu_flow$Sum_amount)
View(consu_flow)

names(consu_flow) <- c('age_group', 'date', 'Sum_amount')
View(consu_flow)


# consu_flow 데이터셋을 이용하여 연령대별 지출흐름을 시각화
consu_flow %>% filter(age_group == 23) %>%  ggplot() + 
  geom_col(mapping = aes(date, Sum_amount)) 

consu_flow %>% filter(30 <= age_group & age_group < 40 ) %>%  ggplot() + 
  geom_point(mapping = aes(date, Sum_amount, col = age_group))

consu_flow %>% filter(40 <= age_group & age_group < 50 ) %>%  ggplot() + 
  geom_point(mapping = aes(date, Sum_amount, col = age_group))

consu_flow %>% filter(50 <= age_group & age_group < 60 ) %>%  ggplot() + 
  geom_point(mapping = aes(date, Sum_amount, col = age_group))

consu_flow %>% filter(60 <= age_group) %>%  ggplot() + 
  geom_point(mapping = aes(date, Sum_amount, col = age_group))



qplot(data = consu_flow[consu_flow$age_group == 23,], x = date, y = Sum_amount, geom = 'point')

plot(consu_flow$date, consu_flow$Sum_amount, pch = 20, cex= 0.5, col = 'red')



