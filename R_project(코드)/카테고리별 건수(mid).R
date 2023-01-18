# 교통/차량 
Req_transport <- Req_male_50 %>% filter(large == '교통/차량') %>%
  arrange(mid)

Req_transport <- Req_transport %>% group_by(date, mid) %>%
  summarise(n = length(mid))

View(Req_transport)


# 기타
Req_transport %>% filter(mid == '기타') %>% ggplot() +
  labs(title = '기타') +
  geom_col(width = 0.8, aes(date, n), fill = 'blue')
  

Req_online <- Req_male_50 %>% filter(large == '온라인쇼핑') %>%
  arrange(mid)

Req_online <- Req_online %>% group_by(date, mid) %>% summarise(n = length(mid)) %>%
  arrange()
View(Req_online)

Req_online %>% ggplot() + 
  geom_col(aes(mid, n, fill = mid))


# 연령대별

table(Req_male_50 %>% filter(large == '생활/마트')%>% select(mid))

