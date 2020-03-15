library(tidyverse)
library(lubridate)
library(RColorBrewer)
time_series_df <- read_csv('csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv')

confirmed_ts <- time_series_df %>% 
  filter(`Country/Region`==('Japan')) %>% 
  select(-Lat, -Long,-`Province/State`) %>% 
  pivot_longer(c(-`Country/Region`), names_to = "日付", values_to = "累計感染者数") 

confirmed_ts$日付 <- mdy(confirmed_ts$日付)
confirmed_ts$日付 <- as.Date(confirmed_ts$日付)
confirmed_ts <- confirmed_ts %>% 
  #filter(日付=='2020-02-06') %>% 
  mutate(累計感染者数 = case_when(日付=='2020-02-06'~ 25, 日付!='2020-02-06'~累計感染者数))

confirmed_ts %>% 
  ggplot(aes(日付, 累計感染者数))+
  ggtitle('新型コロナウイルス感染者数の推移（日本）')+
  scale_x_date(date_breaks = '5 days', date_labels = '%m/%d') +
  theme(axis.text.x = element_text(angle=30, hjust=1)) +
  geom_bar(stat = 'identity', fill='#fff494')+
  #scale_color_manual(values = c("#f0e68c"))+
  theme_dark(base_family = "HiraKakuPro-W3")

confirmed_ts %>% 
  ggplot(aes(日付, 累計感染者数))+
  #scale_x_date(date_breaks = '2 days', date_labels = '%y-%m-%d') +
  theme(axis.text.x = element_text(angle=30, hjust=1)) +
  geom_point()+
  geom_smooth(method = 'loess')+
  theme_gray(base_family = "HiraKakuPro-W3")