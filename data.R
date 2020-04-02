library(tidyverse)
library(lubridate)
#time_series_confermed <- read_csv('csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv')
#time_series_deaths <- read_csv('csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv')
#time_series_recovered <- read_csv('csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv')

time_series_confermed <- read_csv('time_series_covid19_confirmed_global.csv')
time_series_deaths <- read_csv('time_series_covid19_deaths_global.csv')
time_series_recovered <- read_csv('time_series_covid19_recovered_global.csv')


confirmed <- time_series_confermed %>% 
  filter(`Country/Region`==('Japan')) %>% 
  select(-Lat, -Long,-`Province/State`) %>% 
  pivot_longer(c(-`Country/Region`), names_to = "日付", values_to = "累計患者数") 

confirmed$日付 <- mdy(confirmed$日付)
confirmed$日付 <- as.Date(confirmed$日付)

#confirmed <- confirmed %>% 
#  mutate(累計患者数 = case_when(日付=='2020-01-23'~ 2, 日付!='2020-02-06'~累計患者数)) %>% 
#  mutate(累計患者数 = case_when(日付=='2020-02-06'~ 25, 日付!='2020-02-06'~累計患者数))

confirmed %>% 
  ggplot(aes(日付, 累計患者数))+
  ggtitle('新型コロナウイルス患者数の推移（日本）')+
  scale_x_date(date_breaks = '10 days', date_labels = '%m/%d') +
  theme(axis.text.x = element_text(angle=30, hjust=1)) +
  geom_bar(stat = 'identity', fill='#fff494')+
  theme_dark(base_family = "HiraKakuPro-W3")



recovered <- time_series_recovered %>% 
  filter(`Country/Region`==('Japan')) %>% 
  select(-Lat, -Long,-`Province/State`) %>% 
  pivot_longer(c(-`Country/Region`), names_to = "日付", values_to = "累計回復者数")
recovered$日付 <- mdy(recovered$日付)
recovered$日付 <- as.Date(recovered$日付)

deaths <- time_series_deaths %>% 
  filter(`Country/Region`==('Japan')) %>% 
  select(-Lat, -Long,-`Province/State`) %>% 
  pivot_longer(c(-`Country/Region`), names_to = "日付", values_to = "累計死亡者数") 
deaths$日付 <- mdy(deaths$日付)
deaths$日付 <- as.Date(deaths$日付)



japan_covid <- confirmed %>% 
  left_join(select(recovered, "日付", "累計回復者数"), by = "日付") %>% 
  left_join(select(deaths, "日付","累計死亡者数"), by = "日付") 

japan_covid <- japan_covid %>%
  pivot_longer(c(-`Country/Region`,-日付), names_to = "カテゴリ", values_to = "人数")

japan_covid %>% 
  ggplot(aes(x=日付,y=人数,fill=factor(カテゴリ)))+
  ggtitle('新型コロナウィルス患者数の推移（日本）')+
  geom_bar(stat = 'identity', position = position_identity())+
  scale_x_date(date_breaks = '5 days', date_labels = '%m/%d') +
  scale_fill_manual(values = c(累計患者数 = "#fff494", 累計回復者数 = "#00fa9a",累計死亡者数="#ff6347"),
                    limits=c("累計患者数","累計回復者数","累計死亡者数"))+
  theme_dark(base_family = "HiraKakuPro-W3")+
  theme(legend.position = c(0.2,0.8),
        axis.text.x = element_text(angle=30, hjust=1),
        plot.title = element_text(face = "bold")) +
  guides(fill=guide_legend(title = NULL))



