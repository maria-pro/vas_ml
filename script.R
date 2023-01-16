library(tidyverse)
library(lubridate)

data<-read_csv("data/Merged_1.csv")

data_clean<-read_csv("data/data_dates_realigned_mp.csv")

data_clean1<-data_clean%>%
  mutate(date= dmy(date))%>%
  group_by(id)%>%
  mutate(
    start_date=min(date,na.rm=TRUE),
    end_date=max(date,na.rm=TRUE),
    duration=end_date-start_date
  )%>%
  ungroup()%>%
  group_by(id, date)%>%
  mutate(
    number_sleeps=n()
  )%>%
  ungroup()

data_clean1%>%write_csv("data/data_dates_realigned_mp2.csv")

  dates_transform<-tibble(
  id=data_clean1$id, start_date=data_clean1$start_date,
  day_names=list(paste0("day_", rep(1:5, each=1))),
  day=list(rep(0:4, each=1)))

data_clean1<-data_clean1%>%select(-start_date)


dates_transform<-dates_transform%>%unnest(c(day_names, day))%>%
  mutate(
    date_4_days=start_date+day
  )%>%
  select(id, start_date, day_names, date_4_days)%>%distinct()
  

test<-right_join(dates_transform, data_clean1, by=c("id"="id", "date_4_days"="date"))

data_5days<-test%>%filter(!is.na(day_names))

data_5days<-data_5days%>%rename(date=date_4_days)
  
data_5days%>% write_csv("data/data_dates_realigned_5daywindow.csv")

data_5days_transf<-data_5days%>%pivot_wider(names_from=day_names, values_from=date_4_days, values_fn = c())

  
as.Date(data_5days_transf$day_1, format = "%Y%m%d")
  
arrange(id, date_4_days)%>%
  rename(date=date_4_days)
  
  
))%>%distinct(id, day_names)


  group_by(id, start_date)%>%
#  nest()%>%
  mutate(
    date_4_days=list((seq(from=(start_date), by=1, length.out=30)))
  )

,
date_4_days=list(seq(from=as.Date("2022-06-28"), by=1, length.out=30))




date_nest<-date%>%
  group_by(ID, start_date)%>%
  nest()%>%
  mutate(
    day_names=list(paste0("day_", rep(1:30, each=1)))
  )

dates%>%count(ID,sort=TRUE)


unique(dates$ID)

min(dates$date, na.rm = TRUE)
