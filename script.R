library(tidyverse)
library(lubridate)

data<-read_csv("data/Merged_1.csv")


dates<-data%>%select(ID, Date)%>%
  mutate(date= as.Date(dmy(Date)))%>%
  group_by(ID)%>%
  mutate(
    start_date=min(date,na.rm=TRUE),
    end_date=max(date,na.rm=TRUE),
    duration=end_date-start_date,
    id=ID
  )%>%select(id, start_date, date, end_date, duration)%>%
  select(-ID, )
  ungroup()

dates_transform<-tibble(
  id=dates$ID, start_date=dates$start_date,
  day_names=list(paste0("day_", rep(1:30, each=1))),
  day=list(rep(1:30, each=1)))
)

dates_transform<-dates_transform%>%unnest(c(day_names, day))%>%
  mutate(
    date_4_days=start_date+day
  )%>%
  select(id, start_date, day_names, date_4_days)
  

test<-full_join(dates_transform, dates)%>%distinct(id, day_names)


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
