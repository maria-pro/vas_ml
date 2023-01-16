library(tidyverse)
library(lubridate)
library(janitor)

data<-read_csv("data/Merged_1.csv", col_types=cols(
  ID=col_character()
))%>%clean_names()


data<-data%>%
  mutate(
    sleep_end_time=na_if(sleep_end_time, "1"),
    date= parse_date_time(date, orders=c('dmy', 'ymd', 'mdy')),
    sleep_start_time=parse_date_time(sleep_start_time, orders=c('dmy_HM', 'ymd_HM', 'mdy_HMS')),
    sleep_end_time=parse_date_time(sleep_end_time, orders=c('dmy_HM', 'ymd_HM', 'mdy_HMS')),
    date=as.Date(coalesce(date, sleep_start_time))
  )

data%>%write_csv("data/data_dates_clean.csv")

day<-data%>%select(
  id, date, calories_burned, steps, distance, floors, 
  minutes_sedentary, minutes_lightly_active, minutes_fairly_active, minutes_very_active,
  activity_calories
)%>%distinct()

sleep<-data%>%select(
  id, sleep_start_time, sleep_end_time, 
  minutes_asleep, minutes_awake, number_of_awakenings,
  time_in_bed, minutes_rem_sleep, minutes_light_sleep, minutes_deep_sleep
)%>%
  mutate(
    sleep_day=as.Date(sleep_start_time)
  )

#remove empty rows 
sleep_clean<-sleep%>%drop_na()%>%distinct()

#join by date
data_clean<-full_join(day, sleep_clean, by=c("id"="id", "date"="sleep_day"))

data_clean<-data_clean%>%
  group_by(id, date)%>%mutate(
    number_sleeps=n()
  )%>%ungroup()

#data_clean2<-data_clean%>%distinct()

data_clean%>%write_csv("data/data_dates_realigned.csv")

#removed remaining dupes manually

data_clean<-read_csv("data/data_dates_realigned_mp.csv")

data_clean<-data_clean%>%
  group_by(id, date)%>%mutate(
    number_sleeps=n()
  )%>%ungroup()


max(data_clean$number_sleeps)
#data_dates<-read_csv("data/participat_date.csv")%>%drop_na(id)


#data_dates<-data_dates%>%mutate(
#  date=na_if(date, "-"),
#  date= #dmy(date, tz="Australia/Melbourne")
#    as.Date(date, "%d/%m/%Y")

#)


#tidy<- data_dates

