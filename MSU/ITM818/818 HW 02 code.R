## HW 02

library(dplyr)
data=read.csv("/Users/chriz_yu/Documents/MSU/ITM 818/818 LAB 04/flightdata.csv",header=TRUE,sep=",")

summary(data)
mean(data$distance)

s1 = arrange(subset(data, distance > mean(data$distance)), desc(distance))
a1 = data %>%
    subset(distance > mean(data$distance)) %>%
    arrange(desc(distance))

head(s1, 10)
head(a1, 10)

s2 = arrange(subset(data, !(is.na(depdelay))), desc(depdelay))
a2 = data %>%
    subset(!(is.na(depdelay))) %>%
    arrange(desc(depdelay))
head(s2, 10)
head(a2, 10)

data$date = as.Date(data$date, format="%Y-%m-%d")
s3 = subset(data, date >= '2016-04-06' & date <= '2016-04-12')
a3 = data %>%
    subset(date >= '2016-04-06' & date <= '2016-04-12')
head(s3, 10)
head(a3, 10)

s4 = arrange(summarise(group_by(data, carrier),cancelled_rate = sum(cancelled)/n()), desc(cancelled_rate))

a4 = data %>%
    group_by(carrier) %>%
    summarise(cancelled_rate = sum(cancelled)/n()) %>%
    arrange(desc(cancelled_rate))

summary(data)

posi = subset(data, depdelay > 0)
x = median(subset(data, depdelay > 3)$depdelay, na.rm = TRUE)
x

s5 = mutate(data, Delay = case_when(
          depdelay <= 0 | is.na(depdelay) ~ "NoDelay",
          depdelay > 0 & depdelay <= median(subset(data, depdelay > 0)$depdelay, na.rm = TRUE) ~ "LowDelay",
          TRUE ~ "HighDelay"))
head(s5, 10)


