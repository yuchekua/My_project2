

getwd()
setwd('/Users/chriz_yu/Documents/MSU/ITM 818/Final Exam')

data_q1 = read.csv('LaptopSalesJanuary2008.csv')
library(dplyr)
library(ggplot2)
names(data_q1)

bar = data_q1 %>%
  select(RetailPrice, StorePostcode) %>%
  group_by(StorePostcode) %>% 
  summarise(avg_retail = mean(RetailPrice, na.rm=TRUE)) %>%
  arrange(desc(avg_retail))

ggplot(data = bar, aes(x=avg_retail, y= reorder(StorePostcode, avg_retail))) + 
  geom_bar(stat="identity")+
  coord_cartesian(xlim = c(475, 500)) + 
  labs(x="Average Price",
    y="Store Postcod")

#	 N17 6QA store has the highest average
#  W4 3PH store has the lowest average

################

ggplot(data_q1, 
       aes(x=RetailPrice, 
           y=reorder(StorePostcode,RetailPrice, FUN = median)
           )) + 
  geom_boxplot()

box2 = data_q1%>%
  filter(StorePostcode == "N17 6QA" | StorePostcode == "W4 3PH")

ggplot(box2, 
       aes(x=RetailPrice, 
           y=reorder(StorePostcode,RetailPrice, FUN = median)
       )) + 
  geom_boxplot()
# N17 6QA has higher Q1 and Q3 values than W4 3PH 

################
bar2 = data_q1 %>%
  select(RetailPrice, HDSize) %>%
  group_by(HDSize) %>% 
  summarise(avg_retail = mean(RetailPrice,na.rm=TRUE),
            count=n(),
            sdr=sd(RetailPrice,na.rm=TRUE)) %>%
  mutate(ser=qt(0.975,df=count-1)*sdr/sqrt(count)) %>%
  arrange(desc(avg_retail))

ggplot(data = bar2, aes(x=reorder(HDSize, avg_retail), 
                        y= avg_retail, 
                        fill=as.factor(HDSize))) + 
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=avg_retail-ser,ymax=avg_retail+ser), width=0.5)+
  coord_cartesian(ylim = c(400, 550)) + 
  labs(x="HDSize",
       y="Average Price",
       fill='HDSize') 
###########

data_q1$Date = as.Date(data_q1$Date, "%m/%d/%y")

line = data_q1 %>%
  select(Date, RetailPrice) %>%
  group_by(Date) %>% 
  summarise(avg_retail = mean(RetailPrice,na.rm=TRUE)) %>%
  arrange(desc(avg_retail))

ggplot(data = line, aes(x = Date, y = avg_retail))+
  geom_point(position=position_dodge(0.1),size=2,color="red")+
  geom_line()+
  labs(y="AverageRetailPrice")

