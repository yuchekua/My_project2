
library(ggplot2)
library(dplyr)
library(gridExtra)
library(sqldf)
library(boot)

getwd()
setwd('/Users/chriz_yu/Documents/MSU/ITM 818/Final Exam')
data_q3 = read.csv("kickedCars.csv")
names(data_q3)
################
# a
q1a = data_q3 %>%
  select(Make,Nationality,Size) %>%
  filter(Nationality == 'AMERICAN') 

q1b = q1a  %>%
  group_by(Make, Size) %>%
  summarise(count = n()) %>%
  mutate(prec = round((count / sum(count))*100,2))

q1c = q1b %>% 
  filter(Size == 'VAN') %>%
  arrange(desc(prec))
q1c

ggplot(data=q1a)+geom_bar(mapping=aes(x=Make,fill=Size),position="fill")

##################
# b

ggplot(data_q3, aes(x=VehOdo, y=Nationality,fill = Nationality)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.2, fill="white")

ggplot(data_q3, aes(x=VehOdo, fill=Nationality)) +
  geom_density(alpha=0.2)

##################
# c
data_q3$PurchDate = as.Date(data_q3$PurchDate, "%m/%d/%y")
data_q3$Year = format(data_q3$PurchDate, format = "%Y")

q3a = data_q3 %>%
  select(IsBadBuy,Make,Year) %>%
  filter(IsBadBuy == 1) %>%
  group_by(Make,Year) %>%
  summarise(count = n())

ggplot(q3a)+
  geom_bar(mapping=aes(x=as.factor(Year),y=count,fill = Make), 
    stat="identity", position="dodge") +
    labs(x="Year")
# CHEVROLET DODGE FORD

q3b = data_q3 %>%
  select(IsBadBuy,Make,VehYear) %>%
  filter(IsBadBuy == 1) %>%
  group_by(Make,VehYear) %>%
  summarise(count = n())

ggplot(q3b)+
  geom_bar(mapping=aes(x=count,y=Make,fill = as.factor(VehYear)), 
           stat="identity", position="dodge") +
  labs(x="Make",
       fill = "VehYear")

##################
# d

ggplot(data_q3, aes(x=as.factor(VehYear), 
                    y=MMRCurrentRetailAveragePrice,
                    fill = as.factor(VehYear))) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.2, fill="white")+
  labs(x="Vehicle Year",
       fill = "Vehicle Year")

ggplot(data_q3, aes(x=as.factor(VehYear), 
                    y=MMRCurrentRetailCleanPrice,
                    fill = as.factor(VehYear))) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.2, fill="white")+
  labs(x="Vehicle Year",
       fill = "Vehicle Year")

##################
# e

q5a = q3 %>%
  filter(count >= 500)

q5b = data_q3 %>%
  select(IsBadBuy,Make,Year,VehYear) %>%
  filter(IsBadBuy == 1 & (Make == "CHEVROLET"| Make == "DODGE"|Make == "FORD" |Make == 'CHRYSLER')) %>%
  group_by(Make,VehYear,IBB = as.factor(IsBadBuy)) %>%
  summarise(n = n())

q5c = q5b %>%
  group_by(VehYear) %>%
  summarise(total_n = sum(n)) 

q5d = merge(x=q5b,y=q5c,by="VehYear")
q5d$prop = round((q5d$n/q5d$total_n)*100,2)
q5d

ggplot(data=q5d, aes(x=as.factor(VehYear), y=prop, group=Make, color = Make)) +
  geom_line()+
  geom_point()+
  labs( x = "Vehicle Year")

##################
# e
q6a = data_q3 %>%
  select(TopThreeAmericanName, VehOdo, MMRAcquisitionAuctionAveragePrice,IsBadBuy) %>%
  filter(TopThreeAmericanName != "OTHER") 

g_buy = q6a %>%
  filter(IsBadBuy == 0)

b_buy = q6a %>%
  filter(IsBadBuy == 1)

ggplot(data=q6a, aes(x=VehOdo,
                     y=MMRAcquisitionAuctionAveragePrice, 
                     color=as.factor(IsBadBuy)))+
  geom_point(size = 0.2)+
  geom_smooth(method=lm, fullrange=TRUE) +
  labs(color = "IsBadBuy")

##################
# f

subset.data = data_q3 %>%
  select(IsBadBuy,VehicleAge,TopThreeAmericanName,WheelType,VehOdo,Size,
         VehBCost,IsOnlineSale,WarrantyCost)%>%
  mutate(VehicleAgeSquared = VehicleAge*VehicleAge)

set.seed(1)
train.index=sample(c(TRUE,FALSE),prob=c(0.7,0.3),nrow(subset.data),replace=TRUE)
train=subset.data[train.index,] 
valid=subset.data[!train.index,]

logit1=glm(IsBadBuy~VehicleAge+
             VehicleAgeSquared+
             as.factor(TopThreeAmericanName)+ 
             as.factor(WheelType)+
             VehOdo+
             Size+
             VehBCost+
             as.factor(IsOnlineSale)+
             WarrantyCost,
           family=binomial(link="logit"),
           data=train)
summary(logit1)

valid$prob=predict(logit1,valid,type="response")
valid$pred=ifelse(valid$prob>0.5,1,0)

confusion=table(actual=valid$IsBadBuy,predicted=valid$pred)
confusion
TP=confusion[2,2]
FN=confusion[2,1]
FP=confusion[1,2]
TN=confusion[1,1]
accuracy=(TP+TN)/nrow(valid)
precision=TP/(TP+FP)
recall=TP/(TP+FN)
error=1-accuracy
c(accuracy,precision,recall,error)

##################

logit2=glm(IsBadBuy~VehicleAge+
             VehicleAgeSquared+
             as.factor(TopThreeAmericanName)+ 
             as.factor(WheelType)+
             VehOdo+
             Size+
             VehBCost+
             as.factor(IsOnlineSale)+
             WarrantyCost,
           family=binomial(link="logit"),
           data=subset.data)
summary(logit2)

cost.accuracy=function(r, pi = 0) mean(abs(r-pi)<0.5)

cost.precision=function(r, pi = 0) { 
  TP2=sum((pi>0.5)&(r==1))
  FP2=sum((pi>0.5)&(r==0))
  return(TP2/(TP2+FP2))
}
cost.recall=function(r, pi = 0) { 
  TP2=sum((pi>0.5)&(r==1))
  FN2=sum((pi<=0.5)&(r==1))
  return(TP2/(TP2+FN2))
}

cv.accuracy=cv.glm(subset.data,logit2,cost=cost.accuracy,K=5)
cv.precision=cv.glm(subset.data,logit2,cost=cost.precision,K=5)
cv.recall=cv.glm(subset.data,logit2,cost=cost.recall,K=5)
c(cv.accuracy$delta[1],cv.precision$delta[1], cv.recall$delta[1])


