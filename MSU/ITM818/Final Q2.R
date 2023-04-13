

library(ggplot2)
library(dplyr)
library(gridExtra)
library(leaps)
getwd()
setwd('/Users/chriz_yu/Documents/MSU/ITM 818/Final Exam')

data_q2 = read.csv("Tayko.csv")
data_q2 = data_q2 %>%
  select(Freq, 
         last_update_days_ago, 
         Web.order,
         Gender.male,
         Address_is_res,
         US,
         Spending)
  
names(data_q2)


###################

a1 = ggplot(data = data_q2, aes(x = as.factor(Web.order),
                           y = Spending))+
  geom_boxplot() + 
  labs(x="is_Web.order") 
  
a2 = ggplot(data = data_q2, aes(x = as.factor(Gender.male),
                           y = Spending))+
  geom_boxplot() + 
  labs(x="is_male") 

a3 = ggplot(data = data_q2, aes(x = as.factor(Address_is_res),
                           y = Spending))+
  geom_boxplot() + 
  labs(x="is_res") 

a4 = ggplot(data = data_q2, aes(x = as.factor(US),
                                y = Spending))+
  geom_boxplot() + 
  labs(x="in_US") 

grid.arrange(a1,a2,a3,a4,ncol = 4)

###################

s1 = ggplot(data_q2, aes(x=Spending, y=Freq)) + 
  geom_point(size=2, shape=1)+
  geom_smooth(method="loess")

s2 = ggplot(data_q2, aes(x=Spending, y=Freq)) + 
  geom_point(size=2, shape=1)+
  geom_smooth(method="lm")

s3 = ggplot(data_q2, aes(x=Spending, y=last_update_days_ago)) + 
  geom_point(size=2, shape=1)+
  geom_smooth(method="loess")

s4 = ggplot(data_q2, aes(x=Spending, y=last_update_days_ago)) + 
  geom_point(size=2, shape=1)+
  geom_smooth(method="lm")

grid.arrange(s1,s2,s3,s4,ncol = 2)

# it seem to be a positive linear relationship between Spending and Freq
# The relationship for Spending and LAST_UPDATE are not clear. 
# spending should be split into two category for high spending and low spending.
# we may see and negative relationship between low spending and LAST_UPDATE.


###################

#i
set.seed(1)
train=sample(c(TRUE,FALSE),prob=c(0.7,0.3),size=nrow(data_q2),replace=TRUE)
valid=!train

#ii

model1 = lm(Spending~ Freq +
              last_update_days_ago+
              as.factor(Web.order)+
              as.factor(Gender.male)+
              as.factor(Address_is_res)+
              as.factor(US),
            data = data_q2)
summary(model1)
AIC(model1)
BIC(model1)

# iii

# web order and high freq of transactions
# not resident and min last update

#iv
step(model1, direction = "backward")

model2=regsubsets(Spending~.,data_q2[train,],nvmax=6, method="backward")
summary(model2)
test.mat=model.matrix(Spending~.,data_q2[valid,])

val.errors=numeric(6)
for(i in 1:6){
  coefi=coef(model2,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((data_q2$Spending[valid]-pred)^2)
  
}
best=which.min(val.errors)
best
coef(model2,best)
plot(val.errors,type="b")

#v 

model3 = lm(Spending~ Freq +
              last_update_days_ago+
              as.factor(Web.order)+
              as.factor(Address_is_res),
            data = data_q2)

valid.df=data_q2[!train,]
#use predict() to make predictions on the validation set
pred=predict(model3,valid.df)
error=valid.df$Spending-pred

#get accuracy metrics
MSE=mean(error^2)
RMSE=sqrt(MSE)
MAE=mean(abs(error))
MAPE=mean(abs(error/valid.df$Spending))*100
c(MSE,RMSE,MAE,MAPE)

#vi
hist(error,breaks=50,xlab="error",main="Histogram of Error")
hist(valid.df$Spending,breaks=50,xlab="Spending",main="Histogram of Spending")
plot(model3)

# does not follow a




















