
getwd()
setwd("/Users/chriz_yu/Documents/MSU/ITM 818/818 HW 04")
data = read.csv("Airfares.csv")
summary(data)

library(corrplot)
library(dplyr)  

num_cols = unlist(lapply(data, is.numeric))
# num_cols = select_if(data, is.numeric)
num_data = data[,num_cols]

cor_data = data.frame(cor(num_data))
corrplot(cor(num_data), method = 'number')
plot(FARE~ COUPON, data = data)
plot(FARE~ DISTANCE, data = data)
plot(FARE~ E_INCOME, data = data)
plot(FARE~ E_POP, data = data)

## cat_data = data[,!num_cols]
## cat_data = select(data, c(VACATION, SW, SLOT, GATE, FARE))
data$VACATION01 = if_else(data$VACATION == 'Yes', 1,0)
data$SW01 = if_else(data$SW == 'Yes', 1,0)
data$SLOT01 = if_else(data$SLOT == 'Free', 0,1)
data$GATE01 = if_else(data$GATE== 'Free', 0,1)

cat_data = select(data, c(VACATION01, SW01, SLOT01, GATE01, FARE))

cat_data_01 = data %>%
              select(VACATION01, SW01, SLOT01, GATE01, FARE) %>%
              group_by(SW01) %>%
              mutate(n = n())
data %>%
  select(VACATION01, SW01, SLOT01, GATE01, FARE) %>%
  group_by(SW01) %>%
  summarise(n = n(), mean = mean(FARE)) %>%
  mutate(SWperc = round(n / sum(n), 3))

cat_data_01 = data %>%
              select(VACATION01, SW01, SLOT01, GATE01, FARE) %>%
              group_by(SW01, VACATION01, SLOT01, GATE01) %>%
              summarise(n = n(), mean = mean(FARE))
cat_data_01 = cat_data_01 %>%
              mutate(perc = 100*round(n/sum(cat_data_01$n),3))

vacation_data = data %>%
                select(Y_N = VACATION, FARE) %>%
                group_by(Y_N) %>%
                summarise(n = n(), AVG_FARE = mean(FARE)) %>%
                mutate(perc = round(n / sum(n), 3)*100)

SW_data = data %>%
  select(Y_N = SW, FARE) %>%
  group_by(Y_N) %>%
  summarise(n = n(), AVG_FARE = mean(FARE)) %>%
  mutate(perc = round(n / sum(n), 3)*100)

SLOT_data = data %>%
  select(Y_N = SLOT, FARE) %>%
  group_by(Y_N) %>%
  summarise(n = n(), AVG_FARE = mean(FARE)) %>%
  mutate(perc = round(n / sum(n), 3)*100)

GATE_data = data %>%
  select(Y_N = GATE, FARE) %>%
  group_by(Y_N) %>%
  summarise(n = n(), AVG_FARE = mean(FARE)) %>%
  mutate(perc = round(n / sum(n), 3)*100)

total <- rbind(SW_data, vacation_data, SLOT_data, GATE_data)
barplot(total$AVG_FARE, 
        main="average fare in each category",
        ylab = "AVG_FARE",
        col=c("darkblue","red"),
        names.arg = c("SW_No", "SW_Yes", "VACA_No", "VACA_Yes","SLOT_C", "SLOT_F","GATE_C", "GATE_F"),
        beside = TRUE)


## 3
# i

set.seed(1)
train=sample(c(TRUE,FALSE),prob=c(0.6,0.4),size=nrow(data),replace=TRUE)
test=!train

# ii validation set approach
n = 13
library(leaps)
col_names = c(names(num_data), c('SW01', 'VACATION01', 'SLOT01', 'GATE01'))
subdata = data[,col_names]

model1=regsubsets(FARE~.,subdata[train,],nvmax=n,method="seqrep")
test.mat=model.matrix(FARE~.,data[test,])

seqrep_val.errors=numeric(n)
seqrep_MSE = numeric(n)
seqrep_RMSE = numeric(n)
seqrep_MAE = numeric(n)
seqrep_MAPE = numeric(n)

for(i in 1:n){
  coefi=coef(model1,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  error=subdata$FARE[test]-pred
  
  seqrep_MSE[i] = mean(error^2)
  seqrep_RMSE[i] = sqrt(seqrep_MSE[i])
  seqrep_MAE[i] = mean(abs(error))
  seqrep_MAPE[i] = mean(abs(error/subdata$FARE[test]))*100
  seqrep_val.errors[i] = mean((subdata$FARE[test]-pred)^2)
}

best_seqrep = which.min(seqrep_val.errors)
best_seqrep
best_coef_seqrep = coef(model1,best_seqrep)
best_coef_seqrep 
plot( seqrep_val.errors,type="b")

# iii

model2=regsubsets(FARE~.,subdata[train,],nvmax=n,method="exhaustive")
test.mat=model.matrix(FARE~.,data[test,])

exhaustive_val.errors=numeric(n)
exhaustive_MSE = numeric(n)
exhaustive_RMSE = numeric(n)
exhaustive_MAE = numeric(n)
exhaustive_MAPE = numeric(n)

for(i in 1:13){
  coefi=coef(model2,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  error=subdata$FARE[test]-pred
  
  exhaustive_MSE[i] = mean(error^2)
  exhaustive_RMSE[i] = sqrt(exhaustive_MSE[i])
  exhaustive_MAE[i] = mean(abs(error))
  exhaustive_MAPE[i] = mean(abs(error/subdata$FARE[test]))*100
  exhaustive_val.errors[i]=mean((subdata$FARE[test]-pred)^2)
}

best_exhaustive = which.min(exhaustive_val.errors)
best_exhaustive
best_coef_exhaustive = coef(model2,best_exhaustive)
best_coef_exhaustive 
plot(exhaustive_val.errors,type="b")


# iv 

acc_table = data.frame(
  Method = c("Seqrep","Exhaustive"), 
  MSE = c(mean(seqrep_MSE), mean(exhaustive_MSE)), 
  RMSE = c(mean(seqrep_RMSE), mean(exhaustive_RMSE)),
  MAE = c(mean(seqrep_MAE), mean(exhaustive_MAE)),
  MAPE = c(mean(seqrep_MAPE), mean(exhaustive_MAPE))
  )

RMSE_seqrep
RMSE_exhaustive
MAPE_seqrep 
MAPE_exhaustive

# v
COUPON = 1.202
NEW = 3
VACATION01 = 0 #No
SW01 = 0 #No
HI = 4442.141
S_INCOME = 28760
E_INCOME = 27664
S_POP = 4557004 
E_POP = 3195503 
SLOT01 = 0 #Free
GATE01 = 0 #Free
PAX = 12782
DISTANCE = 1976 

initail_value = c(1, HI, E_INCOME, S_POP, E_POP, DISTANCE, PAX, SW01, VACATION01, SLOT01, GATE01)

Pred_AVG_FARE = sum(best_coef_exhaustive*initail_value)
Pred_AVG_FARE

# vi

SW01 = 1 #Yes

initail_value = c(1, HI, E_INCOME, S_POP, E_POP, DISTANCE, PAX, SW01, VACATION01, SLOT01, GATE01)

Pred_AVG_FARE_YES = sum(best_coef_exhaustive*initail_value)
Pred_AVG_FARE_YES
Pred_AVG_FARE - Pred_AVG_FARE_YES
# vii
# COUPON NEW VACATION PAX

# viii

new_col_names = col_names[!col_names%in% c('NEW', 'HI', 'PAX')]
subdata = data[,new_col_names]
n = 10


model3=regsubsets(FARE~.,subdata[train,],nvmax=n,method="exhaustive")
test.mat=model.matrix(FARE~.,data[test,])

new_exhaustive_val.errors=numeric(n)
new_exhaustive_MSE = numeric(n)
new_exhaustive_RMSE = numeric(n)
new_exhaustive_MAE = numeric(n)
new_exhaustive_MAPE = numeric(n)

for(i in 1:n){
  coefi=coef(model3,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  error=subdata$FARE[test]-pred
  
  new_exhaustive_MSE[i] = mean(error^2)
  new_exhaustive_RMSE[i] = sqrt(new_exhaustive_MSE[i])
  new_exhaustive_MAE[i] = mean(abs(error))
  new_exhaustive_MAPE[i] = mean(abs(error/subdata$FARE[test]))*100
  new_exhaustive_val.errors[i]=mean((subdata$FARE[test]-pred)^2)
}

new_best_exhaustive = which.min(new_exhaustive_val.errors)
new_best_coef_exhaustive = coef(model3,new_best_exhaustive)
plot(new_exhaustive_val.errors,type="b")
new_best_exhaustive

initail_value = c(1, S_INCOME, E_INCOME, E_POP, DISTANCE, SW01, VACATION01, SLOT01, GATE01)
New_Pred_AVG_FARE = sum(new_best_coef_exhaustive*initail_value)
New_Pred_AVG_FARE

# ix 
new_best_coef_exhaustive
best_coef_exhaustive
acc_table = rbind(acc_table, 
      c("Exhaustive_NEW", mean(new_exhaustive_MSE), 
      mean(new_exhaustive_RMSE),
      mean(new_exhaustive_MAE),
      mean(new_exhaustive_MAPE)
        ))

# x 
MAPE_exhaustive
RMSE_exhaustive
new_MAPE_exhaustive 
new_RMSE_exhaustive
