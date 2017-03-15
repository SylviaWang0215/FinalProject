#first merge the data - 02202017
library(readr)
dataset <- read_csv("~/Documents/Cornell/FinalProject/data.csv")

summary(dataset)


#-------------use the plot to chekc the dataset---------------------------------------------------
plot(dataset$`RF1 HWS VALVE 14`, dataset$TEMPERATURE)
boxplot(dataset$TEMPERATURE~dataset$`RF1 HWS VALVE 14`)

plot(dataset$`A1 DX CAP SIGNAL`, dataset$TEMPERATURE)
hist(dataset$`A1 DX CAP SIGNAL`)

plot(dataset$`RSB P1 START/STOP`, dataset$TEMPERATURE)
boxplot(dataset$TEMPERATURE~dataset$`RSB P1 START/STOP`)

plot(dataset$`CRCP VALVE S28A`, dataset$TEMPERATURE)
hist(dataset$`CRCP VALVE S28A`)

plot(dataset$`GWRV LOOPOUT`, dataset$TEMPERATURE)
hist(dataset$`GWRV LOOPOUT`)

plot(dataset$`M1 AVG FLOW`, dataset$TEMPERATURE)
hist(dataset$`M1 AVG FLOW`)

plot(dataset$`ZONE N121 N125 AVERAGE TEM`, dataset$TEMPERATURE)

plot(dataset$`S1 DPT AVG C`, dataset$TEMPERATURE)
hist(dataset$`S1 DPT AVG C`)

plot(dataset$`HP3 HEAT STAGE TIMER`, dataset$TEMPERATURE)
hist(dataset$`HP3 HEAT STAGE TIMER`)

plot(dataset$`N1 COOLING OFF`, dataset$TEMPERATURE)
boxplot(dataset$TEMPERATURE~dataset$`N1 COOLING OFF`)


#fit a linear model------------------------------------------------------------------
fit <- lm(dataset$TEMPERATURE~., data = dataset)
summary(fit)

library(car)
outlierTest(fit)
leveragePlots(fit)


dataset$`N1 COOLING OFF` <- as.factor(dataset$`N1 COOLING OFF`)
#it doesnt matter if we change those 0-1 vairables into factor since there is only 0-1 situation
fit2 <- lm(dataset$TEMPERATURE~., data = dataset)

#the first 6000 instance show a different pattern compared with the other data, so we do 
#plot ignored the first 6000 data
new_dataset = dataset[6000:nrow(dataset), ]
plot(new_dataset$`RF1 HWS VALVE 14`, new_dataset$TEMPERATURE)
boxplot(new_dataset$TEMPERATURE~new_dataset$`RF1 HWS VALVE 14`)

plot(new_dataset$`A1 DX CAP SIGNAL`, new_dataset$TEMPERATURE)
hist(new_dataset$`A1 DX CAP SIGNAL`)

plot(dataset$`RSB P1 START/STOP`, dataset$TEMPERATURE)
boxplot(dataset$TEMPERATURE~dataset$`RSB P1 START/STOP`)

plot(dataset$`CRCP VALVE S28A`, dataset$TEMPERATURE)
hist(dataset$`CRCP VALVE S28A`)

plot(dataset$`GWRV LOOPOUT`, dataset$TEMPERATURE)
hist(dataset$`GWRV LOOPOUT`)

plot(dataset$`M1 AVG FLOW`, dataset$TEMPERATURE)
hist(dataset$`M1 AVG FLOW`)

fit3 <- lm(TEMPERATURE~., data = new_dataset)
summary(fit3)

library(car)
outlierTest(fit3)
leveragePlots(fit3)

plot(fit3)

par(mfrow=c(2,2))
dev.off()
plot(dataset$TEMPERATURE[1:6624],type="b")
plot(dataset$TEMPERATURE[6625:13248],type="b")
plot(dataset$TEMPERATURE[13249:19872],type="b")
plot(dataset$TEMPERATURE[19873:26493],type="b")

plot(dataset$TEMPERATURE[1500:5000],type="b")
plot(dataset$TEMPERATURE,type="b")

#-------------------------fit a Svm model--------------------------------------------------------------
#install.packages("e1071")
library(e1071)
ahead_time <- c(3,6,12,24,36,48,288)
tr_tf <- c(3,6,12,24,36,48,288) 
count = 1

threshold <- 73

dataset_svm <- na.omit(dataset)
dataset_svm$TEMPERATURE <- ifelse(dataset_svm$TEMPERATURE >threshold,1,0)
plot(dataset$TEMPERATURE,type="b")

#Q: consider the value of cost and gamma

for (i in ahead_time){
  for (j in tr_tf){
    while(count < 6000){
      train_set = dataset_svm[count: count + tr_tf[j], ]
      test_set = dateset_svm[count+tr_tf[j] + ahead_time[i], ]
      svm.model <- svm(TEMPERATURE~., data = train_set, cost = 100, gamma = 1)
      svm.pred <- predict(svm.model, test_set$TEMPERATURE)
      
    }
  }
}

i = 3
j = 7

train_set = dataset_svm[count: (count + tr_tf[j]), ]
test_set = dataset_svm[(count+tr_tf[j]+ ahead_time[1] + 1): (count + tr_tf[j] + ahead_time[i] + 10), ]
svm.model <- svm(TEMPERATURE~., data = train_set, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, test_set[, -11],type="class")


count = 1

accuracy = 0
while(count < 100){
  train_set <- dataset_svm[count: (count + 40), ]
  test_set <- dataset_svm[count+ 40 + ahead_time[i], ]
  svm.model <- svm(TEMPERATURE~., data = train_set, cost = 100, gamma = 1)
  svm.pred <- predict(svm.model, test_set[, -11])
  if ((svm.pred < 0 && test_set[, 11] == 0) || (svm.pred >= 0 && test_set[, 11] == 1)){
    accuracy = accuracy + 1
  }
  count = count + 1
}

# Quesiton: how to treat the constant variable?    


#03142017--------------------------------------------------------------------------
library(e1071)
ahead_time <- c(3,6,12,24,36,48,288)
tr_tf <- c(3,6,12,24,36,48,288) 
count = 1

threshold <- 73

dataset_svm <- na.omit(dataset)
dataset_svm$TEMPERATURE <- ifelse(dataset_svm$TEMPERATURE >threshold,1,0)

#use 3 hour as a train set
train_x <- dataset_svm[1: 36, 1: 10]
#ahead of time is 2h -> which is 24 * 5 mins
train_y <- dataset_svm[60: 95, 11]
train_set <- cbind(train_x, train_y)
