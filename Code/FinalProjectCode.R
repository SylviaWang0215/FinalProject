#first merge the data - 02202017
library(readr)
dataset <- read_csv("~/Documents/Cornell/FinalProject/data.csv")

summary(dataset)
dataset <- data.frame(dataset)

#-------------use the plot to chekc the dataset---------------------------------------------------
plot(dataset$`RF1.HWS.VALVE.14`, dataset$TEMPERATURE)
boxplot(dataset$TEMPERATURE~dataset$`RF1.HWS.VALVE.14`)

plot(dataset$`A1.DX.CAP.SIGNAL`, dataset$TEMPERATURE)
hist(dataset$`A1.DX.CAP.SIGNAL`)

plot(dataset$`RSB.P1.START.STOP`, dataset$TEMPERATURE)
boxplot(dataset$TEMPERATURE~dataset$`RSB.P1.START.STOP`)

plot(dataset$`CRCP.VALVE.S28A`, dataset$TEMPERATURE)
hist(dataset$`CRCP.VALVE.S28A`)

plot(dataset$`GWRV.LOOPOUT`, dataset$TEMPERATURE)
hist(dataset$`GWR. LOOPOUT`)

plot(dataset$`M1.AVG.FLOW`, dataset$TEMPERATURE)
hist(dataset$`M1.AVG.FLOW`)

plot(dataset$`ZONE.N121.N125.AVERAGE.TEM`, dataset$TEMPERATURE)

plot(dataset$`S1.DPT.AVG.C`, dataset$TEMPERATURE)
hist(dataset$`S1.DPT.AVG.C`)

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
plot(new_dataset$`RF1.HWS.VALVE.14`, new_dataset$TEMPERATURE)
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


#plot for a day
plot(dataset$TEMPERATURE[1:288],type="b")

#day1 - 4
par(mfrow=c(2,2))
#dev.off()
plot(dataset$TEMPERATURE[1: 288],type="b")
plot(dataset$TEMPERATURE[289: 576],type="b")
plot(dataset$TEMPERATURE[577: 864],type="b")
plot(dataset$TEMPERATURE[865: 1152],type="b")


#day 5 - 8
par(mfrow=c(2,2))
#dev.off()
plot(dataset$TEMPERATURE[288*5 + 1: 288*6],type="b")
plot(dataset$TEMPERATURE[288*6 + 1: 288*7],type="b")
plot(dataset$TEMPERATURE[288*7 + 1: 288*8],type="b")
plot(dataset$TEMPERATURE[288*8 + 1: 288*9],type="b")

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

count = 1

threshold <- 73

dataset_svm <- na.omit(dataset)
#dataset_svm$TEMPERATURE <- ifelse(dataset_svm$TEMPERATURE >threshold,1,0)

#use 1 day as a train set
train_x <- dataset_svm[1: 288, 1: 10]
scaled.data <- scale(train_x)
#ahead of time is 2h -> which is 24 * 5 mins
train_y <- dataset_svm[60: 347, 11]
train_set <- cbind(train_x, train_y)

test_x <- dataset_svm[289:299, 1: 10]
test_y <- dataset_svm[348: 358, 11]
test_set <- cbind(test_x, test_y)

svm.model <- svm(TEMPERATURE~., data = train_set, cost = 100, gamma = 1)

svm.pred <- predict(svm.model, test_set[, -11])


#use two day as the train dataset-------------------------------------------------------------
train_x <- dataset_svm[1: 576, 1: 10]
scaled.data <- scale(train_x)
#ahead of time is 2h -> which is 24 * 5 mins
train_y <- dataset_svm[60: 635, 11]
train_set <- cbind(train_x, train_y)

test_x <- dataset_svm[577: 587, 1: 10]
test_y <- dataset_svm[636: 646, 11]
test_set <- cbind(test_x, test_y)

svm.model <- svm(TEMPERATURE~., data = train_set, cost = 100, gamma = 1)

svm.pred <- predict(svm.model, test_set[, -11])


#question1: the size of the train set
#question2: the size of the ahead time
#question3: the parameter in the model
#########--------------------------------------------------------------------------------------
ahead_time <- c(3,6,12,24,36,48,288)

tr_tf <- c(144, 288, 576) 

acc = matrix(NA,nrow=3,ncol = 7)


for (j in c(1: 3)){
  for (i in c(1: 7)){
    count = 1
    accuracy = 0
    
    while(count < 101){
      train_x <- dataset_svm[count: (count + tr_tf[j]), 1: 10]
      train_y <- dataset_svm[(count + ahead_time[i]): (count + tr_tf[j] + ahead_time[i]), 11]
      train_set <- cbind(train_x, train_y)
      
      test_x <- dataset_svm[(count + tr_tf[j] + 1), 1: 10]
      test_y <- dataset_svm[(count + tr_tf[j] + ahead_time[i] + 1), 11]
      #test_set <- cbind(test_x, test_y)
      
      svm.model <- svm(TEMPERATURE~., data = train_set, cost = 100, gamma = 1)
      svm.pred <- predict(svm.model, test_x)
      accuracy = accuracy + abs(svm.pred-test_y)
      count = count + 1
     
    }
    acc[j,i] = unlist(accuracy)
    
  }
}

plot(1:7,rep(0,7),ylim=c(0,max(acc)),type="n",xlab = "ahead of time")
sapply(1:3,function(x)points(1:7,acc[x,],type="b",col=x))
legend("topright",c("12hr","24hr","48hr"),col=c(1,2,3),bty="n",pch=1,lty = 1)


for (i in c(1: 7)){
  for (j in c(1: 3)){
    print(c(ahead_time[i], tr_tf[j]))
  }
}


#-test example------------------------------------------------------
library(e1071)

count = 1

threshold <- 73

dataset_svm <- na.omit(dataset)

ahead_time <- c(3,6,12,24,36,48,288)

tr_tf <- seq(from = 288, to = 1440,by = 144) 

acc = matrix(NA,nrow=length(tr_tf),ncol = length(ahead_time))


for (j in c(1: 9)){
  for (i in c(1: 7)){
    count = 1
    accuracy = 0
    
    while(count < 51){
      train_x <- dataset_svm[count: (count + tr_tf[j]), 1: 10]
      train_y <- dataset_svm[(count + ahead_time[i]): (count + tr_tf[j] + ahead_time[i]), 11]
      train_set <- cbind(train_x, train_y)
      
      test_x <- dataset_svm[(count + tr_tf[j] + 1), 1: 10]
      test_y <- dataset_svm[(count + tr_tf[j] + ahead_time[i] + 1), 11]
      #test_set <- cbind(test_x, test_y)
      
      svm.model <- svm(train_y~., data = train_set, cost = 100, gamma = 1)
      svm.pred <- predict(svm.model, test_x)
      accuracy = accuracy + abs(svm.pred-test_y)
      count = count + 1
      
    }
    acc[j,i] = unlist(accuracy)
    
  }
}

plot(1:7,rep(0,7),ylim=c(0,max(acc)),type="n",xlab = "ahead of time")
sapply(1:9,function(x)points(1:7,acc[x,],type="b",col=x))
legend("topright",c("24hr","36hr","48hr","60hr","72hr","84hr","96hr","108hr","120hr"),col=c(1:9),bty="n",pch=1,lty = 1)

#03242017-------------------------------------------------------------------------------------------------------------------
#neural network
library('neuralnet')
dataset_svm <- na.omit(dataset)
train_x <- dataset_svm[1: 500, 1: 10]
train_y <- dataset_svm[25: 524, 11]
train_set <- cbind(train_x, train_y)

test_x <- dataset_svm[501:521, 1: 10]
test_y <- dataset_svm[525: 545, 11]
test_set <- cbind(test_x, test_y)

net.sqrt <- neuralnet(as.formula(paste("train_y~",paste(names(train_set)[1:10],collapse = "+"),sep="")),
                      train_set, hidden = 5, threshold = 0.01)
net.results <- compute(net.sqrt, test_x)


#sliding window for NN---------------------------------------------------------------------
ahead_time <- c(3,6,12,24,36,48,288)

tr_tf <- seq(from = 288, to = 1440,by = 144) 

acc = matrix(NA, nrow=length(tr_tf), ncol = length(ahead_time))


for (j in c(1: 9)){
  for (i in c(1: 7)){
    count = 1
    accuracy = 0
    while(count < 51){
      train_x <- dataset_svm[count: (count + tr_tf[j]), 1: 10]
      train_y <- dataset_svm[(count + ahead_time[i]): (count + tr_tf[j] + ahead_time[i]), 11]
      train_set <- cbind(train_x, train_y)
      
      test_x <- dataset_svm[(count + tr_tf[j] + 1), 1: 10]
      test_y <- dataset_svm[(count + tr_tf[j] + ahead_time[i] + 1), 11]
      #test_set <- cbind(test_x, test_y)
      
      net.sqrt <- neuralnet(as.formula(paste("train_y~",paste(names(train_set)[1:10],collapse = "+"),sep="")),
                            train_set, hidden = 5, threshold = 0.01)
      net.results <- compute(net.sqrt, test_x)
      
      accuracy = accuracy + abs(net.results$net.result - test_y)
      count = count + 1
      
    }
    acc[j,i] = unlist(accuracy)
    
  }
}

plot(1:7,rep(0,7),ylim=c(0,max(acc)),type="n",xlab = "ahead of time")
sapply(1:9,function(x)points(1:7,acc[x,],type="b",col=x))
legend("topright",c("24hr","36hr","48hr","60hr","72hr","84hr","96hr","108hr","120hr"),col=c(1:9),bty="n",pch=1,lty = 1)

#