library(readr)
dataset <- read_csv("~/Documents/Cornell/FinalProject/data.csv")

summary(dataset)
dataset <- data.frame(dataset)

library(e1071)

dataset_matrix <- as.matrix(dataset)

cutoff <- 68.1

dataset_svm <- na.omit(dataset)
ahead_time <- c(3,6,12,24,48,96,192,288)
tr_tf <- seq(from = 288, to = 1440,by = 144) 
acc = matrix(NA,nrow=length(tr_tf),ncol = length(ahead_time))
err1 = matrix(NA,nrow=length(tr_tf),ncol = length(ahead_time))
err2 = matrix(NA,nrow=length(tr_tf),ncol = length(ahead_time))
square_error = matrix(NA, nrow = length(tr_tf), ncol = length(ahead_time))


for (i in c(1: 8)){
  for (j in c(1: 9)){
    count = 1
    accuracy = 0
    error1 = 0
    error2 = 0
    square = 0
    
    while(count < 20000){
      train_x <- dataset_svm[count: (count + tr_tf[j]), 1: 10]
      train_y <- dataset_svm[(count + ahead_time[i]): (count + tr_tf[j] + ahead_time[i]), 11]
      train_set <- cbind(train_x, train_y)
      
      test_x <- dataset_svm[(count + tr_tf[j] + 1), 1: 10]
      test_y <- dataset_svm[(count + tr_tf[j] + ahead_time[i] + 1), 11]
      
      svm.model <- svm(train_y~., data = train_set)
      svm.pred <- predict(svm.model, test_x)
      if ((svm.pred > cutoff & test_y > cutoff)|| (svm.pred < cutoff & test_y <= cutoff)){
        accuracy = accuracy
      }
      else{
        accuracy = accuracy + 1
        if (svm.pred > cutoff & test_y < cutoff){
          error1 = error1 + 1
        }
        else{
          error2 = error2 + 1
        }
      }
      square = square + (svm.pred - test_y)*(svm.pred - test_y)
      count = count + 5
      
    }
    acc[j,i] = unlist(accuracy)
    err1[j, i] = unlist(error1)
    err2[j, i] = unlist(error2)
    square_error[j, i] = unlist(square)
  }
}

colors <- c("#A7A7A7","dodgerblue","firebrick","forestgreen","gold","black","brown",6,4)
plot(1:8,rep(0,8),ylim=c(min(acc1)*0.9, max(acc1)*1.1),type="n",xlab = "ahead of time", main = "Overall 0-1 Error")
sapply(1:9,function(x)points(1:8,acc1[x,],type="b",col=colors[x]))
legend("topright",c("24hr","36hr","48hr","60hr","72hr","84hr","96hr","108hr","120hr"),col=colors,bty="n",pch=1,lty = 1)

plot(1:8,rep(0,8),ylim=c(min(err1_1)*0.9, max(err1_1)*1.1),type="n",xlab = "ahead of time", main="Predicted > CutOff & Observed < Cutoff")
sapply(1:9,function(x)points(1:8,err1_1[x,],type="b",col=colors[x]))
legend("topright",c("24hr","36hr","48hr","60hr","72hr","84hr","96hr","108hr","120hr"),col=colors,bty="n",pch=1,lty = 1)

plot(1:8,rep(0,8),ylim=c(min(square_error1)*0.9, max(square_error1)*1.1),type="n",xlab = "ahead of time", main="Overall Square Loss")
sapply(1:9,function(x)points(1:8,square_error1[x,],type="b",col=colors[x]))
legend("topright",c("24hr","36hr","48hr","60hr","72hr","84hr","96hr","108hr","120hr"),col=colors,bty="n",pch=1,lty = 1)
#save all the results in acc1, err1_1, err2_1, square_error1


#2017-04-24 narrow the search range -----------------------------------------------------------------------------------------------------------------------
library(e1071)

dataset_matrix <- as.matrix(dataset)

cutoff <- 68.1

dataset_svm <- na.omit(dataset)
tr_tf <- c(264, 276, 288, 300, 312, 408, 420, 432, 444, 456)
ahead_time <- c(3, 4, 5, 6, 7, 8, 9)
acc = matrix(NA,nrow=length(tr_tf),ncol = length(ahead_time))
square_error = matrix(NA, nrow = length(tr_tf), ncol = length(ahead_time))
err1 = matrix(NA,nrow=length(tr_tf),ncol = length(ahead_time))
err2 = matrix(NA,nrow=length(tr_tf),ncol = length(ahead_time))


for (i in c(1: 7)){
  for (j in c(1: 10)){
    count = 1
    overall_error = 0
    error1 = 0
    error2 = 0
    square = 0
    
    while(count < 20000){
      train_x <- dataset_svm[count: (count + tr_tf[j]), 1: 10]
      train_y <- dataset_svm[(count + ahead_time[i]): (count + tr_tf[j] + ahead_time[i]), 11]
      train_set <- cbind(train_x, train_y)
      
      test_x <- dataset_svm[(count + tr_tf[j] + 1), 1: 10]
      test_y <- dataset_svm[(count + tr_tf[j] + ahead_time[i] + 1), 11]
      
      svm.model <- svm(train_y~., data = train_set)
      svm.pred <- predict(svm.model, test_x)
      if ((svm.pred > cutoff & test_y > cutoff)|| (svm.pred < cutoff & test_y <= cutoff)){
        overall_error = overall_error
      }
      else{
        overall_error = overall_error + 1
        if (svm.pred > cutoff & test_y < cutoff){
          error1 = error1 + 1
        }
        else{
          error2 = error2 + 1
        }
      }
      square = square + (svm.pred - test_y)*(svm.pred - test_y)
      count = count + 5
      
    }
    acc[j,i] = unlist(overall_error)
    err1[j, i] = unlist(error1)
    err2[j, i] = unlist(error2)
    square_error[j, i] = unlist(square)
  }
}

colors <- c("#A7A7A7","dodgerblue","firebrick","forestgreen","gold","black","brown", 6, 4, 3)
plot(1:7,rep(0,7),ylim=c(min(acc2)*0.9, max(acc2)*1.1),type="n",xlab = "ahead of time", main = "Overall 0-1 Error")
sapply(1:10,function(x)points(1:7,acc2[x,],type="b",col=colors[x]))
legend("topright",c("22hr", "23hr","24hr","25hr","26hr","34hr","35hr","36hr","37hr","38hr"),col=colors,bty="n",pch=1,lty = 1)

plot(1:7,rep(0,7),ylim=c(min(err1_2)*0.9, max(err1_2)*1.1),type="n",xlab = "ahead of time", main="Predicted > CutOff & Observed < Cutoff 0-1 Error")
sapply(1:10,function(x)points(1:7,err1_2[x,],type="b",col=colors[x]))
legend("topright",c("22hr", "23hr","24hr","25hr","26hr","34hr","35hr","36hr","37hr","38hr"),col=colors,bty="n",pch=1,lty = 1)

plot(1:7,rep(0,7),ylim=c(min(square_error2)*0.9, max(square_error2)*1.1),type="n",xlab = "ahead of time", main="Square error")
sapply(1:10,function(x)points(1:7,square_error2[x,],type="b",col=colors[x]))
legend("topright",c("22hr", "23hr","24hr","25hr","26hr","34hr","35hr","36hr","37hr","38hr"),col=colors,bty="n",pch=1,lty = 1)


#save all the results in acc2, err1_2, err2_2, square_error2

#calculate the C and gamma for ahead of time = 15min and training dataset size = 23hr-------------------
cost = c(0.001, 0.01, 0.1, 1, 10, 100)
gamma = c(0.001, 0.01, 0.1, 1, 10, 100)

acc = matrix(NA,nrow=length(cost),ncol = length(gamma))
err1 = matrix(NA,nrow=length(cost),ncol = length(gamma))
err2 = matrix(NA,nrow=length(cost),ncol = length(gamma))




for (i in c(1: 6)){
  for (j in c(1: 6)){
    count = 1
    accuracy = 0
    error1 = 0
    error2 = 0
    
    while(count < 20000){
      train_x <- dataset_svm[count: (count + 276), 1: 10]
      train_y <- dataset_svm[(count + 3): (count + 276 + 3), 11]
      train_set <- cbind(train_x, train_y)
      
      test_x <- dataset_svm[(count + 276 + 1), 1: 10]
      test_y <- dataset_svm[(count + 276 + 3 + 1), 11]
      
      svm.model <- svm(train_y~., data = train_set, cost=cost[i], gamma=gamma[j])
      svm.pred <- predict(svm.model, test_x)
      if ((svm.pred > cutoff & test_y > cutoff)|| (svm.pred < cutoff & test_y <= cutoff)){
        accuracy = accuracy
      }
      else{
        accuracy = accuracy + 1
        if (svm.pred > cutoff & test_y < cutoff){
          error1 = error1 + 1
        }
        else{
          error2 = error2 + 1
        }
      }
      count = count + 5
      
    }
    acc[j,i] = unlist(accuracy)
    err1[j, i] = unlist(error1)
    err2[j, i] = unlist(error2)
  }
}

colors <- c("dodgerblue","firebrick","forestgreen","gold","black","brown")
plot(1:6,rep(0,6),ylim=c(min(svm_result2)*0.9, max(svm_result2)*1.1),type="n",xlab = "ahead of time", main = "Overall Error")
sapply(1:6,function(x)points(1:6,svm_result2[x,],type="b",col=colors[x]))
legend("topright",c("0.001", "0.01","0.1","1","10","100"),col=colors,bty="n",pch=1,lty = 1)

plot(1:6,rep(0,7),ylim=c(min(svm_error1_1)*0.9, max(svm_error1_1)*1.1),type="n",xlab = "ahead of time", main="Predicted > CutOff & Observed < Cutoff")
sapply(1:6,function(x)points(1:6,svm_error1_1[x,],type="b",col=colors[x]))
legend("topright",c("22hr", "23hr","24hr","25hr","26hr","34hr","35hr","36hr","37hr","38hr"),col=colors,bty="n",pch=1,lty = 1)

#change the partition method for the data----------------------------------------------------------

