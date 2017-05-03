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

par(mar = c(5.1, 4.1, 3.1, 5.75), xpd = TRUE)

colors <- c("#A7A7A7","dodgerblue","firebrick","forestgreen","gold","black","brown",6,4)
ahead_time_label = c("15min", "30min", "1hr", "2hr", "4hr", "8hr", "16hr", "24hr")
plot(1:8,rep(0,8),ylim=c(min(acc1)*0.9, max(acc1)*1.1),type="n",
     xlab = "Time in Advance", 
     ylab = "Error Rate(%)", 
     xaxt = "n",
     main = "Overall Misclassification Rate")
axis(1,at=1:8,ahead_time_label)
sapply(1:9,function(x)points(1:8,acc1[x,],type="b",col=colors[x]))
legend(title="Training Size",
       "bottomright",
       c("24hr","36hr","48hr","60hr","72hr","84hr","96hr","108hr","120hr"),
       col=colors,bty="n",pch=1,lty = 1,cex=0.8, inset=c(-0.25,0))

par(mar = c(5.1, 4.1, 3.1, 5), xpd = TRUE)
plot(1:8,rep(0,8),ylim=c(min(err1_1)*0.9, max(err1_1)*1.1),type="n",
     xlab = "Time in Advance",
     ylab = "Error Rate(%)",
     xaxt = "n",
     main="Missed Detection Rate")
ahead_time_label = c("15min", "30min", "1hr", "2hr", "4hr", "8hr", "16hr", "24hr")
axis(1,at=1:8,ahead_time_label)
sapply(1:9,function(x)points(1:8,err1_1[x,],type="b",col=colors[x]))
legend(title="Training Size",
       "bottomright",
       c("24hr","36hr","48hr","60hr","72hr","84hr","96hr","108hr","120hr"),
       col=colors,bty="n",pch=1,lty = 1, cex=0.8, inset=c(-0.25,0))


plot(1:8,rep(0,8),ylim=c(min(square_error1)*0.9, max(square_error1)*1.1),type="n",
     xlab = "Time in Advance",
     ylab = "Error Rate(%)",
     xaxt = "n",
     main="")
ahead_time_label = c("15min", "30min", "1hr", "2hr", "4hr", "8hr", "16hr", "24hr")
axis(1,at=1:8,ahead_time_label)
sapply(1:9,function(x)points(1:8,square_error1[x,],type="b",col=colors[x]))
legend(title="Training Size",
       "bottomright",
       c("24hr","36hr","48hr","60hr","72hr","84hr","96hr","108hr","120hr"),
       col=colors,bty="n",pch=1,lty = 1, cex=0.8, inset=c(-0.25,0))
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




plot(1:7,rep(0,7),ylim=c(min(square_error2)*0.9, max(square_error2)*1.1),type="n",xlab = "ahead of time", main="Square error")
sapply(1:10,function(x)points(1:7,square_error2[x,],type="b",col=colors[x]))
legend("topright",c("22hr", "23hr","24hr","25hr","26hr","34hr","35hr","36hr","37hr","38hr"),col=colors,bty="n",pch=1,lty = 1)



colors <- c("#A7A7A7","dodgerblue","firebrick","forestgreen","gold","black","brown", 6, 4, 3)
plot(1:7,rep(0,7),ylim=c(min(acc2)*0.9, max(acc2)*1.1),type="n",
     xlab = "Time in Advance",
     ylab = "Error Rate(%)",
     xaxt = "n",
     main="Overall Misclassification Rate")
ahead_time_label = c("15min", "20min", "25min", "30min", "35min", "40min", "45min")
axis(1,at=1:7,ahead_time_label)
sapply(1:10,function(x)points(1:7,acc2[x,],type="b",col=colors[x]))
legend(title="Training Size",
       "bottomright",
       c("22hr", "23hr","24hr","25hr","26hr","34hr","35hr","36hr","37hr","38hr"),
       col=colors,bty="n",pch=1,lty = 1, cex=0.8, inset=c(-0.35,0))


plot(1:7,rep(0,7),ylim=c(min(err1_2)*0.9, max(err1_2)*1.1),type="n",
     xlab = "Time in Advance",
     ylab = "Error Rate(%)",
     xaxt = "n",
     main="Missed Detection Rate")
ahead_time_label = c("15min", "20min", "25min", "30min", "35min", "40min", "45min")
axis(1,at=1:7,ahead_time_label)
sapply(1:10,function(x)points(1:7, err1_2[x,],type="b",col=colors[x]))
legend(title="Training Size",
       "bottomright",
       c("22hr", "23hr","24hr","25hr","26hr","34hr","35hr","36hr","37hr","38hr"),
       col=colors,bty="n",pch=1,lty = 1, cex=0.8, inset=c(-0.35,0))

#save all the results in acc2, err1_2, err2_2, square_error2

#calculate the C and gamma for ahead of time = 15min and training dataset size = 26hr-------------------
cost = c(0.001, 0.01, 0.1, 1, 10, 100)
gamma = c(0.001, 0.01, 0.1, 1, 10, 100)

acc = matrix(NA,nrow=length(cost),ncol = length(gamma))
err1 = matrix(NA,nrow=length(cost),ncol = length(gamma))
err2 = matrix(NA,nrow=length(cost),ncol = length(gamma))
square_error = matrix(NA,nrow=length(cost),ncol = length(gamma))

tr = 312
ahead = 3

for (i in c(1: 6)){
  for (j in c(1: 6)){
    count = 1
    accuracy = 0
    error1 = 0
    error2 = 0
    square = 0
    
    while(count < 20000){
      train_x <- dataset_svm[count: (count + tr), 1: 10]
      train_y <- dataset_svm[(count + ahead): (count + tr + ahead), 11]
      train_set <- cbind(train_x, train_y)
      
      test_x <- dataset_svm[(count + tr + 1), 1: 10]
      test_y <- dataset_svm[(count + tr + ahead + 1), 11]
      
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
      square = square + (svm.pred - test_y)*(svm.pred - test_y)
      count = count + 5
      
    }
    acc[j,i] = unlist(accuracy)
    err1[j, i] = unlist(error1)
    err2[j, i] = unlist(error2)
    square_error[j, i] = unlist(square)
  }
}


plot(1:6,rep(0,6),ylim=c(min(acc3)*0.9, max(acc3)*1.1),type="n",xlab = "cost", main = "Overall Error")
sapply(1:6,function(x)points(1:6,acc3[x,],type="b",col=colors[x]))
legend("topright",c("0.001", "0.01","0.1","1","10","100"),col=colors,bty="n",pch=1,lty = 1)

plot(1:6,rep(0,6),ylim=c(min(err1_3)*0.9, max(err1_3)*1.1),type="n",xlab = "cost", main = "Too high error")
sapply(1:6,function(x)points(1:6,err1_3[x,],type="b",col=colors[x]))
legend("topright",c("0.001", "0.01","0.1","1","10","100"),col=colors,bty="n",pch=1,lty = 1)

plot(1:6,rep(0,6),ylim=c(min(square_error3)*0.9, max(square_error3)*1.1),type="n",xlab = "cost", main = "Square Error")
sapply(1:6,function(x)points(1:6,square_error3[x,],type="b",col=colors[x]))
legend("topright",c("0.001", "0.01","0.1","1","10","100"),col=colors,bty="n",pch=1,lty = 1)

#when gamma = 0.1, cost = 100, we have the lowest error
colors <- c("dodgerblue","firebrick","forestgreen","gold","black","brown")
plot(1:6,rep(0,6),ylim=c(min(acc3)*0.9, max(acc3)*1.1),type="n",
     xlab = "Cost",
     ylab = "Error Rate(%)",
     xaxt = "n",
     main="Overall Misclassification Rate")
Cost = c("0.001", "0.01","0.1","1","10","100")
axis(1,at=1:6,Cost)
sapply(1:6,function(x)points(1:6,acc3[x,],type="b",col=colors[x]))
legend(title="Gamma",
       "bottomright",
       c("0.001", "0.01","0.1","1","10","100"),
       col=colors,bty="n",pch=1,lty = 1, cex=0.8, inset=c(-0.35,0))


plot(1:6,rep(0,6),ylim=c(min(err1_3)*0.9, max(err1_3)*1.1),type="n",
     xlab = "Cost",
     ylab = "Error Rate(%)",
     xaxt = "n",
     main="Missed Detection Rate")
Cost = c("0.001", "0.01","0.1","1","10","100")
axis(1,at=1:6,Cost)
sapply(1:6,function(x)points(1:6, err1_3[x,],type="b",col=colors[x]))
legend(title="Gamma",
       "bottomright",
       c("0.001", "0.01","0.1","1","10","100"),
       col=colors,bty="n",pch=1,lty = 1, cex=0.8, inset=c(-0.35,0))

#save all the results in acc3, err1_3, err2_3, square_error3

#narrow the range for the parameters cost and gamma---------------------------------------------

cost = seq(from = 10, to = 100, by = 10)
gamma = seq(from = 0.1, to = 1, by = 0.1)

acc = matrix(NA,nrow=length(cost),ncol = length(gamma))
err1 = matrix(NA,nrow=length(cost),ncol = length(gamma))
err2 = matrix(NA,nrow=length(cost),ncol = length(gamma))
square_error = matrix(NA,nrow=length(cost),ncol = length(gamma))

tr = 312
ahead = 3

cutoff <- 68.1

for (i in c(1: 10)){
  for (j in c(1: 10)){
    count = 1
    accuracy = 0
    error1 = 0
    error2 = 0
    square = 0
    
    while(count < 20000){
      train_x <- dataset_svm[count: (count + tr), 1: 10]
      train_y <- dataset_svm[(count + ahead): (count + tr + ahead), 11]
      train_set <- cbind(train_x, train_y)
      
      test_x <- dataset_svm[(count + tr + 1), 1: 10]
      test_y <- dataset_svm[(count + tr + ahead + 1), 11]
      
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
      square = square + (svm.pred - test_y)*(svm.pred - test_y)
      count = count + 5
      
    }
    acc[j,i] = unlist(accuracy)
    err1[j, i] = unlist(error1)
    err2[j, i] = unlist(error2)
    square_error[j, i] = unlist(square)
  }
}



plot(1:10,rep(0,10),ylim=c(min(square_error4)*0.9, max(square_error4)*1.1),type="n",xlab = "cost", main = "Square Loss")
sapply(1:10,function(x)points(1:10,square_error4[x,],type="b",col=colors[x]))
legend("topright",c("0.1", "0.2","0.3","0.4","0.5","0.6", "0.7", "0.8", "0.9", "1.0"),col=colors,bty="n",pch=1,lty = 1)


colors <- c("#A7A7A7","dodgerblue","firebrick","forestgreen","gold","black","brown",6,4, 3)
plot(1:10,rep(0,10),ylim=c(min(acc4)*0.9, max(acc4)*1.1),type="n",
     xlab = "Cost",
     ylab = "Error Rate(%)",
     xaxt = "n",
     main="Overall Misclassification Rate")
Cost = c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100")
axis(1,at=1:10,Cost)
sapply(1:10,function(x)points(1:10,acc4[x,],type="b",col=colors[x]))
legend(title="Gamma",
       "bottomright",
       c("0.1", "0.2","0.3","0.4","0.5","0.6", "0.7", "0.8", "0.9", "1.0"), 
       col=colors,bty="n",pch=1,lty = 1, cex=0.8, inset=c(-0.35,0))


plot(1:10,rep(0,10),ylim=c(min(err1_4)*0.9, max(err1_4)*1.1),type="n",
     xlab = "Cost",
     ylab = "Error Rate(%)",
     xaxt = "n",
     main="Missed Detection Rate")
Cost = c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100")
axis(1,at=1:10,Cost)
sapply(1:10,function(x)points(1:10, err1_4[x,],type="b",col=colors[x]))
legend(title="Gamma",
       "bottomright",
       c("0.1", "0.2","0.3","0.4","0.5","0.6", "0.7", "0.8", "0.9", "1.0"), 
       col=colors,bty="n",pch=1,lty = 1, cex=0.8, inset=c(-0.35,0))

#save all the results in acc4, err1_4, err2_4, square_error4

#model improvement-------------------------------------------------------------------------------
idx <- as.numeric(row.names(dataset_svm))%%288
data_imp <- dataset_svm[idx<=216 & idx >= 73,]

tr = 312
ahead = 3

count = 1
accuracy = 0
error1 = 0
error2 = 0
square = 0

while(count < 10000){
  train_x <- data_imp[count: (count + tr), 1: 10]
  train_y <- data_imp[(count + ahead): (count + tr + ahead), 11]
  train_set <- cbind(train_x, train_y)
  
  test_x <- data_imp[(count + tr + 1), 1: 10]
  test_y <- data_imp[(count + tr + ahead + 1), 11]
  
  svm.model <- svm(train_y~., data = train_set, cost=10, gamma=0.5)
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


plot(1:10,rep(0,10),ylim=c(min(err1_4)*0.9, max(err1_4)*1.1),type="n",
     xlab = "Cost",
     ylab = "Error Rate(%)",
     xaxt = "n",
     main="Missed Detection Rate")
Cost = c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100")
axis(1,at=1:10,Cost)
sapply(1:10,function(x)points(1:10, err1_4[x,],type="b",col=colors[x]))
legend(title="Gamma",
       "bottomright",
       c("0.1", "0.2","0.3","0.4","0.5","0.6", "0.7", "0.8", "0.9", "1.0"), 
       col=colors,bty="n",pch=1,lty = 1, cex=0.8, inset=c(-0.35,0))
