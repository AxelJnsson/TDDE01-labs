data=read.csv("optdigits.csv", header=FALSE)

#Task 1
#Train
n = dim(data)[1]
set.seed(12345)
id = sample(1:n,floor(n*0.5))
train = data[id,]

#Validation
id1 = setdiff(1:n,id)
set.seed(12345)
id2=sample(id1, floor(n*0.25))
valid = data[id2,]

#Test
id3 = setdiff(id1,id2)
test=data[id3,]

#Task 2
library(kknn)

#KKNN train
m1=kknn(as.factor(V65)~., train,train, k=30, kernel="rectangular")

pred=m1$fitted.values
conf_matr_train=table(train$V65, pred)
conf_matr_train

mse_train=1-sum(diag(conf_matr_train))/sum(conf_matr_train)

#KKNN test
m2=kknn(as.factor(V65)~., train,test, k=30, kernel="rectangular")

pred1=m2$fitted.values
conf_matr_test=table(test$V65, pred1)
conf_matr_test

mse_test=1-sum(diag(conf_matr_test))/sum(conf_matr_test)

#Task 3
m1_prob_raw=m1$prob
index_of_eigths=which(train$V65==8)
rows_8=as.matrix(m1_prob_raw[index_of_eigths,9])
foo=data.frame(index_of_eigths, rows_8)

library(dplyr)
library(tidyr)

foo_sorted=foo%>%arrange(rows_8)

n2=nrow(foo_sorted)

worst_training=train[foo_sorted[1:3,1],]
best_training=train[foo_sorted[(n2-1):n2,1],]

#Good
library(reticulate)

for(i in 1:2) {
  x=as.matrix(best_training[i,])
  x=subset(x,select=-c(V65))
  new_x=array_reshape(x,c(8,8))
  heatmap(new_x, Colv=NA, Rowv=NA)
}

for(i in 1:3) {
  x=as.matrix(worst_training[i,])
  x=subset(x,select=-c(V65))
  new_x=array_reshape(x,c(8,8))
  heatmap(new_x, Colv=NA, Rowv=NA)
}

#Task 4
K=seq(1,30,1)

trainScore=rep(0,30)
valScore=rep(0,30)

for(k in K) {
  mtr=kknn(as.factor(V65)~., train, train, k=k, kernel="rectangular", scale=TRUE)
  conf_matrix_train=table(train$V65, mtr$fitted.values)
  trainScore[k]=1-sum(diag(conf_matrix_train))/sum(conf_matrix_train)
  
  mva=kknn(as.factor(V65)~., train=train, test=valid, k=k, kernel="rectangular", scale=TRUE)
  conf_matrix_valid=table(valid$V65, mva$fitted.values)
  valScore[k]=1-sum(diag(conf_matrix_valid))/sum(conf_matrix_valid)
}

plot(trainScore, type="l", ylab="Misclass. error", xlab="K-value", col="red", ylim=c(0,0.06))
points(valScore, type="l", col="green")
legend('topleft', c('Train', 'Valid'), fill=c('red', 'green'))

#Task 5
cross_entropy=rep(0,30)

help_log=function(x) {
  return(-log(x+1e-15))
}

for(i in 1:30){
  fit_valid=kknn(as.factor(V65)~., train=train, test=valid, k=i, kernel="rectangular")
  
  for(j in 0:9) {
    probability=fit_valid[['prob']][which(valid[,65]==j),j+1]
    log_ce=sum(sapply(probability, help_log))
    cross_entropy[i]=log_ce+cross_entropy[i]
  }
}
plot(cross_entropy, type="l", col="red")
