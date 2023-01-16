data=read.csv("parkinsons.csv")
library(caret)

#Task 1
set.seed(12345)
n = dim(data)[1]
id = sample(1:n, floor(n * 0.6))
train = data[id,]
test = data[-id,]

#Scale
params = preProcess(train)
trainS = predict(params, train)
testS = predict(params, test)

#Task 2
m1=lm(motor_UPDRS~., trainS)
pred=predict(m1)

mse_train=mean((trainS$motor_UPDRS-pred)^2)
mse_test=mean((testS$motor_UPDRS-pred)^2)

summary(m1)

#Task 3

loglik=function(theta, sigma, D=train) {
  n=dim(D)[1]
  Y=D[,1]
  X=as.matrix(D[,2:17])
  
  retval=-n/2*log(2*pi*sigma^2)
  retval=retval-1/(2*sigma^2)*sum((Y-X%*% theta)^2)
  
  return(retval)
}

ridgeFunction=function(params, lambda, useAllData=FALSE) {
  sigma=params[1]
  theta=params[-1]
  
  if(useAllData) {
    return(-loglik(theta, sigma, sourceData) + lambda*sum(theta^2))
  } else {
    return(-loglik(theta, sigma, train)+lambda*sum(theta^2))
  }
}

ridgeOpt=function(lambda, useAllData=FALSE) {
  result=optim(par=c(theta, sigma), fn=ridgeFuntion, lambda=lambda, useAllData=useAllData, method="BFGS")
  
  retval=list()
  retval$sigma=result$par[1]
  retval$w=result$par[-1]
  retval$value=result$value
  
  return(retval)
}

dfRidge=function(lambda, useAllData=FALSE) {
  if(useAllData) {
    X=as.matrix(sourceData[,2:17])
  } else {
    X=as.matrix(train[,2:17])
  }
  
  hatMatrix=X%*% solve(t(X)%*%X+lambda*diag(dim(X)[2])) %*% t(X)
  
  trace=sum(diag(hatMatrix))
  return(trace)
}

lambda=c(1,100,1000)

ridgeOpt(1)
