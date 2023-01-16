library(caret)
library(matlib)
library(ggplot2)
#Task 1
data = read.csv('communities.csv', header=TRUE) #Header is true because we have correct column names
set.seed(12345)

df = as.data.frame(data)
df1= df[, -ncol(df)]
params=preProcess(df1)
df1S = predict(params, df1) #Scale data except last column
dfM = data.matrix(df1S)
S=(1/ncol(df1S))*t(dfM)%*%(dfM) #Covariance matrix
S = as.data.frame(S)

e = eigen(S) 
#Eigen values
values = e$values #Eigen values of S
#Sum of values
sum = sum(values) #Sum of all eigen values
percent = 0
count = 0
for (i in values) { #For.loop to calculate how many PCs are needed for 95% variance
  percent = percent + i/sum
  count = count + 1
  if (percent > 0.95)
    break
}
count #35 components are needed for variance of at least 95%

allVariance = values/sum
PCOneTwo = allVariance[1:2]
PCOneTwo  #Proportion of variance for first two is 25% and 17%

#Task 2
res = princomp(df1S) #Princomp function to get our PCs
#Test to see that we have the same values as above
lambda=res$sdev^2
sprintf("%2.3f",lambda/sum(lambda)*100)
screeplot(res)
scores = as.data.frame(res$scores[,1:2])
###################################################
U = res$loadings #MAtrix of eigenvectors
U

dfg = data.frame("x" = c(scores$Comp.1), "y"=c(scores$Comp.2), "Crimes" = c(df$ViolentCrimesPerPop)) #Data frame with comp 1, comp 2 and violent crimes
ggplot(dfg, aes(x=x, y=y, col=Crimes)) +
  geom_point() +
  labs(colour = "Violent Crimes", x="Comp. 1", y="Comp. 2", title="PC Scores") #Plot with PC-scores colored by Violent crimes

plot(U[,1], main="Traceplot for PC1", ylab="PC1") #Traceplot for PC1

eigen_vectors = res$loadings[,1] #Get the eigen vectors from princomp
ordered_vectors = eigen_vectors[order(abs(eigen_vectors),decreasing = TRUE)] #Order the vectors in decreasing order
top_five = ordered_vectors[1:5] #Pick out the top five vectors
top_five
plot(abs(ordered_vectors),main="Ordered contribution to PC1", ylab="Contribution to PC1") #Plot all eigen vectors in decreasing order


#Task 3
data = read.csv('communities.csv', header=TRUE) #Header is true because we have correct column names

n = nrow(data)
set.seed(12345)
id = sample(1:n, floor(n*0.5))
train= data.frame(data[id,]) #Split data
test = data.frame(data[-id,]) #Split data

params = preProcess(train)
trainS = predict(params, train) #Scale train
testS = predict(params, test) #Scale test

m1 = lm(ViolentCrimesPerPop ~ ., data=trainS) #Train model
summary(m1)

p1 = predict(m1, trainS, type="response")
p2 = predict(m1, testS, type="response")

MSEtest = mean((p2 - testS$ViolentCrimesPerPop)^2) #Mean square error for test data
MSEtest

MSEtrain = mean((p1 - trainS$ViolentCrimesPerPop)^2) #Mean square error for train data
MSEtrain

#Task 4
TestE=rep(0,0) #Vector
TrainE=rep(0,0) #Vector
k=0

costFunction = function(theta, train, test, p1, p2) {
  trainHat = train %*% theta #Multiply scaled train data with theta
  testHat = test %*% theta #Multiply scaled test data with theta
  
  MSETrain = mean((p1- trainHat)^2) #Calculate MSE for train
  MSETest = mean((p2 - testHat)^2) #Calculate MSE for test
  
  .GlobalEnv$k= .GlobalEnv$k+1
  .GlobalEnv$TrainE[[k]]=MSETrain #Add MSE for train in vector
  .GlobalEnv$TestE[[k]]=MSETest #Add MSE for test in vector
  
  return(MSETrain)
  
}

n = ncol(trainS)-1 #Nr of columns except intercept
trainX = as.matrix(trainS[,1:n])
testX = as.matrix(testS[,1:n])
theta = as.matrix(rep(0,n)) #Theta=0, repeat 0 n times

p1 = as.matrix(trainS[c('ViolentCrimesPerPop')]) #Values of ViolentCrimesPerPop for training data
p2 = as.matrix(testS[c('ViolentCrimesPerPop')]) #Values of ViolentCrimesPerPop for test data

set.seed(12345)
res = optim(par=theta, fn=costFunction, train = trainX, test = testX, p1 = p1, p2 = p2, method = "BFGS") #Optimize cost function

TrainOptMSE = res$value #MSE for training data
TrainOptMSE
TestOptMSE = mean((p2 - (testX%*%res$par))^2) #MSE for test data
TestOptMSE

removedTrainData = TrainE[c(500:length(TrainE))] #Remove 500 iterations
removedTestData = TestE[c(500:length(TestE))] #Remove 500 iterations

TestMin = which.min(TestE) #Iteration with min value for test MSE
TrainMin = which.min(TrainE) #Iteration with min value for train MSE
TestMin
TrainMin


plot(removedTrainData, xlim=c(500, 10000), ylim=c(0.1,1.3), col = "black", main="MSE for training in black and test in red", xlab="Iterations 500 - 10 000",ylab="MSE")
points(removedTestData,pch=1, col="red")
points(which.min(TestE),TestE[which.min(TestE)], pch=19, col="blue") #Plot MSE for iterations





