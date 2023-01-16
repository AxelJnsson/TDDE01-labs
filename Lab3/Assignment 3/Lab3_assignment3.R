library(neuralnet)
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

#Task 1
set.seed(1234567890)
Var <- runif(500, 0, 10) #Sample 500 points uniformly random interval [0,10]
mydata <- data.frame(Var, Sin=sin(Var)) #Applying sine function to each point
tr <- mydata[1:25,] # Training
te <- mydata[26:500,] # Test
# Random initialization of the weights in the interval [-1, 1]
winit <- runif(31, -1, 1)
nn <- neuralnet(formula=Sin~Var, data=tr, hidden=10, startweights=winit)
# Plot of the training data (black), test data (blue), and predictions (red)
plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1],predict(nn,te), col="red", cex=1)

#Plot the neural network
plot.nnet(nn)

#Task 2
set.seed(1234567890)
Var <- runif(500, 0, 10) #Sample 500 points uniformly random interval [0,10]
mydata <- data.frame(Var, Sin=sin(Var)) #Applying sine function to each point
tr <- mydata[1:25,] # Training
te <- mydata[26:500,] # Test
# Random initialization of the weights in the interval [-1, 1]
winit <- runif(31, -1, 1)


#Linear function
linear <- function(x) x
nn <- neuralnet(formula=Sin~Var, data=tr, hidden=10, startweights=winit, act.fct=linear)
# Plot of the training data (black), test data (blue), and predictions (red)
plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1],predict(nn,te), col="red", cex=1)

#ReLu function
ReLu = function(x) max(0,x) #This should not work. Comment on why it does not.
nn <- neuralnet(formula=Sin~Var, data=tr, hidden=10, startweights=winit, act.fct=ReLu)
# Plot of the training data (black), test data (blue), and predictions (red)
plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1],predict(nn,te), col="red", cex=1)

#ReLu softplus
softplus = function(x) log(1+exp(x))
nn <- neuralnet(formula=Sin~Var, data=tr, hidden=10, startweights=winit, act.fct=softplus)
# Plot of the training data (black), test data (blue), and predictions (red)
plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1],predict(nn,te), col="red", cex=1)

#Task 3
set.seed(1234567890)
Var <- runif(500, 0, 50) #Sample 500 points uniformly random interval [0,50]
mydata <- data.frame(Var, Sin=sin(Var)) #Applying sine function to each point
tr <- mydata[1:25,] # Training
te <- mydata[26:500,] # Test
# Random initialization of the weights in the interval [-1, 1]
winit <- runif(31, -1, 1)
nn <- neuralnet(formula=Sin~Var, data=tr, hidden=10, startweights=winit)
# Plot of the training data (black), test data (blue), and predictions (red)
plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1],predict(nn,te), col="red", cex=1)
abline(-0.245,0)

#Task 4
nn$weights
nn
#Task 5
set.seed(1234567890)
Var <- runif(500, 0, 10) #Sample 500 points uniformly random interval [0,10]
mydata <- data.frame(Var, Sin=sin(Var)) #Applying sine function to each point
tr <- mydata[1:500,] # Training
# Random initialization of the weights in the interval [-1, 1]
winit <- runif(31, -1, 1)
nn <- neuralnet(formula=Var~Sin, data=tr, hidden=10, startweights=winit, threshold = 0.1) #We are predicting arcsin(x) i.e. the inverse of sin
# Plot of the training data (black), test data (blue), and predictions (red)
plot(tr, cex=2, ylim=c(-1,10))
points(tr[,1],predict(nn,tr), col="red", cex=1)


