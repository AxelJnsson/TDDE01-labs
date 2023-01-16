data=read.csv("tecator.csv")
data$Sample=c()
data$Moisture=c()
data$Protein=c()


n = dim(data)[1]
set.seed(12345)
id = sample(1:n, floor(n * 0.6))
train = data[id,]
test = data[-id,]

#Task 1
set.seed(12345)
m1=lm(Fat~., train)

pred=predict(m1, train)
pred1=predict(m1, test)
mse_train=mean((train$Fat-pred)^2)
mse_test=mean((test$Fat-pred1)^2)

#Task 3
library(glmnet)
features=as.matrix(train[,-101])
target=train[,101]

set.seed(12345)
lasso_model=glmnet(features, target, alpha=1, family="gaussian")

plot(lasso_model, xvar="lambda")
print(lasso_model)

#Task 4
set.seed(12345)
lasso_model1=glmnet(features, target, alpha=0, family="gaussian")

plot(lasso_model1, xvar="lambda")
print(lasso_model1)

#Task 5
lasso_model_cv=cv.glmnet(features, target, alpha=1, family="gaussian")

plot(lasso_model_cv, xvar="lambda")
lambda_min=lasso_model_cv$lambda.min
summary(lasso_model_cv)

lasso_final=glmnet(features, target, family="gaussian", alpha=1, lambda=lambda_min)
Pred_final=predict(lasso_final, as.matrix(test[,-101]))

plot(test$Fat, Pred_final, col=2, pch=19, main="Scatter plot", xlab="Original test values", 
     ylab="Predicted values", xlim=c(0,60), ylin=c(0,60))
abline(0,1)
