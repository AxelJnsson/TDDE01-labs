data=read.csv2("bank-full.csv", stringsAsFactors = TRUE)
data$duration=c()

#Task 1
#Train
n = dim(data)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.4))
train = data[id,]
#Valid
id1 = setdiff(1:n, id)
set.seed(12345)
id2 = sample(id1, floor(n*0.3))
valid = data[id2,]
#Test
id3 = setdiff(id1,id2)
test = data[id3,]

#Task 2
library(tree)
tree_default=tree(as.factor(y)~., data=train)

tree_smallest_node=tree(as.factor(y)~., data=train, minsize=7000)

tree_deviance=tree(as.factor(y)~., data=train, mindev=0.0005)

plot(tree_default)
plot(tree_smallest_node)
plot(tree_deviance)

#Misclass for default tree. Train and validation
y1_default=predict(tree_default, newdata=train, type="class")
y1=table(train$y, y1_default)

mce=1-sum(diag(y1))/sum(y1)

y2_default=predict(tree_default, newdata=valid, type="class")
y2=table(valid$y, y2_default)
mce=1-sum(diag(y2))/sum(y2)

#Misclass for minimum size tree, Train and validation
y1_node_size=predict(tree_smallest_node, newdata=train, type="class")
y1=table(train$y, y1_node_size)

mce=1-sum(diag(y1))/sum(y1)

y2_node_size=predict(tree_smallest_node, newdata=valid, type="class")
y2=table(valid$y, y2_node_size)
mce=1-sum(diag(y2))/sum(y2)

#Misclass for minimum deviance 0.0005. Train and validation
y1_dev=predict(tree_deviance, newdata=train, type="class")
y1=table(train$y, y1_dev)

mce=1-sum(diag(y1))/sum(y1)

y2_dev=predict(tree_deviance, newdata=valid, type="class")
y2=table(valid$y, y2_dev)
mce=1-sum(diag(y2))/sum(y2)

#Task 3
trainScore=rep(0,50)
validScore=rep(0,50)

trainScore[1]=1
trainScore[2]=1
validScore[1]=1
validScore[2]=1

for (i in 2:50) {
  prunedTree=prune.tree(tree_deviance, best=i)
  pred_valid=predict(prunedTree, newdata=valid, type="tree")
  trainScore[i]=deviance(prunedTree)/dim(train)[1]
  validScore[i]=deviance(pred_valid)/dim(valid)[1]
}

#Plot graph for deviance
plot(2:50, trainScore[2:50], type="b", col="red",
     ylim=c(0.5,0.65))
points(2:50, validScore[2:50], type="b", col="blue")

which.min(validScore[1:50])

#Optimal tree
best_pruned_tree=prune.tree(tree_deviance, best=27)
plot(best_pruned_tree)

#Task 4
pred=predict(best_pruned_tree, newdata=test, type="class")
conf_matrix=table(test$y, pred)
conf_matrix

#Accuracy of model
acc=sum(diag(conf_matrix))/sum(conf_matrix)

#F1 score
TP=conf_matrix[2,2]
TN=conf_matrix[1,1]
FN=conf_matrix[2,1]
FP=conf_matrix[1,2]

recall=TP/(TP+FN)
precision=TP/(TP+FP)

F1=2*(precision*recall)/(precision+recall)
F1

#Task 5
#Applying the loss matrix to the prediction of the optimal tree
prediction_test_loss = predict(best_pruned_tree, newdata = test, type ="vector")
prediction_test_loss[,2] = prediction_test_loss[,2]*5
final_pred = apply(prediction_test_loss, 1, function(row) if (row[2]>row[1]) 'yes' else 'no')
final_pred_factor = as.matrix(factor(final_pred))
#Create confusion matrix for the optimal deviance tree with the applied loss matrix
conf_matrix_loss = table(test$y, final_pred_factor)
conf_matrix_loss
#Calculate the accuracy of the model
acc_loss = sum(diag(conf_matrix_loss))/sum(conf_matrix_loss)
acc_loss
#Calculate the F1 score for the model
TP_loss = conf_matrix_loss[2,2]
TN_loss = conf_matrix_loss[1,1]
FN_loss = conf_matrix_loss[2,1]
FP_loss = conf_matrix_loss[1,2]
recall_loss = TP_loss/(TP_loss+FN_loss)
precision_loss = TP_loss/(TP_loss+FP_loss)
F1_loss = 2*(precision_loss*recall_loss)/(precision_loss+recall_loss)
F1_loss

#Task 6
#Logistic model is trained, and lists for true/false positive rates are created
ml = glm(as.factor(y)~., data = train, family = "binomial")
logistic_pred = as.matrix(predict(ml, newdata = test, type = "response"))
logistic_list = list()
logistic_list$tpr = rep(0,19)
logistic_list$fpr = rep(0,19)
#Tree model is trained, and lists for true/false positive rates are created
prediction_test_pi = predict(best_pruned_tree, newdata = test, type ="vector")
tree_list = list()
tree_list$tpr = rep(0,19)
tree_list$fpr = rep(0,19)
#
for (i in 1:19) {
  pi = 0.05*i
  #Data for ROC curve for logistic model is calculated with the principle "yes if p(Y='yes|X)>p
  pred_logistic = sapply(logistic_pred[,1], function(prob) {if (prob>pi) 'yes' else 'no'})
  factor_logistic = factor(pred_logistic, levels = c('no', 'yes'))
  conf_matrix_logistic = table(test$y, factor_logistic)
  conf_matrix_logistic
  logistic_list$tpr[i] = conf_matrix_logistic[2,2]/sum(conf_matrix_logistic[2,])
  logistic_list$fpr[i] = conf_matrix_logistic[1,2]/sum(conf_matrix_logistic[1,])
  #Data for ROC curve for tree model is calculated with the principle "yes if p(Y='yes|X)>p
  pred_tree_pi = sapply(prediction_test_pi[,2], function(prob) {if (prob>pi) 'yes' else 'no'})
  factor_tree = factor(pred_tree_pi, levels = c('no', 'yes'))
  conf_matrix_tree = table(test$y, factor_tree)
  conf_matrix_tree
  tree_list$tpr[i] = conf_matrix_tree[2,2]/sum(conf_matrix_tree[2,])
  tree_list$fpr[i] = conf_matrix_tree[1,2]/sum(conf_matrix_tree[1,])
}
#ROC curves are plotted
plot(tree_list$fpr, tree_list$tpr, type= 'b', col = 'green',
     xlim = c(0, 0.8), ylim = c(0, 1), xlab = 'FPR', ylab = 'TPR')
points(
  logistic_list$fpr, logistic_list$tpr, type= 'b', col = 'red'
)
