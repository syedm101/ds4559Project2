bc <- read.csv("Wisconsin+BC+Data.txt", header=FALSE, colClasses="character")
bc[,1] <- as.factor(bc[,1])
bc[,2] <- as.factor(bc[,2])
bc[,3] <- as.factor(bc[,3])
bc[,4] <- as.factor(bc[,4])
bc[,5] <- as.factor(bc[,5])
bc[,6] <- as.factor(bc[,6])
bc[,7] <- as.factor(bc[,7])
bc[,8] <- as.factor(bc[,8])
bc[,9] <- as.factor(bc[,9])
bc[,10] <- as.factor(bc[,10])
bc[,11] <- as.factor(bc[,11])

bc <- subset(bc, V7!="?")
colnames(bc) <- c('id','Clump Thickness', 'Uniformity of Cell Size', 'Uniformity of Cell Shape', 'Marginal Adhesion', 'Single Epithelial Cell Size ', 'Bare Nuclei', 'Bland Chromatin ','Normal Nucleoli', 'Mitoses', 'Class')

library(randomForest)
library(ROCR)
set.seed(1234)

library(cvTools) #run the above line if you don't have this library

k <- 5 #the number of folds
dataset <- bc
dataset_rand <- dataset[order(runif(683)),]

## Divide data into k folds:

folds <- cvFolds(NROW(dataset), K=k)

dataset_rand$holdoutpred <- rep(0,nrow(bc))

for(i in 1:k){
  train_data <- dataset_rand[folds$subsets[folds$which != i], ] #Set the training set
  validation_data <- dataset_rand[folds$subsets[folds$which == i], ] #Set the validation set
  
  newmod <- randomForest(x=bc[,2:10], y=bc[,11], importance = TRUE, ntree = 1000, proximity=TRUE) #Get your new linear model (just fit on the train data)
  newpred <- predict(newmod,newdata=validation_data[,-11]) #Get the predicitons for the validation set (from the model just fit on the train data)
  
  dataset_rand[folds$subsets[folds$which == i], ]$holdoutpred <- newpred #Put the hold out prediction in the data set for later use.
}

table(dataset_rand$holdoutpred, dataset_rand[,11])
predictions=as.vector(newmod$votes[,2])
pred=prediction(predictions,bc[,11])

perf_AUC=performance(pred,"auc") #Calculate the AUC value
AUC=perf_AUC@y.values[[1]]
AUC
perf_ROC=performance(pred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))
