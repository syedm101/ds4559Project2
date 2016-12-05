#Read data from local folder, everything as a character to ease factoring
bc<- read.csv("Wisconsin+BC+Data.txt", header=FALSE, colClasses = "character")
View(bc)
str(bc)

#Want to convert everything to a factor
bc$V1 <- as.factor(bc$V1)
bc$V2 <- as.factor(bc$V2)
bc$V3 <- as.factor(bc$V3)
bc$V4 <- as.factor(bc$V4)
bc$V5 <- as.factor(bc$V5)
bc$V6 <- as.factor(bc$V6)
bc$V7 <- as.factor(bc$V7)
bc$V8 <- as.factor(bc$V8)
bc$V9 <- as.factor(bc$V9)
bc$V10 <- as.factor(bc$V10)
bc$V11 <- as.factor(bc$V11)
str(bc)

#remove missing values
bc <- subset(bc, V7!="?")
#Set understandable column names
colnames(bc) <- c("id","clumpThickness", "cellSizeUniformity","cellShapeUniformity","marginalAdhesion","epithelialCellSize","bareNuclei","blandChromatin","normalNucleoli","mitoses","class")
View(bc)

#C5.0 Cross Validation code
library(cvTools)
k <- 10 #the number of folds
datasetC5 <- bc
set.seed(1234)
#randomize data
datasetC5_rand <- datasetC5[order(runif(683)),]

## Divide data into k folds:
folds <- cvFolds(NROW(datasetC5_rand), K=k)
#create predictions column to contain the probabilities for each instance
datasetC5_rand$holdoutpred <- rep(0,nrow(datasetC5_rand))

library(C50)
for(i in 1:k){
  train_dataC5 <- datasetC5_rand[folds$subsets[folds$which != i], ] #Set the training set
  validation_dataC5 <- datasetC5_rand[folds$subsets[folds$which == i], ] #Set the validation set
  
  newmod <- C5.0(class ~ .,data=train_dataC5,type="prob") #C5.0 model from non-kth fold
  newpred <- predict(newmod,newdata=validation_dataC5[,-11],type="prob") #Get the probabilities for the validation set (from the model just fit on the train data)
  
  datasetC5_rand[folds$subsets[folds$which == i], ]$holdoutpred <- newpred[,2] #Put the hold out probability in the data set for later use.
}
datasetC5_rand$holdoutpred
#2 is benign, 4 is malignant

library(ROCR)

#predictions for AUC calculation
predictions <- datasetC5_rand$holdoutpred
#true predictions for AUC calculation
labels <- datasetC5_rand[,11]

library(cvAUC)
#Create cross validated area under ROC curve
cvAUC(predictions,labels,label.ordering = NULL,folds)
#area = .9411

pred=prediction(predictions,labels)
perf_ROC=performance(pred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")

#Random Forest Code
library(randomForest)
set.seed(1234)

k <- 10 #the number of folds
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



