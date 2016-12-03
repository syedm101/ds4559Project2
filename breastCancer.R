#Read data from local folder
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

bc <- subset(bc, V7!="?")
#Set understandable column names
colnames(bc) <- c("id","clumpThickness", "cellSizeUniformity","cellShapeUniformity","marginalAdhesion","epithelialCellSize","bareNuclei","blandChromatin","normalNucleoli","mitoses","class")
View(bc)

#C5.0 Cross Validation code
library(cvTools)
k <- 10 #the number of folds
datasetC5 <- bc
set.seed(1234)
datasetC5_rand <- datasetC5[order(runif(683)),]

## Divide data into k folds:
folds <- cvFolds(NROW(datasetC5_rand), K=k)

datasetC5_rand$holdoutpred <- rep(0,nrow(datasetC5_rand))

library(C50)
for(i in 1:k){
  train_dataC5 <- datasetC5_rand[folds$subsets[folds$which != i], ] #Set the training set
  validation_dataC5 <- datasetC5_rand[folds$subsets[folds$which == i], ] #Set the validation set
  
  newmod <- C5.0(class ~ .,data=train_dataC5) #C5.0 model from non-kth fold
  newpred <- predict(newmod,newdata=validation_dataC5[,-11]) #Get the predicitons for the validation set (from the model just fit on the train data)
  
  datasetC5_rand[folds$subsets[folds$which == i], ]$holdoutpred <- newpred #Put the hold out prediction in the data set for later use.
}
datasetC5_rand$holdoutpred

newmod$output

#2 is benign, 4 is malignant
table(datasetC5_rand$holdoutpred,datasetC5_rand$class)
# ~93 accuracy







