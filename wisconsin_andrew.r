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

colnames() <-
bc <- subset(bc, V7!="?")


library(randomForest)
library(ROCR)

## Create randomForest model
rf_output=randomForest(x=usedcars[,1:5], y=usedcars[,6], importance = TRUE, ntree = 1000, proximity=TRUE)
rf_output

## Calculate predictions in terms of votes (percentage of trees that voted for particular class)
predictions=as.vector(rf_output$votes[,2])
## Pred is comparing numerical percentages to binary responses.  This is what we need to create ROC curve
pred=prediction(predictions,usedcars[,6])

perf_AUC=performance(pred,"auc") #Calculate the AUC value
AUC=perf_AUC@y.values[[1]]

perf_ROC=performance(pred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))

## Explore with me;
perf_ROC
perf_AUC