#DS 4559 Project 2
#Nathan Lin, Andrew Ton, Mansoor Syed

#Libraries needed
library(neuralnet)
library(clusterSim)

####Question 1####
#Import the data and assign meaningful attribute names

blood <- read.csv("BloodTransfusion.txt", stringsAsFactors = FALSE, header = TRUE)
colnames(blood) <- c("recency.mo", "frequency", "blood.cc", "time.mo", "donated.2007")
#Assign appropriate column names
#Make the classification column a factor
blood$donated.2007 <- as.factor(blood$donated.2007)

#Check attribute types
sapply(blood, class)

#Normalize each column of the data because of the wide range of numbers found in the dataset, particularly with the volume in cc's
blood2 <- cbind(data.Normalization(blood[-5], "n4", "column"), blood[,5])
colnames(blood2)[5] <- "donated.2007"

#Explanation
#This dataset contains a portion of the donor database for the Blood Transfusion Service Center in Taiwain. The center
#drives a bus to a university every 3 months to collect blood, and the dataset is used to build a variation of the RFM
#(Recency, Frequency, Monetary Value) marketing model in analyzing customer value. In this case, the "customer" is a blood
#doner. Blood donations are vital for emergency and elective surgeries (there was recently a shortage in the US), so identifying
#target doners and those who may deliver the best future value is important for a blood bank.

#Classification with a neural net. Modified the sample code provided
#Set a seed, determine the sample size, and establish the sample command
set.seed(1)
size.sample <- 571 #Sample size is currently 75% of the data
training.sample <- sample(1:nrow(blood2), size.sample)

# With training.sample, split the blood data into training and test sets
blood.train <- blood2[training.sample,]
blood.test <- blood2[-training.sample,]

# Give the data a more transparent name
nnet_blood.train <- blood.train

# Binarize the categorical output
nnet_blood.train <- cbind(nnet_blood.train, blood.train$donated.2007 == 0)
nnet_blood.train <- cbind(nnet_blood.train, blood.train$donated.2007 == 1)

#Rename the two columns of nnet_blood.train with their respective binary meanings
names(nnet_blood.train)[6] <- 'no'
names(nnet_blood.train)[7] <- 'yes'

#Check to make sure everything went out right
head(nnet_blood.train)

## Create neural network model
nnet <- neuralnet(no + yes ~ recency.mo + frequency + blood.cc + time.mo,
                data=nnet_blood.train, 
                hidden=c(1))

#Generate the neural net output plot
plot(nnet)

#Compute predictions on just the test set, rather than both training & test data
pred <- compute(nnet, blood.test[,-5])$net.result
 
# Put multiple binary output to categorical output
maxidx <- function(arr) {
  return(which(arr == max(arr)))
}
idx <- apply(pred, c(1), maxidx)
prediction <- c(0,1)[idx]

#View results in a confusion matrix
table(prediction, blood.test$donated.2007)

#Loop to generate a table with the accuracy for various numbers of nodes
accuracy.table <- data.frame(NULL)

for (i in 1:7){
  set.seed(1)
  nnet <- neuralnet(no + yes ~ recency.mo + frequency + blood.cc + time.mo,
                    data=nnet_blood.train, 
                    hidden=c(i))

  pred <- compute(nnet, blood.test[,-5])$net.result
  
  maxidx <- function(arr) {
    return(which(arr == max(arr)))
  }
  idx <- apply(pred, c(1), maxidx)
  prediction <- c(0,1)[idx]
  
  table(prediction, blood.test$donated.2007)
  accuracy.table[i,1] <- i
  accuracy.table[i,2] <- (table(prediction, blood.test$donated.2007)[1,1]+
                            table(prediction, blood.test$donated.2007)[2,2])/(table(prediction, blood.test$donated.2007)[1,1]+
                                                                                table(prediction, blood.test$donated.2007)[2,2] + 
                                                                                table(prediction, blood.test$donated.2007)[1,2]+
                                                                                table(prediction, blood.test$donated.2007)[2,1])
}

#Plot accuracy.table as a line graph to view progression
plot(accuracy.table, type = "b",
     xlab = "Number of Nodes in a Single Hidden Layer", ylab = "Accuracy/Performance on Test Data", 
     main = "Number of Nodes vs. Accuracy") 

#Our best performance is with 7 nodes. Note that we did try 8 nodes and with 8 nodes, the nnet did not converge.
#Overall, all the trials were witin a range of around 2.5% in terms of accuracy.