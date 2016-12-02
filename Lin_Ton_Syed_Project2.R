#DS 4559 Project 2
#Nathan Lin, Andrew Ton, Mansoor Syed

#Libraries needed
library(neuralnet)

####Question 1####
#Import the data and assign meaningful attribute names
blood <- read.csv("BloodTransfusion.txt", stringsAsFactors = FALSE, header = TRUE)
colnames(blood) <- c("recency.mo", "frequency", "blood.cc", "time.mo", "donated.2007")

#Make the classification column a factor
blood$donated.2007 <- as.factor(blood$donated.2007)

#Check attribute types
sapply(blood, class)

#Explanation
#This dataset contains a portion of the donor database for the Blood Transfusion Service Center in Taiwain. The center
#drives a bus to a university every 3 months to collect blood, and the dataset is used to build a variation of the RFM
#(Recency, Frequency, Monetary Value) marketing model in analyzing customer value. In this case, the "customer" is a blood
#doner. Blood donations are vital for emergency and elective surgeries (there was recently a shortage in the US), so identifying
#target doners and those who may deliver the best future value is important for a blood bank.

#Classification with a neural net. Modified the sample code provided
set.seed(1)
size.sample <- 561 #Sample size is currently 75% of the data

## Use "sample" command to randomize data and choose a subset to be the training data
blood.train <- blood[sample(1:nrow(blood), size.sample),] # get a training sample from iris
## Give the data a more transparent name
nnet_blood.train <- blood.train

# Binarize the categorical output
nnet_blood.train <- cbind(nnet_blood.train, blood.train$donated.2007 == 0)
nnet_blood.train <- cbind(nnet_blood.train, blood.train$donated.2007 == 1)

## Notice,here, we are adding columns to iristrain, each with the name of a possible outcome value
names(nnet_blood.train)[6] <- 'no'
names(nnet_blood.train)[7] <- 'yes'

head(nnet_blood.train)

## Create neural network model (using one hidden layer with three nodes in this case)
nnet <- neuralnet(no + yes ~ recency.mo + frequency + blood.cc + time.mo,
                data=nnet_blood.train, 
                hidden=c(1))

## Take a gander at the network architecture and notice how it difference from the topology of the "concrete"
## topology, which was a numerical prediction problem.
plot(nnet)

## Compute predictions on entire iris dataset, including training (We can separate out the training data
## out later, but let's just include it for our purposes)

pred.plustraining <- compute(nnet, blood[,-5])$net.result

# Put multiple binary output to categorical output
maxidx <- function(arr) {
  return(which(arr == max(arr)))
}
idx <- apply(pred.plustraining, c(1), maxidx)
prediction <- c(0,1)[idx]

## View your results
table(prediction, blood$donated.2007)
