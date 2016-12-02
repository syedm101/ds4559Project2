## Using the "neuralnet" package for categorization

## Load library and data

library(neuralnet)
data(iris)

## indicate sample size for splitting up training and testing data
set.seed(101)
size.sample <- 50

## Use "sample" command to randomize data and choose a subset to be the training data
iristrain <- iris[sample(1:nrow(iris), size.sample),] # get a training sample from iris
## Give the data a more transparent name
nnet_iristrain <- iristrain


# Binarize the categorical output
nnet_iristrain <- cbind(nnet_iristrain, iristrain$Species == 'setosa')
nnet_iristrain <- cbind(nnet_iristrain, iristrain$Species == 'versicolor')
nnet_iristrain <- cbind(nnet_iristrain, iristrain$Species == 'virginica')

## Notice,here, we are adding columns to iristrain, each with the name of a possible outcome value
names(nnet_iristrain)[6] <- 'setosa'
names(nnet_iristrain)[7] <- 'versicolor'
names(nnet_iristrain)[8] <- 'virginica'

## Observe how the commands from lines 19 through 26 have affected nnet_iristrain 
head(nnet_iristrain)

## Create neural network model (using one hidden layer with three nodes in this case)
nn <- neuralnet(setosa+versicolor+virginica ~ 
                  Sepal.Length+Sepal.Width
                +Petal.Length
                +Petal.Width,
                data=nnet_iristrain, 
                hidden=c(3))

## Take a gander at the network architecture and notice how it difference from the topology of the "concrete"
## topology, which was a numerical prediction problem.
plot(nn)

## Compute predictions on entire iris dataset, including training (We can separate out the training data
## out later, but let's just include it for our purposes)

mypredict <- compute(nn, iris[,-5])$net.result

# Put multiple binary output to categorical output
maxidx <- function(arr) {
  return(which(arr == max(arr)))
}
idx <- apply(mypredict, c(1), maxidx)
prediction <- c('setosa', 'versicolor', 'virginica')[idx]

## View your results
table(prediction, iris$Species)









