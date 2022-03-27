
# Plot the sigmoid function
library(ggplot2)
qplot(-10:10, 1/(1 + exp(-(-10:10))), geom=c("point", "line"), xlab="z", ylab="sigmoid function")

# read data
SAheart <- read.table("d:/SAheart.data", sep=",",head=T,row.names=1)

x <- matrix(SAheart$ldl[1:100])
y <- matrix(SAheart$chd[1:100])

# Normalize the feature variable
x <- (x - mean(x)) / sd(x)

# Cost function
sigmoid <- function(X,beta){
  return(1/(1+exp(X %*% beta)))
}

costFunction <- function(X,y,beta){
  return((t(-y)%*%log(sigmoid(X,beta)) - t(1-y)%*%log(1-sigmoid(X,beta)))/nrow(X))
}

# Gradient ascent
gradientAscent <- function(X, y, alpha, iterations, epsilon) {
  # Initialize the parameter value and calculate the gradient
  X <- cbind(rep(1, nrow(X)), X)
  y <- y
  cost <- c(0:0)
  beta <- matrix(rep(0, 2), nrow=2)
  allBeta <- beta
  count <- 0
  # Update the parameter value
  for (i in 1:iterations) {
    beta <- allBeta[,i]+alpha*(t(X)%*%((sigmoid(X,allBeta[,i]))-y))/nrow(X)
    cost[i] <- costFunction(X,y,allBeta[,i])
    allBeta <- cbind(allBeta, beta)
    count <- count + 1
    # Check whether gradient ascent has converged
    if(all(abs(allBeta[,i+1] - allBeta[,i]) < epsilon)) break 
  }
  return(list(allbeta=allBeta,Beta=allBeta[,ncol(allBeta)],cost=cost,count=count))
}

# Choose a value of the learning rate
eta <- c(10,1,0.1,0.01,0.001,1e-4)
par(mfrow=c(2,3))
for (i in 1:length(eta)){
  test <- gradientAscent(x,y,eta[i],1000,1e-5)
  plot(seq(1,unlist(test$count)),test$cost,xlab="iterations",ylab="cost",type="l")
}

# Predict the labels for a set of test examples
finalResult <- gradientAscent(x,y,1,1000,1e-5)
prediction <- function(X,finalBeta){
  X <- cbind(rep(1, nrow(X)), X)
  predictChd <- sigmoid(X,finalBeta)
  return(predictChd) 
}

a <- matrix(SAheart$ldl[100:300])
b <- matrix(SAheart$chd[100:300])
c = cbind(prediction(a,finalResult$Beta),b)
