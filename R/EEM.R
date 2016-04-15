createObject <- function(W, b){
  newS3 <- list(
    array = W,
    bias = b
  )
  class(newS3) <- append(class(newS3),"hidden_layer")
  return(newS3)
}

#dataExample
dataX <- function(n,m){
  X <- c()
  for(i in 1:m){
    x1 <- c()
    for(j in 1:n){
      extra <- sample(-50:50, 1, replace = FALSE)
      x1 <- append(x1, extra)
    }
    X <- append(X,x1)
  }
  X <- matrix(cbind(X),n,m)
  return(X)
}2

dataY <- function(n){
  y <- c()
  for(i in 1:n){
    extra <- sample(-50:50, 1, replace = FALSE)
    y <- append(y, extra)
  }
  return(y)
}

sigmoid <- function(X, W, b){
  bi <- as.vector(b)
  part1 <- X%*%t(W)
  return(1./(1.+exp((part1)-b)))
}

pdf <- function(X, i, sigma, mean){
  result <- 1./(sqrt(2*pi*sigma[i])*exp((-1)*((X - mean)^2)/(2*sigma[i])))
  return(result)
}

hidden_init <- function(X){
  h <- sample(100,1)
  current_h <- max(1, min(h, dim(X)[1]))
  W <- matrix(sample(-50:50, size = current_h*dim(X)[2], replace = FALSE),
              nrow = current_h, ncol = dim(X)[2])
  b <- rnorm(current_h)
  result <- createObject(W, b)
  return(result)
}

fitEEM <- function(X,y){
  hid <- hidden_init(X)
  W <- hid$array
  b <- hid$bias
  H <- sigmoid(X,W,b)
  labels <- array(NA,0)
  labels <- append(labels,min(y))
  labels <- append(labels,max(y))
  m <- c()
  sigma <- c()
  
  for(i in 1:2){
    data <- matrix()
    data <- cbind(data,H[y==labels[i]])
    data <- data[-1]
    data <- cbind(data)
    m[i] <- mean(data)
    #LW covariance estimation
    #estymaja macierzy kowariancji
    sigma[i] <- tawny::cov.shrink(data)
  }
  #Moore-Penrose pseudo-inverse of a matrix
  beta <- MASS::ginv(sigma[1]+sigma[2]) %*% t(m[2]-m[1])
  
  for(i in 1:2){
    m[i] <- t(beta) %*% t(m[i])
    sigma[i] <- t(beta) %*% (sigma[i] %*% beta)
  }
}

predict <- function(X, W, b, beta){
  p <- sigmoid(X, W, b) %*% beta
  result <- 
}






