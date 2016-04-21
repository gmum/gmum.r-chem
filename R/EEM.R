EEMObject <- function(s, b){
  EEM <- list(
    sigma = s,
    beta = b
  )
  class(EEM) <- append(class(EEM),"EEM")
  return(EEM)
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
  result <- (X-mean(X))/sd(X)
  return(result)
}

dataY <- function(n){
  y <- c()
  for(i in 1:n){
    extra <- sample(1:2, 1, replace = TRUE)
    y <- append(y, extra)
  }
  return(y)
}

sigmoid <- function(X, W, b){
  result <- 1./(1.+exp((X%*%t(W))-b))
  return(result)
}

pdf <- function(X, i, sigma){
  mean <- mean(X)
  result <- 1./(sqrt(2*pi*sigma[i])*exp((-1)*((X - mean)^2)/(2*sigma[i])))
  return(result)
}

hidden_init <- function(X, h){
  current_h <- max(1, min(h, dim(X)[1]))
  W <- matrix(sample(-1:1, size = current_h*dim(X)[2], replace = TRUE),
              nrow = current_h, ncol = dim(X)[2])
  b <- rnorm(current_h)
  return(list(W,b))
}

EEM <- function(X, y, h){
  hid <- hidden_init(X, h)
  Wprim <- unlist(hid[1])
  W <- matrix(Wprim, ncol = sqrt(length(Wprim)), nrow = sqrt(length(Wprim))) 
  b <- unlist(hid[2])
  H <- sigmoid(X,W,b)
  labels <- array(NA,0)
  labels <- append(labels,c(min(y),max(y)))
  m <- c()
  sigma <- c()
  
  for(i in 1:2){
    data <- matrix()
    data <- matrix(H[y==labels[i]], ncol = length((H[y==labels[i]])))
    m[i] <- mean(data)
  #  LW covariance estimation
    sigma[i] <- tawny::cov.shrink(data)
  }
  #Moore-Penrose pseudo-inverse of a matrix
  beta <- MASS::ginv(sigma[1]+sigma[2]) %*% t(m[2]-m[1])
  
  for(i in 1:2){
    m[i] <- t(beta) %*% t(m[i])
    sigma[i] <- t(beta) %*% (sigma[i] %*% beta)
  }
  result <- EEMObject(sigma,beta)
  return(result)
}

predict <- function(X, W, b, beta){
  p <- sigmoid(X, W, b) %*% beta
}