EEMObject <- function(s, b, W, bi, m){
  eem <- list(
    sigma = s,
    beta = b,
    array = W,
    bias = bi,
    mean = m
  )
  class(eem) <- append(class(eem),"EEM")
  return(eem)
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
  X <- (X-mean(X))/sd(X)
  result <- matrix(cbind(X),n,m)
  return(result)
}

dataY <- function(n){
  y <- c()
  for(i in 1:n){
    extra <- sample(1:2, 1, replace = TRUE)
    y <- append(y, extra)
  }
  y <- matrix(rbind(y))
  return(y)
}

sigmoid <- function(X, W, b){
  part <- X%*%t(W)
  for(i in 1:dim(part)[1]){
    for(j in 1:dim(part)[2]){
      part[i,j] <- part[i,j] - b[i]
    }
  }
  result <- 1./(1.+exp(part))
  return(result)
}

pdf <- function(X, i, sigma, mean){
  result <- 1./(sqrt(2*pi*sigma[i])*exp((-1)*((X - mean[i])^2)/(2*sigma[i])))
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
  W <- matrix(Wprim, ncol = dim(X)[2], nrow = h) 
  bprim <- unlist(hid[2])
  b <- matrix(bprim, ncol = 1, nrow = length(bprim))
  H <- sigmoid(X,W,b)
  labels <- array(NA,0)
  labels <- append(labels,c(min(y),max(y)))
  sigma <- c()
  mi <- c()
  for(i in 1:2){
    data <- matrix()
    m <- c()
    data <- matrix(H[y==labels[i],], nrow = length(H[y==labels[i],]),ncol = dim(H)[2])
    #prep mean for data - 1,2
    for(k in 1:dim(data)[2]){
      mn <- 0
      for(j in 1:dim(data)[1]){
        mn <- mn + data[j,k]
      }
      m <- append(m,mn/dim(data)[1])
    }
    for(j in 1:length(m)){
      data[,j] <- data[,j] - m[j]
    }
    #prep sum((xi - xm)%*%t(xi - xm))
    res_vec <- c()
    t_data <- t(data)
    for(j in 1:dim(data)[1]){
      c1 <- c(data[j,])
      c2 <- c(t_data[,j])
      il <- c1 %*% c2
      result_vector <- append(res_vec, il)
    }
    sigma[i] <- 1/(length(data)-1)*(sum(result_vector))
    mi <- append(mi,m)
  }
  mi <- matrix(mi, ncol = 2, nrow = length(mi)/2)
  beta <- matrix()
  beta <- MASS::ginv(sigma[1]+sigma[2]) %*% t(mi[,2]-mi[,1])
  m <- c()
  for(i in 1:2){
    c1 <- c(t(beta))
    c2 <- c(t(mi[,i]))
    c3 <- c(beta)
    m <- append(m,c1 %*% c2)
    res <- c(sigma[i] %*% c3)
    sigma[i] <- c(c1 %*% res)
  }
  result <- EEMObject(sigma,beta,W,b,m)
  return(result)
}

predict <- function(X, y, eem){
  W <- eem$array
  b <- eem$bias
  beta <- eem$beta
  pprim <- sigmoid(X, W, b)
  labels <- array(NA,0)
  labels <- append(labels,c(min(y),max(y)))
  p <- c(pprim %*% t(beta))
  pdf1 <- pdf(p, 1, eem$sigma, eem$mean)
  pdf2 <- pdf(p, 2, eem$sigma, eem$mean)
  pdf_mat <- matrix(rbind(pdf1, pdf2), ncol = length(pdf1))
  result <- c()
  for(i in 1:length(pdf1)){
    result <- append(result,which.max(pdf_mat[,i]))
  }
  return(labels[result])
}

final_result <- function(X, y, eem){
  prd <- predict(X,y,eem)
  counter <- 0
  for(i in 1:length(prd)){
    if(prd[i] == y[i]){
      counter <- counter+1
    }
  }
  return(counter/length(prd))
}