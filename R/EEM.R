#EEM Object contains: sigma, beta, W-matrix, bias, mean  
EEMObject <- function(s, b, W, bi, m){
    eem <- list(
      sigma = s,
      beta = b,
      matrix = W,
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
  result <- matrix(cbind(X),m,n)
  result <- t(result)
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
      part[i,j] <- part[i,j] - b[j]
    }
  }
  result <- 1./(1.+exp(part))
  return(result)
}

pdf <- function(X, i, sigma, mean){
  result <- 1./(sqrt(2*pi*sigma[i])*exp((-1)*((X - mean[i])^2)/(2*sigma[i])))
  return(result)
}

hidden_init <- function(X, h, seed){
  set.seed(seed)
  current_h <- max(1, min(h, dim(X)[1]))
  W <- matrix(sample(-1:1, size = current_h*dim(X)[2], replace = TRUE),
              nrow = current_h, ncol = dim(X)[2])
  b <- rnorm(current_h)
  return(list(W,b,current_h))
}

EEM <- function(X, y, h, C, seed){
  flag <- is.null(C)
  hid <- hidden_init(X, h, seed)
  Wprim <- unlist(hid[1])
  W <- matrix(Wprim, ncol = dim(X)[2], nrow = length(Wprim)/dim(X)[2]) 
  bprim <- unlist(hid[2])
  b <- matrix(bprim, ncol = 1, nrow = length(bprim))
  H <- sigmoid(X,W,b)
  current_h <- unlist(hid[3])
  labels <- array(NA,0)
  labels <- append(labels,c(min(y),max(y)))
  sigma <- c()
  mi <- c()
  sigma_res <- c()
  for(i in 1:2){
    data <- matrix()
    m <- c()
    data <- matrix(H[y==labels[i],], nrow = length(H[y==labels[i],])/dim(H)[2],ncol = dim(H)[2])
    
    for(j in 1:dim(data)[2]){
      sum <- 0
      for(k in 1:dim(data)[1]){
        sum <- sum + data[k,j]
      }
      m <- append(m, sum/dim(data)[1])
    }
    
    sigma <- cov(data)
    mi <- append(mi,m)
    if(flag == FALSE){
      D <- Matrix::Diagonal(current_h)
      D <- matrix(D, ncol = current_h, nrow = current_h)
      sigma_C <- D /(2.0*C)
      sigma_res <- append(sigma_res, sigma_C)
    }
    else{
      sigma_res <- append(sigma_res, sigma)
    }
  }
  
  len <- length(sigma_res)
  sigma1 <- matrix(sigma_res[1 : (len/2)], nrow = sqrt((len/2)), ncol = sqrt((len/2)))
  sigma2 <- matrix(sigma_res[(len/2)+1 : (len)], nrow = sqrt((len/2)), ncol = sqrt((len/2)))
  
  if(all(is.finite(sigma1)) && all(is.finite(sigma2))){
    mi <- matrix(mi, nrow = length(mi)/2)
    mi <- t(mi)
    m <- mi[2,] - mi[1,]
    m <- matrix(m, nrow = 1)
    sigma <- sigma1 + sigma2
    beta <- MASS::ginv(sigma) %*% t(m)
    
    mi <- t(mi)
    m <- c()
    c1 <- t(beta)
    c2 <- matrix(mi[,1], nrow = 1)
    c2 <- t(c2)
    c3 <- beta
    m <- append(m,c1 %*% c2)
    res <- sigma1 %*% c3
    sigma1 <- c1 %*% res
    
    c1 <- t(beta)
    c2 <- matrix(mi[,2], nrow = 1)
    c2 <- t(c2)
    c3 <- beta
    m <- append(m,c1 %*% c2)
    res <- sigma2 %*% c3
    sigma2 <- c1 %*% res
    
    sigma <- append(sigma1, sigma2)
    result <- EEMObject(sigma,beta,W,b,m)
    return(result)
  }
  else{
    print("Sigma is not finite!")
  }
}

predict <- function(X, y, eem){
  W <- eem$matrix
  b <- eem$bias
  beta <- eem$beta
  pprim <- sigmoid(X, W, b)
  labels <- array(NA,0)
  labels <- append(labels,c(min(y),max(y)))
  p <- pprim %*% beta
  pdf1 <- pdf(p, 1, eem$sigma, eem$mean)
  pdf2 <- pdf(p, 2, eem$sigma, eem$mean)
  pdf_mat <- matrix(rbind(pdf1, pdf2), ncol = 2)
  pdf_mat <- t(pdf_mat)
  result <- c()
  for(i in 1:length(pdf1)){
    result <- append(result,which.max(pdf_mat[,i]))
  }
  prd <- labels[result]
  counter <- 0
  for(i in 1:length(prd)){
    if(prd[i] == y[i]){
      counter <- counter+1
    }
  }
  return(counter/length(prd))
}

predict_proba <- function(X, eem){
  W <- eem$matrix
  beta <- eem$beta
  b <- eem$bias
  pprim <- sigmoid(X, W, b)
  p <- pprim %*% beta
  pdf1 <- pdf(p, 1, eem$sigma, eem$mean)
  pdf2 <- pdf(p, 2, eem$sigma, eem$mean)
  pdf <- c(pdf1, pdf2)
  result <- array(NA,0)
  result <- append(result, pdf)
  res_sum <- sum(result)
  return(result/res_sum)
}