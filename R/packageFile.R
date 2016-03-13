#object containing coefficients, matrix, vector and time taken to run LR function
createObject <- function(a, b, c, d){
  newS3 <- list(
    first_arg = a,
    second_arg = b,
    third_arg = c,
    fourth_arg = d
  )
  class(newS3) <- append(class(newS3),"LR")
  return(newS3)
}

#data [20][2]
generate.exampleData <- function(){
  x1 <- c(2,5,1,7,-2,6,-7,-2,4,7,5,3,-3,5,8,2,8,2,9,5)
  x2 <- c(6,3,2,1,-2,-4,-3,6,2,5,1,7,-2,6,-7,-2,-4,1,1,3)
  y <- c(1,-4,6,3,-4,6,-8,1,-2,7,-2,8,5,3,8,-4,6,-4,2,1)
  X <- matrix(cbind(x1,x2),20,2)
  result <- createObject(X,y,NULL,NULL)
  return(result)
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
}

dataY <- function(n){
  y <- c()
  for(i in 1:n){
    extra <- sample(-50:50, 1, replace = FALSE)
    y <- append(y, extra)
  }
  return(y)
}

show.LR <- function(c,X,y){
  return(summary.LR(c,X,y))
}

summary.LR <- function(c, ...){
  if(is.null(X)){
    X<- c$second_arg
  }
  b <- c$first_arg 
  time <- c$fourth_arg
  negative <- 0
  positive <- 0
  b0 <- b[1]
  b1 <- b[2]
  b2 <- b[3]
  points <- X[,2] > ((b1/b2)*X[,1]+(b0/b2))
  for(i in 1:dim(X)[1]){if(points[i]==TRUE){positive=positive+1}}
  negative <- dim(X)[1] - positive
  b0prim <- round(b0,3)
  b1prim <- round(b1,3)
  b2prim <- round(b2,3)
  if(b0 != 0){ #include_bias == TRUE
    formula <- paste(b0prim,"+",b1prim,"x1","+",b2prim,"x2")
  }
  else{ #include_bias == FALSE
    formula <- paste(b1prim,"x1","+",b2prim,"x2")
  }
  part1 <- paste(" POSITIVE =",positive)
  part2 <- paste("NEGATIVE =",negative)
  part3 <- paste("TIME =",time,"ms")
  part4 <- paste("FORMULA =",formula)
  cat(part1,"\n",part2,"\n",part3,"\n",part4)
}
 
#LR, X - matrix X, y - vector y
LR <- function(X,y,include_bias){
  if(dim(X)[1] == length(y)){
    op <- options(digits.secs = 3)
    options(op)
    begin <- Sys.time()
    X_prim <- X
    if(include_bias == TRUE){
      x0 <- c()
      n <- dim(X)[1]
      for(i in 1:n){
        x0 <- append(x0,1)
      }
      X_prim <- matrix(cbind(x0,X),length(x0),dim(X)[2]+1)
    }
    X_trans <- t(X_prim)
    m1 <- X_trans%*%X_prim
    inv_m1 <- solve(m1)
    m2 <- inv_m1%*%X_trans
    m3 <- m2%*%y
    b <- array(NA,0)
    if(include_bias == FALSE){
      b <- append(b,0)
    }
    for(i in 1:dim(m3)[1]){
      b <- append(b,m3[i])
    }
    end <- Sys.time()
    result_time <- as.integer((end - begin) * 1000)
    result <- createObject(b,X,y,result_time)
    return(result)
  }
  else{
    print("length(X) != length(y)")
  }
}

#PLOT, optional arguments: X- matrix x, y- vector y
plot.LR <- function(c, ...){
  if(is.null(X) && is.null(y)){
    X<- c$second_arg
    y<- c$third_arg
  }
  b <- c$first_arg
  if(dim(X)[2] == 2){
    b0 <- b[1]
    b1 <- b[2]
    b2 <- b[3]
    points <- X[,2] > (-1)*((b1/b2)*X[,1]+(b0/b2))
    plot(X[points==TRUE,1], X[points==TRUE,2],
         xlim = c(-65,65), ylim = c(-65,65),
         xlab = "X1", ylab = "X2", col = "red")
    par(new=TRUE)
    plot(X[points==FALSE,1], X[points==FALSE,2],
         xlim = c(-65,65), ylim = c(-65,65),
         xlab = "X1", ylab = "X2", col = "blue")
    par(new=TRUE)
    curve((-1)*(b1/b2)*x+(b0/b2), xlim = c(-65,65), ylim = c(-65,65),
          xlab = "X1", ylab = "X2")
  }
  else{
    print("dim(X) > 2")
  }
}