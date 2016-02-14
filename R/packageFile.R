#object containing 3 coefficients
createObject <- function(a, b, c){
  
  newS3 <- list(
    first_arg = a,
    second_arg = b,
    third_arg = c
  )
  
  class(newS3) <- append(class(newS3),"createObject")
  return(newS3)
}

#dataExample
dataX <- function(){
  x1 <- c(2,5,1,7,-2,6,-7,-2,4,7,5,3,-3,5,8,2,8,2,9,5)
  x2 <- c(6,3,2,1,-2,-4,-3,6,2,5,1,7,-2,6,-7,-2,-4,1,1,3)
  X <- matrix(cbind(x1,x2),20,2)
  return(X)
}

dataY <- function(){
  y <- c(1,-4,6,3,-4,6,-8,1,-2,7,-2,8,5,3,8,-4,6,-4,2,1)
  return(y)
}

#Example data dim>2
data3dim <- function(){
  x1 <- c(2,5,1,7,-2,6,-7,-2,4,7,5,3,-3,5,8,2,8,2,9,5)
  x2 <- c(6,3,2,1,-2,-4,-3,6,2,5,1,7,-2,6,-7,-2,-4,1,1,3)
  y <- c(1,-4,6,3,-4,6,-8,1,-2,7,-2,8,5,3,8,-4,6,-4,2,1)
  X <- matrix(cbind(x1,x2,y),20,3)
  return(X)
}

#without time value
summary <- function(X, y, c){
  negative <- 0
  b0 <- c$first_arg
  b1 <- c$second_arg
  b2 <- c$third_arg
  positive <- 0
  x1 <- "x1"
  x2 <- "x2"
  b0prim <- round(b0,3)
  b1prim <- round(b1,3)
  b2prim <- round(b2,3)
  points <- y > (b0 + b1*X[,1] + b2*X[,2])
  for(i in 1:20){if(y[i]==TRUE){positive=positive+1}}
  negative <- length(X)/2 - positive
  formula <- paste(b0prim,"+",b1prim,x1,"+",b2prim,x2)
  time <- 0 #system.time(LR(data))
  sum_object <- matrix(c(positive,negative,time,formula), ncol<-1, nrow<-4, byrow = TRUE)
  colnames(sum_object) <- c("POSITIVE", "NEGATIVE", "TIME", "FORMULA")
  rownames(sum_object) <- c("VALUE")
  return(sum_object)
}

#LR, X - matrix X, y - vector y
LR <- function(X,y,include_bias){
  dim <- dim(X)
  if(dim[2] == 2){
    if(include_bias == TRUE){
      x0 <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
      X <- matrix(cbind(x0,X),length(x0),3)
    }
    X_trans <- t(X)
    m1 <- X_trans%*%X
    inv_m1 <- solve(m1)
    m2 <- inv_m1%*%X_trans
    m3 <- m2%*%y
    if(include_bias == TRUE){
      b0 <- m3[1]
      b1 <- m3[2]
      b2 <- m3[3]
    }
    else{
      b0 <- 0
      b1 <- m3[1]
      b2 <- m3[2]
    }
    result <- createObject(b0,b1,b2)
    return(result)
  }
  else print("Error")
}

#PLOT,X- matrix X, y- vector y, c- coefficients
plot.LR <- function(X, y, c){
  b0 <- c$first_arg
  b1 <- c$seoncd_arg
  b2 <- c$third_arg
  points <- y > (b0 + b1*X[,1] + b2*X[,2])
  plot3d(X[points==TRUE, 1], X[points==TRUE, 2], y[points==TRUE],  
         xlim = c(-10,10), ylim = c(-10,10), zlim = c(-10,10), xlab ="x1", 
         ylab = "x2", zlab = "y", col = "red")
  par(new=TRUE)
  plot3d(X[points==FALSE, 1], X[points==FALSE, 2], y[points==FALSE], 
         xlim = c(-10,10), ylim = c(-10,10), zlim = c(-10,10), xlab ="x1", 
         ylab = "x2", zlab = "y", col = "blue")
}