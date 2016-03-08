#object containing 3 coefficients
createObject <- function(a, b, c){
  
  newS3 <- list(
    first_arg = a,
    second_arg = b,
    third_arg = c
  )
  
  class(newS3) <- append(class(newS3),"LR")
  return(newS3)
}

#dataExample
dataX <- function(n){
  x1 <- c()
  x2 <- c()
  for(i in 1:n){
    extra <- sample(-20:20, 1)
    x1 <- append(x1, extra+(extra %% 7))
    if(extra %% 2 == 1){extra <- extra*(-1)}
    x2 <- append(x2, extra+(extra %% 4))
  }
  X <- matrix(cbind(x1,x2),n,2)
  return(X)
}

#dataX <- function(){
#  x1 <- c(2,5,1,7,-2,6,-7,-2,4,7,5,3,-3,5,8,2,8,2,9,5)
#  x2 <- c(6,3,2,1,-2,-4,-3,6,2,5,1,7,-2,6,-7,-2,-4,1,1,3)
#  X <- matrix(cbind(x1,x2),20,2)
#  return(X)
#}

dataY <- function(n){
  y <- c()
  for(i in 1:n){
    extra <- sample(-20:20, 1)
    y <- append(y, extra+(extra %% 5))
  }
  return(y)
}
#dataY <- function(){
#  y <- c(1,-4,6,3,-4,6,-8,1,-2,7,-2,8,5,3,8,-4,6,-4,2,1)
#  return(y)
#}

#Example data dim>2
data3dim <- function(){
  x1 <- c(2,5,1,7,-2,6,-7,-2,4,7,5,3,-3,5,8,2,8,2,9,5)
  x2 <- c(6,3,2,1,-2,-4,-3,6,2,5,1,7,-2,6,-7,-2,-4,1,1,3)
  x3 <- c(1,-4,6,3,-4,6,-8,1,-2,7,-2,8,5,3,8,-4,6,-4,2,1)
  X <- matrix(cbind(x1,x2,x3),20,3)
  return(X)
}

#without time value
summary.LR <- function(c, X, y){
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
  for(i in 1:dim(X)[1]){if(points[i]==TRUE){positive=positive+1}}
  negative <- dim(X)[1] - positive
  formula <- paste(b0prim,"+",b1prim,x1,"+",b2prim,x2)
  time <- 0 #system.time(LR(data))
  part1 <- paste(" POSITIVE =",positive)
  part2 <- paste("NEGATIVE =",negative)
  part3 <- paste("TIME =",time)
  part4 <- paste("FORMULA =",formula)
  cat(part1,"\n",part2,"\n",part3,"\n",part4)
}
  
#LR, X - matrix X, y - vector y
LR <- function(X,y,include_bias){
  if(length(X)/2 == length(y)){
    op <- options(digits.secs = 3)
    options(op)
    begin <- Sys.time()
    if(dim(X)[2] == 2){
      if(include_bias == TRUE){
        x0 <- c()
        n <- dim(X)[1]
        for(i in 1:n){
          x0 <- append(x0,1)
        }
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
      end <- Sys.time()
      result_time <- as.integer((end - begin) * 1000)
      #return(result_time)
      return(result)
    }
    else{
      end <- Sys.time()
      result_time <- end - begin
      #return(result_time)
      print("dim X > 2!")
    }
  }
  else{
    print("length(X) != length(y)")
  }
}

#PLOT,X- matrix X, y- vector y, c- coefficients
#plot.LR <- function(c, X, y){
#  b0 <- c$first_arg
#  b1 <- c$second_arg
#  b2 <- c$third_arg
#  points <- y > (b0 + b1*X[,1] + b2*X[,2])
#  persp(X[points==TRUE, 1], X[points==TRUE, 2], y[points==TRUE],  
#         xlim = c(-10,10), ylim = c(-10,10), zlim = c(-10,10), xlab ="x1", 
#         ylab = "x2", zlab = "y", col = "red")
#  par(new=TRUE)
#  persp(X[points==FALSE, 1], X[points==FALSE, 2], y[points==FALSE], 
#         xlim = c(-10,10), ylim = c(-10,10), zlim = c(-10,10), xlab ="x1", 
#         ylab = "x2", zlab = "y", col = "blue")
#}

plot.LR <- function(c, X, y){
  b0 <- c$first_arg
  b1 <- c$second_arg
  b2 <- c$third_arg
  points <- y > (b0 + b1*X[,1] + b2*X[,2])
  #dla pierwszego rysuje wiecej, mimo ze jest +- tyle samo
  plot3d(X[points==FALSE, 1], X[points==FALSE, 2], y[points==FALSE],  
          xlim = c(-25,25), ylim = c(-25,25), zlim = c(-25,25), xlab ="x1", 
          ylab = "x2", zlab = "y", col = "red")
  plot3d(X[points==TRUE, 1], X[points==TRUE, 2], y[points==TRUE], 
         xlim = c(-10,10), ylim = c(-10,10), zlim = c(-10,10), xlab ="x1", 
         ylab = "x2", zlab = "y", col = "blue")
  planes3d(b1,b2,-1,b0, col = "grey")
}
