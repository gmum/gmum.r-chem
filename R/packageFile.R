createObject <- function(a, b){
  
  newS3 <- list(
    first_arg = a,
    second_arg = b
  )
  
  class(newS3) <- append(class(newS3),"createObject")
  return(newS3)
}

#data in matrix
dataExample <- function(){
  x <- c(2,5,1,7,-2,6,-7,-2,4,7,1,3,-3,5,8,2,8,1,9,0)
  y <- c(1,-4,6,3,-4,6,-8,1,-2,7,-2,8,5,3,8,-4,6,-4,2,1)
  data <- matrix(cbind(x,y),length(x),2)
  return(data)
}

#without time and formula
summary <- function(data,vector){
  negative <- 0
  a <- vector$first_arg
  b <- vector$second_arg
  positive <- 0
  y <- (data[,2]) > (a*data[, 1]+b)
  for(i in 1:20){if(y[i]==TRUE){positive=positive+1}}
  negative <- length(data)/2 - positive
  formula <- 0
  time <- 0 #system.time(LR(data))
  sum_object <- matrix(c(positive,negative,time,formula), ncol<-1, nrow<-4, byrow = TRUE)
  colnames(sum_object) <- c("POSITIVE", "NEGATIVE", "TIME", "FORMULA")
  rownames(sum_object) <- c("VALUE")
  return(sum_object)
}

#linear regression(time function not finished yet)
LR <- function(data){
  start.time <- Sys.time()
  n = length(data)/2
  x <- data[,1]
  y <- data[,2]
  sum_x = sum(x)
  sum_y = sum(y)
  sum_xy = 0
  x_square = 0
  for(i in 1:n){
    sum_xy = sum_xy + x[i]*y[i]
    x_square = x_square + (x[i])^2
  }
  b = (n*sum_xy - sum_x*sum_y)/(n*x_square - (sum_x)^2)
  y_d = sum_y/n
  x_d = sum_x/n
  a = y_d - b*x_d
  result <- createObject(a,b)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  #print(time.taken)
  return (result)
}

#plot
plotLR <-function(vector, data){
  a <- vector$first_arg
  b <- vector$second_arg
  points <- (data[,2]) > (a*data[,1]+b)
  plot(data[points==TRUE, 1], data[points==TRUE, 2], 
       xlim = c(-10,10), ylim = c(-10,10),xlab ="x", ylab = "y", col = "red")
  par(new=TRUE)
  plot(data[points==FALSE, 1], data[points==FALSE, 2],
       xlim = c(-10,10), ylim = c(-10,10), xlab ="", ylab = "", col = "blue")
  par(new=TRUE)
  curve(a*x+b, xlim = c(-10,10), ylim = c(-10,10), xlab = "", ylab = "", col = "black")
}
