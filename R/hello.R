createObject <- function(a, b){
  
  newS3 <- list(
    first_arg = a,
    second_arg = b
  )
  
  class(newS3) <- append(class(newS3),"createObject")
  return(newS3)
}

#tworzy macierz
dataExample <- function(){
  x <- c(2,5,1,7,-2,6,-7,-2,4,7,1,3,-3,5,8,2,8,1,9,0)
  y <- c(1,-4,6,3,-4,6,-8,1,-2,7,-2,8,5,3,8,-4,6,-4,2,1)
  data <- matrix(cbind(x,y),length(x),2)
  return(data)
}

#narazie zwraca tylko 2 z 4 wartosci
summary <- function(data,vector){
  red_c <- 0
  blue_c <- 0
  n <- length(data)/2
  a <- vector$first_arg
  b <- vector$second_arg
  color <- "green"
  for(i in 1:n){ # tak wiem, ze to kolorowanie jest tragiczne
    value = a*data[i] + b
    if(value > data[i,2]){color <- "red"}
    else if(value <= data[i,2]){color <- "blue"}
    if(color == "red"){red_c <- red_c + 1}
    else if(color == "blue"){blue_c <- blue_c + 1}
  }
  formula <- 0
  time <- 0 #system.time(LR(data))
  sum_object <- matrix(c(red_c,blue_c,time,formula), ncol<-1, nrow<-4, byrow = TRUE)
  colnames(sum_object) <- c("RED", "BLUE", "TIME", "FORMULA")
  rownames(sum_object) <- c("VALUE")
  return(sum_object)
}

#przyjmuje macierz
LR <- function(data){
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
  return (result)
}

#powyrzucane niepotrzebne rzeczy, ale nie ma jeszcze kolorowania pkt
mojPlot <-function(vector, data){
  a <- vector$first_arg
  b <- vector$second_arg
  n <- length(data)/2
  df <- data.frame(data[,1],data[,2])
  plot(df, xlim = c(-10,10), ylim = c(-10,10), xlab ="x", ylab = "y", col = "red")
  par(new=TRUE)
  curve(a*x+b, xlim = c(-10,10), ylim = c(-10,10), xlab = "", ylab = "", col = "green")
}
