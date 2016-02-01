# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

createObject <- function(a, b){
  
  newS3 <- list(
    first_arg = a,
    second_arg = b
  )
  
  class(newS3) <- append(class(newS3),"createObject")
  return(newS3)
}

createObjectPoint <- function(x,y,z){
  newObject <- list(
    x_crd = x,
    y_crd = y,
    color = z
  )
  
  class(newObject) <- append(class(newObject),"createObjectPoint")
  return(newObject)
}

dataExample <- function(){
  x = c(2,5,1,7,-2,6,-7,-2,4,7,1,3,-3,5,8,2,8,1,9,0)
  y = c(1,-4,6,3,-4,6,-8,1,-2,7,-2,8,5,3,8,-4,6,-4,2,1)
  data = c(x,y)
  return(data)
}

pointCoordinates <- function(data,vector){
  n = length(data)/2
  points = c()
  for(i in 1:n){
    value = vector$first_arg * data[i] + vector$second_arg
    if(value > data[n+i]){
      color = "red"
    }
    if(value <= data[n+i]){
      color = "blue"
    }
    point <- createObjectPoint(data[i],data[n+i],color)
    points = c(points, point)
  }
  return(points)
}

printPoints <- function(data){
  n = length(data)/2
  x_arr = c()
  y_arr = c()
  for(i in 1:n){
    x_arr = c(x_arr, data[1+(2*(i-1))])
    y_arr = c(y_arr, data[2+(2*(i-1))])
  }
  df <- data.frame(x_arr, y_arr)
  return (df)
}

LR <- function(data){
  n = length(data)/2
  x = data[seq(1,n)]
  y = data[seq(n+1,2*n)]
  sum_x = sum(x)
  sum_y = sum(y)
  iloczyn = 0
  kwadraty = 0
  for(i in 1:n){
    iloczyn = iloczyn + x[i]*y[i]
    kwadraty = kwadraty + (x[i])^2
  }
  b = (n*iloczyn - sum_x*sum_y)/(n*kwadraty - (sum_x)^2)
  y_d = sum_y/n
  x_d = sum_x/n
  a = y_d - b*x_d
  result <- createObject(a,b)
  return (result)
}

mojPlot <-function(vector, data){
  a = vector$first_arg
  b = vector$second_arg
  n = length(data)/3
  redPoints = c()
  bluePoints = c()
  for(i in 1:n){
    point = c(data[1+(3*(i-1))], data[2+(3*(i-1))])
    if(data[3*i] == "red"){
      redPoints = c(redPoints, point)
    }
    if(data[3*i] == "blue"){
      bluePoints = c(bluePoints, point)
    }
  }
  plot(printPoints(redPoints), col = "red")
  par(new=TRUE)
  #plot(printPoints(bluePoints), col = 'blue')
  lines(curve(a*x+b), col="green")
}
