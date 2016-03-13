#Compare results with another package (RGL)
library(rgl)
X <- dataX(113,2)
y <- dataY(113)

#packageFile's LR
c <- LR(X,y,TRUE)

#rgl's package LR
c_rgl <- lm(y~X[,1]+X[,2])

compareResult <- function(c,c_rgl){
  c <- c$coefs
  cat("packageFile result:", c)
  cat("rgl result:", c_rgl$coefficients[1:3])
}