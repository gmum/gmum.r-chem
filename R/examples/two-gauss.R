#Gaussian dataset
n <- 100
n <- sample(1:150,1)

m <- 2
m <- sample(1:10,1)

X <- c()
y <- c()

p1 <- n/2
p2 <- n/2

if(n %% 2 == 1){
  p1 <- n/2 + 0.5
  p2 <- n/2 - 0.5
}

for(i in 1:p1){
  x <- c()
  for(j in 1:m){
    extra <- sample(0:20, 1, replace = FALSE)
    x <- append(x, extra)
  }
  X <- append(X, x)
  y <- append(y,1)
}
for(i in 1:p2){
  x <- c()
  for(j in 1:m){
    extra1 <- sample(30:50, 1, replace = FALSE)
    x <- append(x, extra1)
  }
  X <- append(X, x)
  y <- append(y,2)
} 

X <- matrix(X, nrow = n, byrow = TRUE)
y <- matrix(y, nrow = n)

PM <- c(sample(1:n, n, replace = FALSE))
PM_X <- c()
PM_y <- c()

for(i in 1:n){
  row <- PM[i]
  PM_X <- append(PM_X, X[row,])
  PM_y <- append(PM_y, y[row])
}

X <- matrix(PM_X, nrow = n, byrow = TRUE)
y <- matrix(PM_y, nrow = n)

h <- sample(1:10, 1)

C <- NULL
C <- 10000

seed <- 777

eem <- EEM(X, y, h, C, seed)

write.csv(X, file = "R/examples/python_dt/matrix_X.csv", row.names=FALSE, quote=FALSE, col.names=FALSE)
write.csv(eem$bias, file = "R/examples/python_dt/bias.csv",  row.names=FALSE, quote=FALSE, col.names=FALSE)
write.csv(W, file = "R/examples/python_dt/matrix_W.csv", row.names=FALSE, quote=FALSE, col.names=FALSE)
write.csv(y, file = "R/examples/python_dt/matrix_Y.csv", row.names=FALSE, quote=FALSE, col.names=FALSE)

p1 <- predict(X, y, eem)
p2 <- predict_proba(X, eem)
p3 <- predict_accuracy(p1)
p4 <- predict_proba_accuracy(p2)

resultFile <- "result.txt"
cat("Predict_accuracy result: ", file = resultFile, append = FALSE)
cat(p3, file = resultFile, append = TRUE, sep = "\n")