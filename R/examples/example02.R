#random dataset
n <- sample(1:10,1)
m <- sample(1:16,1)

X <- c()
y <- c()

for(i in 1:m){
  x1 <- c()
  for(j in 1:n){
    extra <- sample(-50:50, 1, replace = FALSE)
    x1 <- append(x1, extra)
  }
  X <- append(X,x1)
  y <- append(y,sample(1:2, 1))
}
X <- (X-mean(X))/sd(X)
X <- matrix(X, nrow = n, byrow = TRUE)

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

h <- sample(1:10,1)

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