#example dataset
d1 <- c(1,2,3,3,4,5) 
X <- matrix(d1, ncol = 2)
X <- t(X)

d2 <- c(1,2)
y <- matrix(d2, nrow = 2)
h <- 4

C <- NULL
C <- 10000

seed <- NULL
seed <- 0

eem <- EEM(X, y, h, C, seed)
print(eem)
print(predict(X, y, eem))
print(predict_proba(X, eem))