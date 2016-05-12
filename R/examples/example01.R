#data
d1 <- c(1,3,2,4,3,5)
X <- matrix(d1, nrow = 2)
d2 <- c(1,2)
y <- matrix(d2, nrow = 2)
h <- 4

eem <- EEM(X, y, h)
print(eem)
print(predict(X, y, eem))
