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

write(X, file = "R/examples/python_dt/matrix_X", ncolumns = dim(X)[2])
write(eem$matrix, file = "R/examples/python_dt/matrix_W", ncolumns = dim(eem$matrix)[2])
write(y, file = "R/examples/python_dt/matrix_Y", ncolumns = 1)

print(eem)
print(predict(X, y, eem))
print(predict_proba(X, eem))