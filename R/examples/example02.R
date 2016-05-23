#random dataset
n <- sample(1:10,1)
m <- sample(1:16,1)

X <- dataX(n,m)
y <- dataY(n)

h <- sample(1:10,1)

C <- NULL
C <- 10000

seed <- NULL
seed <- 0

eem <- EEM(X, y, h, C, seed)

write(X, file = "R/examples/python_dt/matrix_X", ncolumns = dim(X)[2])
write(eem$matrix, file = "R/examples/python_dt/matrix_W", ncolumns = dim(eem$matrix)[2])
write(y, file = "R/examples/python_dt/matrix_Y", ncolumns = 1)

print(eem)
predict(X, y, eem)
predict_proba(X, eem)