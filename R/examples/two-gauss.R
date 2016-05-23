#Gauss dataset
n <- sample(1:150,1)
m <- sample(1:10,1)

X <- c()

p1 <- n/2
p2 <- n/2

if(n %% 2 == 1){
  p1 <- n/2 + 1
}

for(i in 1:p1){
  x <- c()
  for(j in 1:m){
    extra <- sample(0:20, 1, replace = FALSE)
    x <- append(x, extra)
  }
  X <- append(X, x)
}
for(i in 1:p2){
  x <- c()
  for(j in 1:m){
    extra1 <- sample(30:50, 1, replace = FALSE)
    x <- append(x, extra1)
  }
  X <- append(X, x)
} 
X <- matrix(X, nrow = m, ncol = n)
X <- t(X)

y <- dataY(n)

h <- sample(1:10, 1)

C <- NULL
C <- 10000

seed <- NULL
seed <- sample(1:1000, 1)

eem <- EEM(X, y, h, C, seed)

write(X, file = "R/examples/python_dt/matrix_X", ncolumns = dim(X)[2])
write(eem$matrix, file = "R/examples/python_dt/matrix_W", ncolumns = dim(eem$matrix)[2])
write(y, file = "R/examples/python_dt/matrix_Y", ncolumns = 1)

print(eem)
predict(X, y, eem)
predict_proba(X, eem)