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
print(eem)

print(predict(X, y, eem))
print(predict_proba(X,eem))