n <- sample(1:10,1)
m <- sample(1:16,1)

X <- dataX(n,m)
y <- dataY(n)

h <- sample(1:10,1)

eem <- EEM(X, y, h)
print(eem)

print(predict(X, y, eem))