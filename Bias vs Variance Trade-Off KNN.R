f <- function(X){sin(0.4*X) + sqrt(X)}
X = seq(0,4*pi,length.out = 100)
set.seed(007) 
Y =  f(X) + rnorm(100)


plot(X,f(X), type = 'l',ylab='f(X)',
     ylim = c(-3,6),bty='n', lwd = 3, lty = 1)
points(X,Y, pch = 20)

################################################################################
library(FNN)        
kNN.reg = knn.reg(train = X , test = NULL, y = Y, k = 99)
points(X, kNN.reg$pred, type='l', col = 'red', lwd = 2, lty = 2)
kNN.reg = knn.reg(train = X , test = NULL, y = Y, k = 1)
points(X, kNN.reg$pred, type='l', col = 'green', lwd = 2, lty = 2)
kNN.reg = knn.reg(train = X , test = NULL, y = Y, k = 15)
points(X, kNN.reg$pred, type='l', col = 'blue', lwd = 2, lty = 2)
##############################################################################$
datos = cbind(X,Y)
id.train =sample(100,50, replace = FALSE)

train = datos[ id.train,]
dim(train)
head(train)

test  = datos[-id.train,]
dim(test)

k.par = c(10,15,20,30,35)
MSE = c() 
for(i in 1:5){
  kNNreg = knn.reg(train = as.matrix(train[,1]) ,
                   y = as.matrix(train[,2]),
                   test = as.matrix(test[,1]),
                   k = k.par[i])
  pred = kNNreg$pred
  MSE[i] = sum(pred - test[,2])^2 / 50 
  print(i)
}

which(MSE == min(MSE)) # k = 

plot(k.par, MSE, type = 'b')
MSE1 = MSE
