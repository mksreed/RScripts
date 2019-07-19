install.packages("cPCG")
test_A <- matrix(c(4,1,1,3), ncol = 2)
test_b <- matrix(1:2, ncol = 1)
library(cPCG)
cgsolve(test_A, test_b, 1e-6, 1000)
# preconditioned conjugate gradient method solver,
# with incomplete Cholesky factorization as preconditioner
pcgsolve(test_A, test_b, "ICC")
#############################
A<- matrix(c(6,1,1,3,2,3,2,3,5), ncol = 3)
y <- matrix(1:3, ncol = 1)
b<-A%*%y
solve(A,b)
#############################
n=10
N=n*n
AA<- matrix(seq(1:N)/10+rnorm(N), ncol = n)
AA<- matrix(rnorm(N), ncol = n)
AA<-(AA%*%AA)
A<-AA/min(abs(AA))
A
solve(A)%*%A
y <- matrix(rnorm(n), ncol = 1)
y
b<-A%*%y
x<-solve(A,b)
x
cgsolve(A,b,1e-6, 1000)-x

