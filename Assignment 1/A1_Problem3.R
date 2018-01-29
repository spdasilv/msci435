library("gurobi")
library("Matrix")
library("igraph")
library("rdist")

K <- 4
K
C = 10
Pmin = 10
P = matrix(sample(0:30,K*C,replace = T), nrow=K, ncol=C, byrow=T)
cvec = c(rep(1,each=K), rep(0,each=K*C))

Amat = matrix(0, nrow=(K*C+K+C), ncol=(K+K*C))
bvec = c(rep(0,each=K*C),rep(1,each=C),rep(0,each=K))
dir = c(rep("<=",each=K*C),rep(">=",each=C+K))

row = 1
for(i in 1:C){
  for(j in 1:K){
    bvec[row] = P[j,i]
    row = row + 1
  }
}

for(i in 1:(K*C)){
  Amat[i,K+i] = Pmin
}

row = K
for(i in (K*C+1):(K*C+C)){
  for(j in 1:K){
    Amat[i,row + j] = 1
  }
  row = row + K
}

row = 1
for(i in (K*C+C+1):(K*C+C+K)){
  Amat[i,i - (K*C+C)] = 1000
  Amat[i,seq(i - (K*C+C) + K, by=K, length.out=C)] = -1
}

image(Matrix(Amat))

myLP = list()
myLP$obj = cvec
myLP$A = Amat
myLP$sense = dir
myLP$rhs = bvec
myLP$vtypes = "B"
myLP$ub = 1

mysol = gurobi(myLP)
mysol$objval
mysol$x