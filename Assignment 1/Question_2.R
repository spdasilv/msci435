### Uncapacitated Facility Location Problem ###
library("gurobi")
library("Matrix")
library("igraph") # You need to first install the package!
library("rdist")

L = 5
K = 3

coordinates.l = matrix(c(3,6,4,4,3,0,2,1,0,1), nrow=L, ncol=2, byrow = TRUE)
coordinates.k = matrix(c(3,3,2,4,2,5), nrow=K, ncol=2, byrow = TRUE)
coordinates.all = rbind(coordinates.l, coordinates.k)

P.kl = cdist(coordinates.k, coordinates.l, metric = "euclidean", p = 2)

plot(coordinates.all[,1], coordinates.all[,2], pch=16)
plot(coordinates.k[,1], coordinates.k[,2], pch=16)


D.l = sample(c(20,40,15,30,25))
cvec = c(as.vector(t(P.kl)))
bvec = c(1,1,1,1,1)
dir = c("=","=","=","=","=")

Amat = matrix(0, nrow=(L), ncol=(L*K))
Amat[1,] = c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0)
Amat[2,] = c(0,1,0,0,0,0,1,0,0,0,0,1,0,0,0)
Amat[3,] = c(0,0,1,0,0,0,0,1,0,0,0,0,1,0,0)
Amat[4,] = c(0,0,0,1,0,0,0,0,1,0,0,0,0,1,0)
Amat[5,] = c(0,0,0,0,1,0,0,0,0,1,0,0,0,0,1)

image(Matrix(Amat))

myLP = list()
myLP$obj = cvec
myLP$A = Amat
myLP$sense = dir
myLP$rhs = bvec
myLP$vtypes = "C"
myLP$ub = 1

mysol = gurobi(myLP)
mysol$objval
mysol$x