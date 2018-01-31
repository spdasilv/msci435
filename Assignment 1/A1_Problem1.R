### Uncapacitated Facility Location Problem ###
library("gurobi")
library("Matrix")
library("igraph") # You need to first install the package!
library("rdist")

n <- 8
k <- 3

M <- 10000
T.ij = matrix(c(M, M, 60, 90, M, 180, M, M, M, M, 30, 60, M, 150, M, M, M, M, M, M, 30, M, 90, M, M, M, M, M, M, M, 30, M, M, M, M, M, M, 30, M, M, M, M, M, M, M, M, M, 90, M, M, M, M, M, M, M, 60, M, M, M, M, M, M, M, M), nrow = n, ncol=n, byrow = TRUE)
F.ik.S.ik = c(rep(0, n*k*2))
cvec = c(rep(0, n*n*k),F.ik.S.ik)
col = 1
for(i in 1:n){
  for(j in 1:n){
    for(K in 1:k){
      cvec[col] = T.ij[i,j]
      col = col + 1
    }
  }
}

bvec = c(rep(1, n), rep(1, n), rep(1, n*k), rep(0, n*k), rep(0, n*k), rep(0, k), rep(1, n), rep(1, n))
dir = c(rep("<=", n), rep("<=", n),  rep("<=", n*k), rep(">=", n*k), rep(">=", n*k), rep("=", k), rep("=", n), rep("=", n))

Amat = matrix(0, nrow=(n*2+(n*k)*3+k+n*2), ncol=(n*n*k + n*k*2))
#contraint 1
for (i in 1:n) {
  Amat[i, ((i - 1) * n*k + 1):(i * n*k)] = 1
  Amat[i, ((i - 1) * n*k + i)] = 0
  Amat[i, ((i - 1) * n*k + i + n)] = 0
  Amat[i, ((i - 1) * n*k + i + n*2)] = 0
}
#contraint 2
for (i in 1:n) {
  Amat[i+n, seq(i,by =n*k, length.out = n)] = 1
  Amat[i+n, seq(i+1,by =n*k, length.out = n)] = 1
  Amat[i+n, seq(i+2,by =n*k, length.out = n)] = 1
  Amat[i+n, ((i - 1) * n*k + 1):(i * n*k)] = 0
}
#contraint 3
row = 1
for (i in 1:n) {
  for(K in 1:k){
    Amat[row+n*2, seq(from=row+n*n*k,by =n*k, length.out = 2)] = 1
    row = row + 1
  }
}
#contraint 4
row = 1
col = 1
for (i in 1:n) {
  for(K in 1:k){
    Amat[row+n*2+n*k, seq(row,by =n*k, length.out = n)] = 1
    Amat[row+n*2+n*k, seq(from=n*n*k+n*k+col,by =k, length.out = n)] = -1
    row = row + 1
    col = col + 1
  }
  col = 1
}
#contraint 5
row = 1
col = 1
for (i in 1:n) {
  for(K in 1:k){
    Amat[row+n*2+n*k*2, seq(row,by =n*k, length.out = n)] = 1
    Amat[row+n*2+n*k*2, seq(from=n*n*k+col,by =k, length.out = n)] = -1
    row = row + 1
    col = col + 1
  }
  col = 1
}
#contraint 6
for (i in 1:(k)) {
  Amat[i+n*2+n*k*3, seq(from=n*n*k+i,by =k, length.out = n)] = 1
  Amat[i+n*2+n*k*3, seq(from=n*n*k+n*k+i,by =k, length.out = n)] = -1
}

#contraint 7
offSet = 24
offSetL = 193
for (i in 1:(n)) {
  Amat[i+n*2+n*k*3+k, (offSet - 23):(offSet)] = 1
  Amat[i+n*2+n*k*3+k, offSetL:(offSetL+k-1)] = 1
  offSet = offSet + 24
  offSetL = offSetL + k
}

#contraint 8
offSet = 24
offSetF = 217
for (i in 1:(n)) {
  Amat[i+n*2+n*k*3+k+n, (offSet - 23):(offSet)] = 1
  Amat[i+n*2+n*k*3+k+n, offSetL:(offSetL+k-1)] = 1
  offSet = offSet + 24
  offSetL = offSetL + k
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

crew1 = matrix(rep(0,n*n) ,nrow = n, ncol=n, byrow = TRUE)
count = 1
for(i in 1:n){
  for(j in 1:n){
    crew1[i,j]=mysol$x[count]
    count = count + 3
  }
}
crew2 = matrix(rep(0,n*n) ,nrow = n, ncol=n, byrow = TRUE)
count = 2
for(i in 1:n){
  for(j in 1:n){
    crew2[i,j]=mysol$x[count]
    count = count + 3
  }
}
crew3 = matrix(rep(0,n*n) ,nrow = n, ncol=n, byrow = TRUE)
count = 3
for(i in 1:n){
  for(j in 1:n){
    crew3[i,j]=mysol$x[count]
    count = count + 3
  }
}