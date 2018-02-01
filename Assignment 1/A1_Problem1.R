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

bvec = c(rep(1, n), rep(1, n), rep(1, n*k), rep(0, n*k), rep(0, n*k), rep(0, k), rep(1, n*k), rep(1, n*k), 8, rep(1, n*k), rep(1, n*k), rep(0, n*k), rep(0, 6) )
dir = c(rep("<=", n), rep("<=", n),  rep("<=", n*k), rep(">=", n*k), rep(">=", n*k), rep("=", k), rep("<=", n*k), rep("<=", n*k), "=", rep("<=", n*k), rep("<=", n*k), rep("=", n*k), rep("=", 6))

Amat = matrix(0, nrow=(n*2+(n*k)*8+k+7), ncol=(n*n*k + n*k*2))
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
Count = 1
Count2 = 1
for(i in 1:n){
  for(K in 1:k){
    Amat[Count+n*2+n*k*3+k,seq(from=Count2,by =k, length.out = n)] = 1
    Amat[Count+n*2+n*k*3+k,216+Count] = 1
    Count = Count + 1
    Count2 = Count2 + 1
  }
  Count2 = Count2 + (n-1)*k
}

#contraint 8
Count = 1
Count2 = 1
for(i in 1:n){
  for(K in 1:k){
    Amat[Count+n*2+n*k*4+k,seq(from=Count2,by =n*k, length.out = n)] = 1
    Amat[Count+n*2+n*k*4+k,192+Count] = 1
    Count = Count + 1
    Count2 = Count2 + 1
  }
  #Count2 = Count2 + (n-1)*k
}

#Constraint 9: make sure that when you sum all F and all X it equals to 8
Amat[n*2+n*k*5+k+1,(1):(n*n*k + n*k)] = 1

#Constraint 10: each crew can only have first flight once
row = 1
col = 1
for (i in 1:n) {
  for(K in 1:k){
    Amat[row+n*2+n*k*5+k+1, seq(from=n*n*k+n*k+col,by =k, length.out = n)] = 1
    row = row + 1
    col = col + 1
  }
  col = 1
}
#Constraint 11: each crew can only have last flight once
row = 1
col = 1
for (i in 1:n) {
  for(K in 1:k){
    Amat[row+n*2+n*k*6+k+1, seq(from=n*n*k+col,by =k, length.out = n)] = 1
    row = row + 1
    col = col + 1
  }
  col = 1
}
#Constraint 12: make sure that if a crew goes from i to j then j to something else should still be done by the same crew (His paper)
row = 1
offSet = 0
for (i in 1:n) {
  for(K in 1:k){
    Amat[row+n*2+n*k*7+k+1, seq(row,by =n*k, length.out = n)] = 1
    Amat[row+n*2+n*k*7+k+1, seq(row + offSet + k,by =k, length.out = n-1)] = -1
    Amat[row+n*2+n*k*7+k+1, seq(from=row+n*n*k,by =n*k, length.out = 1)] = 1
    Amat[row+n*2+n*k*7+k+1, seq(from=row+n*n*k+n*k,by =n*k, length.out = 1)] = -1
    row = row + 1
  }
  offSet = offSet + 24
}

#constraint 13: make sure that if someone starts somewhere it also needs to end in the same city
Crew1NotF = c(2,3,4,6,8)
for(i in 1:length(Crew1NotF)){
  Amat[n*2+n*k*8+k+2, n*n*k+(Crew1NotF[i]-1)*3+1] = 1
}
Crew2NotF = c(1,2,5,7,8)
for(i in 1:length(Crew2NotF)){
  Amat[n*2+n*k*8+k+3, n*n*k+(Crew2NotF[i]-1)*3+2] = 1
}
Crew3NotF = c(1,2,5,7,8)
for(i in 1:length(Crew3NotF)){
  Amat[n*2+n*k*8+k+4, n*n*k+(Crew3NotF[i]-1)*3+3] = 1
}

#Constraint 14: make sure that each crew can only start in one given city
Crew1F = c(1,5,7)
for(i in 1:length(Crew1F)){
  Amat[n*2+n*k*8+k+5, n*n*k+(Crew1F[i]-1)*3+1] = 1
}
Crew1L = c(3,4,8)
for(i in 1:length(Crew1L)){
  Amat[n*2+n*k*8+k+5, n*n*k+n*k+(Crew1L[i]-1)*3+1] = 1
}
Crew2F = c(3,4,6)
for(i in 1:length(Crew2F)){
  Amat[n*2+n*k*8+k+6, n*n*k+(Crew2F[i]-1)*3+2] = 1
}
Crew2L = c(1,2,5)
for(i in 1:length(Crew2L)){
  Amat[n*2+n*k*8+k+6, n*n*k+n*k+(Crew2L[i]-1)*3+2] = 1
}
Crew3F = c(2,8)
for(i in 1:length(Crew3F)){
  Amat[n*2+n*k*8+k+7, n*n*k+(Crew3F[i]-1)*3+3] = 1
}
Crew3L = c(6,7)
for(i in 1:length(Crew3L)){
  Amat[n*2+n*k*8+k+7, n*n*k+n*k+(Crew3L[i]-1)*3+3] = 1
}

# #before
# offSet = 24
# offSetL = 193
# for (i in 1:(n)) {
#   Amat[i+n*2+n*k*3+k, (offSet - 23):(offSet)] = 1
#   Amat[i+n*2+n*k*3+k, offSetL:(offSetL+k-1)] = 1
#   offSet = offSet + 24
#   offSetL = offSetL + k
# }
# 
# #contraint 8
# offSet = 24
# offSetF = 217
# for (i in 1:(n)) {
#   Amat[i+n*2+n*k*3+k+n, (offSet - 23):(offSet)] = 1
#   Amat[i+n*2+n*k*3+k+n, offSetL:(offSetL+k-1)] = 1
#   offSet = offSet + 24
#   offSetL = offSetL + k
# }
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