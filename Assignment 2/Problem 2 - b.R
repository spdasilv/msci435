library("gurobi")
library("Matrix")

### Subproblem ###

u1 = 0

mySP = list()
mySP$modelsense = "min"
mySP$obj = c(20-u1,15-u1,3-4*u1,18-4*u1, 10-4*u1)
mySP$A = Matrix(c(3,2,-1,5,3,3,2,-1,3,2), nrow=2, ncol=5, byrow=T, sparse=T)
mySP$sense = c(">=")
mySP$rhs = c(10,10)
mySP$vtypes = "B"

mysol = gurobi(mySP)
x.h = mysol$x
z.SP = mysol$objval

LB = z.SP + u1


### Master Problem ###

myMP = list()
myMP$modelsense = "max"
myMP$obj = c(1,10)
myMP$A = Matrix(c(1,10), nrow=1, ncol=2, byrow=T, sparse=T)
myMP$sense = c("<=")
myMP$rhs = c(63)
myMP$vtypes = "C"
myMP$lb = c(-Inf, 0) ### This is important !!! 

mysol = gurobi(myMP)

theta = mysol$x[1]
u1 = mysol$x[2]

UB = mysol$objval

check = (LB==UB)
