library("gurobi")
library("Matrix")

L1 = 0.95
L2 = 0.92

### Iteration 1 ###
u1 = 100
u2 = 100

### Subproblem 1 ###

mySP1 = list()
mySP1$modelsense = "min"
mySP1$obj = c(15+0.7*L1+u1,10+0.7*L2+u2,15+0.4*L1+u1,10+0.4*L2+u2)
mySP1$A = Matrix(c(0.4,0.32,0.4,0.32,0.2,0.4,0.2,0.4), nrow=2, ncol=4, byrow=T, sparse=T)
mySP1$sense = c(">=",">=")
mySP1$rhs = c(300,800)
mySP1$vtypes = "I"

mysol = gurobi(mySP1)
x1.h = mysol$x
z.SP1 = mysol$objval

LB1 = z.SP1 - 4000*u1 - 3000*u2

### Subproblem 2 ###

mySP2 = list()
mySP2$modelsense = "min"
mySP2$obj = c(15+0.5*L1+u1,10+0.5*L2+u2,15+0.9*L1+u1,10+0.9*L2+u2)
mySP2$A = Matrix(c(0.4,0.32,0.4,0.32,0.2,0.4,0.2,0.4,0.35,0.2,0.35,0.2), nrow=3, ncol=4, byrow=T, sparse=T)
mySP2$sense = c(">=",">=",">=")
mySP2$rhs = c(600,400,800)
mySP2$vtypes = "I"

mysol = gurobi(mySP2)
x2.h = mysol$x
z.SP2 = mysol$objval

LB2 = z.SP2 - 4000*u1 - 3000*u2

### Subproblem 3 ###

mySP3 = list()
mySP3$modelsense = "min"
mySP3$obj = c(15+0.7*L1+u1,10+0.7*L2+u2,15+0.4*L1+u1,10+0.4*L2+u2)
mySP3$A = Matrix(c(0.4,0.32,0.4,0.32,0.2,0.4,0.2,0.4,0.35,0.2,0.35,0.2), nrow=3, ncol=4, byrow=T, sparse=T)
mySP3$sense = c(">=",">=",">=")
mySP3$rhs = c(900,300,500)
mySP3$vtypes = "I"

mysol = gurobi(mySP3)
x3.h = mysol$x
z.SP3 = mysol$objval

LB3 = z.SP3 - 4000*u1 - 3000*u2


