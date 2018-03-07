library("gurobi")
library("Matrix")

L1 = 1
L2 = 1

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

LB = z.SP1 +z.SP2 + z.SP3 - 4000*u1 - 3000*u2

### Master Problem ###

myMP = list()
myMP$modelsense = "max"
myMP$obj = c(1,1,1,-4000,-3000)
myMP$A = Matrix(c(1,0,0,-(x1.h[1] + x1.h[3]),-(x1.h[2] + x1.h[4]),
                  0,1,0,-(x2.h[1] + x2.h[3]),-(x2.h[2] + x2.h[4]),
                  0,0,1,-(x3.h[1] + x3.h[3]),-(x3.h[2] + x3.h[4])),
                  nrow=3, ncol=5, byrow=T, sparse=T)
myMP$sense = c("<=","<=","<=")
myMP$rhs = c(15.7*x1.h[1] + 10.7*x1.h[2] + 15.4*x1.h[3] + 10.4*x1.h[4],
             15.5*x1.h[1] + 10.5*x1.h[2] + 15.9*x1.h[3] + 10.9*x1.h[4],
             15.7*x1.h[1] + 10.7*x1.h[2] + 15.4*x1.h[3] + 10.4*x1.h[4])
myMP$vtypes = "C"
myMP$lb = c(-1000000, -1000000, -1000000, 0, 0)
myMP$ub = c(1000000, 1000000, 1000000, 1000000, 1000000)

mysol = gurobi(myMP)

theta1 = mysol$x[1]
theta2 = mysol$x[2]
theta3 = mysol$x[3]
u1 = mysol$x[4]
u2 = mysol$x[5]

UB = mysol$objval

check = (LB==UB)