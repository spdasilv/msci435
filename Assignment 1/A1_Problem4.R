library("gurobi")
library("Matrix")
library("igraph")

n <- 7
C.ij = matrix(c(10000, 82, 34, 64, 141, 201, 62, 82, 10000, 94, 124, 79, 142, 123, 34, 94, 10000, 57, 154, 214, 52, 64, 124, 57, 10000, 184, 244, 22, 141, 79, 154, 184, 10000, 81, 179, 201, 142, 214, 244, 81, 10000, 239, 62, 123, 52, 22, 179, 239, 10000), nrow = n, ncol=n, byrow = TRUE)
cvec = as.vector(t(C.ij))

bvec = c(rep(1, n), rep(1, n))
dir = c(rep("=", n), rep("=", n))

# Original/initial matrix #
Amat = Matrix(0, nrow = (n + n), ncol = (n * n))
for (j in 1:n) {
  Amat[j, seq(j, by = n, length.out = n)] = 1
  Amat[j, ((j - 1) * n + j)] = 0
}
for (i in 1:n) {
  Amat[n + i, ((i - 1) * n + 1):(i * n)] = 1
  Amat[n + i, ((i - 1) * n + i)] = 0
}

myLP = list()
myLP$obj = cvec
myLP$A = Amat
myLP$sense = dir
myLP$rhs = bvec
myLP$vtypes = "B"
# myLP$vtypes = "C"
# myLP$ub = 1

check = F

# Solve the TSP without subtour elimination constraints 
mysol = gurobi(myLP)
# get the decision variables in matrix format 
x.ij = matrix(mysol$x, nrow = n, ncol = n, byrow = T)

# Check if there are subtours in your solution using graph formulation 
myG = graph_from_adjacency_matrix(x.ij, weighted = T)
plot(myG)

# The following subroutine checks whether there are subtours in a given solutio
# and adds the subtour elimination constraints for each subtour found
decomposed.graph = clusters(myG)
if (decomposed.graph$no > 1) {
  for (i in 1:decomposed.graph$no) {
    cities = which(decomposed.graph$membership == i)
    links = t(combn(cities, 2))
    d.ij = matrix(0, n, n)
    for (m in 1:nrow(links)) {
      d.ij[links[m, 1], links[m, 2]] = 1
    }
    d.ij = d.ij + t(d.ij)
    myLP$A = rBind(myLP$A, as.vector(d.ij))
    myLP$rhs = c(myLP$rhs, (length(cities) - 1))
    myLP$sense = c(myLP$sense, "<=")
  }
}
if (decomposed.graph$no == 1) {
  check = T
}
#



### Mini TSP Solver ###


# Add subtour elimination constraints iteratively until there is no subtour in 
# the solution found. This means that the solution is optimal!
while (!check) {
  params = list(OutputFlag = 0)
  mysol = gurobi(myLP, params)
  x.ij = matrix(mysol$x, nrow = n, ncol = n, byrow = T)
  
  # Check if there are subtours 
  myG = graph_from_adjacency_matrix(x.ij, weighted = T)
  
  decomposed.graph = clusters(myG)
  if (decomposed.graph$no > 1) {
    for (i in 1:decomposed.graph$no) {
      cities = which(decomposed.graph$membership == i)
      links = t(combn(cities, 2))
      d.ij = matrix(0, n, n)
      for (m in 1:nrow(links)) {
        d.ij[links[m, 1], links[m, 2]] = 1
      }
      d.ij = d.ij + t(d.ij)
      myLP$A = rBind(myLP$A, as.vector(d.ij))
      myLP$rhs = c(myLP$rhs, (length(cities) - 1))
      myLP$sense = c(myLP$sense, "<=")
    }
  }
  if (decomposed.graph$no == 1) {
    check = T
  }
}
plot(myG)