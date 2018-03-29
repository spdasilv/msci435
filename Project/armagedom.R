library("Matrix")
library("igraph")
library("rdist")
library("gurobi")

I = 10
J = 22
T = 8
D = 5
C = 2
K = 2
M = 1000

H = c(2,	2,	3,	2,	3,	2,	2,	2,	2,	2,	2,	2,	2,	2,	2,	2,	2,	2,	2,	2,	2,	2)
Cohort = matrix(c(1, 1,	1, 1,	0, 0,	0, 1,	1, 0,	0, 0,	0, 1,	1, 1,	1, 1,	1, 1,	1, 1,
                  0, 0, 0, 0,	1, 1,	1, 0,	0, 1,	1, 1,	1, 0,	0, 0,	0, 0,	0, 0,	0, 0), nrow=J, ncol=C, byrow=T)
P = matrix(c(0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	1, 	1, 	0, 	0, 	0, 	0, 	0, 	1, 	1, 
             0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	1, 	1, 	0, 	0, 	0, 	0, 	0, 	1, 	1, 
             0, 	0, 	1, 	1, 	0, 	0, 	0, 	1, 	1, 	1, 	1, 	1, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 
             0, 	0, 	1, 	1, 	0, 	0, 	0, 	1, 	1, 	1, 	1, 	1, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 
             0, 	0, 	0, 	0, 	0, 	1, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	1, 	1, 	1, 	1, 	0, 	0, 	0, 
             1, 	1, 	0, 	0, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	1, 	0, 	0, 
             0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	1, 	1, 	0, 	0, 	1, 	0, 	0, 
             0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	1, 	1, 	0, 	0, 	1, 	1, 	1, 	1, 	0, 	0, 	0, 
             1, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	1, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	1, 	0, 	0, 
             1, 	1, 	0, 	0, 	0, 	1, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	1, 	0, 	0), nrow=I, ncol=J, byrow=T)

Amat = Matrix(0, nrow=((T - 1)*D*C + (T - 2)*D*C + 2*I*J + 2*I + J + 1 + I*C + I*(T-1)*D + I*(T-2)*D + I*D), ncol=(I*J*T*D*C + I*D + I*K + I*J))
bvec = c()
dir = c()

cvec = c()
for (i in 1:I){
  for (j in 1:J){
    for (t in 1:T){
      for (d in 1:D){
        for (c in 1:C){
          if (t == 1) {
            cvec = c(cvec, 1)
          }
          else if (H[j] == 2) {
            if (t == 3 || t == 4) {
              cvec = c(cvec, 1)
            }
            else if (t == 7 || t == 8) {
              cvec = c(cvec, 1)
            }
            else {
              cvec = c(cvec, 0)
            }
          }
          else if (H[j] == 3) {
            if (t == 2 || t == 3 || t == 4) {
              cvec = c(cvec, 1)
            }
            else if (t == 6 || t == 7 || t == 8) {
              cvec = c(cvec, 1)
            }
            else {
              cvec = c(cvec, 0)
            }
          }
        }
      }
    }
  }
}

for (i in 1:I){
  for (k in 1:K){
    if (d == 1) {
      cvec = c(cvec, 3)
    } else if(d == 5) {
      cvec = c(cvec, 3)
    } else {
      cvec = c(cvec, 1)
    }
  }
}

for (i in 1:I){
  for (d in 1:D){
    if (d == 1) {
      cvec = c(cvec, 3)
    } else if(d == 5) {
      cvec = c(cvec, 3)
    } else {
      cvec = c(cvec, 1)
    }
  }
}

cvec = c(cvec, rep(0,I*J))

X_Vars = I*J*T*D*C
Z_Vars = I*D + X_Vars
Y_Vars = I*K + Z_Vars
W_Vars = I*J + Y_Vars

#constraint (12)
I_Count = 1
D_Count = 1
count = 0

row_start = 1
row_end = (I*D)
for(g in row_start:row_end){
  Xijtdc = array(0,dim=c(I,J,T,D,C))
  for (j in 1:J) {
    for (t in 1:T) {
      for (c in 1:C) {
        Xijtdc[I_Count, j, t, D_Count, c] = 1
      }
    }
  }
  
  count = 0
  Xijtdc.vector = integer(I*J*T*D*C)
  vector.position = 1
  for (i in 1:I){
    for (j in 1:J){
      for (t in 1:T){
        for (d in 1:D){
          for (c in 1:C){
            count = count + Xijtdc[i,j,t,d,c]
            Xijtdc.vector[vector.position] = Xijtdc[i,j,t,d,c]
            vector.position = vector.position + 1
          }
        }
      }
    }
  }
  
  Zid = array(0,dim=c(I,D))
  Zid[I_Count, D_Count] = -M
  
  Zid.vector = integer(I*D)
  vector.position = 1
  for (i in 1:I){
    for (d in 1:D){
      count = count - Zid[i,d]/M
      Zid.vector[vector.position] = Zid[i,d]
      vector.position = vector.position + 1
    }
  }
  
  Amat[g,1:X_Vars] = Xijtdc.vector
  Amat[g,(X_Vars+1):Z_Vars] = Zid.vector
  bvec = c(bvec, 0)
  dir = c(dir, '<=')
  
  if (D_Count == 5){
    D_Count = 1
    I_Count = I_Count + 1
  }
  else {
    D_Count = D_Count + 1
  }
}

#constraint (13)
I_Count = 1
J_Count = 1
count = 0

row_start = 1 + row_end
row_end = row_end + (I*J)
for(g in row_start:row_end){
  Xijtdc = array(0,dim=c(I,J,T,D,C))
  for (t in 1:T) {
    for (d in 1:D) {
      for (c in 1:C) {
        Xijtdc[I_Count, J_Count, t, d, c] = 1
      }
    }
  }
  
  count = 0
  Xijtdc.vector = integer(I*J*T*D*C)
  vector.position = 1
  for (i in 1:I){
    for (j in 1:J){
      for (t in 1:T){
        for (d in 1:D){
          for (c in 1:C){
            count = count + Xijtdc[i,j,t,d,c]
            Xijtdc.vector[vector.position] = Xijtdc[i,j,t,d,c]
            vector.position = vector.position + 1
          }
        }
      }
    }
  }
  
  Wij = array(0,dim=c(I,J))
  Wij[I_Count, J_Count] = -M
  
  Wij.vector = integer(I*J)
  vector.position = 1
  for (i in 1:I){
    for (j in 1:J){
      count = count - Wij[i,j]/M
      Wij.vector[vector.position] = Wij[i,j]
      vector.position = vector.position + 1
    }
  }
  
  Amat[g,1:X_Vars] = Xijtdc.vector
  Amat[g,(Y_Vars+1):W_Vars] = Wij.vector
  bvec = c(bvec, 0)
  dir = c(dir, '<=')
  
  if (J_Count == 22){
    J_Count = 1
    I_Count = I_Count + 1
  }
  else {
    J_Count = J_Count + 1
  }
}

#constraint (2)
I_Count = 1
count = 0

row_start = 1 + row_end
row_end = row_end + (I)
for(g in row_start:row_end){
  Wij = array(0,dim=c(I,J))
  for (j in 1:J) {
    Wij[I_Count, j] = 1
  }
  
  Wij.vector = integer(I*J)
  vector.position = 1
  for (i in 1:I){
    for (j in 1:J){
      count = count + Wij[i,j]
      Wij.vector[vector.position] = Wij[i,j]
      vector.position = vector.position + 1
    }
  }
  
  Amat[g,(Y_Vars+1):W_Vars] = Wij.vector
  bvec = c(bvec, 2)
  dir = c(dir, '<=')
  
  if (I_Count == 10){
    I_Count = 1
  }
  else {
    I_Count = I_Count + 1
  }
}


#constraint (11)
I_Count = 1
count = 0

row_start = 1 + row_end
row_end = row_end + (I)
for(g in row_start:row_end){
  Wij = array(0,dim=c(I,J))
  for (j in 1:J) {
    Wij[I_Count, j] = 1
  }
  
  Wij.vector = integer(I*J)
  vector.position = 1
  for (i in 1:I){
    for (j in 1:J){
      count = count + Wij[i,j]
      Wij.vector[vector.position] = Wij[i,j]
      vector.position = vector.position + 1
    }
  }
  
  Yik.vector = integer(I*K)
  vector.position = 1
  for (i in 1:I){
    count = count + 2 + 1
    Yik.vector[vector.position] = -1
    vector.position = vector.position + 1
    Yik.vector[vector.position] = -2
    vector.position = vector.position + 1
  }
  
  Amat[g,(Y_Vars+1):W_Vars] = Wij.vector
  Amat[g,(Z_Vars+1):Y_Vars] = Yik.vector
  bvec = c(bvec, 0)
  dir = c(dir, '=')
  
  if (I_Count == 10){
    I_Count = 1
  }
  else {
    I_Count = I_Count + 1
  }
}

#constraint (10)
J_Count = 1
count = 0

row_start = 1 + row_end
row_end = row_end + (J)
for(g in row_start:row_end){
  Wij = array(0,dim=c(I,J))
  for (i in 1:I) {
    Wij[i, J_Count] = 1
  }
  
  Wij.vector = integer(I*J)
  vector.position = 1
  for (i in 1:I){
    for (j in 1:J){
      count = count + Wij[i,j]
      Wij.vector[vector.position] = Wij[i,j]
      vector.position = vector.position + 1
    }
  }
  
  Amat[g,(Y_Vars+1):W_Vars] = Wij.vector
  bvec = c(bvec, 1)
  dir = c(dir, '=')
  
  if (J_Count == 22){
    J_Count = 1
  }
  else {
    J_Count = J_Count + 1
  }
}

#constraint (1)
row_start = 1 + row_end
row_end = row_end + 1
Amat[row_start,(Y_Vars+1):W_Vars] = c(rep(1, I*J))
bvec = c(bvec, 22)
dir = c(dir, '=')


#constraint (14)
count = 0
row_start = 1 + row_end
row_end = row_end + (I*J)
for(g in row_start:row_end){
  vector.position = Y_Vars + 1
  for (i in 1:I){
    for (j in 1:J){
      count = count + 1
      Amat[g,vector.position] = 1
      vector.position = vector.position + 1
    }
  }
}
for (i in 1:I){
  for (j in 1:J){
    bvec = c(bvec, P[i,j])
    dir = c(dir, '<=')
  }
}

#constraint (2)
I_Count = 1
C_Count = 1
count = 0

row_start = 1 + row_end
row_end = row_end + (I*C)
for(g in row_start:row_end){
  Wij = array(0,dim=c(I,J))
  for (j in 1:J) {
    Wij[I_Count, j] = Cohort[j, C_Count]
  }
  
  Wij.vector = integer(I*J)
  vector.position = 1
  for (i in 1:I){
    for (j in 1:J){
      count = count + Wij[i,j]
      Wij.vector[vector.position] = Wij[i,j]
      vector.position = vector.position + 1
    }
  }
  
  Amat[g,(Y_Vars+1):W_Vars] = Wij.vector
  bvec = c(bvec, 1)
  dir = c(dir, '<=')
  
  if (I_Count == 10){
    I_Count = 1
    C_Count = C_Count + 1
  }
  else {
    I_Count = I_Count + 1
  }
}

#constraint (4)
T_Count = 1
D_Count = 1
C_Count = 1
count = 0

row_start = 1 + row_end
row_end = row_end + ((T-1)*D*C)
for(g in row_start:row_end){
  Xijtdc = array(0,dim=c(I,J,T,D,C))
  
  for (i in 1:I) {
    for (j in 1:J) {
      if (H[j] == 2) {
        Xijtdc[i,j,T_Count,D_Count,C_Count] = 1
        Xijtdc[i,j,T_Count + 1,D_Count,C_Count] = 1
      }
    }
  }
  
  count = 0
  Xijtdc.vector = integer(I*J*T*D*C)
  vector.position = 1
  for (i in 1:I){
    for (j in 1:J){
      for (t in 1:T){
        for (d in 1:D){
          for (c in 1:C){
            count = count + Xijtdc[i,j,t,d,c]
            Xijtdc.vector[vector.position] = Xijtdc[i,j,t,d,c]
            vector.position = vector.position + 1
          }
        }
      }
    }
  }
  
  Amat[g,1:X_Vars] = Xijtdc.vector
  bvec = c(bvec, 1)
  dir = c(dir, '<=')
  
  if (C_Count == 2){
    C_Count = 1
    D_Count = D_Count + 1
  }
  else {
    C_Count = C_Count + 1
  }
  if (D_Count == 6) {
    D_Count = 1
    T_Count = T_Count + 1
  }
}

#constraint (4)
T_Count = 1
D_Count = 1
C_Count = 1
count = 0

row_start = 1 + row_end
row_end = row_end + ((T-2)*D*C)
for(g in row_start:row_end){
  Xijtdc = array(0,dim=c(I,J,T,D,C))
  
  for (i in 1:I) {
    for (j in 1:J) {
      if (H[j] == 3) {
        Xijtdc[i,j,T_Count,D_Count,C_Count] = 1
        Xijtdc[i,j,T_Count + 1,D_Count,C_Count] = 1
        Xijtdc[i,j,T_Count + 2,D_Count,C_Count] = 1
      }
    }
  }
  
  count = 0
  Xijtdc.vector = integer(I*J*T*D*C)
  vector.position = 1
  for (i in 1:I){
    for (j in 1:J){
      for (t in 1:T){
        for (d in 1:D){
          for (c in 1:C){
            count = count + Xijtdc[i,j,t,d,c]
            Xijtdc.vector[vector.position] = Xijtdc[i,j,t,d,c]
            vector.position = vector.position + 1
          }
        }
      }
    }
  }
  
  Amat[g,1:X_Vars] = Xijtdc.vector
  bvec = c(bvec, 1)
  dir = c(dir, '<=')
  
  if (C_Count == 2){
    C_Count = 1
    D_Count = D_Count + 1
  }
  else {
    C_Count = C_Count + 1
  }
  if (D_Count == 6) {
    D_Count = 1
    T_Count = T_Count + 1
  }
}


#constraint (3)
I_Count = 1
T_Count = 1
D_Count = 1
count = 0

row_start = 1 + row_end
row_end = row_end + ((T-1)*D*I)
for(g in row_start:row_end){
  Xijtdc = array(0,dim=c(I,J,T,D,C))
  
  for (j in 1:J) {
    for (c in 1:C) {
      if (H[j] == 2) {
        Xijtdc[I_Count,j,T_Count,D_Count,c] = 1
        Xijtdc[I_Count,j,T_Count + 1,D_Count,c] = 1
      }
    }
  }
  
  count = 0
  Xijtdc.vector = integer(I*J*T*D*C)
  vector.position = 1
  for (i in 1:I){
    for (j in 1:J){
      for (t in 1:T){
        for (d in 1:D){
          for (c in 1:C){
            count = count + Xijtdc[i,j,t,d,c]
            Xijtdc.vector[vector.position] = Xijtdc[i,j,t,d,c]
            vector.position = vector.position + 1
          }
        }
      }
    }
  }
  
  Amat[g,1:X_Vars] = Xijtdc.vector
  bvec = c(bvec, 1)
  dir = c(dir, '<=')
  
  if (D_Count == 5){
    D_Count = 1
    T_Count = T_Count + 1
  }
  else {
    D_Count = D_Count + 1
  }
  if (T_Count == 8) {
    T_Count = 1
    I_Count = I_Count + 1
  }
}

#constraint (3)
I_Count = 1
T_Count = 1
D_Count = 1
count = 0

row_start = 1 + row_end
row_end = row_end + ((T-2)*D*I)
for(g in row_start:row_end){
  Xijtdc = array(0,dim=c(I,J,T,D,C))
  
  for (j in 1:J) {
    for (c in 1:C) {
      if (H[j] == 3) {
        Xijtdc[I_Count,j,T_Count,D_Count,c] = 1
        Xijtdc[I_Count,j,T_Count + 1,D_Count,c] = 1
        Xijtdc[I_Count,j,T_Count + 2,D_Count,c] = 1
      }
    }
  }
  
  
  count = 0
  Xijtdc.vector = integer(I*J*T*D*C)
  vector.position = 1
  for (i in 1:I){
    for (j in 1:J){
      for (t in 1:T){
        for (d in 1:D){
          for (c in 1:C){
            count = count + Xijtdc[i,j,t,d,c]
            Xijtdc.vector[vector.position] = Xijtdc[i,j,t,d,c]
            vector.position = vector.position + 1
          }
        }
      }
    }
  }
  
  Amat[g,1:X_Vars] = Xijtdc.vector
  bvec = c(bvec, 1)
  dir = c(dir, '<=')
  
  if (D_Count == 5){
    D_Count = 1
    T_Count = T_Count + 1
  }
  else {
    D_Count = D_Count + 1
  }
  if (T_Count == 7) {
    T_Count = 1
    I_Count = I_Count + 1
  }
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