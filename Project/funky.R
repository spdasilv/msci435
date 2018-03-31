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

H = c(2,	2,	3,	2,	2,	2,	2,	2,	2,	2,	2,	2,	2,	2,	2,	2,	2,	2,	2,	2,	3,	3)
Cohort = matrix(c(1, 	0, 
                  1, 	0, 
                  1, 	0, 
                  1, 	0, 
                  0, 	1, 
                  0, 	1, 
                  1, 	0, 
                  1, 	0, 
                  0, 	1, 
                  0, 	1, 
                  0, 	1, 
                  0, 	1, 
                  1, 	0, 
                  1, 	0, 
                  1, 	0, 
                  1, 	0, 
                  1, 	0, 
                  1, 	0, 
                  1, 	0, 
                  1, 	0, 
                  0, 	1, 
                  1, 	0), nrow=J, ncol=C, byrow=T)
P = matrix(c(0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	1, 	1, 	0, 	0, 	0, 	0, 	1, 	1, 	0, 	0, 
             0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	1, 	1, 	0, 	0, 	0, 	0, 	1, 	1, 	0, 	0, 
             0, 	0, 	1, 	1, 	0, 	0, 	1, 	1, 	1, 	1, 	1, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 
             0, 	0, 	1, 	1, 	0, 	0, 	1, 	1, 	1, 	1, 	1, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 
             0, 	0, 	0, 	0, 	1, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	1, 	1, 	1, 	1, 	0, 	0, 	0, 	0, 
             1, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	1, 	1, 
             0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	1, 	1, 	0, 	0, 	0, 	0, 	0, 	1, 
             0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	1, 	1, 	0, 	0, 	1, 	1, 	1, 	1, 	0, 	0, 	0, 	0, 
             1, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 	1, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	1, 
             1, 	1, 	0, 	0, 	1, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	1), nrow=I, ncol=J, byrow=T)

Amat = Matrix(0, nrow=(4 + J + 3*I*J + 2*I + ((J-2)*I)/2 + I*C + I*D + (T - 1)*D*C + (T - 2)*D*C + I*(T-1)*D + I*(T-2)*D + D*(J-2)/2 + J*C + T*D*C + (T-1)*D + (T-2)*D +(T*D)), ncol=(I*J*T*D*C + I*D + I*K + I*J))
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

#constraint (1) - Match number of courses
Amat[1,(Y_Vars+1):W_Vars] = c(rep(1, I*J))
bvec = c(bvec, 22)
dir = c(dir, '=')

#constraint (1) - Match number of courses
Amat[2,1:X_Vars] = c(rep(1, I*J*T*D*C))
bvec = c(bvec, 22)
dir = c(dir, '=')

#constraint (15)
row_start = 3
row_end = ((J-2)*I)/2 + 2

I_Count = 1
J_Count = 1
for(g in row_start:row_end){
  Wij = array(0,dim=c(I,J))
  Wij[I_Count, J_Count] = 1
  Wij[I_Count, J_Count + 1] = -1
  
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
  bvec = c(bvec, 0)
  dir = c(dir, '=')
  
  if (J_Count == 19){
    J_Count = 1
    I_Count = I_Count + 1
  }
  else {
    J_Count = J_Count + 2
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
  
  J_Count = J_Count + 1
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

#constraint (16)
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
  
  Amat[g,1:X_Vars] = Xijtdc.vector
  bvec = c(bvec, 1)
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
  bvec = c(bvec, 4)
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
  for (j in seq(1, J, 2)) {
    Wij[I_Count, j] = 1
    if (j == 21) {
      Wij[I_Count, j + 1] = 1
    }
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
  
  Yik = array(0,dim=c(I,K))
  Yik[I_Count,1] = -1
  Yik[I_Count,2] = -2
  
  Yik.vector = integer(I*K)
  vector.position = 1
  for (i in 1:I){
    for (k in 1:K){
      Yik.vector[vector.position] = Yik[i,k]
      vector.position = vector.position + 1
    }
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

#constraint (2)
I_Count = 1
C_Count = 1
count = 0

row_start = 1 + row_end
row_end = row_end + (I*C)
for(g in row_start:row_end){
  Wij = array(0,dim=c(I,J))
  for (j in seq(1, J, 2)) {
    Wij[I_Count, j] = Cohort[j, C_Count]
    if (j == 21) {
      Wij[I_Count, j + 1] = Cohort[j + 1, C_Count]
    }
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

#constraint (14)
count = 0
row_start = 1 + row_end
row_end = row_end + (I*J)
vector.position = Y_Vars + 1
for(g in row_start:row_end){
  count = count + 1
  Amat[g,vector.position] = 1
  vector.position = vector.position + 1
}
for (i in 1:I){
  for (j in 1:J){
    bvec = c(bvec, P[i,j])
    dir = c(dir, '<=')
  }
}

#constraint (12)
I_Count = 1
D_Count = 1
count = 0

row_start = 1 + row_end
row_end = row_end + (I*D)
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
      }
      Xijtdc[i,j,T_Count + 1,D_Count,C_Count] = 1
      if(C_Count==2){
        if(j == 13 || j == 14 || j == 19 || j == 20 || j == 22){
          Xijtdc[i,j,T_Count + 1,D_Count,1] = 1
        }
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
      }
      Xijtdc[i,j,T_Count + 1,D_Count,C_Count] = 1
      Xijtdc[i,j,T_Count + 2,D_Count,C_Count] = 1
      if(C_Count==2){
        if(j == 13 || j == 14 || j == 19 || j == 20 || j == 22){
          Xijtdc[i,j,T_Count + 1,D_Count,1] = 1
          Xijtdc[i,j,T_Count + 2,D_Count,1] = 1
        }
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
      }
      Xijtdc[I_Count,j,T_Count + 1,D_Count,c] = 1
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
      }
      Xijtdc[I_Count,j,T_Count + 1,D_Count,c] = 1
      Xijtdc[I_Count,j,T_Count + 2,D_Count,c] = 1
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

#constraint (5)
T_Count = 1
D_Count = 1
C_Count = 1
count = 0

row_start = 1 + row_end
row_end = row_end + (T*D*C)
for(g in row_start:row_end){
  Xijtdc = array(0,dim=c(I,J,T,D,C))
  
  for (i in 1:I) {
    for (j in 1:J) {
      Xijtdc[i,j,T_Count,D_Count,C_Count] = 1
    }
  }
  
  Xijtdc.vector = integer(I*J*T*D*C)
  vector.position = 1
  for (i in 1:I){
    for (j in 1:J){
      for (t in 1:T){
        for (d in 1:D){
          for (c in 1:C){
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

#constraint (17)
row_start = 1 + row_end
row_end = row_end + (1)
for(g in row_start:row_end){
  Xijtdc = array(0,dim=c(I,J,T,D,C))
  
  for (i in 1:I) {
    for (j in 1:J) {
      for(d in 1:D){
        for(c in 1:C){
          if (H[j] == 2) {
            Xijtdc[i,j,7,d,c] = 1
            Xijtdc[i,j,8,d,c] = 1
          }
        }
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
  bvec = c(bvec, 0)
  dir = c(dir, '=')
}

#constraint (18)
row_start = 1 + row_end
row_end = row_end + (1)
for(g in row_start:row_end){
  Xijtdc = array(0,dim=c(I,J,T,D,C))
  
  for (i in 1:I) {
    for (j in 1:J) {
      for(d in 1:D){
        for(c in 1:C){
          if (H[j] == 3) {
            Xijtdc[i,j,6,d,c] = 1
            Xijtdc[i,j,7,d,c] = 1
            Xijtdc[i,j,8,d,c] = 1
          }
        }
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
  bvec = c(bvec, 0)
  dir = c(dir, '=')
}

#constraint (19)
D_Count = 1
J_Count = 1
count = 0

row_start = 1 + row_end
row_end = row_end + D*(J-2)/2
for(g in row_start:row_end){
  Xijtdc = array(0,dim=c(I,J,T,D,C))
  for (i in 1:I) {
    for(t in 1:T){
      for(c in 1:C){
        Xijtdc[i, J_Count, t, D_Count, c] = 1
        Xijtdc[i, J_Count + 1, t, D_Count, c] = 1
      }
    }
  }
  
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
    J_Count = J_Count + 2
  }
  else {
    D_Count = D_Count + 1
  }
}

#constraint (20)
J_Count = 1
C_Count = 1
count = 0

row_start = 1 + row_end
row_end = row_end + (J*C)
for(g in row_start:row_end){
  Xijtdc = array(0,dim=c(I,J,T,D,C))
  for (i in 1:I) {
    for (t in 1:T) {
      for (d in 1:D) {
        Xijtdc[i, J_Count, t, d, C_Count] = 1
      }
    }
  }
  
  Xijtdc.vector = integer(I*J*T*D*C)
  vector.position = 1
  for (i in 1:I){
    for (j in 1:J){
      for (t in 1:T){
        for (d in 1:D){
          for (c in 1:C){
            Xijtdc.vector[vector.position] = Xijtdc[i,j,t,d,c]
            vector.position = vector.position + 1
          }
        }
      }
    }
  }
  
  Amat[g,1:X_Vars] = Xijtdc.vector
  bvec = c(bvec, Cohort[J_Count, C_Count])
  dir = c(dir, '<=')
  
  if (C_Count == 2){
    C_Count = 1
    J_Count = J_Count + 1
  }
  else {
    C_Count = C_Count + 1
  }
}


#constraint (6)
T_Count = 1
D_Count = 1
count = 0

row_start = 1 + row_end
row_end = row_end + ((T-1)*D)
for(g in row_start:row_end){
  Xijtdc = array(0,dim=c(I,J,T,D,C))
  
  for (i in 1:I) {
    for (j in 1:J) {
      for (c in 1:C){
        if (j == 13 || j == 14 || j == 19 || j == 20) {
          Xijtdc[i,j,T_Count,D_Count,c] = 1
        }
        Xijtdc[i,j,T_Count + 1,D_Count,c] = 1
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
}

#constraint (7)
T_Count = 1
D_Count = 1
count = 0

row_start = 1 + row_end
row_end = row_end + ((T-2)*D)
for(g in row_start:row_end){
  Xijtdc = array(0,dim=c(I,J,T,D,C))

  for (i in 1:I) {
    for (j in 1:J) {
      for (c in 1:C){
        if (j == 22) {
          Xijtdc[i,j,T_Count,D_Count,c] = 1
        }
        Xijtdc[i,j,T_Count + 1,D_Count,c] = 1
        Xijtdc[i,j,T_Count + 2,D_Count,c] = 1
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
}

#constraint (8)
T_Count = 1
D_Count = 1
count = 0

row_start = 1 + row_end
row_end = row_end + (T*D)
for(g in row_start:row_end){
  Xijtdc = array(0,dim=c(I,J,T,D,C))

  for (i in 1:I) {
    for (j in 1:J) {
      Xijtdc[i,j,T_Count,D_Count,2] = 1
      if(j == 13 || j == 14 || j == 19 || j == 20){
        Xijtdc[i,j,T_Count,D_Count,1] = 1
      }
    }
  }

  Xijtdc.vector = integer(I*J*T*D*C)
  vector.position = 1
  for (i in 1:I){
    for (j in 1:J){
      for (t in 1:T){
        for (d in 1:D){
          for (c in 1:C){
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
}

image(Matrix(Amat[1787:1830,1:100]))

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

Xijtdc = mysol$x[1:17600]
vector.position = 1
for (i in 1:I){
  for (j in 1:J){
    for (t in 1:T){
      for (d in 1:D){
        for (c in 1:C){
          if(Xijtdc[vector.position] == 1) {
            print(paste(i,j,t,d,c, sep = " "))
          }
          vector.position = vector.position + 1
        }
      }
    }
  }
}