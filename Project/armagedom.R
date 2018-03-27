library("Matrix")
library("igraph")
library("rdist")

I = 10
J = 12
T = 8
D = 5
C = 2
K = 2
M = 1000

H = c(4,	5,	3,	4,	4,	4,	4,	4,	4,	4,	3,	4)
Cohort = matrix(c(1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0), nrow=J, ncol=C, byrow=T)
P = matrix(c(0, 	0, 	0, 	0, 	0, 	0, 	0, 	1, 	0, 	0, 	0, 	1, 
             0, 	0, 	0, 	0, 	0, 	0, 	0, 	1, 	0, 	0, 	0, 	1, 
             0, 	1, 	0, 	0, 	1, 	1, 	1, 	0, 	0, 	0, 	0, 	0, 
             0, 	1, 	0, 	0, 	1, 	1, 	1, 	0, 	0, 	0, 	0, 	0, 
             0, 	0, 	0, 	1, 	0, 	0, 	0, 	0, 	1, 	1, 	0, 	0, 
             1, 	0, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	1, 	0, 
             0, 	0, 	0, 	0, 	0, 	0, 	0, 	0, 	1, 	0, 	1, 	0, 
             0, 	0, 	0, 	0, 	0, 	0, 	1, 	0, 	1, 	1, 	0, 	0, 
             1, 	0, 	0, 	0, 	0, 	1, 	0, 	0, 	0, 	0, 	1, 	0, 
             1, 	0, 	0, 	1, 	0, 	0, 	0, 	0, 	0, 	0, 	1, 	0), nrow=I, ncol=J, byrow=T)

Amat = matrix(0, nrow=(J*C + I*T*D + T*D*C + I*D + I*J + 2*I + J + 1 + I*J), ncol=(I*J*T*D*C + I*D + I*K + I*J))

#Constraint (1) and (5)
J_Count = 1
C_Count = 1
count = 0

row_start = 1
row_end = (J*C)
for(g in row_start:row_end){
  Xijtdc = array(0,dim=c(I,J,T,D,C))
  for (i in 1:I) {
    for (t in 1:T) {
      for (d in 1:D) {
        Xijtdc[i,J_Count,t,d,C_Count] = 1
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
  
  Amat[g,1:9600] = Xijtdc.vector
  
  if (C_Count == 2){
    C_Count = 1
    J_Count = J_Count + 1
  }
  else {
    C_Count = C_Count + 1
  }
}

#constraint (3)
I_Count = 1
T_Count = 1
D_Count = 1
count = 0

row_start = 1 + row_end
row_end = row_end + (I*T*D)
for(g in row_start:row_end){
  Xijtdc = array(0,dim=c(I,J,T,D,C))
  for (j in 1:J) {
    for (c in 1:C) {
      Xijtdc[I_Count, j, T_Count, D_Count, c] = 1
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
  
  Amat[g,1:9600] = Xijtdc.vector
  
  if (D_Count == 5){
    D_Count = 1
    T_Count = T_Count + 1
  }
  else {
    D_Count = D_Count + 1
  }
  if (T_Count == 9) {
    T_Count = 1
    I_Count = I_Count + 1
  }
}

#constraint (4)
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
      Xijtdc[i, j, T_Count, D_Count, C_Count] = 1
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
  
  Amat[g,1:9600] = Xijtdc.vector
  
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
  
  Amat[g,1:9600] = Xijtdc.vector
  Amat[g,9601:9650] = Zid.vector
  
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
  
  Amat[g,1:9600] = Xijtdc.vector
  Amat[g,9671:9790] = Wij.vector
  
  if (J_Count == 12){
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
  
  Amat[g,9671:9790] = Wij.vector
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
    Yik.vector[vector.position] = 1
    vector.position = vector.position + 1
    Yik.vector[vector.position] = 2
    vector.position = vector.position + 1
  }
  
  Amat[g,9671:9790] = Wij.vector
  Amat[g,9651:9770] = Yik.vector
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
  
  Amat[g,9671:9790] = Wij.vector
  if (J_Count == 12){
    J_Count = 1
  }
  else {
    J_Count = J_Count + 1
  }
}


#constraint (1)
row_start = 1 + row_end
row_end = row_end + 1
Amat[row_start,9651:9670] = c(rep(1, 20))


#constraint (14)
count = 0

row_start = 1 + row_end
row_end = row_end + (J)

for(g in row_start:row_end){
  vector.position = 9671
  for (i in 1:I){
    for (j in 1:J){
      count = count + 1
      Amat[g,vector.position] = Wij.vector = 1
      vector.position = vector.position + 1
    }
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
  
  Amat[g,9671:9790] = Wij.vector
  
  if (I_Count == 10){
    I_Count = 1
    C_Count = C_Count + 1
  }
  else {
    I_Count = I_Count + 1
  }
}
