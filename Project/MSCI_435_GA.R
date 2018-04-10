

get.fitness = function(X, weight=wlist, V=100){
  unpacked.items = vector()
  topack.items = X
  bins = 0
  packed.items = 0
  curr_bin = 0
  while (packed.items < 10) {
    bins = bins + 1
    curr_bin = 0
    for (i in 1:length(topack.items)) {
      if (curr_bin + weight[topack.items[i]] <= V) {
        curr_bin = curr_bin + weight[topack.items[i]]
        curr_bin
        packed.items = packed.items + 1
      } else {
        unpacked.items = cbind(unpacked.items, topack.items[i])
      }
    }
    topack.items = unpacked.items
    unpacked.items <- vector()
  }
  
  1/bins
}

P = Matrix(c(), nrow=10, ncol=22, byrow=T)
stuff = which(apply(P==0,1,all))

#This creates the initial population.
get.random.population = function(n){
  population = vector("list", 100)
  for (g in 1:100) {
    courseToProf = list(sample(c(6,9,10)), sample(c(3,4)), sample(c(5,10)), sample(c(3,4)), sample(c(3,4,9)), sample(c(3,4,8)),
                        sample(c(1,2)), sample(c(5,7,8)), sample(c(5,8)), sample(c(1,2)), c(6), sample(c(6,7,9,10)))
    Cohort_1 = Matrix(c(1,2,3,4,7,8,13,14,15,16,17,18,19,20,22,rep(0,31)), nrow=23, ncol=2, byrow=F)
    Cohort_2 = Matrix(c(5,6,9,10,11,12,21,rep(0,35)), nrow=21, ncol=2, byrow=F)
    
    for (i in seq(1, 15, 2)) {
      if (Cohort_1[i,1] == 22) {
        for (j in 1:length(courseToProf[[12]])) {
          if (length(which(Cohort_1[,2]==courseToProf[[12]][j],2)) == 0) {
            Cohort_1[i,2] = courseToProf[[12]][j]
            break
          } else if (j == length(courseToProf[[12]])) {
            Cohort_1[i,2] = 11
          }
        }
      } else {
        index = ceiling(Cohort_1[i,1]/2)
        for (j in 1:length(courseToProf[[index]])) {
          if (length(which(Cohort_1[,2]==courseToProf[[index]][j],2)) == 0) {
            Cohort_1[i,2] = courseToProf[[index]][j]
            Cohort_1[i+1,2] = courseToProf[[index]][j]
            break
          } else if (j == length(courseToProf[[index]])) {
            Cohort_1[i,2] = 11
          }
        }
      }
    }
    Cohort_1 = Cohort_1[sample(nrow(Cohort_1)),]
    
    for (i in seq(1, 7, 2)) {
      if (Cohort_2[i,1] == 21) {
        for (j in 1:length(courseToProf[[11]])) {
          if (length(which(Cohort_2[,2]==courseToProf[[11]][j],2)) == 0) {
            Cohort_2[i,2] = courseToProf[[11]][j]
            break
          } else if (j == length(courseToProf[[12]])) {
            Cohort_1[i,2] = 11
          }
        }
      } else {
        index = ceiling(Cohort_2[i,1]/2)
        for (j in 1:length(courseToProf[[index]])) {
          if (length(which(Cohort_2[,2]==courseToProf[[index]][j],2)) == 0) {
            Cohort_2[i,2] = courseToProf[[index]][j]
            Cohort_2[i+1,2] = courseToProf[[index]][j]
            break
          } else if (j == length(courseToProf[[index]])) {
            Cohort_1[i,2] = 11
          }
        }
      }
    }
    Cohort_2 = Cohort_2[sample(nrow(Cohort_2)),]
    
    population[[g]] = list(Cohort_1, Cohort_2)
  }
  population
}

#For each member of the population, this generates the probability of being chosen.
roulette.wheel = function(fitness){
  probs = fitness/sum(fitness)
  sample(length(fitness), length(fitness), replace=T, prob=probs)
}

#This function decides which two members of the population will be mating.
cross.over = function(mating.population, p.c=0.75){
  rlist = runif(ncol(mating.population),0,1)
  c.indices = which(rlist < p.c)
  if(length(c.indices) <= 1){
    c.indices = order(rlist)[1:2]
  }
  if( (length(c.indices) %% 2) ==1 ){
    c.indices = c.indices[-which(rlist[c.indices] == max(rlist[c.indices]))]
  }
  pairs = matrix(c.indices, ncol=2, byrow=T)
  offsprings = NULL 
  for(i in 1:nrow(pairs)){
    offsprings = cbind(offsprings, coitus(mating.population[(1:10),pairs[i,1]],mating.population[(1:10),pairs[i,2]]),
                       coitus(mating.population[(1:10),pairs[i,2]],mating.population[(1:10),pairs[i,1]]))
  }
  list(offsprings = offsprings, c.indices = c.indices)
}

#The chosen parents mate and generate an offspring.
coitus = function(parent_1, parent_2) {
  offspring = vector()
  for (i in 1:length(parent_1)) {
    if (!(parent_1[i] %in% offspring)) {
      offspring = c(offspring, parent_1[i])
    }
    if (!(parent_2[i] %in% offspring)) {
      offspring = c(offspring, parent_2[i])
    }
  }
  offspring
}

#This allows mutation, where two items of an offspring are swapped in their order.
mutation = function(offsprings, p.m=0.1){
  for(i in 1:ncol(offsprings)){
    v = runif(1)
    if (1 < p.m) {
      swap = sample(1:10, 2, replace=F)
      tmp = offsprings[swap[1],i]
      offsprings[swap[1],i] = offsprings[swap[2],i]
      offsprings[swap[2],i] = tmp
    }
  }
  offsprings
}