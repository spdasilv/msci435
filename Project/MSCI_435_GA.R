H = c(2,	2,	3,	2,	2,	2,	2,	2,	2,	2,	2,	2,	2,	2,	2,	2,	2,	2,	2,	2,	3,	3)

get.fitness = function(Cohort_1, Cohort_2, H){
  Cohort_1 = population[1][[1]][1][[1]]
  Cohort_2 = population[1][[1]][2][[1]]
  schedule_1 = matrix(list(), nrow=8, ncol=5)
  schedule_2 = matrix(list(), nrow=8, ncol=5)
  mondayProfs = c()
  fridayProfs = c()
  
  # Evaluate objective function
  penalty = 0
  
  # Get number of courses taught by professors
  profToCourse = as.data.frame(table(c(Cohort_1[,2], Cohort_2[,2])))
  for (i in 2:nrow(profToCourse)) {
    if (profToCourse[6,2] > 2) {
      penalty = penalty + 8
    } else {
      penalty = penalty + 4
    }
  }
  
  
  # Penalize number of professors
  penalty = penalty + 10*(length(unique(c(Cohort_1[,2], Cohort_2[,2]))) - 1)
  
  # Generater Schedule for Cohort 1
  j = 1
  Cohort_1_Selected = c(rep(FALSE,23))
  while (j <= 5)
  {
    i = 1
    while (i <= 8)
    {
      feasible = FALSE
      for (g in 1:length(Cohort_1_Selected)) {
        if (!Cohort_1_Selected[g] && Cohort_1[g,1] == 0) {
          schedule_1[[i,j]] = c(0,0)
          i = i + 1
          Cohort_1_Selected[g] = TRUE
          feasible = TRUE
          break
        } else if (!Cohort_1_Selected[g] && (9 - i >= H[Cohort_1[g,1]])) {
          for (h in 1:H[Cohort_1[g,1]]) {
            if (Cohort_1[g,1] %in% c(13,14,19,20,22)) {
              schedule_2[[i,j]] = c(Cohort_1[g,1], Cohort_1[g,2])
            }
            schedule_1[[i,j]] = c(Cohort_1[g,1], Cohort_1[g,2])
            i = i + 1
          }
          Cohort_1_Selected[g] = TRUE
          feasible = TRUE
          
          # If a course is scheduled on Morning, Lunch of Evening slots penalize it
          if (i %in% c(1,4,8)) {
            penalty = penalty + 5
          }
          
          # If a professor is scheduled on a Monday or Friday penalize it
          if (j  == 1) {
            mondayProfs = c(mondayProfs, Cohort_1[g,2])
          }
          
          if (j  == 5) {
            fridayProfs = c(fridayProfs, Cohort_1[g,2])
          }
          
          break
        }
      }
      if (!feasible) {
        i = i + 1
      }
    }
    j = j + 1
  }
  
  # Generater Schedule for Cohort 2
  j = 1
  Cohort_2_Selected = c(rep(FALSE,21))
  while (j <= 5)
  {
    i = 1
    while (i <= 8)
    {
      feasible = FALSE
      for (g in 1:length(Cohort_2_Selected)) {
        if (!Cohort_2_Selected[g] && Cohort_2[g,1] == 0) {
          if(length(which(sapply(schedule_2[i,j], is.null) == FALSE)) == 0){
            schedule_2[[i,j]] = c(0,0)
            i = i + 1
            Cohort_2_Selected[g] = TRUE
            feasible = TRUE
            break
          } else {
            break
          }
        } else if (!Cohort_2_Selected[g] && (9 - i >= H[Cohort_2[g,1]])) {
          if(length(which(sapply(schedule_2[i:(i+H[Cohort_2[g,1]]-1),j], is.null) == FALSE)) == 0){
            for (h in 1:H[Cohort_2[g,1]]) {
              schedule_2[[i,j]] = c(Cohort_2[g,1], Cohort_2[g,2])
              i = i + 1
            }
            Cohort_2_Selected[g] = TRUE
            feasible = TRUE
            
            # If a course is scheduled on Morning, Lunch of Evening slots penalize it
            if (i %in% c(1,4,8)) {
              penalty = penalty + 5
            }
            
            # If a professor is scheduled on a Monday or Friday penalize it
            if (j  == 1) {
              mondayProfs = c(mondayProfs, Cohort_1[g,2])
            }
            
            if (j  == 5) {
              fridayProfs = c(fridayProfs, Cohort_1[g,2])
            }
            
            break
          }else {
            break
          }
        }
      }
      
      if (!feasible) {
        i = i + 1
      }
    }
    j = j + 1
  }
  
  penalty = penalty + 8*(length(unique(c(mondayProfs))))
  penalty = penalty + 8*(length(unique(c(fridayProfs))))
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