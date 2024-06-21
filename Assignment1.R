library(gurobi)
library(slam)

###Exercise 2c###

model <- list()
model$A <- rbind(
#Lower bound of shipping locations
  c(1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0), 
  c(0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0),
  c(0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0),
  c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1), 
  
#upper bound 
  c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1)
)

model$obj <- c(14, 13, 10, 19, 15, 19, 14, 19, 20, 18, 13, 14)
model$modelsense <- 'min'
model$rhs <- c(66, 62, 88, 74, 150, 125, 100)
model$sense <- c('>', '>', '>', '>', '<', '<',"<")
model$vtype <- 'C'

params <- list(OutputFlag = 0)

result <- gurobi(model, params)
print('Solution:')
print(result$objval)
print(result$x)

### Exercise 2d ###
model<- list()

cost <- matrix(c(14, 13, 10, 19, 15, 19, 14, 19, 20, 18, 13, 14), nrow = 4, ncol= 3)

L_loc <- c(66, 62, 88, 74)
Uf <- c(150, 125, 100)
nlocation<- length(L_loc) 
nfaciliy <- length(Uf)

smallestCost <- NA 
x <- matrix(0:0, nrow = 4, ncol = 3)
colnames(x) <- c('Uf1', 'Uf2', 'Uf3')
rownames(x) <- c('La', 'Lb', 'Lc', 'Ld')
sum(x[nrow(x), 1])

for (L in 1:nlocation){
  for (f in 1:nfaciliy) {
    if(is.na(smallestCost)) {
      smallestCost <- cost[L,f]
    }
    else if (cost[L,f] < smallestCost) {
      smallestCost <- cost[L,f] 
    }
    else {next}
  }
}

if (sum(x[1:nlocation, f]) <= Uf[f])
  if(x[L, f] == 0)
    x[L,f] <- min(L_loc[L],Uf[f])
if (sum(x[2, 1:nfaciliy]) >= L_loc[L] && sum(x[1:nlocation, f]) <= Uf[f]) {next}


#Question 3 
#function to put the text file as lists 

knapsack <- readLines("knapsack.txt")

parse_line <- function(line) {
  # Remove parentheses
  line <- gsub("\\(", "", line)
  line <- gsub("\\)", "", line)
  
  # Split the line into components
  components <- strsplit(line, ",")[[1]]
  
  # Convert the components to numeric
  components <- as.numeric(components)
  
  # Return the components as a list
  return(components)
}

knapsack <- lapply(knapsack, parse_line)

#Cleaning data and extracting numbers from list

read_data <- function(dataset){
    W <- list()
    V <- list()
    n <- list()
    p <-  list()
    w <-  list()
    v <-  list()
  for (i in 1:length(dataset)) {
    subset <- dataset[[i]]
    W[[i]] <- subset[[1]]
    V[[i]] <- subset[[2]]
    n[[i]] <- subset[[3]]
    p[[i]] <- subset[seq(4, length(subset), by = 3)]
    w[[i]] <- subset[seq(5, length(subset), by = 3)]
    v[[i]] <- subset[seq(6, length(subset), by = 3)]
  }
  return(list(W = W, V = V, n = n, p = p, w = w, v = v))
}

knapsack_data_clean <- read_data(knapsack)

#converting data into integer
for (i in 1:30){
  knapsack_data_clean$W[i] <- lapply(knapsack_data_clean$W[i], as.integer)
  knapsack_data_clean$V[i] <- lapply(knapsack_data_clean$V[i], as.integer)
  knapsack_data_clean$n[i] <- lapply(knapsack_data_clean$n[i], as.integer)
  knapsack_data_clean$p[i] <- lapply(knapsack_data_clean$p[i], as.integer)
  knapsack_data_clean$w[i] <- lapply(knapsack_data_clean$w[i], as.integer)
  knapsack_data_clean$v[i] <- lapply(knapsack_data_clean$v[i], as.integer)
}


#Question 3: Solving the knapsack using gurobi

solve_knapsack <- function(p, w, v, W, V) { 
  model3a <- list()
  
  ObjVec <- p
  
  A <- matrix(0, nrow = 2, ncol = length(p)) 
  A[1, ] <- w
  A[2, ] <- v
  
  model3a$A <- A
  model3a$obj <- ObjVec
  model3a$modelsense <- 'max'
  model3a$rhs <- c(W, V)
  model3a$sense <- c('<', '<')
  model3a$vtype <- 'B'
  
  params <- list(OutputFlag = 0)
  
  result <- gurobi(model3a, params)
  
  total_w <- sum(w * result$x)
  total_v <- sum(v * result$x)
  
  list(selected_items = result$x, total_profit = result$objval, total_weight = total_w, total_volume = total_v)
}

solutions <- list()
for (i in 1:length(knapsack_data_clean$p)){
  solutions[[i]] <- solve_knapsack(knapsack_data_clean$p[[i]], knapsack_data_clean$w[[i]], 
                             knapsack_data_clean$v[[i]], knapsack_data_clean$W[[i]], 
                             knapsack_data_clean$V[[i]])
}



### QUESTION 3b ##
greedy_knapsack <- function(p, w, v, W, V, sortMethod){ 
  
  if (sortMethod == "p") {
    sortVector <- p
  }
  else if (sortMethod == "w") {
    sortVector <- -w
  }
  else if (sortMethod == "v") {
    sortVector <- v
  }
  else if(sortMethod =="p/w"){ 
    sortVector <- p/w
  }
  else {
    sortVector <- p/v
  }
    
  
  sortIndices <- sort(sortVector, decreasing=TRUE, index.return=TRUE)$ix
  
  currentWeight <- 0
  currentVolume <- 0 
  currentProfit <- 0
  x <- numeric(length(p))
  for (i in 1:length(p)) {
    if (currentWeight + w[sortIndices[i]] > W) {
      next
    }
    if (currentVolume + v[sortIndices[i]] > V) {
      next
      }
    x[sortIndices[i]] <- 1
    currentWeight <- currentWeight + w[sortIndices[i]]
    currentVolume <- currentVolume + v[sortIndices[i]]
    currentProfit <- currentProfit + p[sortIndices[i]]
  }
  result <- list()
  result$x <- x
  result$p <- currentProfit
  result$w <- currentWeight
  result$v <- currentVolume
  
  return(result)
}
solutions_greedy <- list(solutions_p <- list(),
  solutions_w <- list(),
  solutions_v <- list(),
  solutions_pw <- list(),
  solutions_pv <- list())

sorting_methods <- c('p', 'w', "v", "p/w", "p/v")

for (sol in solutions_greedy) {
  for(i in 1:length(knapsack_data_clean$p)){
    for(sortMethod in sorting_methods){ 
      sol[[i]] <- greedy_knapsack(knapsack_data_clean$p[[i]], knapsack_data_clean$w[[i]], 
                              knapsack_data_clean$v[[i]], knapsack_data_clean$W[[i]], 
                              knapsack_data_clean$V[[i]], sortMethod)} 
  }
  
}

for (i in 1:length(knapsack_data_clean$p)){
    
  solutions_p[[i]] <- greedy_knapsack(knapsack_data_clean$p[[i]], knapsack_data_clean$w[[i]], 
                                   knapsack_data_clean$v[[i]], knapsack_data_clean$W[[i]], 
                                   knapsack_data_clean$V[[i]], 'p')
  solutions_w[[i]] <- greedy_knapsack(knapsack_data_clean$p[[i]], knapsack_data_clean$w[[i]], 
                                     knapsack_data_clean$v[[i]], knapsack_data_clean$W[[i]], 
                                     knapsack_data_clean$V[[i]], 'w')
  solutions_v[[i]] <- greedy_knapsack(knapsack_data_clean$p[[i]], knapsack_data_clean$w[[i]], 
                                     knapsack_data_clean$v[[i]], knapsack_data_clean$W[[i]], 
                                     knapsack_data_clean$V[[i]], 'v')
  solutions_pw[[i]] <- greedy_knapsack(knapsack_data_clean$p[[i]], knapsack_data_clean$w[[i]], 
                                     knapsack_data_clean$v[[i]], knapsack_data_clean$W[[i]], 
                                     knapsack_data_clean$V[[i]], 'p/w')
  solutions_pv[[i]] <- greedy_knapsack(knapsack_data_clean$p[[i]], knapsack_data_clean$w[[i]], 
                                     knapsack_data_clean$v[[i]], knapsack_data_clean$W[[i]], 
                                     knapsack_data_clean$V[[i]], 'p/v')
}

#Comparing Objective values 
best_solutions <- list(best_solution_p <- NA,
                       best_solution_w <- NA,
                       best_solution_v <- NA,
                       best_solution_pw <- NA, 
                       best_solution_pv<- NA)


best_solution_so_far <- function(solution_list, best_solution){ 
  for(i in 1:length(solution_list)){
    sub_solution <- solution_list[[i]]

    if (is.na(best_solution)){
      best_solution <- sub_solution$p
    }
    else if (sub_solution$p > best_solution){ 
      best_solution <- sub_solution$p
    }
  }
  return(best_solution)
 }


best_solutions <- list(best_solution_p <- best_solution_so_far(solutions_p, best_solution_p), 
                       best_solution_w <- best_solution_so_far(solutions_w, best_solution_w), 
                       best_solution_v <- best_solution_so_far(solutions_v, best_solution_v), 
                       best_solution_pw <- best_solution_so_far(solutions_pw, best_solution_pw), 
                       best_solution_pv <- best_solution_so_far(solutions_pv, best_solution_pv))
#out of the various instances, it seems that the greedy approach that leads to the highest profit is profit over volume




