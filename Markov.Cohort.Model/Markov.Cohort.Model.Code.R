#Create number of cycles
n_t <- 40
#States
n_s <- 3
#Cohort
n_c <- 1000
#State names
v_state_names <- c("Healthy", "Sick", "Dead")

# Matrix Probabilities with transition periods
trans_mat <- array(NA_real_,
             dim = c(n_s, n_s, n_t),
            dimnames = list(from = v_state_names,
                            to = v_state_names,
                            cycle = 1:n_t))

#Rules (like dead can't go healthy)
trans_mat[2,1, ] <- 0
trans_mat[3,1, ] <- 0
trans_mat[3,2, ] <- 0
#Base transition probabilities
trans_mat[1,2, ] <- 0.03
trans_mat[3,3, ] <- 1
#Vary transition probabilities
trans_mat[1,3, 1:10] <- 0.01
trans_mat[1,3, 11:20 ] <- 0.02
trans_mat[1,3, 21:30] <- 0.04
trans_mat[1,3, 31:40] <- 0.08

trans_mat[2,3, ] <- trans_mat[1,3, ] + 0.04

trans_mat[2, 2, ] <- 1 - trans_mat[2, 3, ]

trans_mat[1,1,] <- 1 - apply(trans_mat[1, , ], 2, sum, na.rm = TRUE)

head(trans_mat, 5)

#Create parking lot for state membership
state_membership <- array(NA_real_, dim = c(n_t, n_s),
                          dimnames = list(cycle = 1:n_t, state = v_state_names))
#Create initial state
state_membership[1, ] <- c(n_c, 0, 0)
#State membership iteration loop
for (i in 2:n_t) { state_membership[i, ] <- state_membership[i-1, ] %*% 
  trans_mat[ , , i - 1]}

head(state_membership, 5)

#Create payoff matrix
payoffs <- array(NA_real_,
                 dim = c(n_s, 2, n_t),
                 dimnames = list(state = v_state_names,
                                  payoff = c("Cost", "QALY"),
                                  cycle = 1:n_t))
payoffs[, , 1:10] <- c(10, 800, 0, 0.95, 0.65, 0)
payoffs[, , 11:20] <- c(25, 1000, 0, 0.92, 0.60, 0)
payoffs[, , 21:30] <- c(40, 1200, 0, 0.88, 0.55, 0)
payoffs[, , 31:40] <- c(80, 1000, 0, 0.85, 0.50, 0)

head(payoffs, 5)

payoff_trace <- array(NA_real_,
                      dim = c(n_t, 2),
                      dimnames = list(cycle = 1:n_t, payoff = c("Cost", "QALY")))
for(i in 1:n_t) {
  payoff_trace[i, ] <- state_membership[i, ] %*% payoffs[, , i]
}

head(payoff_trace, 5)

#Get Avg 
colSums(payoff_trace)/n_c

#Plotting for state trends
matplot(1:n_t, state_membership, type = 'l')
