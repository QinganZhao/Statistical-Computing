library(pryr)
library(ggplot2)

##problem 4b##
#this function computes beta hat using the method presented in Problem 4a
beta_hat <- function(X, Y, A, b){
  X_QR <- qr(X)
  Q <- qr.Q(X_QR)
  R <- qr.R(X_QR)
  C <- crossprod(R, R)
  Cinv_d <- backsolve(R, crossprod(Q, Y))
  A_Rinv <- A %*% solve(R)
  A_Cinv_At <- tcrossprod(A_Rinv, A_Rinv)
  beta <- Cinv_d + solve(C, crossprod(A, solve(A_Cinv_At, -A %*% Cinv_d + b)))
  return(beta)
}

#run a example and compare the time with the naive approach
set.seed(123)
X <- matrix(runif(1e6), ncol=1000)
Y <- matrix(runif(1e6), ncol=1000)
A <- matrix(runif(1e6), ncol=1000)
b <- matrix(runif(1e6), ncol=1000)
system.time(beta_1 <- beta_hat(X, Y, A, b))
system.time(beta_2 <- solve(t(X) %*% X) %*% (t(X)%*%Y) + solve(t(X) %*% X) %*% 
              t(A) %*% solve(A %*% solve(t(X) %*% X) %*% t(A)) %*% 
              (-A %*% solve(t(X) %*% X) %*% (t(X) %*% Y)+b))

#check if we got the correct answer
all.equal(beta_1, beta_2, tolerance=1e-5)

##problem 6##
#generate A and Gamma
set.seed(123)
n <- 100
Z <- matrix(rnorm(n^2), n, n)
A <- crossprod(Z)
Gamma <- cbind(eigen(A)$vectors)

#create eigenvalues with different magnitudes and vary between equal & having a range
#magnitude from 1 to 1e12 (intervel is 100)
#so the number of sets of eigenvalues is 7*2=14
num_sets <- 14
eigs_actual <- matrix(0, num_sets, n)
eigs_compute <- matrix(0, num_sets, n)
A_create <- array(0, c(num_sets, n, n))
magnitude <- rep(0, num_sets)
condition_num <- rep(0, num_sets)
error <- rep(0, num_sets)
pos_definite <- rep(NA, num_sets)
  
for (i in seq(1, 13, 2)){
  eigs_actual[i, ] <- rep(10^((i-1)), n) 
  magnitude[i] <- 10^((i-1))
}

for (i in seq(2, 14, 2)){
  eigs_actual[i, ] <- seq(10^(-(i-2)), 10^((i-2)), length.out = n)
  magnitude[i] <- 10^((i-2))
}

#employ eigen decomposition & compute the following items
for (i in 1:num_sets){
  A_create[i, , ] <- Gamma %*% diag(eigs_actual[i, ]) %*% solve(Gamma)
  eigs_compute[i, ] <- eigen(A_create[i, , ])$values
  condition_num[i] <- abs(max(eigs_actual[i, ]) / min(eigs_actual[i, ]))
  pos_definite[i] <- all(eigs_compute[i, ]>0)
  error[i] <- sum((eigs_compute[i, ] - eigs_actual[i, ])^2)
}

#create a data frame to visualize the results
data_frame <- data.frame(Magnitude = magnitude, Condition_number = condition_num,
                         Error = error, Positive_definite = pos_definite)
data_frame

#plot magnitude vs error when all eigenvalues are equal
ggplot(data_frame[seq(1, num_sets-1, 2), ], aes(x=Magnitude)) + 
  geom_line(aes(y=Error))

#plot magnitude vs error when all eigenvalues having a range from large to small
ggplot(data_frame[seq(2, num_sets, 2), ], aes(x=Magnitude)) + 
  geom_line(aes(y=Error))

#since when all eigenvalues are equal, condition numbers are the same
#hence we do not plot condition number vs error under this condtion

#plot condition number vs error when all eigenvalues having a range from large to small
ggplot(data_frame[seq(2, num_sets, 2), ], aes(x=Condition_number)) + 
  geom_line(aes(y=Error))
