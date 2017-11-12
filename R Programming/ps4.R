##########code for ps4##########
library(pryr)
library(microbenchmark)
library(ggplot2)

####problem 1b####
#function "myFun"
x <- 1:1000000
f <- function(input){
  data <- input
  g <- function(param) return(param * data)
  return(g)
}
myFun <- f(x)
#generate bytes storing information in the closure
length(serialize(myFun, NULL))
#create a new environment with vector x
e <- new.env()
e$x <- 1:1000000
#generate bytes storing information in the environment
length(serialize(e, NULL))

####problem 1c####
x <- 1:10
f <- function(data){
  g <- function(param) return(param * data)
  return(g)
}
myFun <- f(x)
rm(x)
#assign data in the environment of the closure
environment(myFun)$data <- 1:10
myFun(3)

####problem 2a (in R not Rstudio)####
#create a list of 2 vectors
vec_a <- 1:10
vec_b <- 1:20
list_1 <- list(vec_a, vec_b)
.Internal(inspect(list_1))
list_1[[1]][1] <- 2
.Internal(inspect(list_1))

environment(myFun)$data <- 1:1000000
length(serialize(myFun, NULL))

####problem 2b (in R not Rstudio)####
#make a copy of the list
list_2 <- list_1
.Internal(inspect(list_2))

#make a change in the first vector
list_2[[1]] <- 1:15
.Internal(inspect(list_2))

####problem 2c (in R not Rstudio)####
#create a list of 2 lists
list_3 <- list(list_1, list_2)
#copy the list
list_4 <- list_3
.Internal(inspect(list_3))

#add an element 1:10 to the second list
list_4[[3]] <- list(1:10)
.Internal(inspect(list_4))

####problem 2d (in R not Rstudio)####
#code provided by the instructor
gc()
tmp <- list()
x <- rnorm(1e7)
tmp[[1]] <- x
tmp[[2]] <- x
.Internal(inspect(tmp))
object.size(tmp)
gc()

object_size(tmp)

####problem 3####
#original code provided by the instructor
load('/Users/franklin/Desktop/ps4prob3.Rda') # should have A, n, K
ll <- function(Theta, A) {
  sum.ind <- which(A==1, arr.ind=T)
  logLik <- sum(log(Theta[sum.ind])) - sum(Theta)
  return(logLik)
}
oneUpdate <- function(A, n, K, theta.old, thresh = 0.1) {
  theta.old1 <- theta.old
  Theta.old <- theta.old %*% t(theta.old)
  L.old <- ll(Theta.old, A)
  q <- array(0, dim = c(n, n, K))
  for (i in 1:n) {
    for (j in 1:n) {
      for (z in 1:K) {
        if (theta.old[i, z]*theta.old[j, z] == 0){
          q[i, j, z] <- 0
        } else {
          q[i, j, z] <- theta.old[i, z]*theta.old[j, z] /
            Theta.old[i, j]
        }
      }
    }
  }
  theta.new <- theta.old
  for (z in 1:K) {
    theta.new[,z] <- rowSums(A*q[,,z])/sqrt(sum(A*q[,,z]))
  }
  Theta.new <- theta.new %*% t(theta.new)
  L.new <- ll(Theta.new, A)
  converge.check <- abs(L.new - L.old) < thresh
  theta.new <- theta.new/rowSums(theta.new)
  return(list(theta = theta.new, loglik = L.new,
              converged = converge.check))
}
# initialize the parameters at random starting values
temp <- matrix(runif(n*K), n, K)
theta.init <- temp/rowSums(temp)
# do single update
out <- oneUpdate(A, n, K, theta.init)

#calculate the running time of the original code
system.time(oneUpdate(A, n, K, theta.init))

#refined code
#the ll() function remains unchanged
oneUpdate <- function(A, n, K, theta.old, thresh = 0.1){
  #'theta.old1 <- theta.old' is not needed
  Theta.old <- theta.old %*% t(theta.old)
  L.old <- ll(Theta.old, A)
  #'q' can be replaced by a temporary value 'tmp' in the only for loop
  #discard 2 of the 4 for loops using matrix multiplication
  #combine the rest 2 for loops into one
  theta.new <- theta.old
  for (z in 1:K){
    #this is actually q[, , z]
    tmp <- theta.old[, z] %*% t(theta.old[, z]) / Theta.old
    #calculate theta.new using only one for loop
    theta.new[, z] <- rowSums(A * tmp) / sqrt(sum(A * tmp))
  }
  #the rest of the code remains unchanged
  Theta.new <- theta.new %*% t(theta.new)
  L.new <- ll(Theta.new, A)
  converge.check <- abs(L.new - L.old) < thresh
  theta.new <- theta.new / rowSums(theta.new)
  return(list(theta = theta.new, loglik = L.new, converged = converge.check))
}
out_new <- oneUpdate(A, n, K, theta.init)

#calculate the running time of the original code
system.time(oneUpdate(A, n, K, theta.init))

#check if the result is correct
identical(out, out_new)

####problem 4####
#PIKK and FYKU code provided by the instructor
PIKK <- function(x, k) {
  x[sort(runif(length(x)), index.return = TRUE)$ix[1:k]]
}

FYKD <- function(x, k) {
  n <- length(x)
  for(i in 1:n) {
    j = sample(i:n, 1)
    tmp <- x[i]
    x[i] <- x[j]
    x[j] <- tmp
  }
  return(x[1:k])
}

#refined PIKK code 'PIKK_pro'
PIKK_pro <- function(x, k) {
  order(runif(x))[1:k]
}

#compare 'PIKK()' with 'PIKK_pro()' given that n=10000 and k=500
x <- 1:100000
k <- 5000
microbenchmark(PIKK(x, k), PIKK_pro(x, k))

#refined 'FYKD' code 'FYKD_pro'
FYKD_pro <- function(x, k){
  #Pre-allocate memory
  vec <- vector('numeric', k)
  n <- length(x)
  #similar to the FYKD code but using 'vec' rather than 'tmp'
  for(i in 1:k){
    j = sample(i:n, 1)
    vec[i] <- x[j]
    x[j] <- x[i]
    x[i] <- vec[i]
  }
  return(vec)
}

#compare 'FYKD()' with 'FYKD_pro()' given that n=10000 and k=500
x <- 1:10000
k <- 500
microbenchmark(FYKD(x, k), FYKD_pro(x, k))

#plots
#let k be 50, n vary from 100 to 10000, and compare 'PIKK' with 'PIKK_pro'
plot_1 <- microbenchmark(PIKK(1:100, 50),
                         PIKK(1:1000, 50),
                         PIKK(1:10000, 50),
                         PIKK_pro(1:100, 50),
                         PIKK_pro(1:1000, 50),
                         PIKK_pro(1:10000, 50))
autoplot(plot_1)

#let n be 10000, k vary from 50 to 5000, and compare 'PIKK' with 'PIKK_pro'
plot_2 <- microbenchmark(PIKK(1:10000, 50),
                         PIKK(1:10000, 500),
                         PIKK(1:10000, 5000),
                         PIKK_pro(1:10000, 50),
                         PIKK_pro(1:10000, 500),
                         PIKK_pro(1:10000, 5000))
autoplot(plot_2)

#let k be 50, n vary from 100 to 10000, and compare 'FYKD' with 'FYKD_pro'
plot_3 <- microbenchmark(FYKD(1:100, 50),
                         FYKD(1:1000, 50),
                         FYKD(1:10000, 50),
                         FYKD_pro(1:100, 50),
                         FYKD_pro(1:1000, 50),
                         FYKD_pro(1:10000, 50))
autoplot(plot_3)

#let n be 10000, k vary from 50 to 5000, and compare 'FYKD' with 'FYKD_pro'
plot_4 <- microbenchmark(FYKD(1:10000, 50),
                         FYKD(1:10000, 500),
                         FYKD(1:10000, 5000),
                         FYKD_pro(1:10000, 50),
                         FYKD_pro(1:10000, 500),
                         FYKD_pro(1:10000, 5000))
autoplot(plot_4)