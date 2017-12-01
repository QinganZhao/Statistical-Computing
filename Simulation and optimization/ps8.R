########## R code for ps8 ##########
library(PtProcess)
library(fields)

##problem 1a##

#compare the density of the two distributions to confirm our conclusion
x = seq(2, 10, 0.1)
par(mfrow=c(1, 2))
#dpareto() is in the 'actuar' package; alpha <- 2 and beta <- 3
plot(x, dpareto(x, 3, 2), ylab='density', type='l', main='Pareto')
plot(x, dexp(x), ylab='density', type='l', main='exponential')


##problem 1b##

set.seed(123)
m <- 10000
#generate x, f(x), and g(x)
x <- rpareto(m, 3, 2)
fx <- dexp(x-2)
gx <- dpareto(x, 3, 2)
#h(x)f(x)/g(x) when h(x)=x
EX <- fx / gx * x
#h(x)f(x)/g(x) when h(x)=x^2
EX2 <- fx / gx * x^2

#histograms
par(mfrow=c(2, 2))
hist(EX)
hist(EX2)
hist(fx / gx)

#variances
var(x)
var(EX)
var(x^2)
var(EX2)


##problem 1c##

set.seed(123)
#regenerate x, f(x), and g(x)
x <- rexp(m) + 2    #dpareto() does not allow x<alpha so we just add 2
fx <- dpareto(x, 3, 2)
gx <- dexp(x-2)
#h(x)f(x)/g(x) when h(x)=x
EX <- fx / gx * x
#h(x)f(x)/g(x) when h(x)=x^2
EX2 <- fx / gx * x^2

#histograms
par(mfrow=c(2, 2))
hist(EX)
hist(EX2)
hist(fx / gx)

#variances
var(x)
var(EX)
var(x^2)
var(EX2)


##problem 2##

#"helical valley" function provided by the instructor
theta <- function(x1,x2) atan2(x2, x1)/(2*pi)

f <- function(x) {
  f1 <- 10*(x[3] - 10*theta(x[1],x[2]))
  f2 <- 10*(sqrt(x[1]^2 + x[2]^2) - 1)
  f3 <- x[3]
  return(f1^2 + f2^2 + f3^2)
}

par(mfrow=c(4,3))
#assign 5 constants to the third input
#the reson why add '1' in this set is that this could make the length of x3 even
#thus plots arranged in 4x3 would make a nice view of the document
X3 <- c(-10, -2, 0, 1, 2, 10)

#the size that the plots are going to show
x1 <- seq(-10, 10, 0.5)
x2 <- seq(-10, 10, 0.5)
Dim <- length(x1)

for (i in 1:length(X3)){
  #using paste0() to assign f1, f2,... in a for loop
  assign(paste0('f', i), apply(as.matrix(expand.grid(x1, x2)), 1, function(x) f(c(x, X3[i]))))
  x3 <- matrix(get(paste0('f', i)), Dim, Dim)
  #plot the slices in 3D
  persp(x1, x2, x3, main=paste0('Slice (3D) when x3=', X3[i]))
  #plot the slices in 2D
  image(x1, x2, x3, col=tim.colors(32), main=paste0('Slice when x3=', X3[i]))
  #add contour
  contour(x1, x2, x3, add=TRUE)
}

#explore the possibility of local minima
points <- cbind(c(0, 0, 0), c(10, 1, 1), c(10, 0, -6), c(5, -7, 4), c(-3, -6, 2))
for (i in 1:5){
  cat('minimum starting at (', points[1,i], points[2,i], points[3,i], 
      ') using optim():', optim(points[, i], f)$value, '\n')
  cat('minimum starting at (', points[1,i],points[2,i], points[3,i], 
      ') using nlm():', nlm(f, points[, i])$minimum, '\n')
}

#explore the possibility of local minimum points
for (i in 1:5){
  cat('minimum point starting at (', points[1,i], points[2,i], points[3,i], 
      ') using optim(): \n', optim(points[, i], f)$par, '\n')
  cat('minimum point starting at (', points[1,i],points[2,i], points[3,i], 
      ') using nlm(): \n', nlm(f, points[, i])$estimate, '\n')
}


##problem 3c##

#this function estimates the 3 parameters using the EM algorithm presented in 3a
my_EM <- function(dat, max_iter = 1e5){
  #initialization and set the starting values
  par_ini <- summary(lm(dat$y ~ dat$x))
  beta0_tmp <- par_ini$coefficients[1, 1]
  beta1_tmp <- par_ini$coefficients[2, 1]
  sigma_tmp <- par_ini$sigma
  x <- dat$x
  x_t <- x[dat$truncate == TRUE]
  y_star <- dat$y
  Q_tmp <- 0
  i <- 1 #the start point of the loop
  Q <- 1 #make sure the the following loop could start
  
  #this loop would compute the parameters
  while(i <= max_iter & abs(Q - Q_tmp) > 1e-6){
    #updata parameters and Q
    if(i != 1){
      beta0_tmp <- beta0
      beta1_tmp <- beta1
      sigma_tmp <- sigma
      Q_tmp <- Q
    }
    
    #compute E(y|y>tau), V(y|y>tau), and update y*
    mu <- beta0_tmp + beta1_tmp * x_t
    tau_star <- (dat$tau - mu) / sigma_tmp
    rho <- dnorm(tau_star) / (1 - pnorm(tau_star))
    Ey_t <- mu + sigma_tmp * rho
    Vy_t <- sigma_tmp^2 * (1 + tau_star * rho - rho^2)
    y_star[dat$truncate == TRUE] <- Ey_t
    
    #update beta0 and beta1 using lm()
    par <- lm(y_star ~ x)
    beta0 <- par$coefficients[1]
    beta1 <- par$coefficients[2]
    
    #update sigma, Q, and i
    RSS_star <- sum((y_star - beta0 - beta1 * x)^2) + sum(Vy_t)
    sigma <- sqrt(RSS_star / n)
    Q <- -n / 2 * log(2 * pi * sigma^2) - RSS_star / (2 * sigma^2)
    i <- i + 1
  }
  cat('beta0: ', beta0, '\n', 'beta1: ', beta1, '\n', 'sigma2: ', sigma^2, '\n',
      'iteration number: ', i, sep = '')
  return(list(beta0 = beta0, beta1 = beta1, sigma2 = sigma^2))
}

#test data provided by the instructor

set.seed(1)
n <- 100
beta0 <- 1
beta1 <- 2
sigma2 <- 6

x <- runif(n)
yComplete <- rnorm(n, beta0 + beta1*x, sqrt(sigma2))

#data process
tau1 <- quantile(yComplete, 0.2)  #modest proportion
tau2 <- quantile(yComplete, 0.8)  #high proportion
truncate1 <- yComplete > tau1
truncate2 <- yComplete > tau2
dat1 <- list(x = x, y = pmin(tau1, yComplete), tau = tau1, truncate = truncate1)
dat2 <- list(x = x, y = pmin(tau2, yComplete), tau = tau2, truncate = truncate2)

#test
par1 <- my_EM(dat = dat1)
par2 <- my_EM(dat = dat2)


##problem 3d##

#set the starting values
par_ini1 <- summary(lm(dat1$y ~ dat1$x))
par_ini2 <- summary(lm(dat2$y ~ dat2$x))
ini1 <- c(beta0 = par_ini1$coefficients[1, 1], beta1 = par_ini1$coefficients[2, 1], 
          sigma2 = par_ini1$sigma^2)
ini2 <- c(beta0 = par_ini2$coefficients[1, 1], beta1 = par_ini2$coefficients[2, 1], 
          sigma2 = par_ini2$sigma^2)

#this function returns the negative log-likelihood (minimize it!)
NLL <- function(par, dat){
  beta0 <- par[1]
  beta1 <- par[2]
  sigma2 <- par[3]
  mu <- beta0 + beta1 * x
  LL <- sum(dnorm(dat$y[dat$truncate == FALSE], mu[dat$truncate == FALSE], sqrt(sigma2),
                  log = TRUE), pnorm(dat$tau, mu[dat$truncate == FALSE], sqrt(sigma2),
                                      log = TRUE, lower.tail = FALSE))
  return(-LL)
}

#optimization
par1 <- optim(par = ini1, fn = NLL, dat = dat1, hessian = TRUE)
par1$par #estimated parameters
diag(solve(par1$hessian))  #standard errors of the estimated parameters
cat('iteration number:', par1$counts[1])

par2 <- optim(par = ini2, fn = NLL, dat = dat2, hessian = TRUE)
par2$par #estimated parameters
diag(solve(par2$hessian))  #standard errors of the estimated parameters
cat('iteration number:', par2$counts[1])

