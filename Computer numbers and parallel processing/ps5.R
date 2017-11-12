##########Problem Set 5##########

library(pryr)
library(data.table)

####Problem 2####
#execute 2^53-1, 2^53, and 2^53+1
bits(2^53 - 1)
bits(2^53)
bits(2^53 + 1)

####Problem 3####
#set the vector large enough (10^8 numbers)
n <- 1e8
int <- 1:n
num <- as.numeric(int)
#calculate the time of copying the integer vector and the numeric vector
system.time(copy(int))
system.time(copy(num))
#calcuate the time of taking the subset of n/2 from each vector.
system.time(int[1:(n/2)])
system.time(num[1:(n/2)])

####Problem 5####
#find out the exact value in R
options(digits=22)
0.2
0.3
0.01
0.49
0.1
