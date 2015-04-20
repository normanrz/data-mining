# Assignment 1

# a
list <- c(0.03, 0.04, 0.05, 0.49, 0.50, 0.59, 0.66, 0.72, 0.83, 1.17)
a_mean <- mean(list)
a_median <- median(list)
a_var <- var(list)

# b
list <- 2 * runif(100)
b_mean <- mean(list)
b_median <- median(list)
b_var <- var(list)

# c
list <- runif(10000)
hist(list, breaks=20)

