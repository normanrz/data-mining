# Assignment 1

# a
list <- c(0.03, 0.04, 0.05, 0.49, 0.50, 0.59, 0.66, 0.72, 0.83, 1.17)
mean_1a <- mean(list)
median_1a <- median(list)
var_1a <- var(list)

# b
list <- 2 * runif(100)
mean_1b <- mean(list)
median_1b <- median(list)
var_1b <- var(list)

# c
list <- runif(10000)
hist(list, breaks=20)


# Assignment 2

min_2a <- min(faithful$eruptions)
max_2a <- max(faithful$eruptions)
mean_2a <- mean(faithful$eruptions)
median_2a <- median(faithful$eruptions)
diff_2a <- abs(mean_2a - median_2a)


min_2b <- min(faithful$waiting)
max_2b <- max(faithful$waiting)
mean_2b <- mean(faithful$waiting)
median_2b <- median(faithful$waiting)
diff_2b <- abs(mean_2b - median_2b)


# Assigment 3

iris <- read.csv("iris.data")
length(unique(iris$Species))
table(iris$Species)
plot(iris$Sepal.length, iris$Petal.length, 
     xlab = "Sepal length", ylab = "Petal length", 
     main = "Iris analysis")

iris_setosa_subset <- subset(iris, Species == "Iris-setosa")
rest_subset <- subset(iris, Species != "Iris-setosa")
plot(iris_setosa_subset$Sepal.length, iris_setosa_subset$Petal.length, 
     col = "red", xlab = "Sepal length", ylab = "Petal length", 
     main = "Iris analysis", xlim = c(4,8), ylim = c(0.5,7.5))
points(rest_subset$Sepal.length, rest_subset$Petal.length, 
       col = "blue")


# Assigment 4

mat_4 <- matrix(runif(50 * 100), ncol = 100) 
cor_4 <- cor(mat_4)
hist(cor_4, breaks = 100, xlim = c(-.6, .6), 
     col = "blue", xlab = "Correlation of random values", 
     main = "Correlation histogram")


# Assignment 5

age <- c(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27)
height <- c(76.2, 77.1, 78.1, 78.4, 78.8, 79.7, 79.7, 81.1, 81.2, 81.4, 82.8, 83.5)

plot(age, height, xlab = "Age", ylab = "Height", main = "Age/Height")
abline(lm(height~age))


# Assignment 7 I


# Assigment 7 II

# Prec: 0.95
# Rec: 0.48
# Acc: .73, .63, .56, .48

