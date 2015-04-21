# Utility functions
printf <- function(...) cat(sprintf(...))

# Assignment 1

printMeanMedianVar <- function(list) {
  print(list)
  printf("mean: %f\n", mean(list))
  printf("median: %f\n", median(list))
  printf("variance: %f\n", var(list))
}

# a
print("1a")
list <- c(0.03, 0.04, 0.05, 0.49, 0.50, 0.59, 0.66, 0.72, 0.83, 1.17)
printMeanMedianVar(list)

# b
print("1b")
list <- 2 * runif(100)
printMeanMedianVar(list)

# c
list <- runif(10000)
hist(list, breaks=20, 
     main = "Histogram of random uniform numbers", xlab = "")


# Assignment 2

printMinMaxMeanMedianDiff <- function (col) {
  printf("min: %f\n", min(col))
  printf("max: %f\n", max(col))
  printf("mean: %f\n", mean(col))
  printf("median: %f\n", median(col))
  printf("abs diff of median and mean: %f\n", abs(mean(col) - median(col)))
}

print("2")
print("Eruptions")
printMinMaxMeanMedianDiff(faithful$eruptions)
print("Waiting")
printMinMaxMeanMedianDiff(faithful$waiting)


# Assigment 3

iris <- read.csv("iris.data")

# a
print("3a")
printf("number of different species: %i\n", length(unique(iris$Species)))
print("number of data entries per species:")
table(iris$Species)

# b
print("3b")
plot(iris$Sepal.length, iris$Petal.length, 
     xlab = "Sepal length", ylab = "Petal length", 
     main = "Iris analysis")

# c
print("3c")
iris_setosa_subset <- subset(iris, Species == "Iris-setosa")
rest_subset <- subset(iris, Species != "Iris-setosa")
plot(iris_setosa_subset$Sepal.length, iris_setosa_subset$Petal.length, 
     col = "red", xlab = "Sepal length", ylab = "Petal length", 
     main = "Iris analysis (only Iris setosa)", 
     xlim = c(4,8), ylim = c(0.5,7.5))

# d
print("3d")
plot(iris_setosa_subset$Sepal.length, iris_setosa_subset$Petal.length, 
     col = "red", xlab = "Sepal length", ylab = "Petal length", 
     main = "Iris analysis", xlim = c(4,8), ylim = c(0.5,7.5))
points(rest_subset$Sepal.length, rest_subset$Petal.length, 
       col = "blue")


# Assigment 4

print("4")
mat_4 <- matrix(runif(50 * 100), ncol = 100) 
cor_4 <- cor(mat_4)
hist(cor_4, xlab = "Correlation of random values", 
     main = "Correlation histogram")

hist(cor_4, breaks = 100, xlim = c(-.6, .6), 
     col = "blue", xlab = "Correlation coefficients of random values", 
     main = "Correlation coefficients histogram")


# Assignment 5

print("5")
age <- c(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27)
height <- c(76.2, 77.1, 78.1, 78.4, 78.8, 79.7, 79.7, 81.1, 81.2, 81.4, 82.8, 83.5)

print("linear model: height ~ age")
print(lm(height ~ age))

plot(age, height, xlab = "Age", ylab = "Height", main = "Age/Height")
abline(lm(height~age))


# Bonus 1
predicted <- c(0.95, 0.93, 0.93, 0.88, 0.86, 0.85, 0.82, 0.8, 0.8, 0.79, 0.77, 0.76, 0.73, 0.65, 0.63, 0.58, 0.56, 0.49, 0.48)
actual <- c(1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1)

tpr <- mapply(function(a) { sum(actual[0:a])/13 }, c(1:19))
fpr <- mapply(function(a) { (a - sum(actual[0:a]))/13 }, c(1:19))
plot(fpr, tpr, type = "l", 
     xlab = "False positive rate", ylab = "True positive rate", 
     main = "ROC curve", xlim = c(0, 1), ylim = c(0, 1))
