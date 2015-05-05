# Utility functions
pardefault <- par()
printf <- function(...) cat(sprintf(...))

# Assignment 1: Variance

data <- c(58, 74, 69, 81, 64, 120, 55, 71, 77, 65, 23000)

empVar <- function (X) {
  sum((X - mean(X)) ** 2)/length(X)
}

sampVar <- function (X) {
  sum((X - mean(X)) ** 2)/(length(X) - 1)
}

printf("var: %f", var(data))
printf("sampVar: %f", sampVar(data))
printf("empVar: %f", empVar(data))

# Assignment 2

runCoinFlipSimulation <- function (trialCount) {
  flipCount <- 100
  experiment <- mapply(function (a) { 
    sum(sample(c(0, 1), flipCount, replace=TRUE)) 
  }, seq(1, trialCount))
  hist(experiment, 
       xlab = "Number of heads",
       main = paste("Histogram with", trialCount, "trials"))
  printf("trials: %i, mean: %f, median: %f, empVar: %f, sampVar: %f",
         trialCount, mean(experiment), median(experiment), 
         empVar(experiment), sampVar(experiment))
}
runCoinFlipSimulation(100)
runCoinFlipSimulation(1000)
runCoinFlipSimulation(10000)

runPlotProbabilityMassFunctions <- function (flipCount) {
  x <- seq(0, flipCount)
  plot(x, dbinom(x, size=flipCount, prob=0.5), type='p',
       xlab = "Number of heads", ylab = "Probability mass",
       main = paste("Probability mass plot for", flipCount, "coin flips"))
}
runPlotProbabilityMassFunctions(100)
runPlotProbabilityMassFunctions(1000)
runPlotProbabilityMassFunctions(10000)

# Assignment 3

par(mfrow = c(2,4))
data <- runif(100000)
hist(data, main = "Histogram of original data (unif)", xlab = "Random values")
plot(density(data), main = "Density of original data (unif)", xlab = "Random values")
sampleMeans <- mapply(function (a) { mean(sample(data, 100, replace=TRUE)) }, seq(0, 5000))
hist(sampleMeans, main = "Histogram of sample means (unif)", xlab = "Sample means")
plot(density(sampleMeans), main = "Density of sample means (unif)", xlab = "Sample means")

data <- rnorm(100000)
hist(data, main = "Histogram of original data (norm)", xlab = "Random values")
plot(density(data), main = "Density of original data (norm)", xlab = "Random values")
sampleMeans <- mapply(function (a) { mean(sample(data, 100, replace=TRUE)) }, seq(0, 5000))
hist(sampleMeans, main = "Histogram of sample means (norm)", xlab = "Sample means")
plot(density(sampleMeans), main = "Density of sample means (norm)", xlab = "Sample means")

par(pardefault)

# Assignment 4
algoA <- c(0.90, 0.87, 0.92, 0.88, 0.90, 0.85, 0.91, 0.90, 0.87, 0.95, 0.90)
algoB <- c(0.89, 0.95, 0.87, 0.94, 0.92, 0.86, 0.84, 0.92, 0.88, 0.83, 0.90)

meanA <- mean(algoA)
meanB <- mean(algoB)
df <- length(algoA) - 1

meanD <- meanA - meanB
sD <- sqrt(sd(algoA)**2/length(algoA) - sd(algoB)**2/length(algoB))
Y <- (meanD*sqrt(length(algoA)))/sD


# Assignment 7

entropyRegularDice <- - 6 * 1/6 * log2(1/6)
printf("Entropy of regular dice: %f", entropyRegularDice)

manipulatedDiceProbs <- c(0.5, 0.15, 0.15, 0.08, 0.08, 0.04)
entroyManipulatedDice <- - sum(manipulatedDiceProbs * log2(manipulatedDiceProbs))
printf("Entropy of manipulated dice: %f", entroyManipulatedDice)
