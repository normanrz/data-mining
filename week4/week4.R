pardefault <- par()
printf <- function(...) cat(sprintf(...))
require(parallel)

# Assignment 3
# pairs( ~ mtcars$disp + mtcars$hp + mtcars$wt + mtcars$drat)
ass3 <- function () {
  par(mfrow = c(2,3))
  attach(mtcars)
  plot(disp, hp, 
       main = "Displacement (cu.in.) / Gross horsepower", xlab = "Displacement (cu.in.)", 
       ylab = "Gross horsepower")
  plot(disp, wt, 
       main = "Displacement (cu.in.) / Weight (lb/1000)", xlab = "Displacement (cu.in.)", 
       ylab = "Weight (lb/1000)")
  plot(disp, drat, 
       main = "Displacement (cu.in.) / Rear axle ratio", xlab = "Displacement (cu.in.)", 
       ylab = "Rear axle ratio")
  plot(hp, wt, 
       main = "Gross horsepower / Weight (lb/1000)", xlab = "Gross horsepower", 
       ylab = "Weight (lb/1000)")
  plot(hp, drat, 
       main = "Gross horsepower / Rear axle ratio", xlab = "Gross horsepower", 
       ylab = "Rear axle ratio")
  plot(wt, drat, 
       main = "Weight (lb/1000) / Rear axle ratio", xlab = "Weight (lb/1000)", 
       ylab = "Rear axle ratio")
  par(pardefault)
  
  model <- lm(mpg ~ disp + hp + wt + drat)
  summary(model)
  predict(model, data.frame(disp=230, hp=146, wt=3.2, drat=3.6))
}
ass3()

# Ass 4
ass4 <- function () {
  euclideanDist <- function (a, b) {
    dist(rbind(a, b), method = "euclidean")
  }
  manhattanDist <- function (a, b) {
    dist(rbind(a, b), method = "manhattan")
  }
  
  kMeans <- function (k, data, dist) {
    stopifnot(k > 0)
    
    # Init
    itercount <- 0
    n <- nrow(data)
    centers <- data[sample(n, k), ]
    cluster <- sapply(data[,1], function (a) 1) 
    
    while(TRUE) {
      itercount <- itercount + 1
      hasChanged <- FALSE
      
      # Assign step
      newCluster <- mcmapply(function (i) {
        which.min(sapply(1:k, function (j) { dist(data[i,], centers[j,]) }))
      }, 1:n, mc.cores=4)
      printf('%i samples newly assigned\n', sum(newCluster != cluster))
      hist(newCluster)
      if (sum(newCluster != cluster) > 0) {
        hasChanged <- TRUE
        cluster <- newCluster
      }
      
      # Update step
      centers <- do.call(rbind, lapply(1:k, function (j) {
        colMeans(data[cluster == j,])
      }))
      
      printf('%i iterations\n', itercount)
      if (hasChanged == FALSE) {
        break;
      }
    }
    list(cluster=cluster, centers=centers, iter=itercount)
  }
  
  wines <- read.csv('winequality-white.csv')
  wines <- wines[1:11]
  
  str(kmeans(wines, 7))
  str(kMeans(7, wines, euclideanDist))
}
ass4()