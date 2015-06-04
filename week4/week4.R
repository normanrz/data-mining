pardefault <- par()
printf <- function(...) cat(sprintf(...))
require(parallel)
require(NbClust)
require(flexclust)


# Assignment 3
ass3 <- function () {
  attach(mtcars)
  pairs( ~ disp + hp + wt + drat)
  par(mfrow = c(2,3))
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
  print(summary(model))
  print(predict(model, data.frame(disp=230, hp=146, wt=3.2, drat=3.6)))
}
ass3()

euclideanDist <- function (a, b) {
  dist(rbind(a, b), method = "euclidean")
}
manhattanDist <- function (a, b) {
  dist(rbind(a, b), method = "manhattan")
}

# Ass 4
ass4 <- function () {
  
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
  str(kMeans(3, wines, euclideanDist))
  str(kMeans(7, wines, manhattanDist))
  
}
# ass4()


ass5 <- function () {
  wines <- read.csv('winequality-white.csv')
  wines <- wines[1:11]
  
  ass5kMeans <- function () {
    res <- do.call(rbind, lapply(2:10, function (k) {
      res <- kcca(wines, k, family=kccaFamily("kmeans"))
      sqSum <- sum(do.call(rbind, lapply(1:k, function (j) { 
        clusterData <- wines[res@cluster == j,]
        mean(do.call(rbind, lapply(1:nrow(clusterData), function (i) {
          euclideanDist(clusterData[i,], res@centers[j,])
        }))) ** 2
      })))
      printf("kMeans: k = %f sqSum = %f\n", k, sqSum)
      # kcca(wines, k, family=kccaFamily("kmedians"))
      c(k, sqSum)
    }))
    
    plot(res, type="l", 
         xlab="k", ylab="Sum of squared distances", 
         main="kMeans: Sum of squared distances with clusters")
  }
  #ass5kMeans()
  
  ass5kMedians <- function () {
    
    res <- do.call(rbind, lapply(2:10, function (k) {
      res <- kcca(wines, k, family=kccaFamily("kmedians"))
      sqSum <- sum(do.call(rbind, lapply(1:k, function (j) { 
        clusterData <- wines[res@cluster == j,]
        mean(do.call(rbind, lapply(1:nrow(clusterData), function (i) {
          euclideanDist(clusterData[i,], res@centers[j,])
        })))
      })))
      printf("kMedians: k = %f sum = %f\n", k, sqSum)
      c(k, sqSum)
    }))
    
    plot(res, type="l", 
         xlab="k", ylab="Sum of distances", 
         main="kMedians: Sum of distances with clusters")
    
  }
  #ass5kMedians()
  
  ass5NbClust <- function () {
    res <- do.call(rbind, lapply(
      list(
           "kl", "ch", "hartigan", "ccc", "scott", "marriot", 
           "trcovw", "tracew", "friedman", "rubin", "cindex", 
           "db", "silhouette", "duda", "pseudot2", "beale", 
           "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", 
           "dunn", "hubert", "sdindex", "dindex", "sdbw"
      ), function (index) {
        print(index)
        a<-NbClust(wines, min.nc=2, max.nc=10, method="kmeans", index=index)
        print(a$Best.nc[["Number_clusters"]])
      }))
    par(mfrow=c(1,1))
    hist(res, xlim=c(2,10), breaks=8, main="Histogram of optimal k values", xlab="k")
  }
  #ass5NbClust()

  ass5NbClust01 <- function () {
    
    wines2 <- do.call(cbind, lapply(1:ncol(wines), function (i) { 
      a <- wines[,i]
      a <- a - min(a)
      a <- a / max(a)
      a
    }))
    
    res <- do.call(rbind, lapply(
      list(
        "kl", "ch", "hartigan", "ccc", "scott", "marriot", 
        "trcovw", "tracew", "friedman", "rubin", "cindex", 
        "db", "silhouette", "duda", "pseudot2", "beale", 
        "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", 
        "dunn", "hubert", "sdindex", "dindex", "sdbw"
      ), function (index) {
        print(index)
        a<-NbClust(wines, min.nc=2, max.nc=10, method="kmeans", index=index)
        print(a$Best.nc[["Number_clusters"]])
      }))
    par(mfrow=c(1,1))
    hist(res, xlim=c(2,10), breaks=8, main="Histogram of optimal k values", xlab="k")
  }
  ass5NbClust01()
  
  
}
ass5()


ass6 <- function () {
  # split dataset by modulo, not shuffling
}
