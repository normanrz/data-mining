pardefault <- par()
printf <- function(...) cat(sprintf(...))

require(mixtools)

ass6 <- function () {
  data <- scan("gaussians.txt")
  par(mfrow = c(3,1))
  
  data <- sample(data)
  
  model <- normalmixEM(data, k=2)
  summary.mixEM(model)
  plot.mixEM(model, whichplots=2, main2=paste("GMM: k =", 2))
  
  hist(data, breaks=100)

  model <- normalmixEM(data, k=3)
  summary.mixEM(model)
  plot.mixEM(model, whichplots=2, main2=paste("GMM: k =", 3))
  
  par(mfrow = c(1,1))
}
#ass6()

ass8 <- function () {
  
  data1 <- rnorm(10000, -1, 1)
  data2 <- rnorm(10000, 1, .5)
  
  plot(density(data1), 
       xlim=c(-5,4), ylim=c(0,1), 
       main="Density of gaussians",
       xlab="",
       col="red", lwd=3)
  lines(density(data2),
        col="blue", lwd=3)
  lines(density(c(data1, data2)),
        col="green", lwd=3)
  
  model <- normalmixEM(c(data1,data2), k=2)
  summary.mixEM(model)
  
  em <- function (data, k=2, epsilon=1e-4) {
    # http://www.ics.uci.edu/~smyth/courses/cs274/notes/EMnotes.pdf    
    
    # Initialize
    n <- length(data)
    mu <- sample(data, k)
    sigma <- rep(1, k)
    lambda <- rep(1/k, k)
    
    logLikelihood <- 0
    iterations <- 0
    
    while (TRUE) {
      # E-step
      w <- matrix(0, n, k)
      
      for (i in 1:n) {
        x <- data[i]
        margin <- sum(sapply(1:k, function (m) { 
          dnorm(x, mu[m], sigma[m]) * lambda[m] 
        }))
        for (j in 1:k) {
          w[i, j] <- (dnorm(x, mu[j], sigma[j]) * lambda[j]) / margin
        }
      }
      
      # M-step
      for (j in 1:k) {
        Nk <- sum(w[,j])
        lambda[j] <- Nk / n
        mu[j] <- sum(w[,j] * data) / Nk
        sigma[j] <- sum(w[,j] * ((data - mu[j]) ^ 2)) / Nk
      }
      
      iterations <- iterations + 1
      
      newLogLikelihood <- sum(sapply(1:n, function (i) {
        x <- data[i]
        log(sum(sapply(1:k, function (j) {
          lambda[j] * dnorm(x, mu[j], sigma[j])
        })))
      }))
      
      printf("%i %f\n", iterations, newLogLikelihood)
      if (abs(newLogLikelihood - logLikelihood) < epsilon) {
        break
      }
      logLikelihood <- newLogLikelihood
    }
    
    list(mu=mu, sigma=sigma, lambda=lambda, iterations=iterations)
  }
  
  str(em(c(data1, data2), k=2))
  
}
ass8()