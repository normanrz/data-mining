pardefault <- par()
printf <- function(...) cat(sprintf(...))


# Ass 8
ass8 <- function () {
  require(MASS)
  sigmoid <- function (x, L=1, k=1, x0=0) { L / (1 + exp(-k*(x-x0))) }
  par(mfrow = c(1,2))
  plot(menarche$Age, menarche$Menarche / menarche$Total,
       xlab="Age", ylab="Menarche / Total", main="Menarche data")
  x <- seq(-6, 6, 0.01)
  plot(x, sigmoid(x, 1), type='l', 
       xlab="x", ylab="y", main="Sigmoid function")
  par(mfrow = c(1,1))
  plot(menarche$Age, menarche$Menarche / menarche$Total,
       xlab="Age", ylab="Menarche / Total", main="Menarche data with fitted sigmoid function")
  model <- glm(cbind(menarche$Menarche, menarche$Total-menarche$Menarche) ~ menarche$Age,
               family=binomial(logit), data=menarche)
  lines(menarche$Age, model$fitted, type="l", col="red")
}
ass8()