pardefault <- par()
printf <- function(...) cat(sprintf(...))


# Ass 7
ass7 <- function () {
  test <- data.frame(Sepal=c(4.5, 5, 5.5, 6, 7), Petal=c(2, 1.5, 3, 4.3, 7))
  
  # Implementation 1
  model <- lda(data.frame(Sepal=iris$Sepal.Length, Petal=iris$Petal.Length), iris$Species)
  print(predict(model, test)$class)
  
  # Implementation 2
  require(mvtnorm)
  for (i in 1:nrow(test)) {
    test_row <- test[i,]
    printf("%f %f %s\n", test_row[,1], test_row[,2], levels(iris$Species)[which.max(lapply(levels(iris$Species), function (k) {
      rows <- iris[iris$Species == k,]
      mu <- colMeans(cbind(rows$Sepal.Length, rows$Petal.Length))
      sig <- cov(cbind(rows$Sepal.Length, rows$Petal.Length))
      dmvnorm(test_row, mu, sig)[1]
    }))])
  }
  
  
}
ass7()

# Ass 4
ass4 <- function () {
    is_null = FALSE
    is_red = FALSE
    is_black = FALSE
    win_sum = 0
    bet = 1
    n = 0
    winning_bets <- vector()
    
    while(win_sum < 3000){
        rand_nr <- sample(0:36,1)
        if(rand_nr == 0){
            is_null = TRUE
            bet = bet*2
        }
        else if(rand_nr <= 18 ){
            is_red = TRUE
            win_sum = win_sum+1
            winning_bets <- c(winning_bets,bet)
            bet=1
        }
        else if(rand_nr > 18){
            is_black = TRUE
            bet = bet*2
        }
        n = n+1
        #temp <- paste("round:", n,"- rand_nr:",rand_nr,"- bet:",bet,"- win_sum:",win_sum)
        #print(temp)
        is_null = FALSE
        is_red = FALSE
        is_black = FALSE
    }
    
    hist(winning_bets,main="Histogram of winning bets", xlab="bet amount in EURO")
    printf("Highest bet: %s \n",max(winning_bets))
    printf("Total number of bets: %s \n",n)
}
ass4()

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
# ass8()
