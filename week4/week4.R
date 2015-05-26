pardefault <- par()

# Assignment 3
# pairs( ~ mtcars$disp + mtcars$hp + mtcars$wt + mtcars$drat)
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
