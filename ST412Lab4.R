case1001 <- read.csv("case1001.csv",header=TRUE)
case1001
str(case1001) # Check the structure of the data
Distance <- case1001$Distance
Height <- case1001$Height
plot(Height,Distance, pch=20, col="blue")
abline(lm(Distance~Height), col="blue")
cor(Height,Distance)
fit1 <- lm(Distance ~ Height)
summary(fit1)
plot(Height,fit1$res, pch=20, col="blue")
abline(h=0) # Add the horizontal line at 0
summary(fit1)$r.squared
summary(fit1)$adj.r.squared
Height2 <- Height^2 #Need to define the variable to include in the model
fit2 <- lm(Distance ~ Height + Height2)
summary(fit2)
plot(Height,fit2$res, pch=20, col="blue")
abline(h=0) # Add the horizontal line at 0
fit2.I <- lm(Distance ~ Height + I(Height^2))
fit2.poly <- lm(Distance ~ poly(Height,2,raw=TRUE))
summary(fit2)$coef
summary(fit2.I)$coef
summary(fit2.poly)$coef
fit3 <- lm(Distance ~ poly(Height,3,raw=TRUE))
summary(fit3)
plot(Height,fit3$res, pch=20, col="blue")
abline(h=0) # Add the horizontal line at 0
fit4 <- lm(Distance ~ poly(Height,4,raw=TRUE))
summary(fit4)
plot(Height,fit4$res, pch=20, col="blue")
abline(h=0) # Add the horizontal line at 0
fit5 <- lm(Distance ~ poly(Height,5,raw=TRUE))
summary(fit5)
fit6 <- lm(Distance ~ poly(Height,6,raw=TRUE))
summary(fit6)
plot(Height,fit6$res, pch=20, col="blue")
abline(h=0) # Add the horizontal line at 0
anova(fit1,fit2)
anova(fit2,fit1)
anova(fit2,fit3)
anova(fit3,fit4)
anova(fit4,fit5)
anova(fit5,fit6)
grid <- data.frame(Height=seq(from=0, to=1100, by=1)) # Range based on observed data
grid$pred1 <- predict(fit1, grid) # Evaluate the grid using fitted model
plot(grid$Height, grid$pred1, col = "red") # Plotting the points we just created
grid$pred2 <- predict(fit2.poly, grid) # Using the version where we used the function poly()
grid$pred3 <- predict(fit3, grid)
grid$pred4 <- predict(fit4, grid)
grid$pred5 <- predict(fit5, grid)
grid$pred6 <- predict(fit6, grid)
par(mfrow=c(2,3))
# Linear
plot(Height,Distance, pch=20, col="blue", main="linear")
lines(grid$Height, grid$pred1, col = "red")

# Quadratic
plot(Height,Distance, pch=20, col="blue", main="quadratic")
lines(grid$Height, grid$pred2, col = "red")

# Cubic
plot(Height,Distance, pch=20, col="blue", main="cubic")
lines(grid$Height, grid$pred3, col = "red")

# Quartic
plot(Height,Distance, pch=20, col="blue", main="quartic")
lines(grid$Height, grid$pred4, col = "red")

# Quintic
plot(Height,Distance, pch=20, col="blue", main="quintic")
lines(grid$Height, grid$pred5, col = "red")

# Sextic
plot(Height,Distance, pch=20, col="blue", main="sextic")
lines(grid$Height, grid$pred6, col = "red")