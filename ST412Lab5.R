rnorm(5,0,1) # This function is generating 5 random observations from a Normal distribution with mean 0 and standard deviation 1
set.seed(512) # In the function we are using "512" as the seed, but we can pick any other integer 
rnorm(5,0,1)
mu <- 1 # Population mean (unknown)
sigma <- 1 # Population standard deviation (known)
n.sample <- 10 # Sample size in one run of the experiment (i.e. number of observations we have)
N.rep <- 1000 # Number of times we will repeat the experiment 
alpha <- 0.05
x.bar <- rnorm(N.rep, mean=mu, sd=sigma/sqrt(n.sample)) # Sample means generated from sampling distribution
head(x.bar)
z <- qnorm(alpha/2, mean=0, sd=1, lower.tail=FALSE) # z-value for confidence intervals
low.bound <- (x.bar - z*sigma/sqrt(n.sample)) # Interval lower bound
up.bound <- (x.bar + z*sigma/sqrt(n.sample)) # Interval upper bound
CI <- data.frame(low.bound,up.bound) # Data frame to see the intervals in columns
head(CI)
contains <- (mu >= low.bound & mu <= up.bound) # Check if reulsting interval contains the true parameter 
sum(contains)/N.rep # Observed proportion of intervals that cover the true value of the paramater
X1 <- runif(12,15,25) # This is the explanatory variable
Y <- 5 + 0.5*X1 + rnorm(length(X1),0,2) # This is the response variable follwoing the true model with intercept 5 and slope 0.5. The term "rnorm(length(X1),0,2)" is generating random errors
fit <- lm(Y ~ X1) # Here we are fitting the model to the generated data
summary(fit) # Are the estimated coefficients any close to the true values?
plot(X1, Y, type="p",pch=20, col="blue", xlim=c(0,25), ylim=c(0, 25), xlab="X",ylab="Y", main="")
abline(fit, col="blue", lwd=1.5)
abline(5,0.5, col="red", lwd=1.5)
newx1 <- seq(0, 25, by=0.05) # We will construct intervals at each one of these points so we can plot them later as a "confidence band" surrounding the fitted line
conf_interval <- predict(fit, newdata=data.frame(X1=newx1), interval="confidence",
                           +                          level = 0.95)
plot(X1, Y, type="p",pch=20, col="blue", xlim=c(0,25), ylim=c(0, 25), xlab="X",ylab="Y", main="")
abline(fit, col="blue", lwd=1.5)
abline(5,0.5, col="red", lwd=1.5)
lines(newx1, conf_interval[,2], col="blue", lty=2) # This will plot the lower band
lines(newx1, conf_interval[,3], col="blue", lty=2) # This will plot the upper band
pred_interval <- predict(fit, newdata=data.frame(X1=newx1), interval="prediction",
                           +                          level = 0.95)
plot(X1, Y, type="p",pch=20, col="blue", xlim=c(0,25), ylim=c(0, 25), xlab="X",ylab="Y", main="")
abline(fit, col="blue", lwd=1.5)
abline(5,0.5, col="red", lwd=1.5)
lines(newx1, conf_interval[,2], col="blue", lty=2)
lines(newx1, conf_interval[,3], col="blue", lty=2)
lines(newx1, pred_interval[,2], col="darkgreen", lty=2)
lines(newx1, pred_interval[,3], col="darkgreen", lty=2)
# Set up the simluation paeramters
M <- 1000
n.sample <- 12
sigma <-  2
pvalue <- vector("numeric", M)
beta1 <- vector ("numeric",M)
coverage <- vector("logical",M)
alpha <- 0.05
for(i in 1:M){
X <- runif(n.sample,15,25)
Y <- 5+0.5*X+rnorm(length(X),0,sigma)
t <- qt(alpha/2,n.sample-1,lower.tail=FALSE)

model <- lm(Y ~ X)
beta1[i] <- summary(model)$coeff[2,1] # Extract estimated coefficient
pvalue[i] <- summary(model)$coeff[2,4] # Extract corresponding p-value
coverage[i] <- (0.5 >= summary(model)$coeff[2,1] - t*summary(model)$coeff[2,2] & 0.5 <= summary(model)$coeff[2,1] + t*summary(model)$coeff[2,2]) # Check if reulting interval contains the true parameter 
}
hist(beta1) # Construct histogram
sum(coverage)/M # Observed proportion of intervals that cover the true value of the paramater
fail.reject <- (pvalue > alpha) # Check if P-value is not "significant" 
sum(fail.reject)/M # Observed proportion of rejections