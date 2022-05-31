HW4Dat <- read.csv("ex0915.csv",stringsAsFactors=TRUE)
attach(HW4Dat)
plot(Rainfall,Yield, pch=20, col="blue")
cor(Rainfall,Yield)
fit1.poly <- lm(Yield ~ poly(Rainfall,1,raw=TRUE))
fit2.poly <- lm(Yield ~ poly(Rainfall,2,raw=TRUE))
fit3.poly <- lm(Yield ~ poly(Rainfall,3,raw=TRUE))
summary(fit1.poly)$coef
summary(fit1.poly)$r.squared
summary(fit1.poly)$adj.r.squared
summary(fit2.poly)$coef
summary(fit2.poly)$r.squared
summary(fit2.poly)$adj.r.squared
summary(fit3.poly)$coef
summary(fit3.poly)$r.squared
summary(fit3.poly)$adj.r.squared
anova(fit1.poly,fit2.poly)
anova(fit2.poly)
anova(fit3.poly)
anova(fit2.poly,fit3.poly)
sigma(fit3.poly)^2
34*sigma(fit3.poly)^2
sigma(fit2.poly)^2
35*sigma(fit2.poly)^2
35*sigma(fit2.poly)^2-34*sigma(fit3.poly)^2
(35*sigma(fit2.poly)^2-34*sigma(fit3.poly)^2/1)/sigma(fit3.poly)^2
pf((35*sigma(fit2.poly)^2-34*sigma(fit3.poly)^2/1)/sigma(fit3.poly)^2,1,34,lower.tail=FALSE)
plot(Year,fit1.poly$res, pch=20, col="blue")
abline(h=0) # Add the horizontal line at 0
pf(0.427977589,2,16,lower.tail=FALSE)
