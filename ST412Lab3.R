case1002 <- read.csv("case1002.csv",header=TRUE)
case1002
str(case1002)
Mass <- case1002$Mass
Type <- case1002$Type
Energy <- case1002$Energy
plot(Mass,Energy,pch = ifelse(Type=="echolocating bats", 1,
                              ifelse(Type=="non-echolocating birds", 0, 2)), col = ifelse(Type=="echolocating bats", "blue",
                                                                                          ifelse(Type=="non-echolocating birds", "red", "darkgreen")))
legend("bottomright", pch=c(1, 0, 2), col=c("blue","red","darkgreen"),
       c("Echolocating bats", "Non-echolocating birds","Non-echolocating bats"))
abline(lm(Energy~Mass)) # add a regression line
logEnergy <- log(Energy)
logMass <- log(Mass)
plot(logMass,logEnergy, xlab = "Body Mass (log-scale)",
     ylab = "Energy Expenditure (log-scale)",
     pch = ifelse(Type=="echolocating bats", 17,
                  ifelse(Type=="non-echolocating birds", 21, 24)))

legend("topleft", pch=c(24, 21, 17),
       c("Non-echolocating bats", "Non-echolocating birds","Echolocating bats"))
abline(lm(logEnergy~logMass))
par(mfrow=c(1,2))
plot(lm(Energy~Mass), which=1, main="Original scale") 
plot(lm(logEnergy~logMass), which=1, main="Log-scale")
bird <- as.numeric(Type=="non-echolocating birds")
ebat <- as.numeric(Type=="echolocating bats")
fit.sep <- lm(logEnergy ~ logMass + bird + ebat + logMass:bird + logMass:ebat) # Observe what happens if you write lm(logEnergy ~logMass*bird*ebat) instead
summary(fit.sep)
summary(lm(logEnergy ~ logMass*bird*ebat))
fit.par <- lm(logEnergy ~ logMass + bird + ebat)
summary(fit.par)
plot(logMass,logEnergy,pch = ifelse(Type=="echolocating bats", 1,
                                    ifelse(Type=="non-echolocating birds", 0, 2)), col = ifelse(Type=="echolocating bats", "blue",
                                                                                                ifelse(Type=="non-echolocating birds", "red", "darkgreen")))
legend("bottomright", pch=c(1, 0, 2), col=c("blue","red","darkgreen"),
       c("Echolocating bats", "Non-echolocating birds","Non-echolocating bats"))
abline(a=fit.par$coef[1],b=fit.par$coef[2], col="blue")
abline(a=fit.par$coef[1]+fit.par$coef[3],b=fit.par$coef[2], col="red")
abline(a=fit.par$coef[1]+fit.par$coef[4],b=fit.par$coef[2], col="darkgreen")
fit.eq <- lm(logEnergy ~ logMass)
summary(fit.eq)
t_ebat <- summary(fit.par)$coef[4,3]
t_ebat
summary(fit.par)$coef[4,1]/summary(fit.par)$coef[4,2] # You get the same computing this ratio
summary(fit.par)$coef[4,4]
qt(0.025,16,lower.tail=FALSE) # Multiplicative-constant for a 95% CI based on the t-distribution
coef(summary(fit.par))["ebat","Estimate"]  # Coefficient estimated value
coef(summary(fit.par))["ebat","Std. Error"]  # Coeficient standard error
low_lim <- coef(summary(fit.par))["ebat","Estimate"] - qt(0.025,16,lower.tail=FALSE)*coef(summary(fit.par))["ebat","Std. Error"]

upp_lim <- coef(summary(fit.par))["ebat","Estimate"] + qt(0.025,16,lower.tail=FALSE)*coef(summary(fit.par))["ebat","Std. Error"]
c(low_lim,upp_lim) # final confidence interval
confint(fit.par, level=0.95) # Compare with the two ends of the confidence interval next to the corresponding variable
anova(fit.par)
anova(fit.eq)
ESS <- anova(fit.eq)["Residuals","Sum Sq"] - anova(fit.par)["Residuals","Sum Sq"] # Extra sum of squares
ESS
MSE.Full <- anova(fit.par)["Residuals","Mean Sq"] # Mean square error from the full model
MSE.Full
F_stat <- (ESS/2)/MSE.Full  # F-statistic to compare full vs reduced model
F_stat
pf(F_stat,2,16,lower.tail=FALSE) # Obtain P-value (DON'T MULTIPLY THIS BY 2!!)
anova(fit.eq,fit.par)