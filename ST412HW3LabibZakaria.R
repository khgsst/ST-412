HW3Dat <- read.csv("case1002.csv",stringsAsFactors=TRUE)
HW3Dat
attach(HW3Dat)
logEnergy <- log(Energy)
logMass <- log(Mass)
bird <- as.numeric(Type=="non-echolocating birds")
ebat <- as.numeric(Type=="echolocating bats")
fit.par<-lm(logEnergy ~ logMass + bird + ebat)
summary(fit.par)
b2=fit.par$coef[3]
b3=fit.par$coef[4]
b3-b2
X <-model.matrix(fit.par)
X
solve(t(X)%*%X)
summary(fit.par)$sigma
(summary(fit.par)$sigma)^2*solve(t(X)%*%X)
VarCov<-(summary(fit.par)$sigma)^2*solve(t(X)%*%X)
(varb3hmb2h<-VarCov[4,4]+VarCov[3,3]-2*VarCov[3,4])
(b3mb295ci<-c(b3-b2-qt(1-.05/2,16)*sqrt(varb3hmb2h),b3-b2+qt(1-.05/2,16)*sqrt(varb3hmb2h)))
lelm <- log(Energy)*log(Mass)^(-1)
lelm
HW3AOV<-aov(logEnergy ~ logMass + bird + ebat)
anova(HW3AOV)
summary(HW3AOV)
HW3AOV2<-aov(logEnergy ~ logMass)
summary(HW3AOV2)
anova(HW3AOV2)
anova(HW3AOV,HW3AOV2)
anova(HW3AOV2,HW3AOV)
nonebat <- as.numeric(Type=="non-echolocating bats")
fit.par2<-lm(logEnergy ~ logMass + bird + nonebat)
summary(fit.par2)
c(fit.par2$coef[3]-qt(1-.05/2,16)*summary(fit.par2)$coef[3,2],fit.par2$coef[3]+qt(1-.05/2,16)*summary(fit.par2)$coef[3,2])