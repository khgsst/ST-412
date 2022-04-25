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
(b3mb295ci<-c(varb3hmb2h-qt(1-.05/2,16)*sqrt(varb3hmb2h),varb3hmb2h+qt(1-.05/2,16)*sqrt(varb3hmb2h)))