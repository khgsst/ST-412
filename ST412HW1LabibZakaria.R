HW1Dat <- read.csv("cholesterol.csv",stringsAsFactors=TRUE)
HW1Dat
attach(HW1Dat)
cor(weight,cholesterol)
cor(HW1Dat["weight"],HW1Dat["cholesterol"])
plot(weight[group==1],cholesterol[group==1],type="p",pch=20, col="blue", main="Cholesterol vs Weight group 1", xlab="Weight",ylab="Cholesterol")
abline(lm(cholesterol[group==1]~weight[group==1]), col="red") 
plot(weight[group==2],cholesterol[group==2],type="p",pch=20, col="blue", main="Cholesterol vs Weight group 2", xlab="Weight",ylab="Cholesterol")
abline(lm(cholesterol[group==2]~weight[group==2]), col="red")
plot(weight[group==3],cholesterol[group==3],type="p",pch=20, col="blue", main="Cholesterol vs Weight group 3", xlab="Weight",ylab="Cholesterol")
abline(lm(cholesterol[group==3]~weight[group==3]), col="red")
mod1 <- lm(cholesterol[group==1]~weight[group==1]) 
summary(mod1)
mod2 <- lm(cholesterol[group==2]~weight[group==2]) 
summary(mod2)
mod3 <- lm(cholesterol[group==3]~weight[group==3]) 
summary(mod3)
c(76.88002-qt(1-.05/2,8)*16.95857,76.88002+qt(1-.05/2,8)*16.95857)
c(-0.08213-qt(1-.05/2,8)*0.10514,-0.08213+qt(1-.05/2,8)*0.10514)
c(14.2550-qt(1-.05/2,6)*17.4860,14.2550+qt(1-.05/2,6)*17.4860)
c(0.2509-qt(1-.05/2,6)*0.1180,0.2509+qt(1-.05/2,6)*0.1180)
ggplot(HW1Dat[1:8,], aes(x=weight, y=cholesterol)) +
  geom_point() +
  geom_smooth(method=lm)+
  xlab("Weight")+
  ylab("HDL Cholesterol")
ggplot(HW1Dat[9:16,], aes(x=weight, y=cholesterol)) +
  geom_point() +
  geom_smooth(method=lm)+
  xlab("Weight")+
  ylab("HDL Cholesterol")
ggplot(HW1Dat[17:26,], aes(x=weight, y=cholesterol)) +
  geom_point() +
  geom_smooth(method=lm)+
  xlab("Weight")+
  ylab("HDL Cholesterol")
c(23.0543-qt(1-.05/2,6)*25.3122,23.0543+qt(1-.05/2,6)*25.3122)
c(0.2496-qt(1-.05/2,6)*0.1573 ,0.2496+qt(1-.05/2,6)*0.1573)