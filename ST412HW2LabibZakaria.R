plot(Intensity[1:12],Flowers[1:12],type="p",pch=paste(1), col="blue", main="# Flowers vs Light Intensity ((mcmol/m^2)/s), Equal Lines", xlab="Light Intensity (mcmol/(m^2/s))",ylab="# Flowers")
abline(lm(Flowers~Intensity), col="green")
points(Intensity[13:24],Flowers[13:24],type="p",pch=paste(2), col="red")
legend(locator(1),c("At PFI","Before PFI"),col=c("blue","red"),pch=c(paste(1),paste(2)),cex=0.8)
plot(Intensity[1:12],Flowers[1:12],type="p",pch=paste(1), col="blue", main="# Flowers vs Light Intensity ((mcmol/m^2)/s),Separate Lines", xlab="Light Intensity (mcmol/(m^2/s))",ylab="# Flowers")
abline(lm(Flowers[1:12]~Intensity[1:12]), col="green")
points(Intensity[13:24],Flowers[13:24],type="p",pch=paste(2), col="red")
abline(lm(Flowers[13:24]~Intensity[13:24]),col="black")
legend(locator(1),c("At PFI","Before PFI"),col=c("blue","red"),pch=c(paste(1),paste(2)),cex=0.8)
legend(locator(1),c("At PFI","Before PFI"),col=c("green","black"),lty=1,cex=0.8)
levels(factor(case0901$Time))
case0901$early <- as.numeric(case0901$Time == "2")
summary(lm(Flowers ~ Intensity + early, data = case0901))
pred2 <- predict(lm(Flowers ~ Intensity + early, data = case0901), data.frame(Intensity=600,early=0))
pred1 <- predict(lm(Flowers ~ Intensity + early, data = case0901), data.frame(Intensity=300,early=0))
pred1-pred2
pred4 <- predict(lm(Flowers ~ Intensity + early, data = case0901), data.frame(Intensity=600,early=1))
pred3 <- predict(lm(Flowers ~ Intensity + early, data = case0901), data.frame(Intensity=300,early=1))
pred3-pred4