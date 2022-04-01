data <- read.csv("polymers.csv",header=TRUE) # Define the .csv file as "data" 
data # Print the data frame
data[,1]
data$ph
attach(data)
ph
subset(data, polymer==1)
ph[1:6]
ph[polymer==1]
plot(ph,solids,type="p",pch=20, col="blue", main="Solids vs pH ignoring polymers", xlab="pH",ylab="Amount of Solids") # Produce the scaterplot
abline(lm(solids~ph), col="red") # adds the regression line in the scatterplot
cor(ph,solids)
par(mfrow=c(1,3)) # Print three plots in a horizontal array in the same window 

# Scaterplot, regression line and correlation for observation in polymer 1 
plot(ph[polymer==1],solids[polymer==1],type="p",pch=20, col="blue", main="Solids vs pH polymer 1", xlab="pH",ylab="Amount of Solids")
abline(lm(solids[polymer==1]~ph[polymer==1]), col="red") 
cor(solids[polymer==1],ph[polymer==1])
# Scaterplot, regression line and correlation for observation in polymer 2
plot(ph[polymer==2],solids[polymer==2],type="p",pch=20, col="blue", main="Solids vs pH polymer 2", xlab="pH",ylab="Amount of Solids")
abline(lm(solids[polymer==2]~ph[polymer==2]), col="red")
cor(solids[polymer==2],ph[polymer==2])
# Scaterplot, regression line and correlation for observation in polymer 3
plot(ph[polymer==3],solids[polymer==3],type="p",pch=20, col="blue", main="Solids vs pH polymer 3", xlab="pH",ylab="Amount of Solids")
abline(lm(solids[polymer==3]~ph[polymer==3]), col="red")
cor(solids[polymer==3],ph[polymer==3])
plot(ph[1:6],solids[1:6],type="p",pch=paste(1), col="blue", xlim=c(min(ph),max(ph)), ylim=c(min(solids),max(solids)), main="Solids vs pH by polymers", xlab="pH",ylab="Amount of Solids") #  Scatterplot of observations for polymer 1. Note xlim and ylim is fixin the scale of the plot
abline(lm(solids[1:6]~ph[1:6]),col="blue") # add regression line for polymer 1

points(ph[7:12],solids[7:12],type="p",pch=paste(2), col="red") # add points corresponding to polymer 2
abline(lm(solids[7:12]~ph[7:12]),col="red") # add regression line for polymer 2

points(ph[13:18],solids[13:18],type="p",pch=paste(3), col="darkgreen") # add points corresponding to polymer 3
abline(lm(solids[13:18]~ph[13:18]),col="darkgreen") # add regression line for polymer 3
mod0 <- lm(solids~ph) # Name model "mod0"
summary(mod0) # Print the summary of the model
# Model and oputput for polymer 1
mod1 <- lm(solids[polymer==1]~ph[polymer==1]) 
summary(mod1)
# Model and oputput for polymer 2
mod2 <- lm(solids[polymer==2]~ph[polymer==2]) 
summary(mod2)
# Model and oputput for polymer 3
mod3 <- lm(solids[polymer==3]~ph[polymer==3]) 
summary(mod3)
poly1 <- as.numeric(polymer==1) # This variable has 1's for the observations that correspond to polymer 1 and 0's everywhere else
poly1
mod4 <- lm(solids[1:12] ~ ph[1:12] + poly1[1:12] + ph[1:12]*poly1[1:12]) # Polymers 1 and 2 correspond to the firts 12 observations in the data 
summary(mod4)