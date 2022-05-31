case1101 <- read.csv("case1101.csv",header=TRUE)
# I will define the variables to call them by name throughout the problem, but we can also attach the data or use "$" to call specific columns
Metabol <- case1101$Metabol
Gastric <- case1101$Gastric
Sex <- case1101$Sex
Alcohol <- case1101$Alcohol
Obs <- case1101$Subject # We will use the observation number later when looking at som case influence statistics
case1101 #Print the data set
myBg <- ifelse(Sex=="Female","black","white") # This specifies the colors for the symbols
myPch <- ifelse(Alcohol=="Alcoholic",21,24) # This specifies the symbols (circle or triangle)
plot(Gastric, Metabol, pch=myPch,bg=myBg, xlab="Gastric AD Activity", ylab="First-Pass Metabolism")
legend(1,12, pch=c(24,24,21,21), pt.bg=c("white","black", "white", "black"), c("Non-alcoholic Males", "Non-alcoholic Females", "Alcoholic Males", "Alcoholic Females"))
which(Gastric > 4) # Which observations have values of Gastric greater than 4 
which(Gastric > 5) # Which observations have values of Gastric greater than 5 
myBg <- ifelse(Sex=="Female","black","white") # This specifies the colors for the symbols
myPch <- ifelse(Alcohol=="Alcoholic",21,24) # This specifies the symbols (circle or triangle)
plot(Gastric, Metabol, pch=myPch,bg=myBg, xlab="Gastric AD Activity", ylab="First-Pass Metabolism")
legend(1,12, pch=c(24,24,21,21), pt.bg=c("white","black", "white", "black"), c("Non-alcoholic Males", "Non-alcoholic Females", "Alcoholic Males", "Alcoholic Females"))
labelsubs <- subset(case1101, Gastric>4) # Identify observations with Gastric values > 4 
text(labelsubs$Gastric, labelsubs$Metabol, row.names(labelsubs), pos=1, col="red") # Add the observations nmbers to the plot
meta <- Metabol
gast <- Gastric
alc <- as.numeric(Alcohol=="Alcoholic") # Indicator variables
fem <- as.numeric(Sex=="Female")        # Indicator variables

data_orig <- data.frame(meta,gast,alc,fem) # A data frame contining the original data
mod1 <- lm(meta ~ gast*alc*fem,data=data_orig) # Note that "*" will produce the model above with all possible interactions
summary(mod1)
plot(mod1$fitted, mod1$resid, xlab="Fitted Values", ylab="Residual FPM", pch=16, ylim=c(-4,4))
abline(h=0)
which(mod1$fitted > 8) # Which observations fitted values greater than 8
which(as.vector(mod1$fitted) > 8) # Observe the difference with this statement
mod1$fitted # We can also inspect the entire list of fitted values
data_rem <- data_orig[-c(31,32),] # Remove observation s 31 and 32 from the original data
mod1_rem <- lm(meta ~ gast*alc*fem,data=data_rem) # Re-fit the model removing the observations
summary(mod1_rem) # Print summary for comparisons 
hatvalues(mod1) # Looking at the leverage for each observation in the original model
plot(Obs,hatvalues(mod1), ylab="Leverage",xlab="Observations") # Plot leverage against the observations
abline(h=0.5, lty="dashed",col="red") # We include this value for reference
which(hatvalues(mod1)>0.5) # Identify observations greater than 0.5
plot(Obs,rstandard(mod1), ylab="Standardized residuals",xlab="Observations") # Plot leverage against the observations
abline(h=0, lty="dashed") # We include line at 0 for reference
abline(h=2, lty="dashed",col="red") # We include this value for reference
abline(h=-2, lty="dashed",col="red") # We include this value for reference
which(abs((rstandard(mod1)))>2) # Identify observations that  greater than 2
cooks.distance(mod1) # Looking at the Cook distances for each observation in the original model
plot(Obs,cooks.distance(mod1), ylab="Cook distance",xlab="Observations") # Plot leverage against the observations
abline(h=1, lty="dashed",col="red") # We include this value for reference
which(cooks.distance(mod1)>1) # Identify observations greater than 1
data_rem <- data_orig[-c(31,32),]
mod1_new <- lm(meta ~ gast*alc*fem,data=data_rem)
summary(mod1_new)