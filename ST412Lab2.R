case0901 <- read.csv("case0901.csv",header=TRUE)
attach(case0901)
str(case0901) # Check the structure of the data
case0901$Time
levels(factor(case0901$Time))
case0901$Early <- as.numeric(case0901$Time == "2")
case0901$Early
summary(lm(Flowers ~ Intensity + Early, data = case0901))
summary(lm(Flowers ~ Intensity + factor(Time), data = case0901))
case0901$Intensity_F <- factor(case0901$Intensity)
str(case0901$Intensity_F)
levels(case0901$Intensity_F)
case0901$Intensity_F2 <- factor(case0901$Intensity, levels = c("750", "300", "450", 
                                                               "150", "600", "900"))
levels(case0901$Intensity_F2)
case0901$Intensity_F3 <- relevel(case0901$Intensity_F, ref = "300")
levels(case0901$Intensity_F3)
lm(Flowers ~ Intensity + factor(Time), data = case0901)
fit_1 <- lm(Flowers ~ factor(Intensity) + factor(Time), data = case0901)
summary(fit_1)
fit_2 <- lm(Flowers ~ relevel(factor(Intensity), ref = "600") + factor(Time), data = case0901)
summary(fit_2)
case0901$INTENSITY <- relevel(factor(case0901$Intensity), ref = "600")
case0901$INTENSITY # Print the new variable
fit_3 <- lm(Flowers ~ INTENSITY + Time, data = case0901) # Fit the model
summary(fit_3)
fit_intA <- lm(Flowers ~ Intensity*Time, data = case0901)
summary(fit_intA)
fit_intB <- lm(Flowers ~ Intensity + Time + Time:Intensity, data = case0901)
summary(fit_intB)
# lm(Flowers ~ Intensity + Intensity^2, data = case0901)) # If we try this line in R it won't work
lm(Flowers ~ Intensity + I(Intensity^2), data = case0901) # But this line works
case0901$Intensity2 <- case0901$Intensity^2
lm(Flowers ~ Intensity + Intensity2, data = case0901)