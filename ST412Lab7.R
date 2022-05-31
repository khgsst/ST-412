data <- read.csv("case1201.csv",header=TRUE)
data # Print the data
State <- data$State
SAT <- data$SAT
Takers <- data$Takers
Income <- data$Income
Years <- data$Years
Public <- data$Public
Expend <- data$Expend
Rank <- data$Rank
# Check possible influential points
plot(Public,SAT,pch=20, col="blue")
# Matrix of scatterplots
pairs(~SAT+Takers+Income+Years+Public+Expend+Rank, col="blue")
# Check possible curvature in data
plot(Takers,SAT,pch=20, col="blue")
logTakers <- log(Takers)
plot(logTakers,SAT,pch=20, col="blue")
# Add column to data set
data <- data.frame(data,logTakers)
# Matrix of scatterplots
pairs(~SAT+logTakers+Income+Years+Public+Expend+Rank, col="blue")
# Check possible influential points
plot(Public,SAT,pch=20, col="blue")
State[Public < 50] # Identify state with low Public 
plot(Expend,SAT,pch=20, col="blue")
State[which(abs((rstandard(mod1)))>2)] # Identify states that  greater than 2
State[which(abs((rstandard(mod1)))>3)] # Identify states that  greater than 3
# Leverage $2p'/n=2(7)/50=0.28$
plot(Obs,hatvalues(mod1), ylab="Leverage",xlab="Observations") # Plot leverage against the observations
abline(h=0.28, lty="dashed",col="red") # We include this value for reference
State[which(hatvalues(mod1)>0.28)] # Identify observations greater than 0.28
State[which(hatvalues(mod1)>0.50)] # Identify observations greater than 0.50
# Cook's Distance
plot(Obs,cooks.distance(mod1), ylab="Cook distance",xlab="Observations") 
abline(h=1, lty="dashed",col="red") # We include this value for reference
State[which(cooks.distance(mod2)>1)] # Identify states with distance greater than 1
# Adjusting for variables 
mod3 <- lm(SAT ~ logTakers + Rank, data=subset(data,State != "Alaska"))
mod3_res <- mod3$res
best_SAT <- order(mod3_res,decreasing=TRUE)
State[best_SAT]
# Call required libraries
library(MASS)
library(leaps)
all <- regsubsets(SAT ~ Expend + logTakers + Income + Years + Public + Rank, data = subset(data, State != "Alaska"), nbest = 7, method = "exhaustive")
summary(all)
plot(all)
library(ggplot2) 
library(plyr)
fortify.regsubsets <- function(model, data, ...){
  require(plyr)
  stopifnot(model$intercept)
  models <- summary(model)$which
  rownames(models) <- NULL
  model_stats <- as.data.frame(summary(model)[c("bic","cp","rss","rsq","adjr2")]) 
  dfs <- lapply(coef(model, 1:nrow(models)), 
                function(x) as.data.frame(t(x)))
  model_coefs <- plyr::rbind.fill(dfs)
  model_coefs[is.na(model_coefs)] <- 0
  model_stats <- cbind(model_stats, model_coefs)
  # terms_short <- abbreviate(colnames(models))
  terms_short <- colnames(models)
  model_stats$model_words <- aaply(models, 1, function(row) paste(terms_short[row], collapse = "+"))
  model_stats$size <- rowSums(summary(model)$which)
  model_stats
}

get_model_coefs <- function(model){
  models <- summary(model)$which
  dfs <- lapply(coef(model, 1:nrow(models)), 
                function(x) as.data.frame(t(x)))
  model_coefs <- plyr::rbind.fill(dfs)
  model_coefs[is.na(model_coefs)] <- 0
  model_coefs
}
models <- fortify(all)
head(models)
plot(models$size, models$cp);abline(0,1) # Notice the scale
plot(models$size, models$cp,ylim=c(1,10));abline(0,1) # Change the scale
#
# BIC plot
qplot(size, bic, data = models)
# Identify the model with the lowest Cp
models[models$cp == min(models$cp), ]
mod_lowestCp <- lm(SAT ~ Expend + logTakers + Years + Rank,data = subset(data, State != "Alaska"))
summary(mod_lowestCp)