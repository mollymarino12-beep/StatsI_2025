#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/incumbents_subset.csv")
model_bivar_1 <- lm(voteshare ~ difflog, data = inc.sub)
summary(model_bivar_1)
plot(inc.sub$difflog, inc.sub$voteshare,
     xlab = "Difflog", ylab = "Voteshare",
     main = "Scatterplot of Voteshare vs Difflog",
     pch = 19, col = "blue")
abline(lm(voteshare ~ difflog, data = inc.sub), col = "red", lwd = 2)
residuals_model_1 <- residuals(model_bivar_1)
head(residuals_model_1)


model_bivar_2 <- lm(presvote ~ difflog, data = inc.sub)
summary(model_bivar_2)
plot(inc.sub$difflog, inc.sub$voteshare,
     xlab = "Difflog", ylab = "Presvote",
     main = "Scatterplot of Presvote vs Difflog",
     pch = 19, col = "blue")
abline(lm(presvote ~ difflog, data = inc.sub), col = "red", lwd = 2)
residuals_model_1 <- residuals(model_bivar_2)
head(residuals_model_1)



model_bivar_3 <- lm(voteshare~presvote,data=inc.sub)
summary(model_bivar_3)
plot(inc.sub$presvote,inc.sub$voteshare,
     xlab = "Presvote",
     ylab= "Voteshare",
     main= "Scatterplot of Voteshare vs Presvote",
     pch=19, col="blue")
model <- lm(voteshare ~ presvote, data= inc.sub)
abline(model, col="red", lwd=2)
residuals_model_3 <-residuals(model_bivar_3)
head(residuals_model_3)



resid_model <- lm(residuals_model_1 ~ residuals_model_2)
summary(resid_model)

plot(residuals_model_2, residuals_model_1,
     xlab = "Residuals from Question 2",
     ylab = "Residuals from Question 1",
     main = "Scatter Plot of Residuals from Question 1-2",
     pch = 19,
     col = "blue")
residual_model <- lm(residuals_model_1 ~ residuals_model_2)
abline(residual_model, col = "red", lwd = 2)

model_multi_4 <- lm(voteshare ~ difflog + presvote, data = inc.sub)
summary(model_multi_4)






