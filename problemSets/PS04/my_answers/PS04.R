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
lapply(c("car"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
install.packages(car)
library(car)
data(Prestige)
help(Prestige)
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
table(Prestige$professional)
model <-lm(prestige ~ income * professional, data=Prestige)
summary(model)
ME_prof1 <- coef(model)["income"] + coef(model)["income:professional"]
ME_prof1 * 1000  
ME_prof2 <- coef(model)["professional"] + 6000 * coef(model)["income:professional"]
ME_prof2  
coef_lawnsigns <-0.042
se_lawnsigns <-0.016
t_stat <- coef_lawnsigns/se_lawnsigns
t_stat
p_valu <- 2*(1-pnorm(abs(t_stat)))
p_valu
alpha<-0.05
if(p_valu<alpha){
  conclusion<- "Reject the null hypothesis: evidence that lawn signs effect vote share"
}else{
  conclusion<- "Accept the null hypothesis that no evidence that lawn signs effect vote share"
}  
conclusion 
paste("Estimated effect on vote share", coef_lawnsigns)
coef_lawnsigns_2 <-0.042
se_lawnsigns_2 <-0.013
t_stat <- coef_lawnsigns_2/se_lawnsigns_2
t_stat
p_valu <- 2*(1-pnorm(abs(t_stat)))
p_valu
alpha<-0.05
if(p_valu<alpha){
  conclusion<- "Reject the null hypothesis: evidence that being next to precincts with lawn signs effect vote share"
}else{
  conclusion<- "Accept the null hypothesis that no evidence that being next to precincts with lawn signs effect vote share"
}  
conclusion 
paste("Estimated effect on vote share", coef_lawnsigns_2)


