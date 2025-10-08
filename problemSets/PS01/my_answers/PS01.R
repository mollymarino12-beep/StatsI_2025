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

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

average_student_IQ <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
#I renamed "y" as an object to average_student_IQ because it better represents the data set. 
sample.mean <- mean(average_student_IQ)
# I found the mean of the sample because it is used as a point estimate for the unknown population mean. 
sample.mean
#This step shows the mean in the console. The mean is 98.44.
sample.n <-length(average_student_IQ)
# I did this step to calculate my sample size. The sample.n is 25L
sample.sd <-sd(average_student_IQ)
# The next step of calculating the confidence interval is to find the standard deviation or spread of the data. The sample.sd is 13.09.
sample.se <-sample.sd/sqrt(sample.n)
# I did this step to calculate the standard error and to see how much the mean will fluctuate between the samples. 
sample.se
# This steps shows the standard error in my console. The sample.se is 2.618.
df <- sample.n-1
# I did this step to calculate the shape of the distribution. The df is 24.   
t_critical <- qt(0.95,df=sample.n-1)
# This step is using a t distribution because the population size is less than 30. I adjusted for 90 percent confidence to find the t score. The t_critical value is 1.71.
margin_error <- t_critical * sample.se
# I did this step to find the discrepancy between the sample and the t value.The margin_error is 4.48.
lower_bound <-sample.mean-margin_error
#The lowest possible value within the 90 percent confidence level for the population parameter is 93.95. 
upper_bound <-sample.mean +margin_error
# The highest possible value within the 90 percent confidence level for the population parameter is 102.92. 
print(c(lower_bound,upper_bound))
# This shows the 90 percent confidence intervals  of the average student IQ in the school for upper and lower values. Meaning, if sampling was conducted numerous times 90 percent of the student's IQ scores would fall between 93.95-102.92  

# H0:μ=100
# Ha:μ>100
mu0 <- 100
# muO is a way to store the value null hypothesis mean under a value name. This is the hypothesized mean.
t_value <- (sample.mean - mu0) / sample.se
# The t value is used because the sample is less than 30 and it shows how far the sample mean is away from the null hypothesis. The t_value is -0.595.
t_value
# This shows the t value in the console
df <- n-1
p_value <- pt(t_value, df, lower.tail = FALSE)
# the p value is used to compare against the a=0.05 to determine if we can reject the sample data based on the null hypothesis. I did tail=false because I am only interested in scores above 100 in the upper tail of the data. 
p_value
# The p value is 0.27 and this means that is is greater than 0.05. The null hypothesis cannot be rejected. 
alpha <- 0.05
# The null hypothesis cannot be rejected because the p value of 0.27 is greater than the alpha value of 0.05. This means that there is not enough evidence that the average student IQ score in the teacher's class is higher than the national average. 




      




#####################
# Problem 2
#####################
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)
expenditure$Expenditure_Housing <-expenditure$Y
# I am creating a new column within the expenditure data frame for Y 
expenditure$Personal_Income_State <-expenditure$X1
# I am creating a new column within the expenditure data frame for X1.
expenditure$Financially_Insecure <-expenditure$X2 
# I am creating a new column within the expenditure data frame for X2. 
expenditure$Residents_Urban <-expenditure$X3
# I am creating a new column within the expenditure data frame for X3.
Expenditure_Housing <-expenditure$Y
# I am pulling out the new vector so that R recognizes it as Y. 
Personal_Income_State <-expenditure$X1
# I am pulling out the new vector so that R recognizes it as X1.  
Financially_Insecure <-expenditure$X2
# I am pulling out the new vector so that R recognizes it as X2. 
Residents_Urban <-expenditure$X3
#I am pulling out the new vector so that R recognizes it as X3.

pdf("C:/Users/molly/OneDrive/Documents/GitHub/StatsI_2025/problemSets/PS01/my_answers/Scatterplot_variables.pdf")
expenditure_vars <- expenditure[, c("Expenditure_Housing",
                                    "Personal_Income_State",
                                    "Financially_Insecure",
                                    "Residents_Urban")]
pairs(expenditure_vars,
      main = "Scatterplot of Variables")
# I using the pairs function to compare all the variables against each other in multiple scatter plots. 
dev.off()

# Housing expenditure is positively associated with personal income and the number of residents living in urban areas. Personal income is also positively associated with both housing expenditure and urban residents. The variable financially insecure does not appear to have a strong association with any of the other variables but shows a negative trend: higher financial insecurity is associated with lower housing expenditure and lower personal income. The number of urban residents is positively associated with both personal income and housing expenditure. 



expenditure$Region <- factor(expenditure$Region,
                             levels = c(1, 2, 3, 4),
                             labels = c("Northeast", "North Central", "South", "West"))
# I did this step to create the region value into a factor. I needed to do this change the numbers into labels to be used in the graph. 
Region <-expenditure$Region
# I created region as a new value as part of the expenditure data set. 
avg_expenditure <- tapply(expenditure$Expenditure_Housing, expenditure$Region, mean, na.rm = TRUE)
# I did this step to get the aggregate of the per capita expenditure per region for the graph by the mean.This created a new variable called avg_expenditure. 
avg_expenditure

ggplot(expenditure, aes(x = Region, y = Expenditure_Housing)) +
  geom_col(fill = "blue") + 
  labs(
    title = "Per Capita Expenditure Per Region for Housing Assistance",
    x = "Region",
    y = "Per Capita Expenditure"
  ) +
  theme_minimal()
ggsave("C:/Users/molly/OneDrive/Documents/GitHub/StatsI_2025/problemSets/PS01/my_answers/Per_Capita_Expenditure.pdf")
# I needed to create a barplot because one of the variables is categorical and one is numerical. 
# The region that has the highest per capita expenditure on housing is the West. 


plot(Personal_Income_State,Expenditure_Housing,main="Per Capita Housing Assistance Vs.Personal Income",xlab="Per Capita Income In State",ylab="Per Capita Expenditure On Housing In State",
     pch=19, frame=FALSE)
# The relationship that is displayed by the graph is that as per capita income in the state goes up, the per capita expenditure on housing also goes up. This is a positive association. 
colors<-c("Northeast"="blue", "Northcentral"="green","South"="red","West"="yellow")
symbols<-c("Northeast"=16,"Northcentral"=15,"South"=14,"West"=17)
# I am assigning colors and symbols to the different regions. 
region_colors<-colors[Region]
# I am assigning a variable called region_colors to represent the colors for the region variable. 
region_symbols<-symbols[Region]
# I am assigning a variable called region_symbols to represent symbols for the region variable. 
pdf("C:/Users/molly/OneDrive/Documents/GitHub/StatsI_2025/problemSets/PS01/my_answers/Per_Capita_Housing_Assistance_Colors.pdf")
plot(Personal_Income_State,Expenditure_Housing,main="Per Capita Housing Assistance Vs.Personal Income",xlab="Per Capita Income In State",ylab="Per Capita Expenditure On Housing In State",
     pch=region_symbols,col=region_colors,frame=FALSE)
legend("bottomright",legend=names(colors),col=colors,pch=symbols)
# I am rewriting this code over again to include the newly created variables for color and the symbol. I also made a legend to show what the different symbols mean. 
dev.off()
#I did this because this is not ggplot and has to be saved a special way. 
