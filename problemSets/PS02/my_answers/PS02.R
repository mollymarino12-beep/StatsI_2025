#)a

bribe_table <-as.table(matrix(c(14, 6, 7, 7, 7, 1),
                nrow = 2, byrow = TRUE,
                dimnames = list(
                  class = c("upper-class", "lower-class"),
                  party = c("not-stopped", "bribe-requested", "stopped-warning")
                )))
row_totals  <- rowSums(bribe_table)
col_totals  <- colSums(bribe_table)
grand_total <- sum(bribe_table)
expected <- outer(row_totals, col_totals) / grand_total
dimnames(expected) <- dimnames(bribe_table)
chi_contrib <- (bribe_table - expected)^2 / expected
chi_square_stat <- sum(chi_contrib)



#)b. 
df <- (nrow(bribe_table) - 1) * (ncol(bribe_table) - 1)
p_value <- 1 - pchisq(chi_square_stat, df)


 std_residuals <- (bribe_table - expected) / sqrt(expected)
std_residuals

#Not Stopped Bribe requested Stopped/given warning
#Upper class   0.1360828      -0.8153742              0.818923
#Lower class  -0.1825742       1.0939393             -1.098701




village_data <- read.csv(url("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv"))
summary(lm(water ~ reserved, data = village_data))
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   14.738      2.286   6.446 4.22e-10 ***
#  reserved       9.252      3.948   2.344   0.0197 *  

#Residual standard error: 33.45 on 320 degrees of freedom
#Multiple R-squared:  0.01688,	Adjusted R-squared:  0.0138 
#F-statistic: 5.493 on 1 and 320 DF,  p-value: 0.0197

#The correlation coefficient is 9.255 for the reservation policy. For a one unit increase in reservation policy there is an increase in drinking water facilities of 9.25 units. 

