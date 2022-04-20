#Problem 5
# Load the ak91.csv data
df <- read.csv ("./data/ak91.csv")

# Store years of education and the weekly wage in separate variables
yrs_educ <- df$YRS_EDUC
wkly_wage <- df$WKLY_WAGE

# Find college graduates
has_college_degree <- yrs_educ == 16

# Part (a)
X <- ifelse(yrs_educ==16,1,0)

#sample analogue calculation
sum(X)/length(X) # = 0.1084857

# Part (b)
#calculate sample analogue for y given x equals 1
mu_college <- (sum(X * wkly_wage)/length(X)) / (sum(X)/length(X)) 
# 594.4866

# Part (c)
sd_hat <- ((sum(X*(wkly_wage^2))/sum(X)) - 
             ((sum(X*wkly_wage)/sum(X))^2)) # 184035.5

p_hat <- sum(X)/length(X) # 0.1084857

se_college <- (1/sqrt(length(X))) * sqrt(sd_hat/p_hat) # 2.268982


# Part (d)
z <- qnorm(0.05/2,lower.tail=F) # 1.959964

#confidence interval lower and upper
mu_college - z*se_college # 590.0395
mu_college + z*se_college # 598.9337

# Part (e)
# Given the duality of confidence intervals and hypothesis testing, we
# can use the CI constructed earlier to test the null hypothesis at the
# 95% confidence level. Given that 600 is not within the confidence
# interval, we can reject the null hypothesis that mu = 600 at the 95%
# confidence level. Economically, this means that it is unlikely that 
# a person's expected weekly wage would be $600/week if they went to 
# college/got a college education. 

# Part (f) 
# Similar to (e), we can use  the duality of confidence intervals and 
# hypothesis testing to draw conclusions about this hypothesis test.
# Since 595 is within the confidence interval, we fail to reject the 
# null hypothesis that mu = 595. Economically, this means that it is 
# possible for a person's expected weekly wage would be $600/week if 
# they went to college/got a college education. 

# Problem 6
# Part (a)
# Define a custom function that returns a two-sided confidence interval
my_confint <- function (mu_hat,se,alpha) {
  # Compute and return the confidence interval
  z_alp_two <- qnorm(alpha/2,lower.tail=F)
  left <- mu_hat - z_alp_two*se
  right <- mu_hat + z_alp_two*se
  confint <- c(left,right)
  return(confint)
}

# Test the function
my_confint(mu_college,se_college,0.01) # returns 588.6421 600.3311
# 0.0001 difference most likely due to rounding with qnorm

# Part (b)
# Define a custom function that returns TRUE if mu_0 is not in confint
my_testrejects <- function(confint,mu_0) {
  # Check whether mu_0 is in confint
  is_in_confint <- ifelse((mu_0>confint[1])&&(mu_0<confint[2])
                          ,"yes","no")
  # If mu_0 is in confint, don't reject. Else, reject.
  is_rejected <- ifelse(is_in_confint == "yes",FALSE,TRUE)
  # Return boolean
  return(is_rejected)
}

# Check whether the test rejects on 1% significance level
confint_01 <- my_confint(mu_college,se_college,0.01)
my_testrejects(confint_01,600) # returns FALSE


# Check whether the test rejects on 10% significance level
confint_10 <- my_confint(mu_college,se_college,0.1)
my_testrejects(confint_10,600) # returns TRUE


# Part (c)
# Define a custom function for a two-sided test
my_twosidedtest <- function(mu_hat,se,alpha,mu_0){
  # Compute the confidence interval w/ significance level alpha
  confint <- my_confint(mu_hat,se,alpha)
  # Check whether mu_0 is in the confidence interval
  is_rejected <- my_testrejects(confint,mu_0)
  # Construct test message
  if(is_rejected){
    message <- 
      paste0("We CAN reject the null hypothesis at the ",
            (1-alpha)*100,"% confidence level.")
  } else{
    message <- 
      paste0("We FAIL to reject the null hypothesis at the ",
             (1-alpha)*100,"% confidence level.")
  }#IF
  # Print the message
  print(message)
}

# Check whether the test rejects on 1% significance level
my_twosidedtest(mu_college,se_college,0.01,600) 
# Fail to reject

# Check whether the test rejects on 10% significance level
my_twosidedtest(mu_college,se_college,0.10,600) 
# can reject

