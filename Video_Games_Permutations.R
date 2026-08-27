#STEP 0: --INSTALL PACKAGES--

rm(list = ls()); set.seed(42)

install.packages("devtools")
library(devtools)

library(HypothesisTesting)


#-- LOAD IN CSV -- 

#video game sales data set

df <- read.csv('data/vgsales.csv')


#                           --- PATTERN 1: DIFFERENCE IN MEANS ---

#ATTEMPTED TO LOOK FOR PATTERNS: these are the patterns I tried before settling on one pattern 
unique(df$Genre)
unique(df$Publisher)
colnames(df)
nrow(df)
summary(df)

#QUESTION: For the most popular publisher, Nintendo, do Racing games have higher NA_Sales than Sports games? 

#SUBSET

S <- df[df$Publisher == "Nintendo",]

mean_Racing <- mean(S$NA_Sales[S$Genre == "Racing"], na.rm=TRUE)
mean_Sports <- mean(S$NA_Sales[S$Genre == "Sports"], na.rm=TRUE)

obs_diff <- mean_Racing - mean_Sports
obs_diff #0.19 difference in mean

permutation_test(S, "Genre", "NA_Sales", 10000, "Racing", "Sports")

#NULL HYPOTHESIS: There is no difference between the average NA_Sales of racing games by Nintendo and the average NA_Sales of RPG games by Nintendo
#ALTERNATIVE HYPOTHESIS: The average NA_Sales of racing games by Nintendo is greater than the average NA_Sales of RPG games by Nintendo 

#p-value is around 0.29 so cannot reject null hypothesis. 

#Moving on to find another difference in means pattern in which I can reject null hypothesis

#---------------------------------------------------------------------------------------------

#                     ---This is the pattern I went with for difference in means---

#QUESTION: For the most popular publisher (Nintendo), do Sports games have higher average NA_Sales than Action games?


#1) --SUBSET--

S <- df[df$Publisher == "Nintendo",]
#S <- subset(df, Publisher == "Nintendo")


#2) --OBSERVED DIFFERENCE--

mean_sports <- mean(S$NA_Sales[S$Genre == "Sports"], na.rm=TRUE)
mean_action <- mean(S$NA_Sales[S$Genre == "Action"], na.rm=TRUE)

obs_diff2 <- mean_sports - mean_action
obs_diff2

#observed difference of 0.9921473 million NA_Sales - sports according to this observed difference
#is significantly higher.


#3) --PERMUTATION TEST-- 
# permutation_test(df, group_col, numeric_col, n_perm, group1, group2)
# NOTE:
# The function internally swaps order if needed.
# Always tests: higher mean > lower mean

#instructions on permutation test copied from recitation 4 files for understanding

permutation_test(S, "Genre", "NA_Sales", 10000, "Action", "Sports")


#4) --INTERPRET P-VALUE-- 

#"One-tailed only (upper tail): H1 is higher mean > lower mean."

#p-value: around 0.02419758

#Null hypothesis: mean(S$NA_Sales[S$Genre == "Sports"], na.rm=TRUE) = mean(S$NA_Sales[S$Genre == "Action"], na.rm=TRUE)
#Alternative hypothesis: mean(S$NA_Sales[S$Genre == "Sports"], na.rm=TRUE) > mean(S$NA_Sales[S$Genre == "Action"], na.rm=TRUE)

#reject null hypothesis and accept alternative hypothesis


#now moving on to looking for patterns in difference in proportions

#-------------------------------------------------------------------------------------------------------


#                            --PATTERN 2: DIFFERENCE IN PROPORTIONS--


#ATTEMPTED TO LOOK FOR PATTERNS: these are the patterns I tried before settling on one pattern 
unique(df$Genre)
unique(df$Publisher)
colnames(df)
nrow(df)
summary(df)
unique(df$Platform)

#Question: Are there a higher proportion of action games on PS3 platform compared to X360 platform?

#subset of dataframe where only rows are platforms of either X360 or PS3
S <- df[df$Platform == c("X360", "PS3"),]
S

table(S$Platform, S$Genre)

#difference in proportion
p_X360_action <- mean(S$Genre[S$Platform == "X360"] == "Action", na.rm = TRUE)
p_PS3_action <- mean(S$Genre[S$Platform == "PS3"] == "Action", na.rm = TRUE)

obs_diff <- p_PS3_action - p_X360_action
obs_diff #3% difference in which proportion of action games on PS3 platform is higher than X360 platform

S$ActionIndicator <- S$Genre == "Action"

permutation_test(S, "Platform", "ActionIndicator", 10000, "PS3", "X360")

#NULL HYPOTHESIS: There is no difference between the proportion of action games on PS3 and the proportion of action games on X360.
#ALTERNATIVE HYPOTHESIS: The proportion of action games on PS3 platform is higher than X360 platform

#p-value is around 0.12 so I cannot reject the null hypothesis

#Moving on to find another difference in proportions pattern in which I can reject null hypothesis


#-------------------------------------------------------------------------------------------------------


#                     ---This is the pattern I went with for difference in proportions---

#QUESTION: Is the proportion of sports games higher on Wii than on DS in 2008?

#find most popular platform in global_sales in 2008

df_2008 <- subset(df, df$Year == 2008)
platform_sales <- aggregate(Global_Sales ~ Platform, data = df_2008, sum) 
platform_sales

#Wii is the most popular platform in 2008 in terms of sales followed by DS 

#amount of genres per platform
table(df$Genre, df$Platform)
#sum of total global sales for every platform
global_platform_sales <- tapply(df$Global_Sales, df$Platform, sum)

#1) --SUBSET--

#df_2008 <- df[df$Year == 2008,]
#table(df_2008$Genre, df_2008$Platform)


S <- df[df$Year == 2008 & df$Platform %in% c("Wii", "DS"),]
S

table(S$Genre, S$Platform)

#2) --OBSERVED DIFFERENCE--

prop_wii <- mean(S$Genre[S$Platform == "Wii"] == "Sports", na.rm = TRUE)
prop_ds <- mean(S$Genre[S$Platform == "DS"] == "Sports", na.rm = TRUE)

obs_diff <- prop_wii - prop_ds
obs_diff #11% observed difference 

#numeric indicator 

S$SportsIndicator <- S$Genre == "Sports"

#3)--PERMUTATION TEST--

permutation_test(S, "Platform", "SportsIndicator", 10000, "Wii", "DS")


#4) --INTERPRET P-VALUE--

#"One-tailed only (upper tail): H1 is higher mean > lower mean."

#p-value: 0.00089991

#Null hypothesis: mean(S$NA_Sales[S$Genre == "Sports"], na.rm=TRUE) = mean(S$NA_Sales[S$Genre == "Action"], na.rm=TRUE)
#Alternative hypothesis: mean(S$NA_Sales[S$Genre == "Sports"], na.rm=TRUE) > mean(S$NA_Sales[S$Genre == "Action"], na.rm=TRUE)

#reject null hypothesis and accept alternative hypothesis


#---------------------------------------------------------------------------------------------------------

#                 ---STEP 3: MULTIPLE HYPOTHESIS & BONFERONNI CORRECTION---


#--CORRECT FOR MULTIPLE HYPOTHESES--

#calculate the family-wise error rate

family_wise_error_rate <- 1 - (1 - 0.05)^4
family_wise_error_rate

#the family_wise_error_rate increased, so it is justified to correct because of an 18.5% of a false positive 

#what should the bonferroni correction should be 

bonferroni_correction <- 0.05/4 #default alpha is 0.05 and 4 is number of individual null hypotheses test 
bonferroni_correction

#would patterns still "hold", that is would you reject
#null hypothesis or not?

#WOULD PATTERNS STILL HOLD, THAT IS WOULD YOU REJECT NULL HYPOTHESIS OR NOT

#1) difference in mean -- reject null hypothesis or not?

#permutation test result was p-value:  0.02419758

0.02419758 < bonferroni_correction

#false, so null hypothesis IS NOT rejected based on bonferroni correction
#and multiple hypotheses correction


#2) difference in proportions -- reject null hypothesis or not?

#p-value: 0.00089991
 
0.00089991 < bonferroni_correction

#true, so null hypothesis IS rejected 



#                     --STEP 4: Compute 95% CONFIDENCE INTERVALS FOR MEANS--

# CI for a mean (using z* in the slides):
#   < xbar - MOE , xbar + MOE > (MOE = Margin of Error)
# where MOE = z* * SEM
# and SEM = s / sqrt(n)


#for the mean: 

#1) --OVERALL MEAN-- 

 #overall: all publishers and games 
 #numeric variable: NA_Sales 

 n <- nrow(df)
 xbar <- mean(df$NA_Sales)
 s <- sd(df$NA_Sales)
 conf_level <- 0.95
 
 
 #standard error of the mean
 SEM <- s / sqrt(n)
 SEM
 
 #critical value z star
 alpha <- 1 - conf_level
 z_star <- qnorm(1 - alpha/2)
 z_star  # ~1.959964
 
 #margin of error
 
 MOE <- z_star * SEM
 MOE
 
 #make confidence interval 
 
 
 #lower bound 
 
 CI_lower <- xbar - MOE
 
 #upper bound 
 
 CI_upper <- xbar + MOE
 
 CI_lower; CI_upper
 
 #as a vector
 CI <- c(CI_lower, CI_upper)
 
 CI
 
 #confidence interval is: (0.2522431  0.2770918)
 

#2) --SUBSET MEAN--

 #subset: Nintendo games 
 #numeric variable: NA_Sales 

 S <- df[df$Publisher == "Nintendo",] 
 
 n <- nrow(S) #sample size is 703 
 
 xbar <- mean(S$NA_Sales) # sample mean
 
 s <- sd(S$NA_Sales) #sample standard deviation
 
 conf_level <- 0.95
 
 #standard error of the mean
 SEM <- s / sqrt(n)
 SEM
 
 #critical value z star
 alpha <- 1 - conf_level
 z_star <- qnorm(1 - alpha/2)
 z_star  # ~1.959964
 
 #margin of error
 
 MOE <- z_star * SEM
 MOE
 
 #make confidence interval 
 
 #lower bound
 CI_lower <- xbar - MOE
 
 #upper bound
 CI_upper <- xbar + MOE
 
 CI_lower; CI_upper
 
 #as a vector
 CI <- c(CI_lower, CI_upper)
 
 CI
 
 #confidence interval is (0.9419293 1.3820252)
 

 #             ---STEP 4: COMPUTE 95% CONFIDENCE INTERVAL FOR PROPORTIONS---
 
 #1) overall proportion 
 
 df$SportsIndicator <- df$Genre == "Sports"
 
 n <- nrow(df)
 xbar <- mean(df$SportsIndicator)
 s <- sd(df$SportsIndicator)
 conf_level <- 0.95
 
 
 #standard error of the proportion
 SEM <- sqrt(xbar * (1 - xbar) /n)
 SEM
 
 #critical value z star
 alpha <- 1 - conf_level
 z_star <- qnorm(1 - alpha/2)
 z_star  # should be ~1.959964
 
 #margin of error
 
 MOE <- z_star * SEM
 MOE
 
 #make confidence interval 
 
 #lower
 CI_lower <- xbar - MOE
 
 #upper
 CI_upper <- xbar + MOE
 
 CI_lower; CI_upper
 
 #as a vector
 CI <- c(CI_lower, CI_upper)
 
 CI
 
 
 #confidence interval: (0.1360423 0.1466424)
 
 
 
 #2) subset proportion
 
 S <- df[df$Year == 2008 & df$Platform == c("Wii", "DS"),]
 S$SportsIndicator <- S$Genre == "Sports"
 
 #ds & wii
 
 n <- nrow(S)
 #combining wii and ds games, sports genre made up 12.7% of games
 xbar <- mean(S$SportsIndicator)
 s <- sd(S$SportsIndicator)
 conf_level <- 0.95
 
 
 #standard error of the proportion
 SEM <- sqrt(xbar * (1 - xbar) /n)
 SEM
 
 #critical value z star
 alpha <- 1 - conf_level
 z_star <- qnorm(1 - alpha/2)
 z_star  # should be ~1.959964
 
 
 #make confidence interval 
 
 #lower bound
 CI_lower <- xbar - z_star*SEM
 
 #upper bound
 CI_upper <- xbar + z_star*SEM
 
 CI_lower; CI_upper
 
 #as a vector
 CI <- c(CI_lower, CI_upper)
 
 CI
 
 #confidence interval: (0.09393857 0.16060688)
 
 
 
 #--------------------------------------------------------------------------------------------------------------
 
 
 #just extra confidence intervals did not focus on these 
 
 #ds
 
 S <- df[df$Year == 2008 & df$Platform == "DS",]
 S$SportsIndicator <- S$Genre == "Sports"
 
 n <- nrow(S)
 xbar <- mean(S$SportsIndicator)
 s <- sd(S$SportsIndicator)
 conf_level <- 0.95
 
 #standard error of the proportion
 
 SEM <- sqrt(xbar * (1 - xbar) /n)
 SEM
 
 #critical value z star
 alpha <- 1 - conf_level
 z_star <- qnorm(1 - alpha/2)
 z_star  # should be ~1.959964
 
 #margin of error
 
 MOE <- z_star * SEM
 MOE
 
 #make confidence interval 
 
 CI_lower <- xbar - MOE
 CI_upper <- xbar + MOE
 CI_lower; CI_upper
 
 #as a vector
 CI <- c(CI_lower, CI_upper)
 
 CI
 
 #confidence interval: (0.05891137 0.10775530)
 
 
 #wii
 
 S <- df[df$Year == 2008 & df$Platform == "Wii",]
 S$SportsIndicator <- S$Genre == "Sports"
 
 n <- nrow(S)
 xbar <- mean(S$SportsIndicator)
 s <- sd(S$SportsIndicator)
 conf_level <- 0.95
 
 
 #standard error of the proportion

 SEM <- sqrt(xbar * (1 - xbar) /n)
 SEM
 
 #critical value z star
 alpha <- 1 - conf_level
 z_star <- qnorm(1 - alpha/2)
 z_star  # should be ~1.959964
 
 #margin of error
 
 MOE <- z_star * SEM
 MOE
 
 #make confidence interval 
 
 CI_lower <- xbar - MOE
 CI_upper <- xbar + MOE
 CI_lower; CI_upper
 
 #as a vector
 CI <- c(CI_lower, CI_upper)
 
 CI

 #confidence interval: ( 0.1617462 0.2566935) 
 

 
 
 
 


