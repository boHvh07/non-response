

#-------------------------------------------------------------------------------
# Response Propensity Score Weighting in R
#
# Challenge:
#   Non-respondents are (expected to be) different from respondents.
#   "Other information" (z-variables), such as from the sampling frame, 
#   is available for respondents and non-respondents.
#
# Solution:
#   Use Response Propensity Score Weighting.
#   Then respondents who are similar to non-respondents on the z-variables receive
#   more weight in the analyses. It aims to make MAR or MNAR data to become MCAR data 
#
# Class Example
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# PREPARATION
#-------------------------------------------------------------------------------

# Clear workspace
rm(list=ls(all=TRUE)) 

# (Install and) Load required packages
library(rstudioapi)         # Simplify setting working directory
library(haven)              # Import Stata, SPSS, or SAS data 
library(dplyr)              # A Grammar for Data Manipulation (e.g., add variables)
library(texreg)             # Well-formatted tables (mostly for regression)
library(wCorr)              # Weighted correlations

# Set working directory to directory with R code
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  

# Send output to text file with "any.name" and also to screen (split = TRUE)
sink("UnitNonresponse.1.txt", split = TRUE) 

# Print text below in output: cat("text")
cat("
#-------------------------------------------------------------------------------
# Response Propensity Weighting
#
# Class Example
#-------------------------------------------------------------------------------\n")

date()                      

# Read-in data (SPSS format) and name this "unit.dat". 
unit.dat <- read_spss("UnitNonresponse.1.sav")

# Add comment to output file and then skip to next line by: cat("any.text  \n")
cat("\n# Total n cases (rows) and total p variables (columns) in data frame \n") 
dim(unit.dat)  

cat("\n# Summary statistics of variables in data frame \n") 
summary(unit.dat)  

cat("
#-------------------------------------------------------------------------------
# DATA ANALYSIS: Response Propensity Weighting
#-------------------------------------------------------------------------------\n")

cat("
#-------------------------------------------------------------------------------
# Step 1: Estimate Raw Weights
#-------------------------------------------------------------------------------\n")

cat("# Use Probit regression to predict p(Missing) \n")
cat("# glm is generalized linear model: distribution-family + link function \n")

model.probit <- glm(M_missing ~ Z_gender + Z_account, 
                family = binomial          # Distribution-family is binomial
                (link = "probit"),         # Link function is probit
                data = unit.dat)           # Indicates which data frame is used
# If this Probit regression and its estimates are non-significant,
# Response Propensity Score weighting is not possible (not needed)

cat("
#-------------------------------------------------------------------------------
# Summary of Probit model to predict missingness
#-------------------------------------------------------------------------------\n")

screenreg(list(model.probit), # Models included
          custom.model.names = "Probit Regresssion Model on Missingness",
          single.row = TRUE,                      # Estimate and SE in single row
          digits = 3,                             # Reporting precision
          ci.force = TRUE)                        # Report 95% Confidence Interval (CI)

cat("
# Conclusion:
# Z_gender and Z_Account differ significantly between respondents and nonrespondents.
# The 95% CI of their estimates does not overlap zero.
# It is useful to proceed with case weighting.\n\n")

cat("# Determine raw nonresponse weights and add to data set \n")
pred  <- 1 - predict.glm(model.probit, type="response") # Type = "response" gives probabilities
weight <- 1/pred                            # Raw Weight
unit.dat <- cbind(unit.dat, pred, weight)   # Add new variables to data frame "unit.dat" using cbind

cat("
#-------------------------------------------------------------------------------
# Step 2: Transform into Normalized Weights
#-------------------------------------------------------------------------------\n")

cat("# Aggregate weights \n")
dat.agg.1 <- select(unit.dat, M_missing, weight) # Select from unit.dat variables M_missing and weight
dat.agg.2 <- group_by(dat.agg.1, M_missing)      # Group observations by M_missing
dat.agg.3 <- summarise(dat.agg.2,                # Calculate for each missing status:
                       r_sum = n(),              # n observations, and 
                       weight_sum = sum(weight)) # Sum of the weights

cat("# Normalize weights and add to datafile \n")
unit.dat <- merge(unit.dat, dat.agg.3, by="M_missing")
unit.dat <- mutate(unit.dat, norm_weight=(1-M_missing)*weight*r_sum/weight_sum)

# Clean-up: Delete from the workspace the intermediate OBJECTS in the process
remove(list=c("model.probit","dat.agg.1","dat.agg.2","dat.agg.3","pred", "weight"))

# Clean-up: Delete from datafile the intermediate VARIABLES in the process
unit.dat$pred <- NULL             # Set variable in datafile to NULL to drop it
unit.dat$weight <- NULL 
unit.dat$r_sum <- NULL                         
unit.dat$weight_sum <- NULL

cat("# Check data: Show first 10 and last 5 cases in the datafile \n")
options(digits=3)                               # Show 3 digits only
head(unit.dat, n=10)
tail(unit.dat, n=5)

cat("
#-------------------------------------------------------------------------------
# Step 3: Do Unweighted and Weighted Data Analysis
#-------------------------------------------------------------------------------\n")

cat("# Unweighted and weighted means\n")
# A regression model with only an intercept (= vector of 1's) estimates the mean of y 
cat("\n# Intercept is unweighted mean of `Y_loyalty' \n")
lm(Y_loyalty ~ 1, data = unit.dat)

cat("# Intercept is weighted mean of `Y_loyalty' \n")
lm(Y_loyalty ~ 1, data = unit.dat, weights = norm_weight) # Weight indicates the weighting variable   

cat("# Unweighted and weighted correlations\n")
# We use the weightedCorr() function from R-package WCorr
cat("# Unweighted correlation between `Y_loyalty' and `X_psq' \n")
cor(unit.dat$X_psq[1:10], unit.dat$Y_loyalty[1:10], method = "pearson") # 1:10 uses the first 10 data points 

cat("# Weighted correlation between `Y_loyalty' and `X_psq' \n")
weightedCorr(unit.dat$X_psq[1:10], unit.dat$Y_loyalty[1:10], w = unit.dat$norm_weight[1:10], method = "pearson")

cat("# Unweighted and weighted regression models\n")
cat("# Unweighted regression of `Y_loyalty` on `X_psq`; check summary table in next section\n")
model.unweighted <- lm(Y_loyalty ~ X_psq, data = unit.dat) # Intercept is included by default

cat("# Weighted regression of `Y_loyalty` on `X_psq`; check summary table in next section \n")
model.weighted <- lm(Y_loyalty ~ X_psq, data = unit.dat, weights = norm_weight)    

cat("
#-------------------------------------------------------------------------------
# Report model summaries in a single table
#-------------------------------------------------------------------------------\n")

screenreg(list(model.unweighted, model.weighted), # Models included
          custom.model.names = 
            c("Unweighted Regresssion Model",     # Model names 
              "Weighted Regression Model"),
          single.row = TRUE,                      # Estimate and SE in single row
          digits = 3,                             # Reporting precision
          ci.force = TRUE)                        # Report 95% Confidence Interval (CI)

cat("
# Conclusion:
# Here the unweighted analysis indicates a significant effect of the x-variable,
# but the weighted analysis does not.
#
# Weighting cases does not need to identify more or stronger effects.
# If the MAR assumption is appropriate, the results of the weighted analysis are 
# closer to the population effect.\n\n")

cat("\n
#-------------------------------------------------------------------------------
# Increase the original sample size of 20 to 2000 and re-do regression analyses
#-------------------------------------------------------------------------------\n")

#-------------------------------------------------------------------------------
# Original data frame had 20 cases (10 respondents and 10 non-respondents)
# Replicate the data frame 100 times: 100 x 20 = 2,000

# k = multiplication factor
k <- 100
# rbind means bind rows, and replicate means, well ..., replicate
# Here: keep old name of data frame so all model specification stays the same
unit.dat <- do.call("rbind", replicate(k, unit.dat, simplify = FALSE))
#-------------------------------------------------------------------------------

cat("\n# Total n cases (rows) and total p variables (columns) in data frame \n") 
dim(unit.dat)  

cat("# Unweighted and weighted regression models\n")
model.unweighted <- lm(Y_loyalty ~ X_psq, data = unit.dat) # Intercept is included by default
model.weighted <- lm(Y_loyalty ~ X_psq, data = unit.dat, weights = norm_weight)    

cat("
#-------------------------------------------------------------------------------
# Report model summaries in a single table
#-------------------------------------------------------------------------------\n")

# Combine results in a single table, with 95% Confidence Intervals (ci)
screenreg(list(model.unweighted, model.weighted), # Models included
          custom.model.names = 
            c("Unweighted Regresssion Model",     # Model names 
              "Weighted Regression Model"),
          single.row = TRUE,                      # Estimate and SE in single row
          digits = 3,                             # Reporting precision
          ci.force = TRUE)                        # Report 95% Confidence Interval (CI)

# Same table but now with (SE) and asterisks for p-values
screenreg(list(model.unweighted, model.weighted), # Models included
          custom.model.names = 
            c("Unweighted Regresssion Model",     # Model names 
              "Weighted Regression Model"),
          single.row = TRUE,                      # Estimate and SE in single row
          digits = 3)

cat("\n
# Conclusion:
# At n = 2000, the unweighted and the weighted analysis indicate a significant 
# effect of the x-variable.
# Importantly, the effects of x on y is significantly smaller in the weighted
# regression than in the unweighted regression: the two 95% CIs do not overlap.
#
# If the MAR assumption is appropriate, the results of the weighted analysis are  
# closer to the population effect.\n\n")

# Print end date and time in output
date()

# Stop sinking
sink()

# Show results in console
file.show("UnitNonresponse.1.txt")

#-------------------------------------------------------------------------------
# CONCLUDING REMARKS
#-------------------------------------------------------------------------------

# The R-package "survey" has more functionality to conduct weighted analyses 
# of simple and complex survey data.

# For stratified samples (where some groups in the population are over- or 
# underrepresented in the sample), the same procedures for case weighting as for
# unit nonresponse/response hold.
# Stratification weights (aka as design weights) are proportion of j in population /
# proportion of j in sample.

