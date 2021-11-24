library(dplyr)
# Step 1: Estimate Raw Weights
# Use Probit regression to predict probability(Missing)
model.probit <- glm(M_missing ~ Z_gender + Z_account, 
                    family = binomial          # Distribution-family is binomial
                    (link = "probit"),         # Link function is probit
                    data = unit.dat)           # Indicates which data frame is used
# Determine raw nonresponse weights and add to data frame
pred <- 1 - predict.glm(model.probit, type="response") # "response" = probabilities
weight <- 1/pred # Raw weight
unit.dat <- cbind(unit.dat, pred, weight)   # Add new variables with cbind
# Step 2: Convert into Normalized Weights
# Aggregate weights
dat.agg.1 <- select(unit.dat, M_missing, weight) # Select M_missing and weight
dat.agg.2 <- group_by(dat.agg.1, M_missing)      # Group observations by M_missing
dat.agg.3 <- summarise(dat.agg.2,                # Calculate for each missing status:
                       r_sum = n(), # n observations, and 
                       weight_sum = sum(weight)) # Sum of the weights
# Normalize weights and add to data frame
unit.dat <- merge(unit.dat, dat.agg.3, by="M_missing")
unit.dat <- mutate(unit.dat, norm_weight=(1-as.numeric(M_missing))*weight*r_sum/weight_sum)
# Step 3: Do unweighted and weighted analyses
summary(lm(Y_loyalty ~ X_psq, data = unit.dat))   # Default analysis is unweighted 
summary(lm(Y_loyalty ~ X_psq, data = unit.dat, weights = norm_weight)) 