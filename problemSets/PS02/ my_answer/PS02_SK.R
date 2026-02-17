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

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_2026/blob/main/datasets/climateSupport.RData?raw=true"))

str(climateSupport)
summary(climateSupport)

#1. Additive Model
model_additive <- glm(choice ~ countries + sanctions, 
                      data = climateSupport, 
                      family = binomial(link = "logit"))

summary(model_additive)

#Compare with null model
model_null <- glm(choice ~ 1, 
                  data = climateSupport, 
                  family = binomial(link = "logit"))

anova(model_null, model_additive, test = "LRT")

#####################
# Problem 2
#####################
# 2(a) Interpretation: Odds change for 160 participating countries
# Since model_additive is an additive model, we interpret the coefficient 
# of sanctions independently of the participation level.
# Extract estimated Odds Ratios (exponentiated coefficients)
climateSupport$sanctions <- factor(climateSupport$sanctions, ordered = FALSE)
odds_ratios <- exp(coef(model_additive))

# Calculate the change in odds when sanctions increase from 5% to 15%
# Change = Odds(15%) / Odds(5%)
or_change_a <- odds_ratios["sanctions15%"] / odds_ratios["sanctions5%"]
print(paste("Change in odds (160 countries):", round(or_change_a, 3)))

# 2(b) Interpretation: Odds change for 20 participating countries
# Note: In an additive model without interaction terms, the partial effect 
# of sanctions is constant across all levels of the other variable.

or_change_b <- odds_ratios["sanctions15%"] / odds_ratios["sanctions5%"]
print(paste("Change in odds (20 countries):", round(or_change_b, 3)))

#3. Interaction model test
# Scenario: 80 countries participating and no sanctions ("None")

new_obs_c <- data.frame(countries = "80 of 192", sanctions = "None")

# predict() with type="response" gives the probability P(Y=1|X)
prob_c <- predict(model_additive, newdata = new_obs_c, type = "response")
print(paste("Estimated Probability:", round(prob_c, 3)))

#####################
# Problem 3
#####################
model_interaction <- glm(choice ~ countries * sanctions, 
                         data = climateSupport, 
                         family = binomial(link = "logit"))

summary(model_interaction)

# Compare the additive model and the interaction model using a Likelihood Ratio Test (LRT)
# This test determines if including the interaction term significantly improves the model's fit
anova(model_additive, model_interaction, test = "LRT")















