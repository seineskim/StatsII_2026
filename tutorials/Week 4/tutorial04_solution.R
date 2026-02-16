##################
#### Stats II ####
##################

###############################
#### Tutorial 4: Logit ####
###############################

# In today's tutorial, we'll begin to explore logit regressions
#     1. Estimate logit regression in R using glm()
#     2. Practice makes inferences using logit regression
#     3. Compare logit models

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
lapply(c("stringr", "dplyr", "tidyverse", "stargazer", "ggplot2"),  pkgTest)


# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Binary logits:

# Employing a sample of 1643 men between the ages of 20 and 24 from the U.S. National Longitudinal Survey of Youth.
# Powers and Xie (2000) investigate the relationship between high-school graduation and parents' education, race, family income, 
# number of siblings, family structure, and a test of academic ability. 

#The dataset contains the following variables:
# hsgrad Whether: the respondent was graduated from high school by 1985 (Yes or No)
# nonwhite: Whether the respondent is black or Hispanic (Yes or No)
# mhs: Whether the respondent’s mother is a high-school graduate (Yes or No)
# fhs: Whether the respondent’s father is a high-school graduate (Yes or No)
# income: Family income in 1979 (in $1000s) adjusted for family size
# asvab: Standardized score on the Armed Services Vocational Aptitude Battery test 
# nsibs: Number of siblings
# intact: Whether the respondent lived with both biological parents at age 14 (Yes or No)

graduation <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Powers.txt")

str(graduation)

##Convert Yes/No variables to factors (important for interpretation):
yn_vars <- c("hsgrad","nonwhite","mhs","fhs","intact")
graduation[yn_vars] <- lapply(graduation[yn_vars], factor)

str(graduation)

# (a) Perform a logistic regression of hsgrad on the other variables in the data set.

m_full <- glm(
  hsgrad ~ nonwhite + mhs + fhs + income + asvab + nsibs + intact,
  data = graduation,
  family = binomial(link = "logit")
)

summary(m_full)


# Compute a likelihood-ratio test of the omnibus null hypothesis that none of the explanatory variables influences high-school graduation. 

m_null <- glm(hsgrad ~ 1, data = graduation, family = binomial)

anova(m_null, m_full, test = "LRT")

##Interpretation:
##To test the null hypothesis that none of the explanatory variables influences the probability of high-school graduation, we compare the full model to an intercept-only model using a likelihood-ratio (LR) test.
##The LR test strongly rejects the null (p-value ≪ 0.05), indicating that at least one explanatory variable significantly affects the probability of high-school graduation.

# Then construct 95-percent confidence intervals for the coefficients of the seven explanatory variables. 
confint(m_full)

## Key conclusions from the estimated coefficients and confidence intervals are as follows:
## Cognitive ability (ASVAB) has a large, positive, and highly statistically significant effect on high-school graduation. The confidence interval does not come close to including zero.
## Family income has a positive and statistically significant effect, indicating that youths from higher-income families are more likely to graduate from high school.
## Living in an intact family significantly increases the likelihood of graduation.
## Mother’s education has a positive and statistically significant effect at the 5% level.
## Father’s education has a positive but weaker effect, which is not statistically significant at the 5% level.
## Race (nonwhite) has a statistically significant effect, suggesting systematic differences in graduation probabilities across racial groups after controlling for other factors.
## Number of siblings has a negative but statistically insignificant effect, and its confidence interval includes zero.
## Overall, the results indicate that family background, cognitive ability, and family structure play an important role in predicting high school graduation, while the effect of family size is comparatively weak.

# What conclusions can you draw from these results? Finally, offer two brief, but concrete, interpretations of each of the estimated coefficients of income and intact.

##Family income:
  # Log-odds interpretation:
  # Holding all other variables constant, a one-unit increase in family income (measured in thousands of dollars) increases the log-odds of graduating from high school by approximately 0.053 on average.

  # Odds interpretation:
  # Exponentiating the coefficient, each additional $1,000 of family income increases the odds of graduating high school by a factor of
  # exp(0.053) ≈ 1.05 on average, corresponding to roughly a 5% increase in the odds of graduation.

##Intact family structure:
  # Log-odds interpretation:
  # Holding other covariates constant, individuals who lived with both biological parents at age 14 have log-odds of graduating high school that are approximately 0.72 higher than those who did not on average.

  # Odds interpretation:
  # In odds terms, coming from an intact family multiplies the odds of graduating high school by
  # exp(0.719)≈2.05 on average, implying that youths from intact families have about twice the odds of graduating compared to those from non-intact families.


# (b) The logistic regression in the previous problem assumes that the partial relationship between the log-odds of high-school graduation and number of siblings is linear. 
# Test for nonlinearity by fitting a model that treats nsibs as a factor, performing an appropriate likelihood-ratio test. 
# In the course of working this problem, you should discover an issue in the data. 
# Deal with the issue in a reasonable manner. 
# Does the result of the test change?

graduation$nsibs_f <- factor(graduation$nsibs)

m_factor <- glm(
  hsgrad ~ nonwhite + mhs + fhs + income + asvab + nsibs_f + intact,
  data = graduation,
  family = binomial
)

summary(m_factor)

anova(m_full, m_factor, test = "LRT")

# This LR test evaluates:
  # H₀: The effect of nsibs is linear
  # H₁: The effect of nsibs is nonlinear

## LR test not significant at 5%; Suggests weak evidence of nonlinearity

unique(graduation$nsibs_f)
table(graduation$nsibs_f)

##Issue identified:
#Number of siblings = −3 is impossible.
#Some values (like 14, 15, 17) have very few observations; leads to high S.E., 

graduation_clean <- subset(graduation, nsibs >= 0) #Remove -3 case

graduation_clean$nsibs_cat <- cut(
  graduation_clean$nsibs,
  breaks = c(-1, 1, 3, 5, 10, 20),
  labels = c("0–1", "2–3", "4–5", "6–10", "11+")
)

unique(graduation_clean$nsibs_cat)
table(graduation_clean$nsibs, graduation_clean$nsibs_cat)

m_factor2 <- glm(
  hsgrad ~ nonwhite + mhs + fhs + income + asvab + nsibs_cat + intact,
  data = graduation_clean,
  family = binomial
)

summary(m_factor2)

anova(m_full, m_factor2, test = "LRT")

##Why the error? Number of observations/cases different between the two models (as nsibs == -3 case was removed)

m_full_clean <- glm(
  hsgrad ~ nonwhite + mhs + fhs + income + asvab + nsibs + intact,
  data = graduation_clean,
  family = binomial
)

anova(m_full_clean, m_factor2, test = "LRT")

##Interpretation:
##LR test not statistically significant at 5% or, likelihood-ratio test does not provide evidence against the linearity assumption of number of siblings predictor.
##Once implausible values and sparse categories are handled appropriately, there is no evidence that modeling the number of siblings as a nonlinear effect improves model fit.
##So, the linearity assumption is reasonable; The conclusion from part (a) is robust