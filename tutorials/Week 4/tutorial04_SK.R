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
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

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
graduation <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Powers.txt", header = TRUE)

yn_vars <- c("hsgrad","nonwhite","mhs","fhs", "intact")
graduation[yn_vars] <- lapply(graduation[yn_vars], factor)

str(graduation)
summary(graduation)

# (a) Perform a logistic regression of hsgrad on the other variables in the data set.
m_full <- glm(
  hsgrad ~ nonwhite + mhs + fhs + income + asvab + nsibs + intact, 
  data = graduation,
  family = binomial(link = "logit")
)

summary(m_full)

# Compute a likelihood-ratio test of the omnibus null hypothesis that none of the explanatory variables influences high-school graduation. 
m_null <- glm(hsgrad ~ 1, data = graduation, family = binomial)

anova(m_null, m_full, test = "LRT") #실행코드

# Then construct 95-percent confidence intervals for the coefficients of the seven explanatory variables.
confint(m_full)
exp(coef(m_full))
# What conclusions can you draw from these results? Finally, offer two brief, but concrete, interpretations of each of the estimated coefficients of income and intact.

# (b) The logistic regression in the previous problem assumes that the partial relationship between the log-odds of high-school graduation and number of siblings is linear. 
# Test for nonlinearity by fitting a model that treats nsibs as a factor, performing an appropriate likelihood-ratio test. 
# In the course of working this problem, you should discover an issue in the data. 
# Deal with the issue in a reasonable manner. 
# Does the result of the test change?
graduation$nsibs_f <- factor(graduation$nsibs)

m_factor <- glm(
  hsgrad ~ nonwhite + mhs + fhs + income + asvab + nsibs + intact,
  data = graduation,
  family = binomial
)

summary(m_factor)
anova(m_null, m_factor, tst = "LRT")

unique(graduation$nsibs_f)
table(graduation$nsibs_f)


graduation_clean <- subset(graduation, nsibs >= 0) #remove -3 case

graduation_clean$nsibs_cat <- cut(
  graduation_clean$nsibs,
  breaks = c(-1,1,3,5,10,20),
  labels = c("0-1", "2-3", "4-5", "6-10", "11-")
)

unique(graduation_clean$nsibs_cat)
table(graduation_clean$nsibs, graduation_clean$nsibs_cat)

m_factor_clean <- glm(hsgrad ~ nonwhite + mhs + fhs + income + asvab + nsibs + intact, 
                      data = graduation_clean, 
                      family = binomial(link = "logit"))

m_factor2 <- glm(hsgrad ~ nonwhite + mhs + fhs + income + asvab + nsibs_cat + intact, 
                 data = graduation_clean, 
                 family = binomial(link = "logit"))

anova(m_factor_clean, m_factor2, tst = "LRT")

