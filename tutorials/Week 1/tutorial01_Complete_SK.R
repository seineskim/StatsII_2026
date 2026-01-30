# Remove objects
rm(list=ls())

library(tidyverse)
library(stargazer)

# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

data_t <- read_csv("tutorial01_data.csv")

data <- read_csv("tutorial01_data.csv", 
                 col_types = cols(
                   `Ease of doing business rank (1=most business-friendly regulations) [IC.BUS.EASE.XQ]` = col_double(),
                   `Tax revenue (% of GDP) [GC.TAX.TOTL.GD.ZS]` = col_double(),
                   `GDP per capita (current US$) [NY.GDP.PCAP.CD]` = col_double()))



#### Wrangling the data
# We should now have a dataset where our variables are at least of the correct type.
# However, we need to do a bit of tidying to get the data into a more user-friendly
# format. 

# 1. First, let's have a look at our data object. Use the functions we learned from last
#    term. 

str(data)
summary(data)
ls(data)
ls.str(data)
head(data)

# 2. Let's drop the rows and columns we don't need.
# We only have one year, so the two cols related to year can be dropped; also, we only
# really need one col for country name, so let's drop country code too.

data <- data %>%
  select(-(starts_with("Time")), -(`Country Code`))

head(data)

# 3. Let's also get rid of the variable code in square brackets
#hint: try using the function sub() with the regexp " \\[.*"

names(data) <- sub(" \\[.*", "", names(data))

#### Analysing the data
# Now that we have a dataset in the desired format, we can proceed to the analysis.

# 1. Let's perform some preliminary descriptive analysis using our visualisation skills.
#    Try using ggplot to create a plot of scatter showing GDP p/c vs Tax revenue. Add a
#    simple linear regression line.

data %>%
  ggplot(aes(`Tax revenue (% of GDP)`, `GDP per capita (current US$)`)) +
  geom_point() +
  geom_smooth(method = "lm")

# 2. Now let's try the same using GDP p/c vs Ease of Doing Business.
data %>%
  ggplot(aes(`Ease of doing business rank (1=most business-friendly regulations)`, 
             `GDP per capita (current US$)`)) +
  geom_point() +
  geom_smooth(method = "lm")

# 3. And, for the sake of argument, let's see what the relationship is between Tax and
#    Ease of Doing Business.
data %>%
  ggplot(aes(`Ease of doing business rank (1=most business-friendly regulations)`, 
             `Tax revenue (% of GDP)`)) +
  geom_point() +
  geom_smooth(method = "lm")

# 4. Let's think for a minute before we perform the multivariate regression: what kind
#    of interaction are we seeing with these three plots?

# 5. Now let's run a regression!
formula <- `GDP per capita (current US$)` ~ `Tax revenue (% of GDP)` + `Ease of doing business rank (1=most business-friendly regulations)`

reg <- lm(formula, data)

summary(reg)

# How do we interpret these results?

#Linear regression model examining the relationship between tax revenue (as a percentage of GDP), ease of doing business, and GDP per capita.
#Tax revenue as a percentage of GDP shows a positive and marginally statistically significant association with GDP per capita (β = 1,458.24, p < 0.10). 
#This suggests that, holding ease of doing business constant, a one percentage-point increase in tax revenue (as a share of GDP) is associated with an increase of approximately USD 1,458 in GDP per capita. 
#While the effect is only significant at the 10% level, the magnitude of the coefficient indicates a potentially meaningful economic relationship.

#The coefficient for ease of doing business rank is negative, indicating that countries with worse business environments (higher rank values) tend to have lower GDP per capita. 
#Specifically, a one-unit deterioration in the ease of doing business rank is associated with a decrease of approximately USD 223 in GDP per capita, holding tax revenue constant. 
#However, this relationship is not statistically significant at conventional levels (p > 0.10), suggesting that the effect cannot be distinguished from zero in this sample.

#Model Fit and Overall Significance:
#The model explains approximately 13.3% of the variation in GDP per capita (R² = 0.133), with an adjusted R² of 0.092, indicating modest explanatory power. 
#Despite this, the overall model is statistically significant (F = 3.29, p < 0.05), suggesting that the included predictors jointly contribute to explaining variation in GDP per capita.

#### Communicating
# The final task is to communicate our results. We're going to do this in pdf format 
# using latex, and then upload our results to github, just as we would with a problem
# set!

# 1. Visualisation
# We want a good visualisation of our results, including a title. We've seen that Ease 
# of Doing Business doesn't seem to have a very significant effect (statistically or
# substantively), so let's plot GDP vs Tax, and include Ease of Doing Business as
# either a size or alpha variable to our scatter points. Use the "export" option in the
# plots window to create a pdf of the plot below. Save it in the same folder as your 
# latex template.

pdf("test.pdf")
data %>%
  ggplot(aes(`Tax revenue (% of GDP)`, 
             `GDP per capita (current US$)`, 
             alpha = `Ease of doing business rank (1=most business-friendly regulations)`)) +
  geom_point() +
  geom_smooth(method = "lm", show.legend = FALSE) +
  coord_cartesian(ylim = c(0, 150000)) +
  labs(title = "GDP per capita and Tax Revenue (2019)",
       subtitle = "World Bank data - Europe and Central Asia",
       alpha = "Ease of doing\nbusiness") +
  theme(legend.position = c(.85, .75),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 6),
        legend.key.size = unit(0.5, "cm"))
dev.off()

##This plot shows the relationship between tax revenue and GDP per capita, while also encoding a third variable — ease of doing business — using transparency.
##The points show individual countries, the line summarises the overall trend, and the legend explains how opacity relates to business friendliness.

# 2. Regression table
# We'll use stargazer to create the latex code for our regression table. Clear your 
# console, then run the code below.

stargazer(reg, type = "latex")
