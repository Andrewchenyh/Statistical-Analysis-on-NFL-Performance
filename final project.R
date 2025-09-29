# Load libraries
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(car) 

# Load data
nfl <- read_csv('/Users/andrewchen/UCD/STA 141A/NFL.csv')

# Filter data
# Remove rows with missing values in any column except 'Drafted..tm.rnd.yr.'
nfl_clean <- nfl %>%
  select(-Drafted..tm.rnd.yr.) %>%
  drop_na()


# Make sure Drafted is a factor
nfl_clean$Position <- as.factor(nfl_clean$Position_Type)  
nfl_clean$Drafted <- as.factor(nfl_clean$Drafted)

# Two-way ANOVA 
model <- aov(Sprint_40yd ~ Drafted * Position_Type, data = nfl_clean)
summary(model)


# 1. Plot Residuals vs Fitted Plot to check for heteroscedasticity
plot(model$fitted.values, model$residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "blue", lwd = 2)

# 2. Plot QQ Plot to check normality of residuals
qqnorm(model$residuals,
       main = "QQ Plot of Residuals")
qqline(model$residuals, col = "blue", lwd = 2)

