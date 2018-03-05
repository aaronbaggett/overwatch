# 2018 Scholars' Day Model Code
# An Application of Mixed Effects Modeling to eSports Data 
# PI: Brett Cutts
# Faculty Sponsor: Dr. Aaron R. Baggett

package_list <- c("tidyverse", "lme4", "piecewiseSEM")
utils::install.packages(package_list)

# Load pacakge libraries
library(tidyverse)
library(lme4)
library(piecewiseSEM)

# Read in data
dat <- read.csv("~/Dropbox/UMHB/Scholars' Day/Overwatch/WinstonsLab_data.csv")

# Model without respect to grouping
m0 <- lm(rating ~ fwin, data = dat)
summary(m0)
sem.model.fits(m0)
dat$simple.model <- predict(m0)

# Model with varying intercepts
m1 <- lmer(rating ~ fwin + (1 | hero), data = dat)
summary(m1)
sem.model.fits(m1)
dat$pred_ri <- predict(m1)

# Model with varying slopes
m2 <- lmer(rating ~ fwin + (0 + fwin | hero), data = dat)
summary(m2)
sem.model.fits(m2)
dat$pred_rs <- predict(m2)

# Model with varying slopes and intercepts
m3 <- lmer(rating ~ fwin + (1 + fwin | hero), data = dat)
summary(m3)
sem.model.fits(m3)
dat$pred_rsi <- predict(m3)

# Write out new data frame with predicted values
write.csv(dat, "~/Dropbox/UMHB/Scholars' Day/Overwatch/overwatch_data.csv")
