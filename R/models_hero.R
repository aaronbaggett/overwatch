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
mdat <- read.csv("~/overwatch/data/WinstonsLab_data.csv")
mdat$hero.role <- stringr::str_to_title(mdat$hero.role)

# Model without respect to grouping
m0 <- lm(rating ~ fwin, data = mdat)
summary(m0)
sem.model.fits(m0)
mdat$simple.model <- predict(m0)

# Model with varying intercepts
m1 <- lmer(rating ~ fwin + (1 | hero), data = mdat)
summary(m1)
sem.model.fits(m1)
mdat$pred_ri <- predict(m1)

# Model with varying slopes
m2 <- lmer(rating ~ fwin + (0 + fwin | hero), data = mdat)
summary(m2)
sem.model.fits(m2)
mdat$pred_rs <- predict(m2)

# Model with varying slopes and intercepts
m3 <- lmer(rating ~ fwin + (1 + fwin | hero), data = mdat)
summary(m3)
sem.model.fits(m3)
mdat$pred_rsi <- predict(m3)