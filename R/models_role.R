# 2018 Scholars' Day Model Code (Hero Role)
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
m0.1 <- lm(rating ~ fwin, data = mdat)
summary(m0.1)
coef(m0.1)
sem.model.fits(m0.1)
mdat$simple_model_role <- predict(m0.1)

# Model with varying intercepts
m1.1 <- lmer(rating ~ fwin + (1 | hero.role), data = mdat)
summary(m1.1)
coef(m1.1)
sem.model.fits(m1.1)
mdat$pred_ri_role <- predict(m1.1)

# Model with varying slopes
m2.1 <- lmer(rating ~ fwin + (0 + fwin | hero.role), data = mdat)
summary(m2.1)
coef(m2.1)
sem.model.fits(m2.1)
mdat$pred_rs_role <- predict(m2.1)

# Model with varying slopes and intercepts
m3.1 <- lmer(rating ~ fwin + (1 + fwin | hero.role), data = mdat)
summary(m3.1)
coef(m3.1)
sem.model.fits(m3.1)
mdat$pred_rsi_role <- predict(m3.1)