# 2018 Scholars' Day Figure Code (Hero Role)
# An Application of Mixed Effects Modeling to eSports Data 
# PI: Brett Cutts
# Faculty Sponsor: Dr. Aaron R. Baggett

# Load pacakge libraries
if(!require("tidyverse")){
  utils::install.packages("tidyverse")
}

library("tidyverse")

# Read in data 
fdat <- read.csv("~/overwatch/data/overwatch_data.csv")

# M0 plot
ggplot(data = fdat, aes(x = fwin, y = rating, color = hero.role)) + 
  geom_point(size = 4, alpha = .5) + 
  scale_x_continuous(limits = c(0.1, 0.9), 
    breaks = seq(0, 1, 0.1), name = "\nFight Win Rate") +
  scale_y_continuous(breaks = seq(100, 2000, 200), "Skill Rating\n") +
  geom_line(aes(x = fwin, y = simple_model_role), color = "gray45", size = 1) + 
  scale_color_discrete("Hero") + theme_bw() +
  theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    axis.title = element_text(size = 16, 
      face = "bold"), axis.text = element_text(size = 12), 
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 16, 
      face = "bold"), legend.key = element_rect(fill = NA), 
    legend.background = element_rect(fill = NA),
    legend.position = "bottom") +
  ggtitle("Fixed Effects Null Model") + 
  theme(plot.title = element_text(size = 18, 
    face = "bold"))

# M1 plot
ggplot(data = fdat, aes(x = fwin, y = rating, color = hero.role)) + 
  geom_point(size = 4, alpha = .5) + 
  scale_x_continuous(limits = c(0.1, 0.9), 
    breaks = seq(0, 1, 0.1), name = "\nFight Win Rate") +
  scale_y_continuous(breaks = seq(100, 2000, 200), "Skill Rating\n") +
  geom_line(aes(x = fwin, y = pred_ri_role, group = hero.role, color = hero.role), size = 1) +
  scale_color_discrete("Hero Role") + 
  theme_bw() +
  theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    axis.title = element_text(size = 16, 
      face = "bold"), axis.text = element_text(size = 12), 
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 16, 
      face = "bold"), legend.key = element_rect(fill = NA), 
    legend.background = element_rect(fill = NA),
    legend.position = "bottom") +
  ggtitle("Random Intercepts Model") + 
  theme(plot.title = element_text(size = 18, 
    face = "bold"))

# M2 plot
ggplot(data = fdat, aes(x = fwin, y = rating, color = hero.role)) + 
  geom_point(size = 4, alpha = .5) + 
  scale_x_continuous(limits = c(0.1, 0.9), 
    breaks = seq(0, 1, 0.1), name = "\nFight Win Rate") +
  scale_y_continuous(breaks = seq(100, 2000, 200), "Skill Rating\n") +
  geom_line(aes(x = fwin, y = pred_rs_role, group = hero.role, color = hero.role), size = 1) +
  scale_color_discrete("Hero Role") + 
  theme_bw() +
  theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    axis.title = element_text(size = 16, 
      face = "bold"), axis.text = element_text(size = 12), 
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 16, 
      face = "bold"), legend.key = element_rect(fill = NA), 
    legend.background = element_rect(fill = NA),
    legend.position = "bottom") +
  ggtitle("Random Slopes Model") + 
  theme(plot.title = element_text(size = 18, 
    face = "bold"))

# M3 plot
ggplot(data = fdat, aes(x = fwin, y = rating, color = hero.role)) + 
  geom_point(size = 4, alpha = .5) + 
  scale_x_continuous(limits = c(0.1, 0.9), 
    breaks = seq(0, 1, 0.1), name = "\nFight Win Rate") +
  scale_y_continuous(breaks = seq(100, 2000, 200), "Skill Rating\n") +
  geom_line(aes(x = fwin, y = pred_rsi_role, group = hero.role, color = hero.role), size = 1) +
  scale_color_discrete("Hero Role") + 
  theme_bw() +
  theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    axis.title = element_text(size = 16, 
      face = "bold"), axis.text = element_text(size = 12), 
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 16, 
      face = "bold"), legend.key = element_rect(fill = NA), 
    legend.background = element_rect(fill = NA),
    legend.position = "bottom") +
  ggtitle("Random Intercepts, Random Slopes Model") + 
  theme(plot.title = element_text(size = 18, 
    face = "bold"))
