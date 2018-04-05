# 2018 Scholars' Day Figure Code
# An Application of Mixed Effects Modeling to eSports Data 
# PI: Brett Cutts
# Faculty Sponsor: Dr. Aaron R. Baggett

# Load pacakge libraries
if(!require("tidyverse")){
  utils::install.packages("tidyverse")
}

library("tidyverse")

# Read in data 
mdat <- read.csv("~/overwatch/data/WinstonsLab_data.csv")
mdat$hero.role <- stringr::str_to_title(mdat$hero.role)

cols <- read.csv("data/hero_colors.csv")
names(cols) <- c("hero", "color")
fdat <- inner_join(mdat, cols)
fdat$color <- as.character(fdat$color)

# M0 plot
ggplot(data = fdat, aes(x = fwin, y = rating, color = color)) + 
  geom_point(size = 4, alpha = .5) + 
    scale_x_continuous(limits = c(0.1, 0.9), 
    breaks = seq(0, 1, 0.1), name = "\nFight Win Rate") +
  scale_y_continuous(breaks = seq(100, 2000, 200), "Skill Rating\n") +
  geom_line(aes(x = fwin, y = simple_model), color = "gray45", size = 1) + 
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
    face = "bold")) + scale_color_manual(values = fdat$color)

# M1 plot
ggplot(data = mdat, aes(x = fwin, y = rating, color = hero)) + 
  geom_point(size = 4, alpha = .5) + 
  scale_x_continuous(limits = c(0.1, 0.9), 
    breaks = seq(0, 1, 0.1), name = "\nFight Win Rate") +
  scale_y_continuous(breaks = seq(100, 2000, 200), "Skill Rating\n") +
  geom_line(aes(x = fwin, y = pred_ri, group = hero, color = hero), size = 1) +
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
    face = "bold")) + scale_color_manual(values = fdat$color, name = "Hero")

# M2 plot
ggplot(data = fdat, aes(x = fwin, y = rating, color = hero)) + 
  geom_point(size = 4, alpha = .5) + 
  scale_x_continuous(limits = c(0.1, 0.9), 
    breaks = seq(0, 1, 0.1), name = "\nFight Win Rate") +
  scale_y_continuous(breaks = seq(100, 2000, 200), "Skill Rating\n") +
  geom_line(aes(x = fwin, y = pred_rs, group = hero, color = hero), size = 1) +
  scale_color_discrete("Hero") + 
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
ggplot(data = fdat, aes(x = fwin, y = rating, color = hero)) + 
  geom_point(size = 4, alpha = .5) + 
  scale_x_continuous(limits = c(0.1, 0.9), 
    breaks = seq(0, 1, 0.1), name = "\nFight Win Rate") +
  scale_y_continuous(breaks = seq(100, 2000, 200), "Skill Rating\n") +
  geom_line(aes(x = fwin, y = pred_rsi, group = hero, color = hero), size = 1) +
  scale_color_discrete("Hero") + 
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

# Predicted probabilities plot
mdat$color <- as.character(fdat$color)

pred_dat <- mdat %>% 
  group_by(hero, color) %>% 
  summarize(n = length(hero),
    pred_rank = mean(pred_ri),
    sd_pred_rank = sd(pred_ri),
    se = sd_pred_rank/(sqrt(n)),
    me = 1.96 * se,
    ll = pred_rank - me,
    ul = pred_rank + me) %>% 
  arrange(desc(pred_rank))

pred_dat <- transform(pred_dat, hero = reorder(hero, pred_rank))

ggplot(data = pred_dat, aes(x = pred_rank, y = hero, color = color)) + 
  geom_vline(xintercept = 983.8056, linetype = 2, size = 1, color = "gray55") + 
  geom_point(size = 4) + scale_x_continuous(limits = c(725, 1275), 
    breaks = seq(725, 1275, 50), "\nSkill Rating") +
  scale_y_discrete(name = "Hero\n") +
  scale_color_manual(values = pred_dat$color) +
  theme_bw() + geom_segment(aes(x = ll, y = hero, 
    xend = ul, yend = hero), size = 0.75) +
  theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    axis.title = element_text(size = 16, 
      face = "bold"), axis.text = element_text(size = 12), 
    legend.position = "none") +
  ggtitle("Model Predicted Skill Ratings") + 
  theme(plot.title = element_text(size = 18, 
    face = "bold"))
