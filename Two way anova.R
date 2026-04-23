library(dplyr)
library(readxl)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(emmeans)
library(multcomp)
library(multcompView)
library(lme4)
library(agricolae)

View(Season_1_2)

# Convert variables to factors
Season_1_2$rep <- as.factor(Season_1_2$rep)
Season_1_2$trt <- as.factor(Season_1_2$trt)
Season_1_2$season <- as.factor(Season_1_2$season)

# Fit the two-way ANOVA model (RCBD with Season)
model <- aov(ph_nighty ~ trt * season + rep, data = Season_1_2)

summary(model)


#If Block(rep) is random, use:
model <- lmer(ph_nighty ~ trt * season + (1|rep), data = Season_1_2)
anova(model)

# mean per treatment
emm <- emmeans(model, ~ trt)
emm

# comparison between treatments
emmeans(model, pairwise ~ trt, adjust = "bonferroni")

#getting lsd and mean groupings
lsd_result <- LSD.test(model, "trt", p.adj = "none")
lsd_result$groups

lsd_res <- LSD.test(model, "trt", alpha = 0.05, p.adj = "none")
lsd_res$statistics
