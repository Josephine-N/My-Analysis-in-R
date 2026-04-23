library(dplyr)
library(readxl)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(emmeans)
library(multcomp)
library(multcompView)
library(agricolae)

View(Yield_season_2)
summary(Yield_season_2)

# getting RCBD model
Yield_season_2$trt <- as.factor(Yield_season_2$trt)
Yield_season_2$rep <- as.factor(Yield_season_2$rep)
model <- aov(nt_nighty ~ trt + rep, data = Yield_season_2)
summary(model)

# getting mean per treatment
emm <- emmeans(model, ~ trt)
emm


# Compare between treatments
pairs_result <- pairs(emm, adjust = "bonferroni")
pairs_result


#getting lsd and mean goupings 
result <- LSD.test(model, "trt", group = TRUE)
print(result)

boxplot(nt_nighty ~ trt, data = Yield_season_2,
        main = "nt_nighty by trt",
        xlab = "trt",
        ylab = "nt_nighty")

# Histogram with density curve 
hist(Yield_season_2$nt_nighty,
     main = "Histogram of nt_nighty",
     xlab = "nt_nighty",
     ylab = "Frequency")

# Create histogram with probability scaling
hist(Yield_season_2$nt_nighty, 
     probability = TRUE,   # important for density curve
     col = "lightblue",
     main = "Histogram with Density Curve",
     xlab = "nt_nighty")

# Add density curve
lines(density(Yield_season_2$nt_nighty), 
      col = "red", 
      lwd = 2)

hist(Yield_season_2$tw_nighty,
     main = "Histogram of tw_nighty",
     xlab = "tw_nighty",
     ylab = "Frequency")

# Create histogram with probability scaling
hist(Yield_season_2$tw_nighty, 
     probability = TRUE,   # important for density curve
     col = "lightblue",
     main = "Histogram with Density Curve",
     xlab = "nt_nighty")

# Add density curve
lines(density(Yield_season_2$nt_nighty), 
      col = "red", 
      lwd = 2)

# scatter plot
ggplot(Yield_season_2, aes(x = nt_nighty, y = tw_nighty)) +
  geom_point(color = "blue") +
  ggtitle("Scatter Plot of nt_nighty and tw_nighty") +
  xlab("nt_nighty") +
  ylab("tw_nighty")
ggplot(Yield_season_2, aes(x = nt_nighty, y = tw_nighty)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
 
## Correlation
# linear relationship
cor(Yield_season_2$nt_nighty, Yield_season_2$tw_nighty, method = "pearson")  
# ranks (non-normal data)
cor(Yield_season_2$nt_nighty, Yield_season_2$tw_nighty, method = "spearman") 
# small samples
cor(Yield_season_2$nt_nighty, Yield_season_2$tw_nighty, method = "kendall") 

