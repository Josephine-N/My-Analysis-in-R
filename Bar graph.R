library(dplyr)
library(readxl)
library(tidyr)
library(tidyverse)
library(ggplot2)

View(STATA_FILE)
ggplot(STATA_FILE, aes(x = sex)) +
  geom_bar()
 