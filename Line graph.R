library(readxl)
library(ggplot2)


View(Line_graph)
str(Line_graph)
Line_graph$year <- as.character(Line_graph$year)
Line_graph$year <- factor(Line_graph$year, levels = sort(unique(Line_graph$year)))
ggplot(Line_graph, aes(x = year, y = number_of_coffee_grains, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Number of coffee grains over the years",
       x = "Year",
       y = "Number of coffee grains") +
  theme_minimal()
