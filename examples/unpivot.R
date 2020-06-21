library(flexpivot)
data("diamonds", package = "ggplot2")

# Revert format
unpivot(pivot_table(diamonds, rows = "cut", cols = c("color", "clarity")))
unpivot(pivot_table(diamonds, rows = "cut", cols = c("color")))
