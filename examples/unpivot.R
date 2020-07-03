library(flexpivot)
library(magrittr)
data("nobel_laureates")

# Revert format
nobel_laureates %>%
  pivot_table(
    rows = "category",
    cols = c("gender", "birth_continent")
  ) %>%
  unpivot()

nobel_laureates %>%
  pivot_table(
    rows = "category",
    cols = "birth_continent"
  ) %>%
  unpivot()

