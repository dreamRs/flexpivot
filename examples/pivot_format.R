library(flexpivot)
library(magrittr)
data("nobel_laureates")

# With two variables
nobel_laureates %>%
  pivot_table("category", "gender") %>%
  pivot_format()

# Only count
nobel_laureates %>%
  pivot_table("category", "gender", stats = "n") %>%
  pivot_format(drop_stats = TRUE)

# Only percent
nobel_laureates %>%
  pivot_table("category", "gender", stats = "p") %>%
  pivot_format()


# Two variable as rows
nobel_laureates %>%
  pivot_table(c("birth_continent", "gender"), "category", stats = "p") %>%
  pivot_format()


# Without cols
pivot_format(pivot_table(nobel_laureates, rows = "category"))
pivot_format(pivot_table(nobel_laureates, rows = c("category", "gender")))


# Customize
nobel_laureates %>%
  pivot_table("category") %>%
  pivot_format(
    background = "#A3BE8C",
    labels = pivot_labels(n = "Count", p = "Percentage")
  )







