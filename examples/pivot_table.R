library(flexpivot)
data("nobel_laureates")

# One variable
pivot_table(nobel_laureates, rows = "category")

# With two variables
pivot_table(nobel_laureates, rows = "category", cols = "birth_continent")

# Only count
pivot_table(nobel_laureates, rows = "category", cols = "gender", stats = "n")

# Only percent
pivot_table(nobel_laureates, rows = "category", cols = "gender", stats = "p")

# Without total
pivot_table(nobel_laureates, rows = "category", cols = "gender", total = FALSE)


# Two variable as rows
pivot_table(nobel_laureates, rows = c("birth_continent", "category"), cols = "gender")

# Two variable as cols
pivot_table(nobel_laureates, rows = "category", cols = c("gender", "laureate_type"))


# Without cols
pivot_table(nobel_laureates, rows = "category")
pivot_table(nobel_laureates, rows = c("category", "birth_continent"))
