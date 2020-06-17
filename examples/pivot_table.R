
data("diamonds", package = "ggplot2")

# With two variables
pivot_table(diamonds, rows = "cut", cols = "color")

# Only count
pivot_table(diamonds, rows = "cut", cols = "color", stats = "n")

# Only percent
pivot_table(diamonds, rows = "cut", cols = "color", stats = "p")

# Without total
pivot_table(diamonds, rows = "cut", cols = "color", total = FALSE)


# Two variable as rows
pivot_table(diamonds, rows = c("clarity", "cut"), cols = "color")

# Two variable as cols
pivot_table(diamonds, rows = "cut", cols = c("color", "clarity"))


# Without cols
pivot_table(diamonds, rows = "cut")
pivot_table(diamonds, rows = c("clarity", "cut"))
