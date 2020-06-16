
data("diamonds", package = "ggplot2")

# With two variables
pivot_format(pivot_table(diamonds, rows = "cut", cols = "color"))

# Only count
pivot_format(pivot_table(diamonds, rows = "cut", cols = "color", stats = "n"))

# Only percent
pivot_format(pivot_table(diamonds, rows = "cut", cols = "color", stats = "p"))



# Two variable as rows
pivot_format(pivot_table(diamonds, rows = c("clarity", "cut"), cols = "color"))

# Two variable as cols
pivot_format(pivot_table(diamonds, rows = "cut", cols = c("color", "clarity")))


# Without cols
pivot_format(pivot_table(diamonds, rows = "cut"))
pivot_format(pivot_table(diamonds, rows = c("clarity", "cut")))
