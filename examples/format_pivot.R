
data("diamonds", package = "ggplot2")

# With two variables
flex(pivot_table(diamonds, rows = "cut", cols = "color"))

# Only count
flex(pivot_table(diamonds, rows = "cut", cols = "color", stats = "n"))

# Only percent
flex(pivot_table(diamonds, rows = "cut", cols = "color", stats = "p"))



# Two variable as rows
flex(pivot_table(diamonds, rows = c("clarity", "cut"), cols = "color"))

# Two variable as cols
flex(pivot_table(diamonds, rows = "cut", cols = c("color", "clarity")))


# Without cols
flex(pivot_table(diamonds, rows = "cut"))
flex(pivot_table(diamonds, rows = c("clarity", "cut")))
