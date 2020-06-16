
data("diamonds", package = "ggplot2")

# One variable
pt <- pivot_table(diamonds, rows = "cut")

pivot_format(pt)
pivot_format(pt, formatter = pivot_formatter(
  n = function(x) {
    format(round(x), big.mark = " ")
  }
))


# Two variable as rows
pt <- pivot_table(diamonds, rows = c("cut", "color"))

pivot_format(pt)
pivot_format(pt, formatter = pivot_formatter(
  n = function(x) {
    format(round(x), big.mark = " ")
  }
))



# One row, one column
pt <- pivot_table(diamonds, rows = "cut", cols = "color")

pivot_format(pt)
pivot_format(pt, formatter = pivot_formatter(
  n = function(x) {
    format(round(x), big.mark = " ")
  }
))

