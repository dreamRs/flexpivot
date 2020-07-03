library(flexpivot)
data("nobel_laureates")

# One variable
pt <- pivot_table(nobel_laureates, rows = "category")

pivot_format(pt)
pivot_format(pt, formatter = pivot_formatter(
  n = function(x) {
    format(round(x * 100), big.mark = " ")
  }
))


# Two variable as rows
pt <- pivot_table(nobel_laureates, rows = c("category", "gender"))

pivot_format(pt)
pivot_format(pt, formatter = pivot_formatter(
  n = function(x) {
    format(round(x * 100), big.mark = " ")
  }
))



# One row, one column
pt <- pivot_table(nobel_laureates, rows = "category", cols = "gender")

pivot_format(pt)
pivot_format(pt, formatter = pivot_formatter(
  n = function(x) {
    format(round(x * 100), big.mark = " ")
  }
))

