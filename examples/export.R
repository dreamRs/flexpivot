library(flexpivot)
data("diamonds", package = "ggplot2")

pt <- pivot_table(diamonds, rows = "cut", cols = "color")

# To PowerPoint
path_pptx <- tempfile(fileext = ".pptx")
export_pptx(pt, path_pptx)
browseURL(path_pptx)


# To Word
path_docx <- tempfile(fileext = ".docx")
export_docx(pt, path_docx)
browseURL(path_docx)


# To Excel
path_xlsx <- tempfile(fileext = ".xlsx")
export_xlsx(pt, path_xlsx)
browseURL(path_xlsx)


# Clean up
unlink(path_pptx)
unlink(path_docx)
