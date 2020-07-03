library(flexpivot)
library(magrittr)
data("nobel_laureates")

# Change labels displayed in table
nobel_laureates %>%
  pivot_table("category", "gender", na_label = "Manquant") %>%
  pivot_format(
    labels = pivot_labels(
      stats = "Statistique",
      n = "Effectif",
      p = "Pourcentage",
      p_col = "% colonne",
      p_row = "% ligne",
      rows = c("Categorie"),
      cols = "Genre"
    )
  )
