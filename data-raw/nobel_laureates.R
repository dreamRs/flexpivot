
#  ------------------------------------------------------------------------
#
# Title : Nodel lureates dataset
#    By : Victor
#  Date : 2020-07-03
#
#  ------------------------------------------------------------------------


# Source : https://www.nobelprize.org/about/developer-zone-2/
# API: https://app.swaggerhub.com/apis/NobelMedia/NobelMasterData/2



# Packages ----------------------------------------------------------------

library(data.table)
library(crul)




# Fetch data --------------------------------------------------------------


cli <- HttpClient$new(url = "https://api.nobelprize.org/")

laureates <- cli$get(path = "2.0/laureates", query = list(limit = 1000))
laureates$raise_for_status()
laureates <- jsonlite::fromJSON(laureates$parse("UTF-8"))

nobel_laureates <- as.data.table(laureates$laureates)
nobel_laureates <- nobel_laureates[, list(
  id = id,
  full_name = fullName.en,
  org_name = orgName.en,
  gender = gender,
  birth_date = birth.date,
  birth_continent = birth.place.continent.en,
  n_prizes = vapply(nobelPrizes, FUN = nrow, FUN.VALUE = numeric(1)),
  nobelPrizes = nobelPrizes
)]
nobel_laureates <- nobel_laureates[rep(seq_len(.N), times = n_prizes)]
nobel_laureates[, seq_prizes := rowid(id)]

nobel_laureates[, year := mapply(FUN = function(df, i) {
  df[["awardYear"]][i]
}, df = nobelPrizes, i = seq_prizes)]

nobel_laureates[, category := mapply(FUN = function(df, i) {
  df[["category"]][["en"]][i]
}, df = nobelPrizes, i = seq_prizes)]

nobel_laureates[, award_portion := mapply(FUN = function(df, i) {
  df[["portion"]][i]
}, df = nobelPrizes, i = seq_prizes)]


nobel_laureates[, seq_prizes := NULL]
nobel_laureates[, nobelPrizes := NULL]


nobel_laureates[, name := fifelse(is.na(full_name), org_name, full_name)]
nobel_laureates[, laureate_type := fifelse(is.na(full_name), "Organisation", "Individual")]
nobel_laureates[, org_name := NULL]
nobel_laureates[, full_name := NULL]
nobel_laureates[, n_prizes := NULL]

setorder(nobel_laureates, category, year)
setcolorder(nobel_laureates, c("id", "name", "gender", "birth_date", "birth_continent", "year", "category", "award_portion", "laureate_type"))
nobel_laureates




nobel_laureates <- as.data.frame(nobel_laureates)

usethis::use_data(nobel_laureates, overwrite = TRUE)


