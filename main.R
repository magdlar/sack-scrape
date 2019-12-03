library(dplyr)
library(rvest)
options(stringsAsFactors = FALSE)

get_table <- function(page) {
  page <- read_html(page)
  # list of all tables
  tables <- page %>%
    html_nodes("table")
  # read each table as elemet of list
  tables <- lapply(tables, function(x) {
    html_table(x, fill = TRUE)
  })
  # list of true/false
  columns <- lapply(tables, function(x) {
    "Outgoing manager" %in% names(x)
  })
  # select correct table from list of tables
  columns <- columns %>% unlist()
  table <- tables[columns][[1]]
}

build_links <- function(max_year=2020) {
  # vector of season starting years
  low_year <- 1992:(max_year - 1)
  # vector of season ending years
  high_year <- 1993:max_year %>% substr(start = 3, stop = 4)
  # vector of links to each season
  links <- mapply(function(x, y) {
    paste0("https://en.wikipedia.org/wiki/",
           x, "%E2%80%93", y,
           "_Premier_League")},
    low_year,
    high_year)
  # fix for different link in 1999-2000 season
  gsub("1999%E2%80%9300", "1999%E2%80%932000", links)
}

pages <- build_links()
tables <- list()
for (i in seq_len(length(pages))) {
  tables[[i]] <- get_table(pages[i])
  # standardize column names
  names(tables[[i]]) <- c("Team",
                          "Outgoing manager",
                          "Manner of departure",
                          "Date of vacancy",
                          "Position in table",
                          "Incoming manager",
                          "Date of appointment")
}

# stack the tables
tables <- do.call(rbind, tables)

# remove brackets, sources in wikipedia
tables <- apply(tables, 2, function(x) {
  gsub("\\[.*?\\]", "", x)}) %>%
  as.data.frame()

# blank to NA
tables[tables == ""] <- NA

# convert to dates
# NAs are created as there are some tables rvest struggles to read
tables[["Date of vacancy"]] <- tables[["Date of vacancy"]] %>%
  as.Date(format = "%d %B %Y")
tables[["Date of appointment"]] <- tables[["Date of appointment"]] %>%
  as.Date(format = "%d %B %Y")

# save dataframe
write.csv2(tables,  file = "output\\manager_sackings.csv", row.names = FALSE)
