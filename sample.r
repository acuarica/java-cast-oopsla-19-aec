
library("DBI")
library(RSQLite)
library(reshape2)
library(ggplot2)

con = dbConnect(SQLite(), dbname="output.sqlite3")
df = dbGetQuery(con, "select * from casts where link!=''")
df$file <- NULL

takeSample <- function(n) {
  s = df[sample(nrow(df), n), ]
  write.csv(s,sprintf('sample-casts-%s.csv', n))
}

takeSample(5000)
