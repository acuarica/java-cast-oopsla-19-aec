
library(DBI)
library(RSQLite)
library(reshape2)
library(ggplot2)

con = dbConnect(SQLite(), dbname="output.sqlite3")
df = dbGetQuery(con, "select * from casts where repoid<=10")


#melt 



ggplot(df, aes(repoid))+ geom_histogram()
ggplot(df, aes(repoid))+stat_count()
