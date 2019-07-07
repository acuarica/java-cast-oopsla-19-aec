
library(DBI)
library(RSQLite)
library(reshape2)
library(ggplot2)

write.plot <- function(pp, path) {
  cat("[Writing plot '", path, "']\n", sep='')
  pdf(path)
  print(pp)
  dev.off()
}

con = dbConnect(SQLite(), dbname="output.sqlite3")
reposid = dbGetQuery(con, "select repoid from repos")
df = dbGetQuery(con, "select * from casts")
df$castid <- rownames(df)
reposWithNoCasts <- setdiff(reposid$repoid, df$repoid)

df.1 <- dcast(df, repoid~'nocasts', length, value.var='castid')
df.2 <- data.frame(repoid=reposWithNoCasts, nocasts=rep(0, length(reposWithNoCasts)) )

df.count <- merge(df.1 ,df.2, all=TRUE)
if (sum(df.count$nocasts) != nrow(df)) stop("sum: ", sum(df.count$nocasts), " != casts: ", nrow(df))

csv <- read.csv('casts.csv')
csv.count <- dcast(csv, repoid~'nocasts', length, value.var='castid')

pp <- ggplot(df.count, aes(nocasts))+stat_ecdf(geom="point", size=0.1)+scale_x_log10()
write.plot(pp, 'dist-population.pdf')

pp <- ggplot(csv.count, aes(nocasts))+stat_ecdf(geom="point", size=0.5)+scale_x_log10()
write.plot(pp, 'dist-csv.pdf')
