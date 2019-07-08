
library(DBI)
library(RSQLite)
library(reshape2)
library(ggplot2)

write.plot <- function(pp, path) {
  cat("[Writing plot '", path, "']\n", sep='')
  pdf(path, height=4)
  print(pp)
  dev.off()
}

con = dbConnect(SQLite(), dbname="output.sqlite3")
reposid = dbGetQuery(con, "select repoid from repos")
df.1 = dbGetQuery(con, "select repoid, count(*) as nocasts from casts group by repoid")
reposWithNoCasts <- setdiff(reposid$repoid, df.1$repoid)
df.2 <- data.frame(repoid=reposWithNoCasts, nocasts=rep(0, length(reposWithNoCasts)) )

df.count <- merge(df.1 ,df.2, all=TRUE)

csv <- read.csv('casts.csv')
csv.count <- dcast(csv, repoid~'nocasts', length, value.var='castid')

labs <- labs(x="No. of Casts (log scale)", y="Empirical Cumulative Density of no. of projects")

pp <- ggplot(df.count, aes(nocasts))+stat_ecdf(geom="point", size=0.1)+scale_x_log10()+labs
write.plot(pp, 'dist-population.pdf')

pp <- ggplot(csv.count, aes(nocasts))+stat_ecdf(geom="point", size=0.5)+scale_x_log10()+labs
write.plot(pp, 'dist-csv.pdf')
