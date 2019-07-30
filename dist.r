
library(DBI)
library(RSQLite)
library(reshape2)
library(ggplot2)

write.plot <- function(pp, path) {
  cat("[Writing plot '", path, "']\n", sep='')
  pdf(path, height=3, width=6)
  print(pp)
  dev.off()
}

con = dbConnect(SQLite(), dbname="output.sqlite3")
reposid = dbGetQuery(con, "select repoid from repos")
df.1 = dbGetQuery(con, "select repoid, count(*) as nocasts from casts where link!='' group by repoid")
reposWithNoCasts <- setdiff(reposid$repoid, df.1$repoid)
df.2 <- data.frame(repoid=reposWithNoCasts, nocasts=rep(0, length(reposWithNoCasts)) )

df.count <- merge(df.1 ,df.2, all=TRUE)
df.count$ds <- 'population'

csv <- read.csv('casts.csv')
csv.count <- dcast(csv, repoid~'nocasts', length, value.var='castid')

df.subset <- df.count[df.count$repoid %in% csv.count$repoid,]
df.subset$ds <- 'sample'

df.all <- merge(df.count, df.subset, all=TRUE)

pp <- ggplot(df.all, aes(nocasts,color=ds))+
  stat_ecdf(geom="point")+
  scale_x_log10()+
  theme(legend.position='top')+
  labs(x="No. of Casts (log scale)", y="Empirical Cumulative Density of Projects")+
  scale_color_discrete(
    name="Dataset", breaks=c("population", "sample"),
    labels=c("Entire Population", "Subset of Projects where at least a cast is being sampled"))
write.plot(pp, 'dist-all-log.pdf')
