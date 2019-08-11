library(RSQLite)
library(reshape2)
library(ggplot2)

dbReadObs <- function(dbPath) {
  print(sprintf("[Reading SQLite DB '%s']", dbPath))
  dbReadTable(dbConnect(SQLite(), dbname=dbPath), "obs")
}

writeDef <- function(defPath, values) {
  print(sprintf("[Writing DEF '%s']", defPath))
  f <- file(defPath)
  writeLines(values, f)
  close(f)
}

# qr.db = dbConnect(SQLite(), dbname="output.sqlite3")
# reposid = dbGetQuery(qr.db, "select * from repos")
# df = dbGetQuery(qr.db, "select repoid, count(*) as nocasts from casts where link!='' group by repoid")

stats.raw <- dbReadObs("stats-results/.output.sqlite3")

stats.raw$value <- as.numeric(stats.raw$value)
stats.project <- dcast(stats.raw, project~stat)
stats.project$ratio <- stats.project$MethodWithCast/stats.project$Method

pdf('stats-methodwcastXproject.pdf', height = 1.35)
ggplot(stats.project, aes(x="", y=ratio))+
  geom_boxplot(outlier.shape=NA)+
  geom_jitter(aes(size=Method))+
  coord_flip()+
  theme_minimal()+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank())+
  labs(size="No. of Methods")
dev.off()

values <- c(
  sprintf("\\newcommand{\\nproject}{%s}", format(nrow(stats.project), big.mark=',')),
  sprintf("\\newcommand{\\nloc}{%s}", format(sum(stats.project$LOC), big.mark=',')),
  sprintf("\\newcommand{\\nexpr}{%s}", format(sum(stats.project$Expr), big.mark=',')),
  sprintf("\\newcommand{\\nstmt}{%s}", format(sum(stats.project$Stmt), big.mark=',')),
  sprintf("\\newcommand{\\ncast}{%s}", format(sum(stats.project$Cast), big.mark=',')),
  sprintf("\\newcommand{\\nmethod}{%s}", format(sum(stats.project$Method), big.mark=',')),
  sprintf("\\newcommand{\\nmethodwithcast}{%s}", format(sum(stats.project$MethodWithCast), big.mark=',')),
  sprintf("\\newcommand{\\castpercentage}{%#.2f}", mean(stats.project$ratio, na.rm=TRUE)*100)
)

writeDef('stats.def', values)
