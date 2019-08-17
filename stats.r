library(RSQLite)
library(reshape2)
library(ggplot2)
library(dplyr)

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

stats.raw <- dbReadObs("stats-results/.output.sqlite3")

stats.raw$value <- as.numeric(stats.raw$value)
stats.project <- dcast(stats.raw, project~stat)
stats.project$ratio <- stats.project$MethodWithCast/stats.project$Method
stats.project.outliers <- stats.project[stats.project$Method>0,]
stats.project.outliers <- stats.project.outliers %>% mutate(outlier = ratio > quantile(ratio,.75)+IQR(ratio)*1.5)

pdf('stats-methodwcastXproject.pdf', height = 1.35)
ggplot(stats.project.outliers, aes(x="", y=ratio))+
  geom_point(data = function(x) dplyr::filter_(x, ~ outlier), aes(size=CU,alpha=CU), position='jitter') +
  geom_violin() +
  geom_boxplot(outlier.shape=NA)+
  coord_flip()+
  theme_minimal()+
  theme(axis.title.y=element_blank(), axis.title.x=element_blank())+
  labs(size="No. of C.U.",alpha="No. of C.U.")
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


# qr.db = dbConnect(SQLite(), dbname="output.sqlite3")
# casts.table = dbReadTable(qr.db, "casts")
# repos.table = dbReadTable(qr.db, "repos")
# casts.with.link = casts.table[casts.table$link != '',]
# casts.no.link = casts.table[casts.table$link == '',]
# 
# if (nrow(casts.table)!=nrow(casts.with.link)+nrow(casts.no.link)) stop("Casts with/without link mismatch")
# 
# repos.with.casts = unique(casts.with.link$repoid)
# repos.no.link = unique(casts.no.link$repoid)
# repos.no.casts = setdiff(repos.table$repoid, repos.with.casts)
# 
# repos.sum = dcast(casts.with.link, repoid~'nocasts', length)
# df <- merge(repos.table, repos.sum, by='repoid', all=TRUE)
# df$internalid <- NULL
# df$project <- paste(df$user, '/', df$repo, '@', df$host, sep='')
# df <- merge(stats.project, df, all=TRUE)
# df[is.na(df$Cast),]$Cast = 0
# df[is.na(df$nocasts),]$nocasts = 0
# df$diff <- df$nocasts - df$Cast
# sum(df$diff)
