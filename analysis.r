library(tidyr)
library(plyr)
library(reshape2)
library(ggplot2)
library(UpSetR)

taxonomy = list(
  'Typecase' = list(
    'features' = c('GuardByInstanceOf', 'GuardByTypeTag', 'GuardByClassLiteral'),
    'categories' = c('guarded', 'lang', 'tools'),
    'ql' = '\\exis'
  ),
  'Equals' = list(
    'features' = c('Equals'),
    'categories' = c('guarded', 'gen'),
    'ql' = '\\cmark'
  ),
  'OperandStack' = list(
    'features' = c('OperandStack'),
    'categories' = c('guarded', 'lang'),
    'ql' = '\\xmark'
  ),
  'Family' = list(
    'features' = c('Family'),
    'categories' = c('lang'),
    'ql' = '\\xmark'
  ),
  'Factory' = list(
    'features' = c('Factory', 'GetOrCreateByClassLiteral'),
    'categories' = c('tools'),
    'ql' = '\\xmark'
  ),
  'Deserialization' = list(
    'features' = c('Deserialization'),
    'categories' = c('tools'),
    'ql' = '\\exis'
  ),
  'Composite' = list(
    'features' = c('Composite'),
    'categories' = c('lang'),
    'ql' = '\\xmark'
  ),
  'NewDynamicInstance' = list(
    'features' = c('NewDynamicInstance'),
    'categories' = c('tools'),
    'ql' = '\\cmark'
  ),
  'Stash' = list(
    'features' = c('LookupById', 'Tag', 'StaticResource'),
    'categories' = c('tools', 'gen'),
    'ql' = '\\exis'
  ),
  'CovariantReturnType' = list(
    'features' = c('CovariantReturnType', 'Clone'),
    'categories' = c('lang'),
    'ql' = '\\exis'
  ),
  'Redundant' = list(
    'features' = c('Redundant'),
    'categories' = c('dev'),
    'ql' = '\\cmark'
  ),
  'VariableSupertype' = list(
    'features' = c('VariableSupertype'),
    'categories' = c('dev'),
    'ql' = '\\exis'
  ),
  'UseRawType' = list(
    'features' = c('UseRawType'),
    'categories' = c('dev', 'generic', 'boxing'),
    'ql' = '\\cmark'
  ),
  'RemoveWildcard' = list(
    'features' = c('RemoveWildcard'),
    'categories' = c('lang', 'generic'),
    'ql' = '\\cmark'
  ),
  'KnownReturnType' = list(
    'features' = c('KnownReturnType'),
    'categories' = c('tools', 'dev'),
    'ql' = '\\xmark'
  ),
  'ObjectAsArray' = list(
    'features' = c('ObjectAsArray'),
    'categories' = c('dev'),
    'ql' = '\\exis'
  ),
  'AccessSuperclassField' = list(
    'features' = c('AccessSuperclassField'),
    'categories' = c('dev'),
    'ql' = '\\cmark'
  ),
  'SelectOverload' = list(
    'features' = c('SelectOverload'),
    'categories' = c('lang', 'boxing'),
    'ql' = '\\cmark'
  ),
  'ReflectiveAccessibility' = list(
    'features' = c('ReflectiveAccessibility'),
    'categories' = c('lang', 'boxing'),
    'ql' = '\\cmark'
  ),
  'CovariantGeneric' = list(
    'features' = c('CovariantGeneric'),
    'categories' = c('lang', 'generic', 'boxing'),
    'ql' = '\\exis'
  ),
  'SoleSubclassImplementation' = list(
    'features' = c('SoleSubclassImplementation'),
    'categories' = c('lang'),
    'ql' = '\\cmark'
  ),
  'FluentAPI' = list(
    'features' = c('FluentAPI'),
    'categories' = c('lang', 'generic'),
    'ql' = '\\exis'
  ),
  'ImplicitIntersectionType' = list(
    'features' = c('ImplicitIntersectionType'),
    'categories' = c('lang'),
    'ql' = '\\cmark'
  ),
  'GenericArray' = list(
    'features' = c('GenericArray', 'MatchBoxedType'),
    'categories' = c('lang', 'generic', 'boxing'),
    'ql' = '\\cmark'
  ),
  'UnoccupiedTypeParameter' = list(
    'features' = c('UnoccupiedTypeParameter'),
    'categories' = c('lang', 'generic'),
    'ql' = '\\xmark'
  ),
  'Primitive' = list(
    'features' = c('Literal', 'Numeric'),
    'categories' = c('lang')
  ),
  'ToRemove' = list(
    'features' = c('Boxing', 'Unboxing', 'This'),
    'categories' = c('lang')
  )
  )

declared.features <-  unlist(lapply(taxonomy, `[[`, 'features'), use.names=FALSE)
declared.categories <- c('guarded', 'lang', 'tools', 'gen', 'dev', 'generic', 'boxing')
declared.omitted <- c('BrokenLink', 'Bug', 'Duplicated')

check.diff <- function(x, y) {
  d <- setdiff(x, y)
  if (length(d) != 0) stop("x: ", x, " < y: ", y)
  d <- setdiff(y, x)
  if (length(d) != 0) stop("x: ", x, " < y: ", y)
}

check.diff(declared.categories, unique(unlist(lapply(taxonomy, `[[`, 'categories'), use.names=FALSE)) )

read.sample <- function(size) {
  df <- read.csv(sprintf('casts-%s.csv', size))
  df$batch <- size
  stopifnot(nrow(df) == size)
  stopifnot(ncol(df) == 7+1)
  
  df.undefined <- df[df$value == '', ]
  stopifnot(nrow(df.undefined)==0)

  df.empty <- df[df$value == '#', ]
  cat("[Remaining cast instances to manually analyze in batch size ", size, ": ", nrow(df.empty), "]", sep='', fill=TRUE)
  stopifnot(nrow(df.empty) == 0)
  df <- df[df$value != '#', ]

  re.omitted <- paste('\\?', declared.omitted, sep='', collapse='|')
  re.features <- paste(declared.features, collapse='|') 
  re <- sprintf("^((#(%s)(:\\w+)?)+,@(src|test|gen)|%s)$", re.features, re.omitted)
  df.malformed <- df[grep(re, df$value, invert=TRUE),]
  if (nrow(df.malformed) != 0) stop("Cast malformed found: ", paste(df.malformed$castid, df.malformed$value, sep='', collapse=' & '))

  df
}

read.samples <- function(...) {
  args <- c(...)
  ls <- lapply(args, read.sample)
  total <- do.call(sum , lapply(ls, nrow))
  df <- do.call(rbind, ls)
  stopifnot(nrow(df)==total)
  df
}

perc <- function(x, total) formatC( (x / total)*100, digits=2, format='f')

write.def <- function(patterns.def, casts.def, file='casts.def') {
  cat("[Writing def '", file, "']", sep='', fill=TRUE)
  values <- c('% DO NOT EDIT, AUTOMATICALLY GENERATED BY analysis.r')
  num <- function(x) format(x, big.mark=',')
  for (k in names(patterns.def)) {
    values <- append(values, c(
      sprintf("\\newcommand{\\n%s}{%s}", k, num(patterns.def[[k]]))
    ))
  }
  for (k in names(casts.def)) {
    values <- append(values, c(
      sprintf("\\newcommand{\\n%s}{%s}", k, num(casts.def[[k]])),
      sprintf("\\newcommand{\\p%s}{%s}", k, perc(casts.def[[k]], casts.def$Size))
    ))
  }
  write(values, file)
}

write.plot <- function(pp, path, height=7) {
  cat("[Writing plot '", path, "']\n", sep='')
  pdf(path, height=height)
  print(pp)
  dev.off()
}

plot.height <- function(n) {
  0.472441+0.629921+n*0.25
}

plot.height.col <- function(c) {
  plot.height(length(unique(c)))
}

check.consistency <- function(castids, casts.def) {
  stopifnot(casts.def$Seen == casts.def$Size+casts.def$BrokenLink+casts.def$Bug+casts.def$Duplicated)
  
  castsids.duplicated <- castids[duplicated(castids)]
  if (length(castsids.duplicated) != 0) stop("Duplicated casts found: ", castids.duplicated)
  else cat("[OK: No casts duplicated found]", sep='', fill=TRUE)
}

check.auto <- function(df) {
  check.diff <- function(df.auto, df.manual, msg) {
    x <- df[df$castid %in% setdiff(df.auto$castid, df.manual$castid),]
    if (!empty(x)) stop("Miscategorized ", msg, " casts: ", paste(x$castid, x$value, sep='', collapse=' & '))
    
    x <- df[df$castid %in% setdiff(df.manual$castid, df.auto$castid),]
    if (!empty(x)) stop("Miscategorized ", msg, " casts: ", paste(x$castid, x$value, sep='', collapse=' & '))
  }
  
  types.prim <- c('boolean', 'byte', 'char', 'short', 'int', 'long', 'float', 'double')
  
  df.prim.auto <- df[df$target %in% types.prim & df$source %in% types.prim,]
  df.prim.manual <- df[grep('#Numeric|#Literal', df$features),]
  check.diff(df.prim.auto, df.prim.manual, 'primitive')
  
  df.boxing.auto <- df[df$source %in% types.prim & !(df$target %in% types.prim),]
  df.boxing.manual <- df[grep('#Boxing', df$features),]
  check.diff(df.boxing.auto, df.boxing.manual, 'boxing')
  
  df.unboxing.auto <- df[!(df$source %in% types.prim) & df$target %in% types.prim,]
  df.unboxing.manual <- df[grep('#Unboxing', df$features),]
  check.diff(df.unboxing.auto, df.unboxing.manual, 'unboxing')
}

df <- read.samples(5000, 480, 47, 3)
df.out <- df
df.out$qltag <- NULL
df.out$batch <- NULL
write.csv(df.out, 'casts.csv', row.names=FALSE)

casts.def <- list()
patterns.def = list()
 
casts.def['Seen'] <- nrow(df)
cat("[Links seen: ", casts.def$Seen, "]", sep='', fill=TRUE)

for (key in declared.omitted) {
  make.key <- paste('?', key, sep='')
  df.omitted <- df[df$value == make.key, ]
  cat("[# ", key, ": ", nrow(df.omitted), "]", sep='', fill=TRUE)
  casts.def[key] <- nrow(df.omitted)
  df <- df[df$value != make.key, ]
}

casts.def['Size'] <- nrow(df)
cat("[Accountable cast instances (sample size): ", casts.def$Size, "]", sep='', fill=TRUE)
df <- separate(df, value, c('features', 'scope'), sep=',')
check.consistency(df$castid, casts.def)
check.auto(df)

df$scope <- substring(df$scope, 2)
df$scope <- factor(df$scope, levels=c('src', 'test', 'gen'))
stopifnot(empty(df[is.na(df$scope),]))

df$features <- substring(df$features, 2)
df <- separate_rows(df, features, sep='#')
df <- separate(df, features, c('features', 'args'), sep=':', fill='right')
df$pattern <- NA
for (p in names(taxonomy)) {
  df[df$features %in% taxonomy[[p]]$features,]$pattern <- p
}
stopifnot(empty(subset(df, is.na(pattern))))
check.diff(declared.features, unique(df$features))

patterns.def['UniqueProjects'] <- length(unique(df$repoid))
df.repo <- dcast(df, repoid+features~'count', length, value.var="features")
df.repo.distinct <- dcast(df.repo, repoid~'count', length, value.var="features")
df.repo.distinct <- df.repo.distinct[order(df.repo.distinct$count, decreasing=TRUE),]
cat("[Repoid showing more different features: ", df.repo.distinct[1,]$repoid, "]", sep='', fill=TRUE)

labs.instances <- labs(x=NULL, y = "# Instances")
scale.scope <- scale_fill_discrete(
    name="Scope",
    breaks=c("src", "test", "gen"),
    labels=c("Application/Library code", "Test code", "Generated code")
    )

for (pname in levels(as.factor(df$pattern))) {
  x <- df[df$pattern==pname,]
  pp <- ggplot(x, aes(x=features))+
    geom_bar(aes(fill=scope), position=position_stack(reverse = TRUE))+
    geom_text(stat='count', aes(label=..count..,y=..count..+3))+
    coord_flip()+
    theme(legend.position="top")+
    labs.instances+scale.scope
  write.plot(pp, sprintf('patterns/table-pattern-%s-features.pdf', pname), plot.height.col(x$features))
  
  casts.def[sprintf("%sPattern", pname)] <- nrow(x)
  casts.def[sprintf("%sPatternSrc", pname)] <- nrow(x[which(x$scope=='src'),])
  casts.def[sprintf("%sPatternTest", pname)] <- nrow(x[which(x$scope=='test'),])
  casts.def[sprintf("%sPatternGen", pname)] <- nrow(x[which(x$scope=='gen'),])
  
  patterns.def[sprintf("%sPatternSrcPerc", pname)] <- perc( nrow(x[which(x$scope=='src'),]), nrow(x))
  patterns.def[sprintf("%sPatternTestPerc", pname)] <- perc( nrow(x[which(x$scope=='test'),]), nrow(x))
  patterns.def[sprintf("%sPatternGenPerc", pname)] <- perc( nrow(x[which(x$scope=='gen'),]), nrow(x))

  for (subp in levels(as.factor(x$features))) {
    y <- x[which(x$features==subp),]
    casts.def[sprintf("%s%sSubpattern", pname, subp)] <- nrow(y)
    pp <- ggplot(y, aes(x=args))+
      geom_bar(aes(fill=scope), position=position_stack(reverse = TRUE))+
      geom_text(stat='count', aes(label=..count..,y=..count..+3))+
      coord_flip()+
      theme(legend.position="top")+
      labs.instances+scale.scope
    write.plot(pp, sprintf('patterns/table-pattern-%s-%s-args.pdf', pname, subp), plot.height.col(y$args))
  }
}

casts.def["PatternSrc"] <- nrow(df[which(df$scope=='src'),])
casts.def["PatternTest"] <- nrow(df[which(df$scope=='test'),])
casts.def["PatternGen"] <- nrow(df[which(df$scope=='gen'),])

df <- df[!(df$pattern %in% c('Primitive', 'ToRemove')), ]

tb <- table(df$pattern)
df$pattern <- factor(df$pattern, levels=names(tb[order(tb, decreasing = FALSE)]))
input.patterns.def <- c()
table.def <- c()
table.categories.def <- c()
i <- 1 
#for (p in levels(df$pattern)) {
for (p in names(tb[order(tb, decreasing = TRUE)])) {
  row.color <- if (i %% 2 == 0) '\\alt' else '\\row'
  table.def <- append(table.def, sprintf("%s \\nameref{pat:%s} & \\%sDesc & \\n%sPattern & \\p%sPattern \\%% \\\\", row.color, p, p, p, p))
  input.patterns.def <- append(input.patterns.def, sprintf("\\input{chapters/casts/patterns/%s}", p))
  
  a <- declared.categories %in% taxonomy[[p]]$categories
  r <- paste(sapply(a, function(b) if (b) '\\cmark' else ''), collapse=' & ', sep=' & ')
  r <- sprintf("%s & %s", r, taxonomy[[p]]$ql)
  table.categories.def <- append(table.categories.def, sprintf("%s \\nameref{pat:%s} & %s \\\\", row.color, p, r))
  i = i+1
}
write(table.def, 'table-casts-patterns.def')
write(input.patterns.def, 'input-patterns.def')
write(table.categories.def, 'table-casts-categories.def')

pp <- ggplot(df, aes(x=pattern))+
  geom_bar(aes(fill=scope), position=position_stack(reverse = TRUE))+
  geom_text(stat='count', aes(label=..count..,y=..count..+3))+
  coord_flip()+
  theme(strip.text.y=element_text(angle = 0), legend.position="top")+
  labs.instances+scale.scope
write.plot(pp, 'table-patterns.pdf', plot.height.col(df$pattern))

lpatterns <- levels(as.factor(df$pattern))
patterns.def['Pattern'] = length(lpatterns)

df.guarded <- df[which(df$pattern %in% c('Typecase', 'Equals', 'OperandStack')),]
df.upcast <- rbind(
  df[which(df$pattern=='SelectOverload'),],
  df[which(df$pattern=='CovariantGeneric' & df$args=='Upcast'),],
  df[which(df$pattern=='CovariantGeneric' & df$args=='Null'),],
  df[which(df$pattern=='CovariantGeneric' & df$args=='Boxing'),]
)

nReference <- casts.def$Size-casts.def$PrimitivePattern

casts.def['Reference'] = nReference
casts.def['Guarded'] = nrow(df.guarded)
casts.def['Unguarded'] = nReference - nrow(df.guarded)
casts.def['Upcast'] = nrow(df.upcast)
casts.def['Downcast'] = nReference - nrow(df.upcast)

patterns.def['EqualsOutOfGuarded'] = perc(casts.def$EqualsPattern, casts.def$Guarded)

declared.ql <- unlist(lapply(taxonomy, `[[`, 'ql'), use.names=FALSE)
patterns.def['ExisCount'] <- length(declared.ql[declared.ql=='\\exis'])
patterns.def['CmarkCount'] <- length(declared.ql[declared.ql=='\\cmark'])
patterns.def['XmarkCount'] <- length(declared.ql[declared.ql=='\\xmark'])

patterns.def['ExisPerc'] = perc(patterns.def$ExisCount, patterns.def$Pattern)
patterns.def['CmarkPerc'] = perc(patterns.def$CmarkCount, patterns.def$Pattern)
patterns.def['XmarkPerc'] = perc(patterns.def$XmarkCount, patterns.def$Pattern)

write.def(patterns.def, casts.def, 'casts.def')

# df.equals <- subset(df, df$features=='Equals')
# df.wide <- dcast(df.equals, castid~args, length, value.var="args")
# df.wide <- dcast(df, castid~features, length)
# #upset(df.wide,nsets=ncol(df.wide),nintersects=NA,mb.ratio = c(0.3, 0.7))
# upset(df.wide,nsets=ncol(df.wide) )
#upset(df.wide)
