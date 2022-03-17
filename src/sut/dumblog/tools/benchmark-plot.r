#!/usr/bin/env Rscript

df <- read.csv("/tmp/bench-stats.csv")

df$workload <- factor(df$workload , levels=c(
  'journal-6', 'sqlite-6',
  'journal-7', 'sqlite-7',
  'journal-8', 'sqlite-8',
  'journal-9', 'sqlite-9',
  'journal-10', 'sqlite-10',
  'journal-11', 'sqlite-11',
  'journal-12', 'sqlite-12',
  'journal-13', 'sqlite-13',
  'journal-14', 'sqlite-14'))

X11()
boxplot(df$throughput ~ df$workload, xlab="# of clients", ylab="throughput")

message("Press Return To Continue")
invisible(readLines("stdin", n=1))
