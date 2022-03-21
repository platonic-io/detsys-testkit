#!/usr/bin/env Rscript

df <- read.csv("/tmp/bench-stats.csv")

bartlett.test(df$throughput ~ df$workload, data=df)

#message("If p-value >= 0.05, use var.equal=TRUE below")
#
#t.test(df$throughput ~ df$workload, data=df,
#       var.equal=TRUE,
#       conf.level=0.95)

#df$workload <- factor(df$workload , levels=c(
#  'journal-6', 'sqlite-6',
#  'journal-7', 'sqlite-7',
#  'journal-8', 'sqlite-8',
#  'journal-9', 'sqlite-9',
#  'journal-10', 'sqlite-10',
#  'journal-11', 'sqlite-11',
#  'journal-12', 'sqlite-12',
#  'journal-13', 'sqlite-13',
#  'journal-14', 'sqlite-14'))

df$workload <- factor(df$workload , levels=c(
  'journal-5000',  'sqlite-5000',
  'journal-6000',  'sqlite-6000',
  'journal-7000',  'sqlite-7000',
  'journal-8000',  'sqlite-8000',
  'journal-9000',  'sqlite-9000',
  'journal-10000', 'sqlite-10000',
  'journal-11000', 'sqlite-11000',
  'journal-12000', 'sqlite-12000',
  'journal-13000', 'sqlite-13000',
  'journal-14000', 'sqlite-14000',
  'journal-15000', 'sqlite-15000'))

#df$workload <-
#  factor(df$workload , levels=c('journal-6',  'journal-7', 'journal-8',
#                                'journal-9', 'journal-10', 'journal-11',
#                                'journal-12', 'journal-13', 'journal-14',
#                                'sqlite-6',  'sqlite-7', 'sqlite-8',
#                                'sqlite-9', 'sqlite-10', 'sqlite-11',
#                                'sqlite-12', 'sqlite-13', 'sqlite-14'))
#
#colours = c(rep("red", 9), rep("blue", 9))

X11()

# https://stackoverflow.com/questions/37694234/two-boxplots-on-the-same-graph

boxplot(df$throughput ~ df$workload,
        #names=c(2^6, 2^7, 2^8, 2^9, 2^10, 2^11, 2^12, 2^13, 2^14,
                #2^6, 2^7, 2^8, 2^9, 2^10, 2^11, 2^12, 2^13, 2^14),
        xlab="# of clients",
        ylab="throughput",
        )#col=colours)

message("Press return to continue")
invisible(readLines("stdin", n=1))
