library(networkdata)
library(ergm)
library(ergm.tapered)

data(hansell) 
help(hansell)

fit_hansell <- ergm.tapered::ergm.tapered(hansell ~ triadcensus + nodematch('sex'))
summary(fit_hansell)

summary(hansell ~ triadcensus)
a = summary(hansell ~ triadcensus)

boxplot(a)
barplot(a)
gof(fit_hansell)

plot(gof(fit_hansell))

