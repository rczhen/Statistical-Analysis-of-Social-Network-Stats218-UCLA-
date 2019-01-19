# install.packages("blockmodels")
library(latentnet)
library(ergm)
library(sna)
library(blockmodels)

data(sampson)

## (a) blockmodel in sna ----
eq<-equiv.clust(samplike)
fit_block_1 <- sna::blockmodel(samplike,eq,k=3)
plot(fit_block_1,cex=0.1)
# Same as
# sna::plot.blockmodel(fit_block_1)

names <- samplike %v% "vertex.names"
turks <- samplike %v% "group" =="Turks"
names[turks]
outcasts <- samplike %v% "group" =="Outcasts"
names[outcasts]
loyal <- samplike %v% "group" =="Loyal"
names[loyal]

## (b) blockmodels package ----

g2<-sna::as.sociomatrix.sna(samplike)
                       
b1 <- blockmodels::BM_gaussian("SBM",g2)
b1$estimate()
which.max(b1$ICL)

b2 <- blockmodels::BM_bernoulli("SBM",g2)
b2$estimate()
which.max(b2$ICL)

b3 <- blockmodels::BM_poisson("SBM",g2)
b3$estimate()
which.max(b3$ICL)
