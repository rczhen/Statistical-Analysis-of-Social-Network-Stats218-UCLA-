library(networkdata) 
data(butland_ppi) 
help(butland_ppi)

library(sna)
library(ergm)
library(ergm.tapered)

ppi_net <- network(butland_ppi, directed = T)
plot(ppi_net)
title(main = "PPI Directed Network")

summary(ppi_net ~ triadcensus)

# 0
fit_ppi_0 <- ergm.tapered::ergm.tapered(ppi_net ~ edges + triadcensus(c(1,3,4,5,8)))
summary(fit_ppi_0)
mcmc.diagnostics(fit_ppi_0)                                      
plot(gof(fit_ppi_0))

# istar, ostar, gwodegree, gwidegree, dgwest, dgwdsp, ctriple, ttriple
# 1
fit_ppi_1 <- ergm.tapered::ergm.tapered(ppi_net ~ edges + istar(3))
summary(fit_ppi_1)
mcmc.diagnostics(fit_ppi_1)                                      
plot(gof(fit_ppi_1))
# 2
fit_ppi_2 <- ergm.tapered::ergm.tapered(ppi_net ~ edges + ostar(3))
summary(fit_ppi_2)
mcmc.diagnostics(fit_ppi_2)                                      
plot(gof(fit_ppi_2))
# 3
fit_ppi_3 <- ergm.tapered::ergm.tapered(ppi_net ~ edges + gwodegree(0.5,fixed = TRUE))
summary(fit_ppi_3)
mcmc.diagnostics(fit_ppi_3)                                      
plot(gof(fit_ppi_3))
# 4
fit_ppi_4 <- ergm.tapered::ergm.tapered(ppi_net ~ edges + gwidegree(0.5,fixed = TRUE))
summary(fit_ppi_4)
mcmc.diagnostics(fit_ppi_4)                                      
plot(gof(fit_ppi_4))
# 5
fit_ppi_5 <- ergm.tapered::ergm.tapered(ppi_net ~ edges + dgwesp(0.5,fixed = TRUE))
summary(fit_ppi_5)
mcmc.diagnostics(fit_ppi_5)                                      
plot(gof(fit_ppi_5))
# 6
fit_ppi_6 <- ergm.tapered::ergm.tapered(ppi_net ~ edges + dgwdsp(0.5,fixed = TRUE))
summary(fit_ppi_6)
mcmc.diagnostics(fit_ppi_6)                                      
plot(gof(fit_ppi_6))
# 7
fit_ppi_7 <- ergm.tapered::ergm.tapered(ppi_net ~ edges + ctriple)
summary(fit_ppi_7)
mcmc.diagnostics(fit_ppi_7)                                      
plot(gof(fit_ppi_7))
# 8
fit_ppi_8 <- ergm.tapered::ergm.tapered(ppi_net ~ edges + ttriple)
summary(fit_ppi_8)
mcmc.diagnostics(fit_ppi_8)                                      
plot(gof(fit_ppi_8))

##################################################################
# 11
fit_ppi_11 <- ergm.tapered::ergm.tapered(ppi_net ~ edges 
                                         + gwidegree(1,fixed = TRUE)
                                         + gwodegree(1,fixed = TRUE) 
                                         + dgwesp(3,fixed = TRUE)
                                         + ttriple)
summary(fit_ppi_11)
mcmc.diagnostics(fit_ppi_11)                                      
plot(gof(fit_ppi_11))
