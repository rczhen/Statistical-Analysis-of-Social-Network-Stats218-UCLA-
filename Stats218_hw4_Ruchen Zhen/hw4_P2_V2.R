library(networkdata) 
data(gfriends) 
help(gfriends)

library(sna)
library(ergm)
library(ergm.tapered)

# The full network has isolated nodes: g_full_net
g_full_net <- network(gfriends$Y,directed = T,
                      vertex.attr = list(gfriends$X[,1],gfriends$X[,2],gfriends$X[,3]),
                      vertex.attrnames=list("grade","gpa","smoke"))

plot(g_full_net)
title(main = "Full Network")

# Delete vertices directed, using giant component method will delete extra vertices
delete.vertices(g_full_net,vid = which(get.vertex.attribute(g_full_net,"grade")>9))

plot(g_full_net)
title(main = "Grade 9 Girls Friendship Network")

summary(g_full_net ~ triadcensus)

fit_g9 <- ergm.tapered(g_full_net ~ edges + nodecov("gpa")+triadcensus(c(1:7)),
                       control=control.ergm(MCMC.burnin=15000, MCMC.samplesize=1000)) 
summary(fit_g9)

plot(gof(fit_g9))

mcmc.diagnostics(fit_g9)

# (d) Abbreviated model
fit_g9_abbrev <- ergm.tapered(g_full_net ~ edges + nodecov("gpa")+triadcensus(3))
summary(fit_g9_abbrev)
mcmc.diagnostics(fit_g9_abbrev)
plot(gof(fit_g9_abbrev))
