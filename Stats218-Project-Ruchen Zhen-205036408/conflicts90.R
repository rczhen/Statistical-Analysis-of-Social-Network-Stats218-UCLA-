## Load Library ----
library(networkdata)
data(package="networkdata")
data(conflict90s)
help(conflict90s)

library(network)
library(sna)
library(ergm)
library(ergm.tapered)
library(latentnet)
library(amen)
library(MASS)

## 0. Build up network ----
conf_net <- network(conflict90s$conflicts)
# Network attributes: vertices = 130, total edges= 203
set.vertex.attribute(conf_net,"pop",conflict90s$nodevars[,"pop"])
set.vertex.attribute(conf_net,"gdp",conflict90s$nodevars[,"gdp"])
set.vertex.attribute(conf_net,"polity",conflict90s$nodevars[,"polity"])

plot(conf_net,displaylabels = T,label.cex = 0.5,vertex.col="red")
title(main = "Conflict90s Full Network")

# Build another network removing isolates: 91 vertices, 203 edges
conf_no_iso_net <- network(conflict90s$conflicts)
set.vertex.attribute(conf_no_iso_net,"pop",conflict90s$nodevars[,"pop"])
set.vertex.attribute(conf_no_iso_net,"gdp",conflict90s$nodevars[,"gdp"])
set.vertex.attribute(conf_no_iso_net,"polity",conflict90s$nodevars[,"polity"])
delete.vertices(conf_no_iso_net,vid=isolates(conf_no_iso_net))

plot(conf_no_iso_net,displaylabels = T,label.cex = 0.5,vertex.col="red")
title(main = "Conflict90s Network (Remove Isolates)")

## Consider typical ego networks: USA, IRQ
USA_ego <- ego.extract(giant_net, ego = which(giant_net%v%"vertex.names"=="USA"), neighborhood = "combined")
USA_egonet <- network(USA_ego$USA,matrix.type = "adjacency")
plot(USA_egonet,displaylabels=T,label.cex = 0.5)
title(main = "USA Ego Network")

ego.extract(conf_net, ego = which(conf_net%v%"vertex.names"=="IRQ"), neighborhood = "combined")


## 1. Start by presenting visual summaries of the networks, followed by numerical summary measures ----
# Degree Distribution and Density
table(sna::degree(conf_net,cmode = "indegree"))
plot(table(sna::degree(conf_net,cmode = "indegree")),xlab="outdegree",ylab="# of countries")
title(main = "Indegree Distribution")
table(sna::degree(conf_net,cmode = "outdegree"))
plot(table(sna::degree(conf_net,cmode = "outdegree")),xlab="outdegree",ylab="# of countries")
title(main = "Outdegree Distribution")
table(sna::degree(conf_net,cmode = "freeman"))
plot(table(sna::degree(conf_net,cmode = "freeman")),xlab="outdegree",ylab="# of countries")
title(main = "Total Degree Distribution")

cor(sna::degree(conf_net,cmode = "indegree"),sna::degree(conf_net,cmode = "outdegree"))
cor(sna::degree(conf_no_iso_net,cmode = "indegree"),sna::degree(conf_no_iso_net,cmode = "outdegree"))

network::network.density(conf_net) # Network Density
length(isolates(conf_net)) # Number of peaceful nations = 39


# Aggressive: large outdegree, vulnerable: large indegree


## 2. Describe the impact of the covariates on the patterns of ties. ----

# Analysis of GDP
gdp <- conflict90s$nodevars[,"gdp"]
mean(gdp)
median(gdp)
sd(gdp)

normal_gdp <- MASS::fitdistr(gdp, "normal")
para_gdp <- normal_gdp$estimate
hist(gdp, prob = TRUE,nclass = 200)
curve(dnorm(x, para_gdp[1], para_gdp[2]), col = 2, add = TRUE)

gdp_level = ifelse(gdp >= para_gdp["mean"] + para_gdp["sd"], "rich", "poor")
conf_net %v% "gdp_level" = gdp_level
conf_no_iso_net %v% "gdp_level" = ifelse(conf_no_iso_net %v% "gdp" >= para_gdp["mean"] + para_gdp["sd"], "rich", "poor")

# Analysis of Population
pop <- conflict90s$nodevars[,"pop"]
mean(pop)
median(pop)
sd(pop)

normal_pop <- MASS::fitdistr(pop, "normal")
para_pop <- normal_pop$estimate
hist(pop, prob = TRUE,nclass = 200)
curve(dnorm(x, para_pop[1], para_pop[2]), col = 2, add = TRUE)

conf_net %v% "pop_level" = ifelse(conf_net %v% "pop" >= para_pop["mean"] + para_pop["sd"], "large", "small")
conf_no_iso_net %v% "pop_level" = ifelse(conf_no_iso_net %v% "pop" >= para_pop["mean"] + para_pop["sd"], "large", "small")


# Analysis of Polity
polity <- conflict90s$nodevars[,"polity"]
mean(polity)
median(polity)
sd(polity)

normal_polity <- MASS::fitdistr(polity, "normal")
para_polity <- normal_polity$estimate
hist(polity, prob = TRUE,nclass = 200)
curve(dnorm(x, para_polity[1], para_polity[2]), col = 2, add = TRUE)

conf_net %v% "polity_level" = ifelse(conf_net %v% "polity" >= 0, "positive", "negative")
conf_no_iso_net %v% "polity_level" = ifelse(conf_no_iso_net %v% "polity" >= 0, "positive", "negative")

# Nodecov model
fit_ergm_nodecov <- ergm.tapered::ergm.tapered(conf_no_iso_net ~ edges + nodecov("gdp") + nodecov("pop") + nodecov("polity"))

summary(fit_ergm_nodecov)
mcmc.diagnostics(fit_ergm_nodecov)
plot(gof(fit_ergm_nodecov, GOF = ~ idegree + odegree + triadcensus))

# Homophily model

fit_ergm_homo <- ergm.tapered::ergm.tapered(conf_no_iso_net ~ edges + nodematch("gdp_level") + nodematch("polity_level", diff = T))

summary(fit_ergm_homo)
mcmc.diagnostics(fit_ergm_homo)
plot(gof(fit_ergm_homo, GOF = ~ idegree + odegree + triadcensus))


## 3. Describe any sub-group cohesion you see in the networks. ----

## Consider Giant Component of the network: 
# Strong connected: 35 vertices, 111 edges
# Weak connected: 83 vertices, 197 edges
giant_edgelist <- component.largest(conf_net,connected = "weak",result = "graph")
giant_net <- network(giant_edgelist) 
plot(giant_net,displaylabels=T,label.cex = 0.5)
title(main = "Giant Component of Conflicts90s Network (Weakly Connected)")
degree(giant_net)

giant_edgelist_strong <- component.largest(conf_net,connected = "strong",result = "graph")
giant_net_strong <- network(giant_edgelist_strong) 
plot(giant_net_strong,displaylabels=T,label.cex = 0.5)
title(main = "Giant Component of Conflicts90s Network (Strongly Connected)")

ego.extract(giant_net, ego = which(giant_net%v%"vertex.names"=="AUS"), neighborhood = "combined")

## 4. Describe the pattern of centralization of the actors. ----
# degree centrality (based on degree) 
# closeness centrality (based on average distances) --- not appropatite
# betweeness centrality (based on geodesics) --- not appropatite
# eigenvector centrality (The centrality of each vertex is proportional to the sum of the centralities of its neighbors)
sna::centralization(conf_net,degree)
sna::centralization(conf_net,closeness)
sna::centralization(conf_net,betweenness)
sna::centralization(conf_net,evcent)

sna::centralization(conf_no_iso_net,degree)
sna::centralization(conf_no_iso_net,closeness)
sna::centralization(conf_no_iso_net,betweenness)
sna::centralization(conf_no_iso_net,evcent)


## 5. Fit latent position and latent position cluster models to the network. Is there evidence of clustering? ----
conf_no_iso_latent <- latentnet::ergmm(conf_no_iso_net~euclidean(d=2,G=6), verbose=TRUE)
summary(conf_no_iso_latent)

mcmc.diagnostics(conf_no_iso_latent)

plot(conf_no_iso_latent, pie=TRUE,labels=TRUE)


# group <- samplike %v% "group" 
# homogroup <- outer(group, group, "==") 
# fit <- ergmm(samplike ~ atentcov("homogroup") + euclidean(d=2))


## 6. If you have directed data, fit a triad census model to the networks. Is there evidence of balance in the patterns of ties? ----
triad.census(conf_net)
sum(triad.census(conf_net)[c(9,12,13,16)]) # Transitive: 47
sum(triad.census(conf_net)[c(6,7,8,10,11,14,15)]) # Cyclic: 553
triad.census(conf_no_iso_net) 
sum(triad.census(conf_no_iso_net)[c(9,12,13,16)]) # Transitive: 47
sum(triad.census(conf_no_iso_net)[c(6,7,8,10,11,14,15)]) # Cyclic: 553

# Fit triad census ergm models
f_triad_ergm <- ergm.tapered::ergm.tapered(conf_net ~ edges 
                                           + ctriad + ttriad)
summary(f_triad_ergm)
mcmc.diagnostics(f_triad_ergm)
plot(gof(f_triad_ergm, GOF = ~ idegree + odegree + triadcensus))

f_triad_ergm_no_iso <- ergm.tapered::ergm.tapered(conf_no_iso_net ~ edges 
                                           + ttriad + ctriad)
summary(f_triad_ergm_no_iso)
mcmc.diagnostics(f_triad_ergm_no_iso)
plot(gof(f_triad_ergm_no_iso, GOF = ~ idegree + odegree + triadcensus))



## 7. You can fit any ERGM you like to represent the structure. ----

# Fit ergm model

f_ergm_overall <- ergm.tapered::ergm.tapered(conf_no_iso_net ~ edges 
                                     + gwodegree(0.5,fixed = TRUE)
                                     + gwidegree(0.5,fixed = TRUE)
                                     + dgwesp(0.5,fixed = TRUE)
                                     + dgwdsp(0.5,fixed = TRUE)
                                     )
summary(f_ergm_overall)
mcmc.diagnostics(f_ergm_overall)
plot(gof(f_ergm_overall, GOF = ~ idegree + odegree + triadcensus))

# Blance of Network


