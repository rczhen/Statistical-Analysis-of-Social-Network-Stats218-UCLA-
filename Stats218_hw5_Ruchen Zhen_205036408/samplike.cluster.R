# install.packages("mclust")
# install.packages("shapes")

library(latentnet)
library(ergm)
#
# Read in the set of ties from Sampson monastery data 
# These are the cumulative ties over three periods.
# A tie  from monk A to monk B exists if A nominated 
# B as one of his three best friends at any of the last
# three time points.
# 
data(sampson)
#
# Summarize the graph
#
summary(samplike)
#
# Look at the names of the monks (followed by there order in the original)
#
network.vertex.names(samplike)
#
# Look at the cohesive sub-groups designated by White et.al 1976.
#
get.vertex.attribute(samplike,"group")
#
# What are the degree distributions
#
degreedist(samplike)
#
# Plot the network
#
# pdf(file="samplike.cluster.pdf")
#
# Fit the two-dimensional clustered latent social space model 
#
efit0 <- ergmm(samplike ~ euclidean(d=2))
bic.ergmm(efit0)
summary(efit0)
plot(efit0, pie=TRUE,labels=TRUE)

efit2 <- ergmm(samplike ~ euclidean(d=2, G=2))
bic.ergmm(efit2)
summary(efit2)
plot(efit2, pie=TRUE,labels=TRUE)
#

# The ngroups parameter fits 3 groups
#
efit3 <- ergmm(samplike ~ euclidean(d=2, G=3))
#
# Look at the goodness-of-fit of the 4 group model
#
bic.ergmm(efit3)

plot(efit3, pie=TRUE,labels=TRUE)
#
# Let's try the fit of the 4 group model
#
efit4 <- ergmm(samplike ~ euclidean(d=2, G=4))
#
# Look at the goodness-of-fit of the 4 group model
# The BIC (Bayesian Information Criterion) is a form
# of (negative) deviance penalized for the complexity of the model
# (as measured by the number of dimensions and the number of groups).
#
bic.ergmm(efit4)
plot(efit4, pie=TRUE,labels=TRUE)
#
# The better model has the higher BIC, so we will go with the 
# 3 group model
#
# First save the results for later
#
save(efit3,file="efit3.cluster.RData")
#
# Summarize the fit
#
summary(efit3)
#
# Print out the probabilities of group membership 
# for each monk
#
# First for the 4 group model
#
#round(attr(efit4$sample,"Q"),2)
round(efit3$mkl$mbc$Z.pK,2)
#
# Now for the 3 group model
#
#round(attr(efit3$sample,"Q"),2)
round(efit3$mkl$mbc$Z.pK,2)
#
# Plot some diagnostics
#
mcmc.diagnostics(efit3)
#
# Plot the best fitting positions
#
plot(samplike,displaylabels=TRUE,
	    arrowhead.cex = 0.1, label.cex=0.5, pad=0.1,
	    coord=efit3$mkl$Z,
	    main="Best Friends in a monastery over Time")
#
# Plot the best fitting positions with White et. al 1976 groups
#
plot(samplike, vertex.col="group", displaylabels=TRUE,
  arrowhead.cex = 0.1, label.cex=0.5, pad=0.1,
  coord=efit3$mkl$Z)
title(cex.main=0.5,
  main="Best Friends in a monastery over Time\n White et al's groups marked")
#
# Add a legend
#
legend("topleft", bty="n",
       xjust=1,
       legend=c("Turks","Outcasts","Loyal"),col=1:3,pch=19)
#
aaa <- plot(samplike, vertex.col=efit3$mkl$Z.K, displaylabels=TRUE,
  arrowhead.cex = 0.1, label.cex=0.5, pad=0.1,
  coord=efit3$mkl$Z)
title(cex.main=0.5,
  main="Best Friends in a monastery over Time\n Bayesian Posterior Clusters marked")
legend("topleft", bty="n",
       xjust=1,
       legend=c("1","2","3"),col=1:3,pch=19)



#
# Plot the 3 group and 4 group model
#
groupToLetter <- get.vertex.attribute(samplike,"group")
groupToLetter[groupToLetter=="Turks"] <- "T"
groupToLetter[groupToLetter=="Outcasts"] <- "O"
groupToLetter[groupToLetter=="Loyal"] <- "L"
groupToLetter
plot(efit4,label=groupToLetter)
title(sub="Color represents the estimated groups; Labels the White et. al groups")
#
plot(efit3,label=groupToLetter)
title(sub="Color represents the estimated groups; Labels the White et. al groups")
plot(efit3,density=c(2,2))

par(mfrow=c(2,2))
#
# Plot 24 versions of the positions in latent space to
# see how uncertain they are
#
number.to.plot <- 24
ones.to.plot <- sample.int(dim(efit3$sample$Z)[1],number.to.plot,replace=F)
for(i in ones.to.plot){
 tmp <- plot(samplike, vertex.col=efit3$sample$Z.K[i,], label="",
  arrowhead.cex = 0.1, vertex.cex=2,
  coord=efit3$sample$Z[i,,],
  main=paste("Sample number",i))
}
## Same locations
#for(i in ones.to.plot){
# tmp <- plot(samplike, vertex.col=efit3$sample$Z.k[i,], label="",
#  arrowhead.cex = 0.1, vertex.cex=2,
#  coord=efit3$sample$Z[i,,],
#  main=paste("Sample number",i))
#}
