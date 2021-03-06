# install.packages("vegan")

library(ergm)
library(latentnet)
#
# Use 'data(package = "latentnet")' to list the data sets in a
#
data(package="latentnet")
#
# Read in the set of ties from Sampson monastery data 
# These are the cumulative ties over three periods.
# A tie from monk A to monk B exists if A nominated 
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
# pdf(file="samplike.cohesion.pdf")
#
plot(samplike,displaylabels=T,label.cex=0.5,pad=0.1,main="Plot of the best Friends in a monastery over Time")



# (a) ----
# Fit the two-dimensional latent social space model 
#
#
# Using Sampson's Monk data, lets fit a 
# simple latent position model
#
samp.fit <- ergmm(samplike ~ euclidean(d=2))
#
# Summarize the fit
#
summary(samp.fit)
#
# See if we have convergence in the MCMC
#
# Plot some diagnostics
#
mcmc.diagnostics(samp.fit)

#
# Plot the best fitting positions
#
aaa <- plot(samplike, displaylabels=T,
  label.cex=0.5, pad=0.1,
  coord=samp.fit$mkl$Z,
  main="Best Friends in a monastery over Time")
#
# Plot the best fitting positions with Breiger's groups
#
aaa <- plot(samplike, vertex.col="group", displaylabels=TRUE,
  label.cex=0.5, pad=0.1,
  coord=samp.fit$mkl$Z,
  main="Best Friends in a monastery over Time\n Breiger's groups marked")
#
# Add a legend
#
legend("topleft",bty="n",
       legend=c("Turks","outcasts","loyal"),text.col=1:3)

# plot(samp.fit, pie=TRUE,labels=TRUE) # meaningless for G = 1 fit
# (c) ----
# Plot 12 versions of the positions in latent space to
# see how uncertain they are
#
library(vegan)
par(mfrow=c(2,2),mar=c(0,0,2,0))
Z.mkl <- samp.fit$mkl$Z
#Z.mkl <- samp.fit$sample$Z[1,,]
for(i in 1:12){
 isamp <- 1+ (i-1)*round(dim(samp.fit$sample$Z)[1]/12)
 sim <- procrustes(Z.mkl,samp.fit$sample$Z[isamp,,])$Yrot
 aaa <- plot(samplike, vertex.col="group",
  label.cex=5, pad=0.2,
  coord=sim,
  main=paste(isamp))
}
