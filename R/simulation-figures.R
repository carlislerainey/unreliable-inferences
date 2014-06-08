library(compactr)
library(mgcv)

rm(list = ls())
setwd("~/Dropbox/projects/unreliable-inferences")
d <- read.csv("output/simulations.csv")
attach(d)
x0 <- seq(-.5, .5, length.out = 1000)
sign.error <- 1*(sign(fd.part) != sign(fd.true))

m.sign.error <- gam(sign.error ~ s(fd.true), family = "binomial")

X <- as.matrix(x0, ncol = 1)
colnames(X) <- "fd.true"
X <- data.frame(X)
p <- predict(m.sign.error, newdata = data.frame(X), type = "response")


## Histograms of Predicted Probabilities
pdf("doc/figs/sims_pr.pdf", height = 3, width = 7, family = "serif")
par(mfrow = c(1, 2), mar = c(1,1,1,1), oma = c(3,2,1,.5), 
    xaxs = "i",yaxs = "i", family = "serif")
breaks <- seq(min(prob.part - prob.true), max(prob.true - prob.part),
              length.out = 50)
h1 <- hist(prob.full - prob.true, plot = FALSE, breaks = breaks)
h2 <- hist(prob.part - prob.true, plot = FALSE, breaks = breaks)
eplot(xlim = mm(c(h1$breaks, h2$breaks)),
      ylim = c(0, 1.15*max(c(h1$density, h2$density))),
      main = "Full Observability Model",
      xlab = "Bias\n(Estimated Probability - True Probability)", xlabpos = 2.3,
      ylab = "Count")
plot(h1, add = TRUE, freq = FALSE, col = "grey70", border = NA)
lines(density(prob.full - prob.true, adjust = 1), lwd = 2)
abline(v = 0, lty = 3)
abline(h = 0)
aplot("Partial Observability Model")
plot(h2, add = TRUE, freq = FALSE, col = "grey70", border = NA)
lines(density(prob.part - prob.true, adjust = 1), lwd = 2)
abline(v = 0, lty = 3)
abline(h = 0)
quantile(prob.full - prob.true, c(.05, .5, .95))
quantile(prob.part - prob.true, c(.05, .5, .95))
dev.off()

## Histograms of Effects
pdf("doc/figs/sims_fd.pdf", height = 3, width = 7, family = "serif")
par(mfrow = c(1, 2), mar = c(1,1,1,1), oma = c(3,2,1,.5), 
    xaxs = "i",yaxs = "i", family = "serif")
breaks <- seq(min(c(fd.part - fd.true, fd.full - fd.true)), 
              max(c(fd.part - fd.true, fd.full - fd.true)),
              length.out = 100)
h1 <- hist(fd.full - fd.true, plot = FALSE, breaks = breaks)
h2 <- hist(fd.part - fd.true, plot = FALSE, breaks = breaks)
eplot(xlim = mm(c(h1$breaks, h2$breaks)),
      ylim = c(0, 1.2*max(c(h1$density, h2$density))),
      main = "Full Observability Model",
      xlab = "Bias\n(Estimated Effect - True Effect)", xlabpos = 2.3,
      ylab = "Count")
plot(h1, add = TRUE, freq = FALSE, col = "grey70", border = NA)
lines(density(fd.full - fd.true, adjust = 1.5), lwd = 2)
abline(v = 0, lty = 3)
abline(h = 0)
aplot("Partial Observability Model")
plot(h2, add = TRUE, freq = FALSE, col = "grey70", border = NA)
lines(density(fd.part - fd.true, adjust = 1.5), lwd = 2)
abline(v = 0, lty = 3)
abline(h = 0)
quantile(fd.full - fd.true, c(.05, .5, .95))
quantile(fd.part - fd.true, c(.05, .5, .95))
dev.off()

# Scatterplots Comparing True to Estimated Probabilities
pdf("doc/figs/sims_pr_comp.pdf", height = 3, width = 7, family = "serif")
par(mfrow = c(1, 2), mar = c(1,1,1,1), oma = c(3,3,1,.5), 
    xaxs = "i",yaxs = "i", family = "serif")
eplot(xlim = c(0, 1), ylim = c(0, 1), 
      xlab = "True Probability", 
      ylab = "Estimated Probability", ylabpos = 2,
      main = "Full Observability")
points(prob.true, prob.full)
abline(a = 0, b = 1)
aplot("Partial Observability")
points(prob.true, prob.part)
abline(a = 0, b = 1)
abline(h = 0, lty = 3)
abline(v = 0, lty = 3)
dev.off()

# Scatterplots Comparing True to Estimated Probabilities
pdf("doc/figs/sims_me_comp.pdf", height = 3, width = 7, family = "serif")
par(mfrow = c(1, 2), mar = c(1,1,1,1), oma = c(3,3,1,.5), 
    xaxs = "i",yaxs = "i", family = "serif")
eplot(xlim = c(-1, 1), ylim = c(-1, 1), 
      xlab = "True Effect", 
      ylab = "Estimated Effect", ylabpos = 2,
      main = "Full Observability")
points(fd.true, fd.full)
abline(a = 0, b = 1)
aplot("Partial Observability")
points(fd.true, fd.part)
abline(a = 0, b = 1)
abline(h = 0, lty = 3)
abline(v = 0, lty = 3)
dev.off()

detach(d)