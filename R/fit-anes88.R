# clear workspace
rm(list = ls())

# load packages
library(foreign) # to load a Stata data set
library(arm)     # misc functions
library(compactr)  # graphics

# set working directory
setwd("~/Dropbox/projects/unreliable-inferences")

# load functions to estimate partial-observability models
source("R/fn_pobs.R")

# load and clean data
d <- read.dta("data/anes88.dta")[, c("validated_turnout", "self_report_turnout", "post_int_days", "black",
                                           "educ_years", "age", "agesquared")]

d0 <- d[, c("validated_turnout", "self_report_turnout", "post_int_days", "black",
            "educ_years", "age", "agesquared")]
d0 <- na.omit(d0)
d0$misreport <- 1*(d0$validated_turnout != d0$self_report_turnout)
d0$misreport[d0$validated_turnout == 1] <- NA

# rescale variables
self_report_turnout <- d0$self_report_turnout
validated_turnout <- d0$validated_turnout
misreport <- d0$misreport
post_int_days <- rescale(log(d0$post_int_days + 1))
educ_years <- rescale(d0$educ_years)
black <- rescale(d0$black)
age <- rescale(d0$age)
agesquared <- rescale(d0$agesquared)

# fit logit model to self-report turnout data
m.self <- glm(self_report_turnout ~ educ_years + black + age + agesquared, family = "binomial") 
sim.self <- coef(sim(m.self, 1000))
display(m.self)

# fit logit model to validated turnout data
m.valid <- glm(validated_turnout ~ educ_years + black + age + agesquared, family = "binomial") 
sim.valid <- coef(sim(m.valid, 1000))
display(m.valid)

# fit logit model predicting misreport
m.mis <- glm(misreport ~ educ_years + black + age + agesquared + post_int_days, family = "binomial") 
sim.mis <- coef(sim(m.mis, 1000))
display(m.mis)

# fit partial-observability model to self-report data
y <- self_report_turnout
X <- cbind(1, educ_years, black, age, agesquared)
Z <- cbind(1, educ_years, black, age, agesquared, post_int_days)
# maximize the log-likelihood using 200 different random starting values to
# ensure that the maximum is global. This seems to work better and faster than 
# other algorithms such as genoud
lik <- -9999999
for (z in 1:200) {
  print(z)
  cand <- pobs(y, X, Z)
  if (lik < cand$lik) {
    lik <- cand$lik
    m.part <- cand
  }
}
sim.part <- mvrnorm(1000, m.part$beta, m.part$cov[1:length(m.part$beta), 1:length(m.part$beta)])

var.names <- c("Intercept", "Education\n(in years)", "Black", "Age", "Age Squared", "Days Since\nElection")

###################
# Coefficient Plots
###################

pdf("doc/figs/coef_plot.pdf", height = 3, width = 8.5, family = "serif")
par(mfrow = c(1,2), mar = rep(.75, 4), oma = c(2,4.5,1,0),
    family = "serif")
## Main Equation
eplot(xlim = c(-6, 8), ylim = c(6.5, .5), yat = 1:6, yticklab = var.names,
      xlab = "Logit Estimates", main = "Turnout")
abline(v = 0, lty = 3)
# validated
est <- coef(m.valid)
se <- sqrt(diag(vcov(m.valid)))
deduct <- .2
for (cf in 1:5) {
  points(est[cf], cf - deduct, pch = 19, cex = .5)
  lines(c(est[cf] + 1.64*se[cf], est[cf] - 1.64*se[cf]), c(cf - deduct, cf - deduct))
}
# self report
est <- coef(m.self)
se <- sqrt(diag(vcov(m.self)))
deduct <- -.2
for (cf in 1:5) {
  points(est[cf], cf - deduct, pch = 19, cex = .5, col = "red")
  lines(c(est[cf] + 1.64*se[cf], est[cf] - 1.64*se[cf]), c(cf - deduct, cf - deduct), col = "red")
}
# partial observability
est <- m.part$beta
se <- m.part$se.beta
deduct <- 0
for (cf in 1:5) {
  points(est[cf], cf - deduct, pch = 19, cex = .5, col = "blue")
  lines(c(est[cf] + 1.64*se[cf], est[cf] - 1.64*se[cf]), c(cf - deduct, cf - deduct), col = "blue")
}

legend(x = par("usr")[2] - .1, y = par("usr")[3] - .1, xjust = 1, yjust = 0,
       pch = 19, cex = .6, pt.cex = .5, lty = 1, col = c("black", "blue", "red"), 
       legend = c("Full Observability", "Partial Observability", "Naive Model"), box.lwd = -1)

## Nuisance Equation
eplot(xlim = c(-6, 8), ylim = c(6.5, .5), yat = 1:5, yticklab = var.names,
      xlab = "Logit Estimates", main = "Misreport")
abline(v = 0, lty = 3)
# validated
est <- coef(m.mis)
se <- sqrt(diag(vcov(m.mis)))
deduct <- .2
for (cf in 1:6) {
  points(est[cf], cf - deduct, pch = 19, cex = .5)
  lines(c(est[cf] + 1.64*se[cf], est[cf] - 1.64*se[cf]), c(cf - deduct, cf - deduct))
}

# partial observability
est <- m.part$gamma
se <- m.part$se.gamma
deduct <- 0
for (cf in 1:6) {
  points(est[cf], cf - deduct, pch = 19, cex = .5, col = "blue")
  lines(c(est[cf] + 1.64*se[cf], est[cf] - 1.64*se[cf]), c(cf - deduct, cf - deduct), col = "blue")
}

legend(x = par("usr")[2] - .1, y = par("usr")[3] - .1, xjust = 1, yjust = 0,
       pch = 19, cex = .6, pt.cex = .5, lty = 1, col = c("black", "blue"), 
       legend = c("Full Observability", "Partial Observability"), box.lwd = -1)
dev.off()

#########################
# Predicted Probabilities
#########################

x.educ <- seq(min(educ_years), max(educ_years), length.out = 100)
x.black <- median(black)
x.age <- median(age)
x.agesquared <- median(agesquared)

X.pred <- cbind(1, x.educ, x.black, x.age, x.agesquared)

p.self <- plogis(X.pred%*%t(sim.self))
p.valid <- plogis(X.pred%*%t(sim.valid))
p.part <- plogis(X.pred%*%t(sim.part))

me.self <- t(dlogis(X.pred%*%t(sim.self)))*sim.self[, 2]
me.valid <- t(dlogis(X.pred%*%t(sim.valid)))*sim.valid[, 2]
me.part <- t(dlogis(X.pred%*%t(sim.part)))*sim.part[, 2]

q.p.self <- apply(p.self, 1, quantile, c(.05, .5, .95))
q.p.valid <- apply(p.valid, 1, quantile, c(.05, .5, .95))
q.p.part <- apply(p.part, 1, quantile, c(.05, .5, .95))

q.me.self <- apply(me.self, 2, quantile, c(.05, .5, .95))
q.me.valid <- apply(me.valid, 2, quantile, c(.05, .5, .95))
q.me.part <- apply(me.part, 2, quantile, c(.05, .5, .95))

pdf("doc/figs/pr.pdf", height = 2, width = 6.5, family = "serif")
par(mfrow = c(1, 3), mar = rep(.75, 4), oma = c(2,3,2,.5), family = "serif")
eplot(xlim = mm(x.educ), ylim = c(0, 1),
      xlab = "Years of Education",
      ylab = "Pr(Vote)", ylabpos = 1.75,
      main = "Logit Model\nUsing Validated Data",
      xat = sort(unique(educ_years))[0:8*2 + 2], xticklab = 0:8*2 + 1)
lines(x.educ, q.p.valid[2, ], lwd = 2)
lines(x.educ, q.p.valid[1, ], lty = 3)
lines(x.educ, q.p.valid[3, ], lty = 3)

aplot("Logit Model\nNaively Using Self-Reported Data")
lines(x.educ, q.p.self[2, ], lwd = 2)
lines(x.educ, q.p.self[1, ], lty = 3)
lines(x.educ, q.p.self[3, ], lty = 3)

aplot("Partial Observability Model\nUsing Self-Reported Data")
lines(x.educ, q.p.part[2, ], lwd = 2)
lines(x.educ, q.p.part[1, ], lty = 3)
lines(x.educ, q.p.part[3, ], lty = 3)
dev.off()

##################
# Marginal Effects
##################

pdf("doc/figs/me.pdf", height = 2, width = 6.5, family = "serif")
par(mfrow = c(1, 3), mar = rep(.75, 4), oma = c(2,3,2,.5), family = "serif")
eplot(xlim = mm(x.educ), ylim = c(0, 0.8),
      xlab = "Years of Education",
      ylab = "Marginal Effect of\nEducation on Pr(Vote)", ylabpos = 1.75,
      main = "Logit Model\nUsing Validated Data",
      xat = sort(unique(educ_years))[0:8*2 + 2], xticklab = 0:8*2 + 1)
lines(x.educ, q.me.valid[2, ], lwd = 2)
lines(x.educ, q.me.valid[1, ], lty = 3)
lines(x.educ, q.me.valid[3, ], lty = 3)

aplot("Logit Model\nNaively Using Self-Reported Data")
lines(x.educ, q.me.self[2, ], lwd = 2)
lines(x.educ, q.me.self[1, ], lty = 3)
lines(x.educ, q.me.self[3, ], lty = 3)

aplot("Partial Observability Model\nUsing Self-Reported Data")
lines(x.educ, q.me.part[2, ], lwd = 2)
lines(x.educ, q.me.part[1, ], lty = 3)
lines(x.educ, q.me.part[3, ], lty = 3)
dev.off()

#######################
# First-Difference Plot
#######################

fd.self <- p.self[94, ] - p.self[71, ]
fd.valid <- p.valid[94, ] - p.valid[71, ]
fd.part <- p.part[94, ] - p.part[71, ]

q.fd.self <- quantile(fd.self, c(.05, .5, .95))
q.fd.valid <- quantile(fd.valid, c(.05, .5, .95))
q.fd.part <- quantile(fd.part, c(.05, .5, .95))
q.fd.valid
q.fd.self
q.fd.part

pdf("doc/figs/fd.pdf", height = 3, width = 5, family = "serif")
par(mfrow = c(1,1), mar = c(0,0,0,0), oma = c(4.5, 9.5, .5, .5), family = "serif")
eplot(xlim = c(0, .5), ylim = c(.5, 3.5), 
      yat = 1:3, yticklab = c("Partial Observability Logit\nUsing Self-Reported Data",
                              "Naive Logit Using\nSelf-Reported Data",
                              "Logit Using Validated Data"),
      xlab = "The Effect of Obtaining a College Degree\non the Probability of Voting",
      xlabpos = 3)
# pobs
lines(q.fd.part[c(1, 3)], c(1,1))
points(q.fd.part[2], 1, pch = 19)
# naive
lines(q.fd.self[c(1, 3)], c(2,2))
points(q.fd.self[2], 2, pch = 19)
# validated
lines(q.fd.valid[c(1, 3)], c(3,3))
points(q.fd.valid[2], 3, pch = 19)
dev.off()