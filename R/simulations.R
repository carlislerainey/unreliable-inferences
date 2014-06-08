# #set.seed(7453271)
# rm(list = ls())
# 
# library(compactr)
# 
# f.main <- function(b, x1) {
#   b[1] + b[2]*x1 + b[3]*x1^2
# }
# 
# f.nuisance <- function(g, x1, x2) {
#   p.nuisance <- g[1] + g[2]*x1 + g[3]*x2 + g[4]*x1*x2 + g[5]*x1^2 + g[6]*x2^2
#   return(p.nuisance)
# }
# 
# check.main <- function(b) {
#   # check1: monotonicity
#   check1 <- sign(b[2]) == sign(b[2] + 2*b[3])
#   # check2: p > 0, check3: p < 1
#   p <- NULL
#   i <- 0
#   for (x in 0:1) {
#     for (z in 0:1) {
#       i <- i + 1
#       p[i] <- b[1] + b[2]*x + b[3]*x^2
#     }
#   }
#   check2 <- min(p) > 0
#   check3 <- max(p) < 1  
#   check <- sum(c(check1, check2, check3)) == 3
# }
# 
# check.nuisance <- function(g) {
#   # check1: montonicity of x
#   init <- sign(g[2] + g[4]*0 + 2*g[5]*0)
#   c1 <- sign(g[2] + g[4]*1 + 2*g[5]*0) == init
#   c2 <- sign(g[2] + g[4]*0 + 2*g[5]*1) == init
#   c3 <- sign(g[2] + g[4]*1 + 2*g[5]*1) == init
#   check1  <- sum(c(c1, c2, c3)) == 3
#   # check2: montonicity of z
#   init <- sign(g[3] + g[4]*0 + 2*g[6]*0)
#   c1 <- sign(g[3] + g[4]*1 + 2*g[6]*0) == init
#   c2 <- sign(g[3] + g[4]*0 + 2*g[6]*1) == init
#   c3 <- sign(g[3] + g[4]*1 + 2*g[6]*1) == init
#   check2  <- sum(c(c1, c2, c3)) == 3                 
#   # check3: p > 0, check4: p < 1
#   p <- NULL
#   i <- 0
#   for (x in 0:1) {
#     for (z in 0:1) {
#       i <- i + 1
#       p[i] <- g[1] + g[2]*x + g[3]*z + g[4]*x*z + g[5]*x^2 + g[6]*z^2
#     }
#   }
#   check3 <- min(p) > 0
#   check4 <- max(p) < 1
#   # check: pass all checks
#   check <- sum(c(check1, check2, check3, check4)) == 4
# }
# 
# pobs <- function(y, X, Z, prior.scale = 2.5, ...) {
#   require(rgenoud)
#   n.beta <- ncol(X)
#   n.gamma <- ncol(Z)
#   beta <- rnorm(n.beta, 0, 1) #numeric(ncol(X))
#   gamma <- rnorm(n.gamma, 0, 1) #numeric(ncol(Z))
#   param <- c(beta, gamma)
#   fit <- optim(fn = ll__, par = param,
#                gr = gr__, 
#                y = y, X = X, Z = Z, prior.scale = prior.scale,
#                method = "BFGS",
#                #hessian = TRUE,
#                control = list("maxit" = 1000,
#                               "reltol" = .Machine$double.eps,
#                               "fnscale" = -1),
#                ...)
#   beta <- fit$par[1:n.beta]
#   gamma <- fit$par[(n.beta + 1):(n.beta + n.gamma)]
#   #cov <- solve(-fit$hessian)
#   #se <- sqrt(diag(cov))
#   #se.beta <- se[1:n.beta]
#   #se.gamma <- se[(n.beta + 1):(n.beta + n.gamma)]
#   lik <- fit$value
#   res <- list(beta = beta,
#               gamma = gamma, 
#               #cov = cov, 
#               #se.beta = se.beta,
#               #se.gamma = se.gamma,
#               lik = lik)
# }
# 
# setwd("~/Dropbox/projects/unreliable-inferences")
# d <- read.csv("output/simulations.csv")
# start <- as.numeric(rownames(d)[nrow(d)]) + 1
# for (new.sims in start:500) {
# 
# n.sims <- 1
# n <- 100000
# fd.true <- fd.full <- fd.part <- NULL
# prob.true <- prob.full <- prob.part <- NULL
# 
# for (sim in 1:n.sims) {
#   cat(paste("Simulation ", new.sims, "\n", sep = ""))
#   # generate g
#   check <- F
#   iter <- 0
#   bds <- c(1, 1, 1, 1, 1, 1)
#   while(!check) {
#     iter <- iter + 1
#     g <- runif(6, -bds, bds)
#     check <- check.nuisance(g)
#     #print(iter)
#   }
#   # generate b
#   check <- F
#   iter <- 0
#   bds <- c(1, 1, 1)
#   while(!check) {
#     iter <- iter + 1
#     b <- runif(3, -bds, bds)
#     check <- check.main(b)
#     #print(iter)
#   }
#   
#   # generate data
#   x0 <- seq(0, 1, length.out = 1000) # used to calculate predictions
#   x1 <- runif(n)
#   x2 <- runif(n)
#   p.main <- f.main(b, x1)
#   p.nuisance <- f.nuisance(g, x1, x2)
#   d.main <- rbinom(n, 1, p.main)
#   d.nuisance <- rbinom(n, 1, p.nuisance)
#   y <- 1*(d.main == 1 | d.nuisance == 1)
#   
#   # plot dgp
#   y1 <- f.nuisance(g, 0, x0)
#   y2 <- f.nuisance(g, .5, x0)
#   y3 <- f.nuisance(g, 1, x0)
#   par(mfrow = c(1,2))
#   eplot(xlim = c(0,1), ylim = c(0, 1))
#   lines(x0, y1, col = "grey50", lwd = 3)
#   lines(x0, y2, col = "grey20", lwd = 3)
#   lines(x0, y3, col = "black", lwd = 3)   
#   aplot()
#   pred <- f.main(b, x0)
#   lines(x0, pred, lwd = 3)
#   fd.true[sim] <- pred[750] - pred[250]
#   prob.true[sim] <- pred[500]
#   
#   # full observability model
#   m.full <- glm(d.main ~ x1, family = binomial)
#   est <- coef(m.full)
#   pred <- plogis(est[1] + x0*est[2])
#   lines(x0, pred, lty = 3, lwd = 3, col = "red")
#   fd.full[sim] <- pred[750] - pred[250]
#   prob.full[sim] <- pred[500]
#   
#   X <- cbind(1, x1)
#   Z <- cbind(1, x1, x2)
#   #n.param <- ncol(X) + ncol(Z)
#   #pop.size <- 1000
# #   cands <- matrix(runif(pop.size*n.param, -10, 10), nrow = pop.size)
# #   for (z in 1:20) {
# #     print(z)
# #     m <- pobs(y, X, Z, prior.scale = 2.5)
# #     cands[z, ] <- c(m$beta, m$gamma)
# #   }
#   
#   # partial observability model
#   n.runs <- 10
#   run.store <- matrix(NA, nrow = n.runs, ncol = 8)
#   for (run in 1:n.runs) {
#     m.part <- pobs(y, X, Z)
#     est <- m.part$beta + runif(2, -.05, .05)
#     pred <- plogis(est[1] + x0*est[2])
#     lines(x0, pred, col = "blue", lty = 2, lwd = 2)
#     fd <- pred[750] - pred[250]
#     prob <- pred[500]
#     lik <- m.part$lik
#     cat(paste("\t\tIteration ", run, ":\t", fd, ",\t", lik, "\n", sep = ""))
#     run.store[run, ] <- c(round(m.part$lik, 4), m.part$beta, m.part$gamma, fd, prob)
#   }
#   fd.part[sim] <- matrix(run.store[run.store[, 1] == max(run.store[, 1]), ], ncol = 8)[1, 7]
#   prob.part[sim] <- matrix(run.store[run.store[, 1] == max(run.store[, 1]), ], ncol = 8)[1, 8]
#   
# }
# 
# 
# setwd("~/Dropbox/unreliable-inferences")
# d <- read.csv("ouput/simulations.csv")
# prev.row.name <- as.numeric(rownames(d)[nrow(d)])
# res <- cbind(new.sims, fd.true, fd.full, fd.part, prob.true, prob.full, prob.part)
# write.table(res, file = "output/simulations.csv", 
#             append = TRUE, col.names = FALSE, 
#             sep = ",", row.names = as.character(prev.row.name + 1:n.sims))
# 
# }
