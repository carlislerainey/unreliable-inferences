
# seet seed
set.seed(7453271)

# clear workspace
rm(list = ls())

# load packages
library(ggplot2)

# create misc. functions
f.main <- function(b, x1) {
  b[1] + b[2]*x1 + b[3]*x1^2
}

f.nuisance <- function(g, x1, x2) {
  p.nuisance <- g[1] + g[2]*x1 + g[3]*x2 + g[4]*x1*x2 + g[5]*x1^2 + g[6]*x2^2
  return(p.nuisance)
}

check.main <- function(b) {
  # check1: monotonicity
  check1 <- sign(b[2]) == sign(b[2] + 2*b[3])
  # check2: p > 0, check3: p < 1
  p <- NULL
  i <- 0
  for (x in 0:1) {
    for (z in 0:1) {
      i <- i + 1
      p[i] <- b[1] + b[2]*x + b[3]*x^2
    }
  }
  check2 <- min(p) > 0
  check3 <- max(p) < 1  
  check <- sum(c(check1, check2, check3)) == 3
}

check.nuisance <- function(g) {
  # check1: montonicity of x
  init <- sign(g[2] + g[4]*0 + 2*g[5]*0)
  c1 <- sign(g[2] + g[4]*1 + 2*g[5]*0) == init
  c2 <- sign(g[2] + g[4]*0 + 2*g[5]*1) == init
  c3 <- sign(g[2] + g[4]*1 + 2*g[5]*1) == init
  check1  <- sum(c(c1, c2, c3)) == 3
  # check2: montonicity of z
  init <- sign(g[3] + g[4]*0 + 2*g[6]*0)
  c1 <- sign(g[3] + g[4]*1 + 2*g[6]*0) == init
  c2 <- sign(g[3] + g[4]*0 + 2*g[6]*1) == init
  c3 <- sign(g[3] + g[4]*1 + 2*g[6]*1) == init
  check2  <- sum(c(c1, c2, c3)) == 3                 
  # check3: p > 0, check4: p < 1
  p <- NULL
  i <- 0
  for (x in 0:1) {
    for (z in 0:1) {
      i <- i + 1
      p[i] <- g[1] + g[2]*x + g[3]*z + g[4]*x*z + g[5]*x^2 + g[6]*z^2
    }
  }
  check3 <- min(p) > 0
  check4 <- max(p) < 1
  # check: pass all checks
  check <- sum(c(check1, check2, check3, check4)) == 4
}

# log-likelihood function
ll__ <- function(param, y, X, Z, n_trials) {
  n_beta <- ncol(X)
  n_gamma <- ncol(Z)
  beta <- param[1:n_beta]
  gamma <- param[(n_beta + 1):(n_beta + n_gamma)]
  p_main <- plogis(X%*%beta)
  p_nuisance <- plogis(Z%*%gamma)
  p_obs <- p_main*p_nuisance
  ll <- sum(y*(log(p_main) + log(p_nuisance)) + (n_trials - y)*log(1 - p_obs))
  return(ll)
}

pobs <- function(y, X, Z, n_trials, n_tries = 10, m1, m2, ...) {
  tries <- 0
  tried_coef <- 0
  n_beta <- ncol(X)
  n_gamma <- ncol(Z)
  max_lik <- -Inf
  while (tries < n_tries) {
    #print(tries)
    if (tried_coef == 1) {
      beta_start <- rnorm(ncol(X), sd = 1)
      gamma_start <- rnorm(ncol(Z), sd = 1)      
    }
    if (tried_coef == 0) {
      beta_start <- coef(m1)
      gamma_start <- coef(m2)
      tried_coef <- 1
    }
    param <- c(beta_start, gamma_start)
    fit <- try(optim(fn = ll__, par = param,
                     y = y, X = X, Z = Z, n_trials = n_trials, 
                     method = "BFGS",
                     hessian = TRUE,
                     control = list("maxit" = 10000,
                                    "reltol" = .Machine$double.eps,
                                    "fnscale" = -1),
                     ...))
    #cat(round(fit$value))
    if (class(fit) != "try-error") {
      tries <- tries + 1
      if (max_lik < fit$value) {
        beta <- fit$par[1:n_beta]
        gamma <- fit$par[(n_beta + 1):(n_beta + n_gamma)]
        max_lik <- fit$value
      }
    }
    } 
  res <- list(beta = beta,
              gamma = gamma)
  return(res)
}


n.sims <- 2000
fd.true <- fd.z.true <- fd.full <- fd.part <- numeric(n.sims)

for (sim in 1:n.sims) {
  if(sim %% 10==0) cat(paste("Simulation ", sim, "\n", sep = ""))
  # generate g
  check <- F
  iter <- 0
  bds <- c(1, 1, 1, 1, 1, 1)
  while(!check) {
    iter <- iter + 1
    g <- runif(6, -bds, bds)
    check <- check.nuisance(g)
    #print(iter)
  }
  # generate b
  check <- F
  iter <- 0
  bds <- c(1, 1, 1)
  while(!check) {
    iter <- iter + 1
    b <- runif(3, -bds, bds)
    check <- check.main(b)
    #print(iter)
  }
  
  # generate data
  n_trials <- 100000000
  x1 <- seq(0, 1, length.out = 5)
  x2 <- seq(0, 1, length.out = 5)
  df <- expand.grid(x1 = x1, x2 = x2)
  p.main <- f.main(b, df$x1)
  p.nuisance <- f.nuisance(g, df$x1, df$x2)
  d.main <- rbinom(nrow(df), n_trials, p.main)
  d.nuisance <- rbinom(nrow(df), n_trials, p.nuisance)
  y <- rbinom(nrow(df), n_trials, p.nuisance*p.main)
  
  # full observability model
  m.full <- glm(cbind(d.main, n_trials - d.main) ~ df$x1, family = binomial)
  est <- coef(m.full)
  fd.full[sim] <- plogis(est[1] + est[2]) - plogis(est[1])
  m2 <- glm(cbind(d.nuisance, n_trials - d.nuisance) ~ df$x1 + df$x2, family = binomial)
  
  # partial observability model
  X <- cbind(1, df$x1)
  Z <- cbind(1, df$x1, df$x2)
  
  m.part <- pobs(y, X, Z, n_trials = n_trials, m1 = m.full, m2 = m2)
  fd.part[sim] <- plogis(m.part$beta[1] + m.part$beta[2]) - plogis(m.part$beta[1])
  
  fd.true[sim] <- f.main(b, 1) - f.main(b, 0)
  fd.z.true[sim] <- mean(f.nuisance(g, x1, 1) - f.nuisance(g, x1, 0))
}

sim_df1 <- data.frame(obs = "full observability", w1_type = "continuous key explanatory variable", est = fd.full, true = fd.true, fd_z = fd.z.true)
sim_df2 <- data.frame(obs = "partial observability", w1_type = "continuous key explanatory variable", est = fd.part, true = fd.true, fd_z = fd.z.true)
sim_df <- rbind(sim_df1, sim_df2)

fd.true <- fd.z.true <- fd.full <- fd.part <- numeric(n.sims)
for (sim in 1:n.sims) {
  if(sim %% 10==0) cat(paste("Simulation ", sim, "\n", sep = ""))
  # generate g
  check <- F
  iter <- 0
  bds <- c(1, 1, 1, 1, 1, 1)
  while(!check) {
    iter <- iter + 1
    g <- runif(6, -bds, bds)
    check <- check.nuisance(g)
    #print(iter)
  }
  # generate b
  check <- F
  iter <- 0
  bds <- c(1, 1, 1)
  while(!check) {
    iter <- iter + 1
    b <- runif(3, -bds, bds)
    check <- check.main(b)
    #print(iter)
  }
  
  # generate data
  n_trials <- 100000000
  x1 <- seq(0, 1, length.out = 2)
  x2 <- seq(0, 1, length.out = 5)
  df <- expand.grid(x1 = x1, x2 = x2)
  p.main <- f.main(b, df$x1)
  p.nuisance <- f.nuisance(g, df$x1, df$x2)
  d.main <- rbinom(nrow(df), n_trials, p.main)
  d.nuisance <- rbinom(nrow(df), n_trials, p.nuisance)
  y <- rbinom(nrow(df), n_trials, p.nuisance*p.main)
  
  # full observability model
  m.full <- glm(cbind(d.main, n_trials - d.main) ~ df$x1, family = binomial)
  est <- coef(m.full)
  fd.full[sim] <- plogis(est[1] + est[2]) - plogis(est[1])
  m2 <- glm(cbind(d.nuisance, n_trials - d.nuisance) ~ df$x1 + df$x2, family = binomial)
  
  # partial observability model
  X <- cbind(1, df$x1)
  Z <- cbind(1, df$x1, df$x2)
  
  m.part <- pobs(y, X, Z, n_trials = n_trials, m1 = m.full, m2 = m2)
  fd.part[sim] <- plogis(m.part$beta[1] + m.part$beta[2]) - plogis(m.part$beta[1])
  
  fd.true[sim] <- f.main(b, 1) - f.main(b, 0)
  fd.z.true[sim] <- mean(f.nuisance(g, x1, 1) - f.nuisance(g, x1, 0))
  
}

sim_df1 <- data.frame(obs = "full observability", w1_type = "binary key explanatory variable", est = fd.full, true = fd.true, fd_z = fd.z.true)
sim_df2 <- data.frame(obs = "partial observability", w1_type = "binary key explanatory variable", est = fd.part, true = fd.true, fd_z = fd.z.true)
sim_df <- rbind(sim_df, rbind(sim_df1, sim_df2))

readr::write_csv(sim_df, "output/mon-sims.csv")





