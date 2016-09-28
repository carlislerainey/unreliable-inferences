
# set seed
set.seed(58724320)

# load packages
library(VGAM)
library(dplyr)
library(ggplot2)

# parameters
n_trials <- 100000000
links <- list(logit = logit,
             probit = probit,
             cloglog = cloglog,
             cauchit = cauchit)
prop_binary <- 0.5
n_iter <- 500

continuous <- seq(0, 1, length.out = 5)
binary <- 0:1


# log-likelihood function
ll__ <- function(param, y, X, Z, n_trials) {
  n_beta <- ncol(X)
  n_gamma <- ncol(Z)
  beta <- param[1:n_beta]
  gamma <- param[(n_beta + 1):(n_beta + n_gamma)]
  p_main <- logit(X%*%beta, inverse = TRUE, bvalue = .Machine$double.eps)
  p_nuisance <- logit(Z%*%gamma, inverse = TRUE, bvalue = .Machine$double.eps)
  p_obs <- p_main*p_nuisance
  ll <- sum(y*(log(p_main) + log(p_nuisance)) + (n_trials - y)*log(1 - p_obs))
  return(ll)
}

# maximizer
pobs <- function(y, X, Z, n_trials, n_tries = 1, m1, m2, ...) {
  tries <- 0
  tried_coef <- 0
  n_beta <- ncol(X)
  n_gamma <- ncol(Z)
  max_lik <- -Inf
  while (tries < n_tries) {
    print(tries)
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
              gamma = gamma,
              max_lik = max_lik)
  return(res)
}

# simulations
df <- NULL
for (iter in 1:n_iter) {
  n_x <- rpois(1, lambda = 1.5)
  n_z <- rpois(1, lambda = 1.5)
  n_w <- rpois(1, lambda = .5) + 1
  if (n_x == 0 & n_z == 0) {
    n_z <- 1
  }
  x_names <- paste("x", 1:n_x, sep = "")
  if (n_x == 0) {  x_names <- NULL  }
  z_names <- paste("z", 1:n_z, sep = "")
  if (n_z == 0) {  z_names <- NULL  }
  w_names <- paste("w", 1:n_w, sep = "")
  X_list <- list()
  if (n_x > 0) {
    for (i in 1:n_x) {
      draw <- runif(1)
      if(draw <= prop_binary) {
        X_list[[i]] <- binary
      }
      if(draw > prop_binary) {
        X_list[[i]] <- continuous
      }
    }
  }

  Z_list <- list()
  if (n_z > 0) {
    for (i in 1:n_z) {
      draw <- runif(1)
      if(draw <= prop_binary) {
        Z_list[[i]] <- binary
      }
      if(draw > prop_binary) {
        Z_list[[i]] <- continuous
      }
    }
  }
  w1_type <- "wut?"
  W_list <- list()
  for (i in 1:n_w) {
    draw <- runif(1)
    if(draw <= prop_binary) {
      W_list[[i]] <- binary
      if (i == 1) {
        w1_type <- "binary"
      }
    }
    if(draw > prop_binary) {
      W_list[[i]] <- continuous
      if (i == 1) {
        w1_type <- "continuous"
      }
    }
  }
  WXZ <- expand.grid(c(W_list, X_list, Z_list))
  colnames(WXZ) <- c(w_names, x_names, z_names)
  
  WX <- as.matrix(cbind(1, select(WXZ, starts_with("w")), select(WXZ, starts_with("x"))))
  WZ <- as.matrix(cbind(1, select(WXZ, starts_with("w")), select(WXZ, starts_with("z"))))
  colnames(WX)[1] <- colnames(WZ)[1] <- "Intercept"
  
  beta__ <- rnorm(ncol(WX), 0, 1)
  beta__[1] <- rnorm(1, 0, 2)
  gamma__ <- rnorm(ncol(WZ), 0, 1)
  gamma__[1] <- rnorm(1, 0, 2)
  p1__ <- plogis(WX%*%beta__)
  p2__ <- plogis(WZ%*%gamma__)
  y1__ <- rbinom(length(p1__), n_trials, p1__)
  y2__ <- rbinom(length(p2__), n_trials, p2__)
  
  for (link in 1:length(links)) {
    cat("working on iteration", iter, "of", names(links)[link], "...\n")
    
    if (names(links)[link] == "logit") {
      beta <- beta__
      gamma <- gamma__
    }
    if (names(links)[link] == "probit") {
      beta <- coef(glm(cbind(y1__, n_trials - y1__) ~ WX - 1, data = df, family = binomial(link = "probit")))
      gamma <- coef(glm(cbind(y2__, n_trials - y2__) ~ WZ - 1, data = df, family = binomial(link = "probit")))
    }
    if (names(links)[link] == "cloglog") {
      beta <- coef(glm(cbind(y1__, n_trials - y1__) ~ WX - 1, data = df, family = binomial(link = "cloglog")))
      gamma <- coef(glm(cbind(y2__, n_trials - y2__) ~ WZ - 1, data = df, family = binomial(link = "cloglog")))
    }
    if (names(links)[link] == "cauchit") {
      beta <- coef(glm(cbind(y1__, n_trials - y1__) ~ WX - 1, data = df, family = binomial(link = "cauchit")))
      gamma <- coef(glm(cbind(y2__, n_trials - y2__) ~ WZ - 1, data = df, family = binomial(link = "cauchit")))
    }
    
    p1 <- links[[link]](WX%*%beta, inverse = TRUE) 
    p2 <- links[[link]](WZ%*%gamma, inverse = TRUE) 
    
    lambda1 <- rbinom(length(p1), n_trials, p1)
    lambda2 <- rbinom(length(p2), n_trials, p2)
    y <- rbinom(length(p1*p2), n_trials, p1*p2)
    
    m1 <- glm(cbind(lambda1, n_trials - lambda1) ~ WX - 1, family = binomial)
    m2 <- glm(cbind(lambda2, n_trials - lambda2) ~ WZ - 1, family = binomial)
    m <- pobs(y = y, X = WX, Z = WZ, m1 = m1, m2 = m2, n_trials = n_trials, n_tries = 10)
    
    WX_pred <- matrix(apply(WX, 2, median), nrow = 2, ncol = ncol(WX), byrow = TRUE)
    WX_pred[1, 2] <- 0; WX_pred[2, 2] <- 1
    
    WZ_pred <- matrix(apply(WZ, 2, median), nrow = 2, ncol = ncol(WZ), byrow = TRUE)
    WZ_pred[1, 2] <- 0; WZ_pred[2, 2] <- 1
    
    pr1_full <- plogis(WX_pred%*%coef(m1))
    pr2_full <- plogis(WZ_pred%*%coef(m2))
    pr1_part <- plogis(WX_pred%*%m$beta)
    pr2_part <- plogis(WZ_pred%*%m$gamma)
    pr1_true <- links[[link]](WX_pred%*%beta, inverse = TRUE)
    pr2_true <- links[[link]](WZ_pred%*%gamma, inverse = TRUE)
    fd1_full <- pr1_full[2, ] - pr1_full[1, ]
    fd2_full <- pr2_full[2, ] - pr2_full[1, ]
    fd1_part <- pr1_part[2, ] - pr1_part[1, ]
    fd2_part <- pr2_part[2, ] - pr2_part[1, ]
    fd1_true <- pr1_true[2, ] - pr1_true[1, ]
    fd2_true <- pr2_true[2, ] - pr2_true[1, ]
    
    df1 <- data.frame(obs = "full observability", 
                      link = names(links)[link],
                      w1_type = w1_type,
                      n_w = n_w,
                      n_x = n_x, 
                      n_z = n_z,
                      fd1 = fd1_full,
                      fd2 = fd2_full,
                      true1 = fd1_true,
                      true2 = fd2_true, 
                      max_lik = NA)
    df2 <- data.frame(obs = "partial observability", 
                      link = names(links)[link],
                      w1_type = w1_type,
                      n_w = n_w,
                      n_x = n_x, 
                      n_z = n_z,
                      fd1 = fd1_part,
                      fd2 = fd2_part,
                      true1 = fd1_true,
                      true2 = fd2_true, 
                      max_lik = m$max_lik)
    df <- rbind(df, rbind(df1, df2))
  }
}

# save simulations
readr::write_csv(df, "output/link-sims.csv")

