# n <- 10000
# x <- rnorm(n)
# z <- rnorm(n)
# 
# X <- cbind(1, x)
# Z <- cbind(1, x, z)
# 
# beta <- c(-1, 1)#runif(ncol(X), -1, 1)
# gamma <- c(-1, 1, 1)#runif(ncol(Z), -1, 1)
# n.beta <- length(beta)
# n.gamma <- length(gamma)
# param <- c(beta, gamma)
# y <- rbinom(n, 1, plogis(X%*%beta) + plogis(Z%*%gamma)*(1 - plogis(X%*%beta)))


# log-likelihood function
ll__ <- function(param, y, X, Z, prior.scale = 2.5) {
  n.beta <- ncol(X)
  n.gamma <- ncol(Z)
  beta <- param[1:n.beta]
  gamma <- param[(n.beta + 1):(n.beta + n.gamma)]
  p.main <- plogis(X%*%beta)
  p.nuisance <- plogis(Z%*%gamma)
  p.obs <- p.main + p.nuisance - p.main*p.nuisance
  ll <- sum(log(dbinom(y, 1, p.obs))) + sum(log(dcauchy(param, 0, prior.scale)))
  return(ll)
}
#ll__(c(beta, gamma), y, X, Z)

dlogcauchy <- function(beta, gamma) {
  -2*beta/(beta^2 + gamma^2)
}
#par(mfrow = c(1,2))
#curve(log(dcauchy(x, 0, 2.5)), xlim = c(-10, 10))
#curve(dlogcauchy(x, 2.5), xlim = c(-10, 10))

# gradient of ll function
gr__ <- function(param, y, X, Z, prior.scale = 2.5) {
  n.beta <- ncol(X)
  n.gamma <- ncol(Z)
  beta <- param[1:n.beta]
  gamma <- param[(n.beta + 1):(n.beta + n.gamma)]
  gr <- numeric(n.beta + n.gamma)
  for (i in 1:n.beta) {
    Q <- plogis(X%*%beta) + plogis(Z%*%gamma) - plogis(X%*%beta)*plogis(Z%*%gamma)
    Q.prime <- dlogis(X%*%beta)*X[, i] - dlogis(X%*%beta)*X[, i]*plogis(Z%*%gamma)
    gr[i] <- sum(Q.prime*(y/Q - (1-y)/(1-Q))) + dlogcauchy(beta[i], prior.scale)
  }
  for (i in 1:n.gamma) {
    Q <- plogis(X%*%beta) + plogis(Z%*%gamma) - plogis(X%*%beta)*plogis(Z%*%gamma)
    Q.prime <- dlogis(Z%*%gamma)*Z[, i] - plogis(X%*%beta)*dlogis(Z%*%gamma)*Z[, i]
    gr[n.beta + i] <- sum(Q.prime*(y/Q - (1-y)/(1-Q))) + dlogcauchy(gamma[i], prior.scale)
  }
  return(gr)
}
#gr__(c(beta, gamma), y, X, Z)


pobs.gen <- function(y, X, Z, prior.scale = 2.5, ...) {
  require(rgenoud)
  n.beta <- ncol(X)
  n.gamma <- ncol(Z)
  beta <- numeric(ncol(X))
  gamma <- numeric(ncol(Z))
  param <- c(beta, gamma)
  fit <- genoud(fn = ll__, nvars = length(param),
                gr = gr__, 
                y = y, X = X, Z = Z, prior.scale = prior.scale,
                max = TRUE,
                hessian = TRUE,
                unif.seed = round(runif(1, 0, 100000)),
                int.seed = round(runif(1, 0, 100000)),
                control = list("maxit" = 1000,
                               "reltol" = .Machine$double.eps),
                ...)
  beta <- fit$par[1:n.beta]
  gamma <- fit$par[(n.beta + 1):(n.beta + n.gamma)]
  cov <- solve(-fit$hessian)
  se <- sqrt(diag(cov))
  se.beta <- se[1:n.beta]
  se.gamma <- se[(n.beta + 1):(n.beta + n.gamma)]
  lik <- fit$value
  res <- list(beta = beta,
              gamma = gamma, 
              cov = cov, 
              se.beta = se.beta,
              se.gamma = se.gamma,
              lik = lik)
}


pobs <- function(y, X, Z, prior.scale = 2.5, ...) {
  require(rgenoud)
  n.beta <- ncol(X)
  n.gamma <- ncol(Z)
  beta <- rnorm(n.beta, 0, 1) #numeric(ncol(X))
  gamma <- rnorm(n.gamma, 0, 1) #numeric(ncol(Z))
  param <- c(beta, gamma)
  fit <- optim(fn = ll__, par = param,
                #gr = gr__, 
                y = y, X = X, Z = Z, prior.scale = prior.scale,
                method = "BFGS",
                hessian = TRUE,
                control = list("maxit" = 1000,
                               "reltol" = .Machine$double.eps,
                               "fnscale" = -1),
                ...)
  beta <- fit$par[1:n.beta]
  gamma <- fit$par[(n.beta + 1):(n.beta + n.gamma)]
  cov <- solve(-fit$hessian)
  se <- sqrt(diag(cov))
  se.beta <- se[1:n.beta]
  se.gamma <- se[(n.beta + 1):(n.beta + n.gamma)]
  lik <- fit$value
  res <- list(beta = beta,
              gamma = gamma, 
              cov = cov, 
              se.beta = se.beta,
              se.gamma = se.gamma,
              lik = lik)
}

pobs.mh <- function(y, X, Z, prior.scale = 2.5, ...) {
  require(rgenoud)
  n.beta <- ncol(X)
  n.gamma <- ncol(Z)
  beta <- rnorm(n.beta, 0, 2) #numeric(ncol(X))
  gamma <- rnorm(n.gamma, 0, 2) #numeric(ncol(Z))
  param <- c(beta, gamma)
  fit <- MCMCmetrop1R(fun = ll__, #theta.init = param,
               #gr = gr__, 
               y = y, X = X, Z = Z,
               seed = round(runif(1, 0, 100000)),
               ...)
  return(fit)
}

#test <- pobs(y, X, Z, prior.scale = 2.5)
#test