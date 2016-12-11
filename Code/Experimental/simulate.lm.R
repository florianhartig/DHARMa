# simulate.lm code from R core for test purposes 

fittedModelGAM


## The lm method includes objects of class "glm"
simulate.lm <- function(object, nsim = 1, seed = NULL, ...)
{
  if(!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    runif(1)                     # initialize the RNG if necessary
  if(is.null(seed))
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }
  fam <- if(inherits(object, "glm")) object$family$family else "gaussian"
  ftd <- fitted(object)             # == napredict(*, object$fitted)
  isMlm <- identical(fam, "gaussian") && is.matrix(ftd)
  nm <- if(isMlm) dimnames(ftd) else names(ftd)
  if(isMlm) ## Not hard. Biggest question: how exactly the data frame should look
    stop("simulate() is not yet implemented for multivariate lm()")
  n <- length(ftd)
  ntot <- n * nsim
  val <- switch(fam,
                "gaussian" = {
                  vars <- deviance(object)/ df.residual(object)
                  if(isMlm) {
                    ## _TODO_
                    ## weights ==> "vars / weights" as matrix with  dim(ftd)
                  } else {
                    if (!is.null(object$weights)) vars <- vars/object$weights
                    ftd + rnorm(ntot, sd = sqrt(vars))
                  }
                },
                if(!is.null(object$family$simulate))
                  object$family$simulate(object, nsim)
                else stop(gettextf("family '%s' not implemented", fam),
                          domain = NA)
  )
  
  if(isMlm) {
    ## _TODO_
  } else if(!is.list(val)) {
    dim(val) <- c(n, nsim)
    val <- as.data.frame(val)
  } else
    class(val) <- "data.frame"
  ## isMlm: conceptually, each "sim_i" could be a *matrix* [unusually]
  names(val) <- paste("sim", seq_len(nsim), sep="_")
  if (!is.null(nm)) row.names(val) <- nm
  attr(val, "seed") <- RNGstate
  val
}




# GLM

function (object, nsim) 
{
  ftd <- fitted(object)
  n <- length(ftd)
  ntot <- n * nsim
  wts <- object$prior.weights
  if (any(wts%%1 != 0)) 
    stop("cannot simulate from non-integer prior.weights")
  if (!is.null(m <- object$model)) {
    y <- model.response(m)
    if (is.factor(y)) {
      yy <- factor(1 + rbinom(ntot, size = 1, prob = ftd), 
                   labels = levels(y))
      split(yy, rep(seq_len(nsim), each = n))
    }
    else if (is.matrix(y) && ncol(y) == 2) {
      yy <- vector("list", nsim)
      for (i in seq_len(nsim)) {
        Y <- rbinom(n, size = wts, prob = ftd)
        YY <- cbind(Y, wts - Y)
        colnames(YY) <- colnames(y)
        yy[[i]] <- YY
      }
      yy
    }
    else rbinom(ntot, size = wts, prob = ftd)/wts
  }
  else rbinom(ntot, size = wts, prob = ftd)/wts
}



# GAM

function (object, nsim) 
{
  ftd <- fitted(object)
  n <- length(ftd)
  ntot <- n * nsim
  wts <- object$prior.weights
  if (any(wts%%1 != 0)) 
    stop("cannot simulate from non-integer prior.weights")
  if (!is.null(m <- object$model)) {
    y <- model.response(m)
    if (is.factor(y)) {
      yy <- factor(1 + rbinom(ntot, size = 1, prob = ftd), 
                   labels = levels(y))
      split(yy, rep(seq_len(nsim), each = n))
    }
    else if (is.matrix(y) && ncol(y) == 2) {
      yy <- vector("list", nsim)
      for (i in seq_len(nsim)) {
        Y <- rbinom(n, size = wts, prob = ftd)
        YY <- cbind(Y, wts - Y)
        colnames(YY) <- colnames(y)
        yy[[i]] <- YY
      }
      yy
    }
    else rbinom(ntot, size = wts, prob = ftd)/wts
  }
  else rbinom(ntot, size = wts, prob = ftd)/wts
}