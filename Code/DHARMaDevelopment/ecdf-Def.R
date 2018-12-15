# test of new ecdf definition
# problem with standard ecdf function is that it is not fully symmetric

# from stats
ecdf <- function (x) 
{
  x <- sort(x)
  n <- length(x)
  if (n < 1) 
    stop("'x' must have 1 or more non-missing values")
  vals <- unique(x)
  rval <- approxfun(vals, cumsum(tabulate(match(x, vals)))/n, 
                    method = "constant", yleft = 0, yright = 1, f = 0, ties = "ordered")
  class(rval) <- c("ecdf", "stepfun", class(rval))
  assign("nobs", n, envir = environment(rval))
  attr(rval, "call") <- sys.call()
  rval
}


DHARMa.ecdf <- function (x) 
{
  x <- sort(x)
  n <- length(x)
  if (n < 1) 
    stop("'x' must have 1 or more non-missing values")
  vals <- unique(x)
  rval <- approxfun(vals, cumsum(tabulate(match(x, vals)))/ (n +1), 
                    method = "linear", yleft = 0, yright = 1, ties = "ordered")
  class(rval) <- c("ecdf", "stepfun", class(rval))
  assign("nobs", n, envir = environment(rval))
  attr(rval, "call") <- sys.call()
  rval
}

x = rnorm(100)

1 - DHARMa.ecdf(x)(max(x))

DHARMa.ecdf(x)(max(x) - 0.00000001)
DHARMa.ecdf(x)(max(x) + 0.00000001)

DHARMa.ecdf(x)(min(x))

DHARMa.ecdf(x)(min(x) - 0.00000001)
DHARMa.ecdf(x)(min(x) + 0.00000001)

