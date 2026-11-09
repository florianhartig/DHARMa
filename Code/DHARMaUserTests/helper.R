# Helper for the user-perspective tests, see README.md
# Run the scenario scripts from the repository root; they source this file.

suppressMessages(devtools::load_all("DHARMa", quiet = TRUE))

# output folder for plots and logs, default: a temporary folder
OUT = Sys.getenv("DHARMA_USERTEST_OUT", file.path(tempdir(), "DHARMaUserTests"))
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
LOG = data.frame()

# p-values of an htest or of a list of htests
pvals <- function(x){
  if(is.list(x) && !is.null(x$uniformity)) return(signif(c(x$uniformity$p.value.cor, x$homogeneity$`Pr(>F)`[1]), 3))  # testCategorical
  if(inherits(x, "htest")) return(signif(x$p.value, 3))
  if(is.list(x)) {
    p = unlist(lapply(x, function(e) if(inherits(e, "htest")) e$p.value))
    if(length(p)) return(signif(p, 3))
  }
  NA
}

# runs one call, records result, p-values, warnings, messages and errors;
# expect = "ok", "detect" (p < 0.05 expected) or "error" documents the expected outcome
chk <- function(label, expr, expect = "ok", png = NULL){
  w = character(); m = character(); err = NA
  if(!is.null(png)) grDevices::png(file.path(OUT, paste0(png, ".png")), width = 900, height = 500)
  res = withCallingHandlers(
    tryCatch(expr, error = function(e) {err <<- conditionMessage(e); NULL}),
    warning = function(cw) {w <<- c(w, conditionMessage(cw)); invokeRestart("muffleWarning")},
    message = function(cm) {m <<- c(m, trimws(conditionMessage(cm))); invokeRestart("muffleMessage")})
  if(!is.null(png)) grDevices::dev.off()
  p = tryCatch(paste(pvals(res), collapse = "; "), error = function(e) NA)
  LOG <<- rbind(LOG, data.frame(label = label, expect = expect, ok = is.na(err), p = p,
                                warnings = paste(unique(w), collapse = " || "),
                                messages = paste(unique(m), collapse = " || "),
                                error = err))
  cat(sprintf("[%s] %s (expect: %s) p = %s%s%s%s\n", if(is.na(err)) "OK " else "ERR", label, expect, p,
              if(length(w)) paste0("\n   W: ", paste(unique(w), collapse = " || ")) else "",
              if(length(m)) paste0("\n   M: ", paste(unique(m), collapse = " || ")) else "",
              if(!is.na(err)) paste0("\n   E: ", err) else ""))
  invisible(res)
}

# standard calls on a DHARMa object
battery <- function(tag, so, fac = NULL, form = NULL, expect = "ok"){
  chk(paste(tag, "plot"), plot(so), expect, png = paste0(tag, "_plot"))
  chk(paste(tag, "testUniformity"), testUniformity(so, plot = FALSE), expect)
  chk(paste(tag, "testDispersion"), testDispersion(so, plot = FALSE), expect)
  chk(paste(tag, "testZeroInflation"), testZeroInflation(so, plot = FALSE), expect)
  chk(paste(tag, "testOutliers"), testOutliers(so, plot = FALSE), expect)
  chk(paste(tag, "testOutliers bootstrap"), testOutliers(so, type = "bootstrap", plot = FALSE), expect)
  chk(paste(tag, "testQuantiles"), testQuantiles(so, plot = FALSE), expect)
  if(!is.null(fac)) chk(paste(tag, "testCategorical"), testCategorical(so, catPred = fac, plot = FALSE), expect)
  for(f in form) chk(paste(tag, "plotResiduals", deparse(f)), plotResiduals(so, form = f))
  invisible(NULL)
}

saveLog <- function(name){
  write.csv(LOG, file.path(OUT, paste0(name, "_log.csv")), row.names = FALSE)
  cat("\nLog and plots in", OUT, "\n")
}
