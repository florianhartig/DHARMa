#' DHARMa standard residual plots
#'
#' This S3 function creates standard plots for the simulated residuals contained in an object of class DHARMa, using [plotQQunif] (left panel) and [plotResiduals] (right panel)
#'
#' @param x An object of class DHARMa with simulated residuals created by [simulateResiduals].
#' @param ... Further options for [plotResiduals]. Consider in particular parameters quantreg, rank and asFactor. xlab, ylab and main cannot be changed when using plot.DHARMa, but can be changed when using [plotResiduals].
#' @param title The title for both panels (plotted via mtext, outer = TRUE).
#'
#' @details The function creates a plot with two panels. The left panel is a uniform qq plot (calling [plotQQunif]), and the right panel shows residuals against predicted values (calling [plotResiduals]), with outliers as a star/asterisk and highlighted in red if outlier test is significant (default color but see Note).
#'
#' Very briefly, we would expect that a correctly specified model shows:
#'
#' a) a straight 1-1 line, as well as non-significance of the displayed tests in the qq-plot (left) -> evidence for an the correct overall residual distribution (for more details on the interpretation of this plot, see [plotQQunif])
#'
#' b) visual homogeneity of residuals in both vertical and horizontal direction, as well as n.s. of quantile tests in the res ~ predictor plot (for more details on the interpretation of this plot, see [plotResiduals])
#'
#' Deviations from these expectations can be interpreted similar to a linear regression. See the vignette for detailed examples.
#'
#' Note that, unlike [plotResiduals], plot.DHARMa command uses the default rank = T.
#'
#' @note The color for highlighting outliers and significant tests can be changed by setting \code{options(DHARMaSignalColor = "red")} to a different color. See \code{getOption("DHARMaSignalColor")} for the current setting. This is convenient for a color-blind friendly display, since red and black are difficult for some people to separate.
#'
#' @seealso [plotResiduals], [plotQQunif]
#' @example inst/examples/plotsHelp.R
#' @import graphics
#' @import utils
#' @export
plot.DHARMa <- function(x, title = "DHARMa residual", ...){

  oldpar <- par(mfrow = c(1,2), oma = c(0,1,2,1))
  on.exit(par(oldpar))

  plotQQunif(x)
  plotResiduals(x, ...)

  mtext(title, outer = TRUE)
}


#' Histogram of DHARMa residuals
#'
#' The function produces a histogram from a DHARMa output. Outliers are marked in red.
#'
#' @param x A DHARMa simulation output (class DHARMa)
#' @param breaks Breaks for hist() function.
#' @param col Color for histogram bars.
#' @param main Plot title.
#' @param xlab Plot x-axis label.
#' @param cex.main Plot cex.main.
#' @param ... Other arguments to be passed on to hist().
#' @details The function calls hist() to create a histogram of the scaled residuals. Outliers are marked red as default but it can be changed by setting \code{options(DHARMaSignalColor = "red")} to a different color. See \code{getOption("DHARMaSignalColor")} for the current setting.
#' @seealso [plotSimulatedResiduals], [plotResiduals]
#' @example inst/examples/plotsHelp.R
#' @export
hist.DHARMa <- function(x,
                        breaks = seq(-0.02, 1.02, len = 53),
                        col = c(.Options$DHARMaSignalColor,rep("lightgrey",50), .Options$DHARMaSignalColor),
                        main = "Hist of DHARMa residuals",
                        xlab = "Residuals (outliers are marked red)",
                        cex.main = 1,
                        ...){

  x = ensureDHARMa(x, convert = TRUE)

  val = x$scaledResiduals
  val[val == 0] = -0.01
  val[val == 1] = 1.01

  hist(val, breaks = breaks, col = col, main = main, xlab = xlab, cex.main = cex.main, ...)
}


#' DHARMa standard residual plots (deprecated)
#'
#' DEPRECATED, use plot() instead
#'
#' @param simulationOutput an object with simulated residuals created by [simulateResiduals]
#' @param ... further options for [plotResiduals]. Consider in particular parameters quantreg, rank and asFactor. xlab, ylab and main cannot be changed when using plotSimulatedResiduals, but can be changed when using plotResiduals.
#' @note This function is deprecated. Use [plot.DHARMa]
#'
#' @seealso [plotResiduals], [plotQQunif]
#' @export
plotSimulatedResiduals <- function(simulationOutput, ...){
  message("plotSimulatedResiduals is deprecated, please switch your code to simply using the plot() function")
  plot(simulationOutput, ...)
}


#' Quantile-quantile plot for a uniform distribution
#'
#' The function produces a uniform quantile-quantile plot from a DHARMa output. Optionally, tests for uniformity, outliers and dispersion can be added.
#'
#' @param simulationOutput A DHARMa simulation output (class DHARMa).
#' @param testUniformity If T, the function [testUniformity] will be called and the result will be added to the plot.
#' @param testOutliers If T, the function [testOutliers] will be called and the result will be added to the plot.
#' @param testDispersion If T, the function [testDispersion] will be called and the result will be added to the plot.
#' @param ... Arguments to be passed on to [gap::qqunif].
#'
#' @details The function calls qqunif() from the R package gap to create a quantile-quantile plot for a uniform distribution, and overlays tests for particular distributional problems as specified.
#' When tests are displayed, significant p-values are highlighted in the color red by default. This can be changed by setting \code{options(DHARMaSignalColor = "red")} to a different color. See \code{getOption("DHARMaSignalColor")} for the current setting.
#' @seealso [plotSimulatedResiduals], [plotResiduals]
#' @example inst/examples/plotsHelp.R
#' @export
plotQQunif <- function(simulationOutput, testUniformity = TRUE, testOutliers = TRUE, testDispersion = TRUE, ...){

  a <- list(...)
  a$pch = checkDots("pch", 2, ...)
  a$bty = checkDots("bty", "n", ...)
  a$logscale = checkDots("logscale", F, ...)
  a$col = checkDots("col", "black", ...)
  a$main = checkDots("main", "QQ plot residuals", ...)
  a$cex.main = checkDots("cex.main", 1, ...)
  a$xlim = checkDots("xlim", c(0,1), ...)
  a$ylim = checkDots("ylim", c(0,1), ...)

  simulationOutput = ensureDHARMa(simulationOutput, convert = "Model")

  do.call(gap::qqunif, append(list(simulationOutput$scaledResiduals), a))

  if(testUniformity == TRUE){
    temp = testUniformity(simulationOutput, plot = FALSE)
    legend("topleft",
           c(paste("KS test: p=", round(temp$p.value, digits = 5)),
             paste("Deviation ", ifelse(temp$p.value < 0.05, "significant", "n.s."))),
           text.col = ifelse(temp$p.value < 0.05, .Options$DHARMaSignalColor, "black" ), bty="n")

  }

  if(testOutliers == TRUE){
    temp = testOutliers(simulationOutput, plot = FALSE)
    legend("bottomright",
           c(paste("Outlier test: p=", round(temp$p.value, digits = 5)),
             paste("Deviation ", ifelse(temp$p.value < 0.05, "significant", "n.s."))),
           text.col = ifelse(temp$p.value < 0.05, .Options$DHARMaSignalColor, "black" ), bty="n")

  }

  if(testDispersion == TRUE){
    temp = testDispersion(simulationOutput, plot = FALSE)
    legend("center",
           c(paste("Dispersion test: p=", round(temp$p.value, digits = 5)),
             paste("Deviation ", ifelse(temp$p.value < 0.05, "significant", "n.s."))),
           text.col = ifelse(temp$p.value < 0.05, .Options$DHARMaSignalColor, "black" ), bty="n")
  }

}



#' Generic res ~ pred scatter plot with spline or quantile regression on top
#'
#' The function creates a generic residual plot with either spline or quantile regression to highlight patterns in the residuals. Outliers are marked in star/asterisk form and highlighted in red if the outlier test is significant.
#'
#' @param simulationOutput An object, usually a DHARMa object, from which residual values can be extracted. Alternatively, a vector with residuals or a fitted model can be provided, which will then be transformed into a DHARMa object.
#' @param form Optional predictor against which the residuals should be plotted. Default is to used the predicted(simulationOutput).
#' @param quantreg Whether to perform a quantile regression based on [testQuantiles] or a smooth spline around the mean. Default NULL chooses T for nObs < 2000, and F otherwise.
#' @param rank If T, the values provided in form will be rank transformed. This will usually make patterns easier to spot visually, especially if the distribution of the predictor is skewed. If form is a factor, this has no effect.
#' @param asFactor Should a numeric predictor provided in form be treated as a factor. Default is to choose this for < 10 unique values, as long as enough predictions are available to draw a boxplot.
#' @param smoothScatter if T, a smooth scatter plot will plotted instead of a normal scatter plot. This makes sense when the number of residuals is very large. Default NULL chooses T for nObs > 10000, and F otherwise.
#' @param quantiles For a quantile regression, which quantiles should be plotted. Default is 0.25, 0.5, 0.75.
#' @param absoluteDeviation If T, switch from displaying normal quantile residuals to absolute deviation from the mean expectation of 0.5 (calculated as 2 * abs(res - 0.5)). The purpose of this is to test explicitly for heteroskedasticity, see details.
#' @param ... Additional arguments to plot / boxplot.
#' @details The function plots residuals against a predictor (by default against the fitted value, extracted from the DHARMa object, or any other predictor).
#'
#' Outliers are drawn if simulationOutput is a DHARMa object and highlighted in red if the outlier test is significant (for information on definition and interpretation of outliers, see [testOutliers]). See the note below to change the highlighting color of the outliers.
#'
#' To provide a visual aid for detecting deviations from uniformity in the y-direction, the plot function calculates an (optional) quantile regression of the residuals, by default for the 0.25, 0.5 and 0.75 quantiles. Since the residuals should be uniformly distributed for a correctly specified model, the theoretical expectations for these regressions are straight lines at 0.25, 0.5 and 0.75, shown as dashed black lines on the plot. However, even for a perfect model, some deviation from these expectations is to be expected by chance, especially if the sample size is small. The function therefore tests whether the deviation of the fitted quantile regression from the expectation is significant, using [testQuantiles]. If so, the significant quantile regression is highlighted in red (as default) and a warning is displayed in the plot. See the note below to change the color of significant quantile lines.
#'
#' Overdispersion typically manifests itself as Q1 (0.25) deviating towards 0 and Q3 (0.75) deviating towards 1. Heteroskedasticity manifests itself as non-parallel quantile lines. To diagnose heteroskedasticity and overdispersion, it can be helpful to additionally plot the absolute deviation of the residuals from the mean expectation of 0.5, using the option absoluteDeviation = T. In this case, we would again expect Q1-Q3 quantile lines at 0.25, 0.5, 0.75, but greater dispersion (also locally in the case of heteroskedasticity) always manifests itself in deviations towards 1.
#'
#' The quantile regression can take some time to calculate, especially for larger data sets. For this reason, quantreg = F can be set to generate a smooth spline instead. This is the default for n > 2000.
#'
#' If form is a factor, a boxplot will be plotted instead of a scatter plot. The distribution for each factor level should be uniformly distributed, so the box should go from 0.25 to 0.75, with the median line at 0.5 (within-group). To test if deviations from those expecations are significant, KS-tests per group and a Levene test for homogeneity of variances is performed. See [testCategorical] for details.
#'
#' @note If nObs > 10000, the scatter plot is replaced by graphics::smoothScatter
#'
#' @note The color for highlighting outliers and quantile lines/splines with significant tests can be changed by setting \code{options(DHARMaSignalColor = "red")} to a different color. See \code{getOption("DHARMaSignalColor")} for the current setting. This is convenient for a color-blind friendly display, since red and black are difficult for some people to distinguish.
#'
#' @return If quantile tests are performed, the function returns them invisibly.
#'
#' @seealso [plotQQunif], [testQuantiles], [testOutliers]
#' @example inst/examples/plotsHelp.R
#' @export
plotResiduals <- function(simulationOutput, form = NULL, quantreg = NULL,
                          rank = TRUE, asFactor = NULL, smoothScatter = NULL,
                          quantiles = c(0.25, 0.5, 0.75),
                          absoluteDeviation = FALSE, ...){


  ##### Checks #####

  a <- list(...)
  yAxis = ifelse(absoluteDeviation == TRUE, "Residual spread [2*abs(res - 0.5)]", "DHARMa residual")
  a$ylab = checkDots("ylab", yAxis , ...)
  a$xlab = checkDots("xlab", ifelse(is.null(form), "Model predictions",
                                    gsub(".*[$]","",deparse(substitute(form)))), ...)
  if(rank == TRUE) a$xlab = paste(a$xlab, "(rank transformed)")


  ### outliers - plot or not issue #453
  simOut <- simulationOutput
  if(is.vector(simOut)) {
    warning("Outliers will be not displayed in the plot when simulationOutputs are not DHARMa objects.")}

  simulationOutput = ensureDHARMa(simulationOutput, convert = TRUE)
  res = simulationOutput$scaledResiduals

  if(absoluteDeviation == TRUE){
    res = 2 * abs(res - 0.5)
  }

  if(inherits(form, "DHARMa"))stop("DHARMa::plotResiduals > argument form cannot be of class DHARMa. Note that the syntax of plotResiduals has changed since DHARMa 0.3.0. See ?plotResiduals.")

  pred = ensurePredictor(simulationOutput, form)

  ##### Rank transform and factor conversion#####

  if(!is.factor(pred)){

    if (rank == TRUE){
      pred = rank(pred, ties.method = "average")
      pred = pred / max(pred)
      a$xlim = checkDots("xlim", c(0,1), ...)
    }

    nuniq = length(unique(pred))
    ndata = length(pred)
    if(is.null(asFactor)) asFactor = (nuniq == 1) | (nuniq < 10 & ndata / nuniq > 10)
    if (asFactor) pred = factor(pred)
  }

  ##### Residual scatter plots #####

  if(is.null(quantreg)) if (length(res) > 2000) quantreg = FALSE else quantreg = TRUE

  switchScatter = 10000
  if(is.null(smoothScatter)) if (length(res) > switchScatter) smoothScatter = TRUE else smoothScatter = FALSE

  blackcol = rgb(0,0,0, alpha = max(0.1, 1 - 3 * length(res) / switchScatter))

  # Note to self: wrapped in do.call because of the check dots, needs to be consolidate, e.g. for testCategorical

  # categorical plot
  if(is.factor(pred)){
    testCategorical(simulationOutput = simulationOutput, catPred = pred,
                    quantiles = quantiles)
  } else{

    # color/shape outliers - related to issue #453 - see lines 200-204 above
    if(is.vector(simOut)) {
      defaultCol = blackcol
      defaultPch = 1
    } else {
      p.outliers <- testOutliers(simulationOutput, plot = F)$p.value
      defaultPch = ifelse(res == 0 | res == 1, 8, 1)
      if(p.outliers < 0.05){
      outlier <- res == 0 | res == 1
      defaultCol <- ifelse(outlier == TRUE, .Options$DHARMaSignalColor, blackcol)
      } else{ defaultCol = blackcol}
    }

    # smooth scatter
    if (smoothScatter == TRUE) {
      do.call(graphics::smoothScatter, append(list(x = pred, y = res ,
                                                   ylim = c(0,1), axes = FALSE,
                                                   colramp = colorRampPalette(c("white", "darkgrey"))),a))
      points(pred[outlier], res[outlier],
             col = .Options$DHARMaSignalColor, cex = 0.5)

      axis(1)
      axis(2, at=c(0, quantiles, 1))
    }
    # normal plot
    else{
      a$col = checkDots("col", defaultCol, ...)
      a$pch = checkDots("pch", defaultPch, ...)
      do.call(plot, append(list(res ~ pred, ylim = c(0,1), axes = FALSE), a))

      axis(1)
      axis(2, at=c(0, quantiles, 1))
    }
  }

  ##### Quantile regressions #####

  main = checkDots("main", ifelse(is.null(form), paste(yAxis, "vs. predicted"), paste(yAxis, "Residual vs. predictor")), ...)
  out = NULL

  if(is.numeric(pred)){
    if(quantreg == FALSE){
      title(main = main, cex.main = 1)
      abline(h = quantiles, col = "black", lwd = 0.5, lty = 2)
      try({
        lines(smooth.spline(pred, res, df = 10), lty = 2, lwd = 2,
              col = .Options$DHARMaSignalColor)
        abline(h = 0.5, col = .Options$DHARMaSignalColor, lwd = 2)
      }, silent = TRUE)
    }else{

      out = testQuantiles(res, pred, quantiles = quantiles, plot = FALSE)

      if(is.na(out$p.value)){
        main = paste(main, "Some quantile regressions failed", sep = "\n")
        maincol = .Options$DHARMaSignalColor
      } else{
        if(any(out$pvals < 0.05, na.rm = TRUE)){
          main = paste(main, "Quantile deviations detected (red curves)", sep ="\n")
          if(out$p.value <= 0.05){
            main = paste(main, "Combined adjusted quantile test significant", sep ="\n")
          } else {
            main = paste(main, "Combined adjusted quantile test n.s.", sep ="\n")
          }
          maincol = .Options$DHARMaSignalColor
        } else {
          main = paste(main, "No significant problems detected", sep ="\n")
          maincol = "black"
        }
      }

      title(main = main, cex.main = 0.8,
            col.main = maincol)

      for(i in 1:length(quantiles)){

        lineCol = ifelse(out$pvals[i] <= 0.05 & !(is.na(out$pvals[i])), .Options$DHARMaSignalColor, "black")
        filCol = ifelse(out$pvals[i] <= 0.05 & !(is.na(out$pvals[i])), "#FF000040", "#00000020")

        abline(h = quantiles[i], col = lineCol, lwd = 0.5, lty = 2)
        polygon(c(out$predictions$pred, rev(out$predictions$pred)),
                c(out$predictions[,2*i] - out$predictions[,2*i+1], rev(out$predictions[,2*i] + out$predictions[,2*i+1])),
                col = "#00000020", border = FALSE)
        lines(out$predictions$pred, out$predictions[,2*i], col = lineCol, lwd = 2)
      }

    }
  }
  invisible(out)
}




#' Ensures the existence of a valid predictor to plot residuals against
#'
#' @param simulationOutput A DHARMa simulation output or an object that can be converted into a DHARMa simulation output.
#' @param predictor An optional predictor. If no predictor is provided, will try to extract the fitted value.
#' @keywords internal
ensurePredictor <- function(simulationOutput,
                            predictor = NULL){
  if(!is.null(predictor)){

    if(length(predictor) != length(simulationOutput$scaledResiduals)) stop("DHARMa: residuals and predictor do not have the same length. The issue is possibly that you have NAs in your predictor that were removed during the model fit. Remove the NA values from your predictor.")

    if(is.character(predictor)) {
      predictor = factor(predictor)
      warning("DHARMa:::ensurePredictor: character string was provided as predictor. DHARMa has converted to factor automatically. To remove this warning, please convert to factor before attempting to plot with DHARMa.")
    }

  } else {

    predictor = simulationOutput$fittedPredictedResponse
    if(is.null(predictor)) stop("DHARMa: can't extract predictor from simulationOutput, and no predictor provided.")
  }
  return(predictor)
}


#plotConventionalResiduals(fittedModel)


#' Conventional residual plot
#'
#' Convenience function to draw conventional residual plots
#'
#' @param fittedModel a fitted model object
#' @export
plotConventionalResiduals <- function(fittedModel){
  opar <- par(mfrow = c(1,3), oma = c(0,1,2,1))
  on.exit(par(opar))
  plot(predict(fittedModel), resid(fittedModel, type = "deviance"), main = "Deviance" , ylab = "Residual", xlab = "Predicted")
  plot(predict(fittedModel), resid(fittedModel, type = "pearson") , main = "Pearson", ylab = "Residual", xlab = "Predicted")
  plot(predict(fittedModel), resid(fittedModel, type = "response") , main = "Raw residuals" , ylab = "Residual", xlab = "Predicted")
  mtext("Conventional residual plots", outer = TRUE)
}
