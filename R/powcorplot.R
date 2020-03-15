#' Plotting of power curves I
#'
#' Plotting of estimated power curves of unpaired and paired t-test in relation
#' to underlying group correlation.
#' In case of no actual mean difference, i.e. effect = 0, the curves represent
#' the observed rate of type I errors and should be interpreted as the
#' significance level of the tests.
#'
#' @param gran integer. Supplies the level of granularity, i.e. the number of
#' points for interpolation of the final curve.
#' @param numpairs integer. Supplies the number of pairs to be simulated.
#' Resulting in two samples of size "npairs".
#' @param effect numeric. Supplies the difference in means between both groups.
#' Can be interpreted as effect size based on the fact that standard normal
#' variates are the basis of simulation.
#' @param diffdisplay logical. If TRUE, plots will show difference between the
#' realised power or significance levels of paired and unpaired test. The
#' difference is calculated by subtracting the values of the unpaired test from
#' the paired tests values.
#' @param mod logical. If TRUE, plots will show the realised power or
#' significance levels of paired and  modified paired tests. The difference for
#' diffdisplay = TRUE is calculated by subtracting the values of the paired test
#' from the modified paired tests values.
#' @return Returns plots of either the power curves(or significance levels)
#' of paired and unpaired t-test in relation to the underlying group correlations,
#' or alternatively the plotted difference between both curves.
#'
#'
#' @examples
#' \donttest{
#' powcorplot(gran = 10, numpairs = 5, effect = 0.5)
#' }
#'
#' @export

powcorplot <- function(gran = 100, numpairs = 10, effect = 0, diffdisplay =
    FALSE, mod = FALSE){
  if (mod == FALSE){
    x <- seq(from = -0.99, to = 0.99, length.out = gran)
    yH_1 <- rep(0, times = gran)
    yH_2 <- rep(0, times = gran)
    for (i in 1:gran) {
      k = x[i]
      util <- powr(npairs = numpairs, eff = effect, rho = k)
      yH_1[(i)] <- util[1]
      yH_2[(i)] <- util[2]
    }
    if(diffdisplay == FALSE){
      if(effect == 0){
        plot(x = x, y = yH_1, type = "l", col = "red", ylim = c(0, 0.25), ylab =
            "Signifikanzniveau", xlab = "Paar-Korrelation", main = paste(
              "Signifikanzniveau bei", numpairs, "Paaren"), lwd = 2)
        lines(x = x, y = yH_2, col = "green", lwd = 2)
        lines(x = x, y = rep(0.05, times = gran), col = "black")
        lines(x = rep(0.1, times = gran), y = x, col = "black", lty = "dashed")
        lines(x = rep(-0.1, times = gran), y = x, col = "black", lty = "dashed")
        legend("topleft", legend=c("Ungep. Test", "Gep. Test"),
          col=c("red", "green"), lty=1:1, cex = 0.8 , lwd = 2)
      } else {
        plot(x = x, y = yH_1, type = "l", col = "red", ylim = c(0, 1), ylab =
            "Power", xlab = "Paar-Korrelation", main = paste("Power bei",
              numpairs, "Paaren"), lwd = 2)
        lines(x = x, y = yH_2, col = "green", lwd = 2)
        lines(x = rep(0.1, times = gran), y = x, col = "black", lty = "dashed")
        lines(x = rep(-0.1, times = gran), y = x, col = "black", lty = "dashed")
        legend("topleft", legend=c("Ungep. Test", "Gep. Test"),
          col=c("red", "green"), lty=1:1, cex = 0.8 , lwd = 2)
      }
    } else if(diffdisplay == TRUE){
      if(effect == 0){
        plot(x = x, y = (yH_2 - yH_1), type = "l", col = "red", ylim = c(-0.1, 0.1),
          ylab = "Signifikanzniveaudifferenz (gep. t-Test - ungep. t-Test)",
          xlab = "Paar-Korrelation", main = paste("Signifikanzniveau bei",
            numpairs, "Paaren"), lwd = 2)
      } else {
        plot(x = x, y = (yH_2 - yH_1), type = "l", col = "red", ylim = c(-0.25, 1)
          , ylab = "Poweraudifferenz (gep. t-Test - ungep. t-Test)",
          xlab = "Paar-Korrelation", main = paste("Power bei", numpairs,
            "Paaren"), lwd = 2)
      }
    }
  } else if(mod == TRUE){
    x <- seq(from = -0.99, to = 0.99, length.out = gran)
    yH_1 <- rep(0, times = gran)
    yH_2 <- rep(0, times = gran)
    for (i in 1:gran) {
      k = x[i]
      util <- modpowr(npairs = numpairs, eff = effect, rho = k)
      yH_1[(i)] <- util[1]
      yH_2[(i)] <- util[2]
    }
    if(diffdisplay == FALSE){
      if(effect == 0){
        plot(x = x, y = yH_1, type = "l", col = "red", ylim = c(0, 0.25), ylab =
            "Signifikanzniveau", xlab = "Paar-Korrelation", main = paste(
              "Signifikanzniveau bei", numpairs, "Paaren"), lwd = 2)
        lines(x = x, y = yH_2, col = "green", lwd = 2)
        lines(x = x, y = rep(0.05, times = gran), col = "black")
        legend("topleft", legend=c("Mod. Test", "Gep. Test"),
          col=c("red", "green"), lty=1:1, cex = 0.8 , lwd = 2)
      } else {
        plot(x = x, y = yH_1, type = "l", col = "red", ylim = c(0, 1), ylab =
            "Power", xlab = "Paar-Korrelation", main = paste("Power bei",
              numpairs, "Paaren"), lwd = 2)
        lines(x = x, y = yH_2, col = "green", lwd = 2)
        legend("topleft", legend=c("Mod. Test", "Gep. Test"),
          col=c("red", "green"), lty=1:1, cex = 0.8 , lwd = 2)
      }
    } else if(diffdisplay == TRUE){
      if(effect == 0){
        plot(x = x, y = (yH_1 - yH_2), type = "l", col = "red", ylim = c(-0.1, 0.1),
          ylab = "Signifikanzniveaudifferenz (Mod. t-Test - gep. t-Test)", xlab
          = "Paar-Korrelation", main = paste("Signifikanzniveau bei", numpairs,
            "Paaren"), lwd = 2)
        } else {
        plot(x = x, y = (yH_2 - yH_1), type = "l", col = "red", ylim = c(-0.25, 1)
          , ylab = "Powerdifferenz (Mod. t-Test - gep. t-Test)",
          xlab = "Paar-Korrelation", main = paste("Power bei", numpairs,
            "Paaren"), lwd = 2)
      }
    }
  }
}
