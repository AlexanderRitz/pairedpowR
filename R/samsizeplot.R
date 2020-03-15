#' Plotting of power curves II
#'
#' Plotting of estimated power curves of unpaired and paired t-test in relation
#' to underlying sample size.
#' In case of no actual mean difference, i.e. effect = 0, the curves represent
#' the observed rate of type I errors and should be interpreted as the
#' significance level of the tests.
#'
#' @param upp integer. Supplies the upper limit of the sample sizes to be
#' simulated.
#' @param corr numeric. Supplies the underlying group correlation.
#' @param effect numeric. Supplies the difference in means between both groups.
#' Can be interpreted as effect size based on the fact that standard normal
#' variates are the basis of simulation.
#' @param mod logical. If TRUE, modified and paired t-test are compared.
#' @return Returns plots of the power curves(or significance levels)
#' of paired and unpaired t-test in relation to the underlying sample sizes.
#'
#'
#' @examples
#' \donttest{
#' samsizeplot(upp = 100, corr = 0.3, effect = 0)
#' }
#'
#' @export

samsizeplot <- function(upp = 100, corr = 0, effect = 0, mod =  FALSE){
  x <- 2:upp
  yH_1 <- rep(0, times = (upp-1))
  yH_2 <- rep(0, times = (upp-1))
  if (mod == FALSE){
  for (i in 1:(upp - 1)) {
    k = x[i]
    util <- powr(npairs = k, eff = effect, rho = corr)
    yH_1[(i)] <- util[1]
    yH_2[(i)] <- util[2]
  }
  if(effect == 0){
    plot(x = x, y = yH_1, type = "l", col = "red", ylim = c(0, 0.25), ylab = "Signifikanzniveau", xlab = "Stichprobenumfang", main = "Signifikanzniveau zu Stichprobenumfang", lwd = 2)
    lines(x = x, y = yH_2, col = "green", lwd = 2)
    lines(x = x, y = rep(0.05, times = (upp-1)), col = "black")
    legend("topleft", legend=c("Ungep. Test", "Gep. Test"),
      col=c("red", "green"), lty=1:1, cex = 0.8 , lwd = 2)
  } else {
    plot(x = x, y = yH_1, type = "l", col = "red", ylim = c(0, 1), ylab = "Power", xlab = "Stichprobenumfang", main = "Power zu Stichprobenumfang",     lwd = 2)
    lines(x = x, y = yH_2, col = "green", lwd = 2)
    legend("topleft", legend=c("Ungep. Test", "Gep. Test"),
      col=c("red", "green"), lty=1:1, cex = 0.8 , lwd = 2)
  }
  } else if(mod == TRUE){
    for (i in 1:(upp - 1)) {
      k = x[i]
      util <- modpowr(npairs = k, eff = effect, rho = corr)
      yH_1[(i)] <- util[1]
      yH_2[(i)] <- util[2]
    }
    if(effect == 0){
      plot(x = x, y = yH_1, type = "l", col = "red", ylim = c(0, 0.25), ylab = "Signifikanzniveau", xlab = "Stichprobenumfang", main = "Signifikanzniveau zu Stichprobenumfang", lwd = 2)
      lines(x = x, y = yH_2, col = "green", lwd = 2)
      lines(x = x, y = rep(0.05, times = (upp-1)), col = "black")
      legend("topleft", legend=c("Mod. Test", "Gep. Test"),
        col=c("red", "green"), lty=1:1, cex = 0.8 , lwd = 2)
    } else {
      plot(x = x, y = yH_1, type = "l", col = "red", ylim = c(0, 1), ylab = "Power", xlab = "Stichprobenumfang", main = "Power zu Stichprobenumfang",     lwd = 2)
      lines(x = x, y = yH_2, col = "green", lwd = 2)
      legend("topleft", legend=c("Mod. Test", "Gep. Test"),
        col=c("red", "green"), lty=1:1, cex = 0.8 , lwd = 2)
    }
  }
}