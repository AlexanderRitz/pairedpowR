#' Estimation of power for paired and modified paired t-test.
#'
#' Estimates the power of paired and modified paired t-test in the case of
#' normal data for a given intra-pair correlation, significance level, sample
#' size and effect size.
#'
#' @param npairs integer. Supplies the number of pairs to be simulated.
#' Resulting in two samples of size "npairs".
#' @param eff numeric. Supplies the difference in means between both groups.
#' Can be interpreted as effect size based on the fact that standard normal
#' variates are the basis of simulation.
#' @param rho numeric. Supplies the correlation coefficient between both groups.
#' @param sig numeric. Supplies the significance level for the test.
#' 0.05 by default.
#' @param rounds integer. Supplies the number of simulation to base the power
#' estimation on. Set to 10000 by default.
#' @return Returns the estimated power for both tests as a vector containing the
#' power of the unpaired t-test in the first place.
#'
#'
#' @examples
#' \donttest{
#' powertest <- modpowr(npairs = 10, eff = 0.5, rho = 0.1)
#' powertest
#' }
#'
#' @export

modpowr <- function(npairs = NULL, eff = NULL, rho = NULL, sig = 0.05, rounds = 10000) {

  k_mod <- 0
  k_paired <- 0
  for (i in 1:rounds) {
    dat <- simdat(npairs = npairs, eff = eff, rho = rho)
    mtest <- mod_test(data = dat)
    ptest <- paired_test(data = dat)
    if (mtest < sig) {
      k_mod <- k_mod + 1
    }
    if (ptest < sig) {
      k_paired <- k_paired + 1
    }
  }
  p_mod <- k_mod/rounds
  p_paired <- k_paired/rounds
  return(c(p_mod, p_paired))
}