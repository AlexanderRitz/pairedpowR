#' Paired t-test
#'
#' Performs a one-sided paired t-test.
#'
#' @param data list constructed by "simdat" function. Supplies data to apply
#' t-test on.
#' @param sig numeric. Supplies the significance level for the test.
#' @return Returns the p-value of the performed test.
#'
#'
#' @examples
#' \donttest{
#' data <- simdat(npairs = 10, eff = 0, rho = -0.8)
#' test <- paired_test(data = data)
#' test
#' }
#'
#' @export

paired_test <-  function(data = NULL, sig = 0.05) {

  n <- length(data$y_Tr)
  diffs <- data$y_Tr - data$y_NTr
  mu_D <- mean(diffs)
  S <- sd(diffs)
  T_stat <- (mu_D) / (S / sqrt(n))
  return(1 - pt(T_stat, df = n - 1))
}
