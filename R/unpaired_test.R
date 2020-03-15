#' Independent Two-samples t-test
#'
#' Performs a one-sided independent two-samples t-test.
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
#' test <- unpaired_test(data = data)
#' test
#' }
#'
#' @export

unpaired_test <- function(data = NULL, sig = 0.05) {

  n_1 <- length(data$y_Tr)
  n_2 <- n_1
  mu_1 <- mean(data$y_Tr)
  mu_2 <- mean(data$y_NTr)
  S <- sqrt(((n_1 - 1)*sd(data$y_Tr)^2 + (n_2 - 1)*sd(data$y_NTr)^2)/(n_1 + n_2 -2))
  T_stat <- (mu_1 -  mu_2) / (S*(sqrt(1/n_1 + 1/n_2)))
  return(1 - pt(T_stat, df = (n_1 + n_2 - 2)))
}
