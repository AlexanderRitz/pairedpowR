#' Modified paired samples t-test
#'
#' Performs a one-sided paired samples t-test modified according to Zimmerman
#' (2005; 2012).
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
#' test <- mod_test(data = data)
#' test
#' }
#'
#' @export

mod_test <- function(data = NULL, sig = 0.05) {

  rho <- cor(data$y_Tr, data$y_NTr)
  n_1 <- length(data$y_Tr)
  n_2 <- n_1
  mu_1 <- mean(data$y_Tr)
  mu_2 <- mean(data$y_NTr)
  S <- sqrt(((n_1 - 1)*sd(data$y_Tr)^2 + (n_2 - 1)*sd(data$y_NTr)^2)/(n_1 + n_2 -2))
  T_stat <- ((mu_1 -  mu_2) / (S*(sqrt(1/n_1 + 1/n_2)))) / sqrt(1 - rho)
  return(1 - pt(T_stat, df = (n_1 + n_2 - 2)))
}