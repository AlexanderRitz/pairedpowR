#' Simulation of normal paired data
#'
#' Simulates samples of normally distributed paired data according to specified
#' sample size, intra-pair correlation and effect size of a given treatment.
#'
#' @param npairs integer. Supplies the number of pairs to be simulated.
#' Resulting in two samples of size "npairs".
#' @param eff numeric. Supplies the difference in means between both groups.
#' Can be interpreted as effect size based on the fact that standard normal
#' variates are the basis of simulation.
#' @param rho numeric. Supplies the correlation coefficient between both groups.
#' @return A sample of paired data, i.e. two samples of size "npairs" with
#' theoretical correlation coefficient "rho". Realised empirical correlation
#' may vary and is not guaranteed to be an exact match of "rho".
#'
#'
#' @examples
#' \donttest{
#' data <- simdat(npairs = 10, eff = 0, rho = -0.8)
#' cor(data$y_Tr, data$y_NTr)
#' }
#'
#' @export


simdat <- function(npairs = NULL, eff = NULL, rho = NULL){

  mu <- c(0, 0)
  sigma <- matrix(c(1, rho, rho, 1), 2)

  G1 <- MASS::mvrnorm(n = (npairs), mu = mu, Sigma = sigma)

  choice <- rbinom(npairs, 1, 0.5)

  negchoice <- (choice - 1) * (-1)

  choice[which(choice == 0)] <- 2
  negchoice[which(negchoice == 0)] <- 2

  NTr <- rep(0, times = npairs)
  Tr <- rep(0, times= npairs)
  for (i in 1:npairs) {
    kTr <- choice[i]
    kNTr <- negchoice[i]

    NTr[i] <- as.vector(G1[i, kNTr])
    Tr[i] <- as.vector(G1[i, kTr])
  }

  yield_Tr <- 20 + eff + Tr
  yield_NTr <- 20 + NTr

  data <- data.frame(y_Tr = yield_Tr, y_NTr = yield_NTr)
  return(data)
}
