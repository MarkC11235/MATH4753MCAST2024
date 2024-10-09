#' This function calculates the number of tickets to sell for a flight
#'
#' @param N number of seats on the flight
#' @param gamma probability of the plane being truly overbooked
#' @param p probability of a passenger showing up
#'
#' @export
#'
#' @examples
#' myntickets(400, 0.02, 0.95)
#' myntickets(200, 0.02, 0.95)
#' myntickets(400, 0.01, 0.90)

myntickets <- function(N = 400, gamma = 0.02, p = 0.95){
  # Discrete
  #############################################
  dis_range = N:(floor(N+N/10))

  # calculate the number of tickets to sell (nd) using appropriate discrete distribution (binomial)
  obj_discrete = 1 - gamma - stats::pbinom(N, dis_range, p)
  # find index where n is 0 and add N to get the number of tickets to sell
  nd = dis_range[which.min(abs(obj_discrete))]


  plot(dis_range, obj_discrete,
       xlab = "Number of Tickets Sold", ylab = "Objective Function",
       main = paste("Objective vs n to find optimal tickets sold\n (", nd, ") gamma = ", gamma, " N = ", N, "discrete"),
       type = "p", pch = 16, col = "blue")
  graphics::lines(dis_range, obj_discrete)
  # add a vertical line at the optimal number of tickets
  graphics::abline(v = nd, col = "red")
  # add a horizontal line at 0.0
  graphics::abline(h = 0.0, col = "red")
  #############################################

  # continuous
  #############################################
  cont_range = seq(N, floor(N+(N/10)), by = 0.001)
  # need to have the + 0.5 to adjust for switch from discrete to continuous
  obj_continuous = 1 - gamma - stats::pnorm(N + 0.5, cont_range*p, sqrt(cont_range*p*(1-p)))

  # calculates the number of tickets to sell (nc) using the normal approximation
  nc = cont_range[which.min(abs(obj_continuous))]

  plot(cont_range, obj_continuous, type = "l", xlab = "Number of Tickets Sold", ylab = "Objective Function",
       main = paste("Objective vs n to find optimal tickets sold\n (", nc, ") gamma = ", gamma, " N = ", N, "continuous"))

  # add a vertical line at the optimal number of tickets
  graphics::abline(v = nc, col = "blue")
  # add a horizontal line at 0.0
  graphics::abline(h = 0.0, col = "blue")
  #############################################

  # prints a named list containing nd, nc, N, p and gamma
  print(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}

