#1
f <- function(x) {
  ifelse(x >= 0 & x < 5, 1/10,
         ifelse(x >= 5 & x < 10, (10 - x)/25,
                0))
}

integral <- integrate(f, lower = 0, upper = 10)$value