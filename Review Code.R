
b <- 0
f <- function(x) {
  ifelse(x > 0 & x <= 4, x/20,
         ifelse(x > 4 & x <= b, x/40,
                0))
}



while (TRUE) {
  integral_temp <- integrate(f, lower = 0, upper = b)$value
  if (abs(integral_temp - 1) < 0.0001) {
    break
  }
  b <- b + 0.001
}

print(b)


