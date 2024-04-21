#1
f <- function(x) {
  ifelse(x >= 0 & x < 5, (1/10),
  ifelse(x >= 5 & x < 10, ((10 - x)/25),0))
}


integral <- integrate(f, lower = -Inf, upper = +Inf)$value



##2



F <- function(x) {
  if (x < 0) {
    return(0)
  } else if (x < 5) {
    integral <- integrate(f, lower = 0, upper = x)
    return(integral$value)
  } else if (x < 10) {
    integral <- integrate(function(t) {ifelse(t < 5, 1/10, (10 - t)/25)}, lower = 0, upper = x)
    return(integral$value)
  } else {
    return(1)
  }
}

x_values <- c(-1,0,4,5,10,11)
for (x in x_values) {
  cat("F(", x, ") =", F(x), "\n")
}

##3
(1-F(7))/(1-F(4))

#OU#
1-((1/2) + ((70-24.5-37.5)/25))

1-(4/10)

##4


fmedia <- function(x) {
  ifelse(x >= 0 & x < 5, x*(1/10),
         ifelse(x >= 5 & x < 10, x*((10 - x)/25),0))
}


integralmedia <- integrate(fmedia, lower = -Inf, upper = +Inf)$value


