f1 <- function (x){
  x*x^2
}

f2 <- function (x){
  x*x
}
f3 <- function (x){
  x*(1/12)
}

media <- (
  0 +
    integrate(f1, lower=-1, upper=0)$value +
    integrate(f2, lower=0, upper=1)$value +
    integrate(f3, lower=1, upper=3)$value +
    0
)

FRACTION::fra(0 +
                integrate(f1, lower=-1, upper=0)$value +
                integrate(f2, lower=0, upper=1)$value +
                integrate(f3, lower=1, upper=3)$value +
                0)


#v[x]

f11 <- function (x){
  (x^2)*x^2
}

f22 <- function (x){
  (x^2)*x
}
f33 <- function (x){
  (x^2)*(1/12)
}

 media2<- (
  0 +
    integrate(f11, lower=-1, upper=0)$value +
    integrate(f22, lower=0, upper=1)$value +
    integrate(f33, lower=1, upper=3)$value +
    0
)

 FRACTION::fra(media2-media^2)
 
 #v[1- x/2]
 (0.25) *(media2-media^2) 
 