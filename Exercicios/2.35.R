a<- 0.2050 
b<- 0.2150
#1
media = (a+b)/2

1 - punif(0.21, a, b)
#2

k = qunif(0.95, a, b) 

#3
z = qunif(0.9,a, b) 

#4
ex = ((a-b)^2)/12

o=sqrt(ex)
