#Ejercicio 1 
#Inciso a 
a<-seq(20,1)
#Inciso b
b<-c(seq(1:20),seq(19,1))
#Inciso c
r<-rep(c(4,6,3),len=31)

#Ejercicio 2
set.seed(50)
xVec<-sample(0:999,250,replace=T)
yVec<-sample(0:999,250,replace=T)

#Inciso a
y<-yVec[-1]
x<-xVec[-length(xVec)]
c<-y-x

#Inciso b
B<-x[-1]
A<-x[-length(x)]
C<-xVec[-1];CC<-C[-1]
D<-A+(2*B)-CC

#Ejercicio 3
nula<-matrix(NA,nrow=5,ncol=5)
M<- matrix(c(10,-10,10),nrow=15,ncol=3,byrow=TRUE)

#Ejercicio 4
Q <- matrix(0, nrow = 5, ncol = 5)
Q[row(Q) == col(Q)+1] <- 1
Q[row(Q)+1 == col(Q)] <- 1

