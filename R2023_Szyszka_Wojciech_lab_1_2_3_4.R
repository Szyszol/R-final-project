#=============LAB 1===============

#zad1

x=c(7,9,13,8,4,2,16,1,6,19,15,12,19,14,8,2,19,11,18,7)
y_1=x[2]
y_2=x[1:5]
y_3=which(x>14)
indx=which(x>-100)
y_4=x[(indx!=6)&(indx!=10)&(indx!=12)]
y_4
x

#zad2

rep(seq(0,6, length=2),3)
seq(1,10,3)
rep(seq(1,3, length=3),4)
#4
#5
seq(1,10,length=3)
#7
#zad3
a=c(2.5,3,1,0,4,-1)
sort(a)
a[order(a)]

#zad4

x=c(7,13,3,8,12,12,20,11)
sr_harm=length(x)/sum(1/x)
sr_harm

#zad5

x=runif(50)
x1=which(x>0.5)
x2=which(x<=0.5)
x[x1]=0
x[x2]=1
x
length(x1)
#powtórzyć proces ileś razy zmieniając parametr w runif - wyniki powinny oscylować w okół
#n/2 więc nie będę przepisywać kodu x razy
#zad6
n=1000
y=sum(1/(seq(1,n,1)))-log(n,base = exp(1))
y

#zad7

n=10000000
p=4*(1+sum(1/(2*seq(2,n,2)+1))-sum(1/(2*seq(1,n-1,2)+1)))
p
pi
p-pi
#=============LAB 2===============

#ZADANIE 1

x=rep(2,5)
y=seq(-1,3, length=5)
z=cbind(x,y)
z[2,]=c(2,4)

#ZADANIE 2

#a
a=c(2,23,8,10,6,90,4,7,12)
A=matrix(a,nrow=3)
#b
B=t(A)
apply(B,1,mean)
apply(B,2,mean)
apply(B,1,prod)
apply(B,2,prod)
#c
sum(B[1,],B[2,])
sum(B[1,])+sum(B[3,])
#d
t(B)
det(B)
diag(B)
sum(diag(B))
#e
B^2
B*B
B%*%B
#f
1/B
B^(-1)
solve(B)
#g
solve(B,c(5,6,7))
#h
indx=which(apply(B,1,sum)>30)
indx

#ZADANIE 3

#a  TAK
iris
#b Parametry sepal i petal(płatek kwiata?) oraz gatunki tych roślin
#c 50 versicolor, średnia 3.057, najmniejsza 2.000
summary(iris)
#d
iris2=subset(iris, Species=="setosa")
#e
sort(iris2$Sepal.Length)
#f
means= apply(iris[, 1:4], 2, mean)
means
#g
sepal_length_means= tapply(iris$Sepal.Length, iris$Species, mean)
sepal_length_means

#ZADANIE 4

#a
c1=c(12220,	6596,	7223,	357,	2239)
c2=c(15310,	6999,	10921,	395,	3111)
c3=c(112958,	40244,	46146,	1247,	7629)
c4=c(8960,	6170,	4661,	279,	4013)
c5=c(12125,	8233,	6653,	553	,3178)
ma=matrix(1:25,nrow=5)
ma
ma=as.data.frame(ma)
str(ma)
names(ma)=c('n. human.','n. ścisłe','medyczne','sportowe','techniczne')
ma
row.names(ma)=c('Bordeaux','Lyon','Paryż','Rennes','Tuluza')
ma
ma[1,]=c1
ma[2,]=c2
ma[3,]=c3
ma[4,]=c4
ma[5,]=c5
ma
#b (a skad wiemy ze jeden student nie jest na dwoch kierunkach?)
liczba_studentow=apply(ma[1:5,],1,sum)
liczba_studentow
posrtowani=ma[order(liczba_studentow),]
posrtowani
#c
liczba_studentow1=apply(ma[,1:5],2,sum)
liczba_studentow1
posrtowani1=ma[,order(liczba_studentow1)]
posrtowani1
#d
ma1=subset(ma, ma$`n. ścisłe`>ma$medyczne)
ma1
ma

#ZADANIE 6

#a
li=list(nazwisko="Duda",imie=c("Jaroslaw","NaN"),miejsce_urodzenia="Kraków")
li
#b
a=c(2,23,8,10,6,90,4,7,12)
A=matrix(a,nrow=3)
B=t(A)
B
res=eigen(B)
res$values
res$vectors
solve(res$vectors,c(0,0,0)) #liniowo niezalezne wiec diagonalizowalna
#c 
library(matlib)
# Wymiary macierzy A
n1= 10
n2= 50
# Tworzenie macierzy A dla n=10 i n=50
A1= B
A2= B
# Diagonalizacja macierzy A
eigen_A1= eigen(A1)
eigen_A2= eigen(A2)
# Macierze P i D dla n=10 i n=50
P1= eigen_A1$vectors
D1= diag(eigen_A1$values)
P2= eigen_A2$vectors
D2= diag(eigen_A2$values)
# Obliczenie potęgi macierzy diagonalnej D dla n=10 i n=50
Dn1=D1^n1
Dn2= D2^n2
# Obliczenie An dla n=10 i n=50
An1= P1 %*% Dn1 %*% solve(P1)
An2= P2 %*% Dn2 %*% solve(P2)
An1
An2
#=============LAB 2,5(d'Hondt i ten drugi)===============

x=c(241790,70054,60938,38080,19750)
w2=x/2
w3=x/3
w4=x/4
w5=x/5
w6=x/6
w7=x/7
w8=x/8
wyniki=rbind(x,w2,w3,w4,w5,w6,w7,w8)
order(wyniki,decreasing=TRUE)
wyniki
order(wyniki,decreasing=TRUE)[1:11]
table(ceiling(order(wyniki,decreasing=TRUE)[1:11]/8))

#Druga wersja

x1=c(232799,232430,127693,83633,58435)
x2=x1/2
x3=x1/3
x4=x1/4
x5=x1/5
x6=x1/6
x7=x1/7
x8=x1/8
wyniki1=rbind(x1,x2,x3,x4,x5,x6,x7,x8)
order(wyniki1,decreasing=TRUE)
wyniki1
order(wyniki1,decreasing=TRUE)[1:14]
table(ceiling(order(wyniki1,decreasing=TRUE)[1:14]/8))

#Nie metoda d'Hondta

a1=c(241790,70054,60938,38080,19750)
a2=a1/3
a3=a1/5
a4=a1/7
a5=a1/9
a6=a1/11
a7=a1/13
a8=a1/15
wyniki2=rbind(a1,a2,a3,a4,a5,a6,a7,a8)
order(wyniki2,decreasing=TRUE)
wyniki2
order(wyniki2,decreasing=TRUE)[1:11]
table(ceiling(order(wyniki2,decreasing=TRUE)[1:11]/8))

#2 nie d'Hondt

b1=c(232799,232430,127693,83633,58435)
b2=b1/3
b3=b1/5
b4=b1/7
b5=b1/9
b6=b1/11
b7=b1/13
b8=b1/15
wyniki3=rbind(b1,b2,b3,b4,b5,b6,b7,b8)
order(wyniki3,decreasing=TRUE)
wyniki3
order(wyniki3,decreasing=TRUE)[1:14]
table(ceiling(order(wyniki3,decreasing=TRUE)[1:14]/8))

#=============LAB 3===============

#ZADANIE 1

y=runif(50)
y
n<-50
for(i in 1:n)
{
  if(y[i]<0.5)
  {
    y[i]=0
  }else
  {
    y[i]=1
  }
}
y
z=runif(50)
z
i=0
while (i<n) {
  i=i+1
  if(z[i]<0.5)
  {
    z[i]=0
  }else
  {
    z[i]=1
  }
  
}
z
a=runif(50)
i=0
repeat{
  if(i>=n){
    break
  }
  i=i+1
  if(a[i]<0.5)
  {
    a[i]=0
  }else
  {
    a[i]=1
  }
}
a

#ZADANIE 2a

u=NULL
n=29
u[1]=10
for (i in 1:n) {
  u[i+1]=sqrt(u[i])
  
}
u

#ZADANIE 2b

u=NULL
i=1
eps=1/10000
u[1]=10
u[2]=sqrt(u[1])
while (abs(u[i+1]-u[i])>eps) {
  i=i+1
  u[i+1]=sqrt(u[i])
}
u
length(u)

#ZADANIE 2c

u=NULL
n=29
i=1
u[1]=10
repeat{
  if(i>=n)
  {break}
  u[i+1]=sqrt(u[i])
  i=i+1
}
u

u=NULL
i=1
u[1]=10
u[2]=sqrt(u[1])
repeat{
  if(abs(u[i+1]-u[i])<eps)
  {break
  }
  i=i+1
  u[i+1]=sqrt(u[i])
}
u
length(u)

#ZADANIE 3

f_rok_przestepny=function(rok) {
  if ((rok %% 4 == 0 && rok %% 100 != 0) || rok %% 400 == 0) {
    return(paste("Rok", rok, "jest rokiem przestępnym"))
  } else {
    return(paste("Rok", rok, "nie jest rokiem przestępnym"))
  }
}
przyklady=  c(1901, 2004, 2100, 2000)
for (rok in przyklady) {
  wynik= f_rok_przestepny(rok)
  wynik
}

#ZADANIE 4

f_trojkat_pascala = function(N) {
  trojkat_pascala = matrix(0, nrow = N, ncol = N)
  trojkat_pascala[1, 1] = 1
  trojkat_pascala[2, 1:2] = 1
  
  for (i in 3:N) {
    trojkat_pascala[i, 1] = 1
    for (j in 2:i) {
      trojkat_pascala[i, j] = trojkat_pascala[i - 1, j - 1] + trojkat_pascala[i - 1, j]
    }
    trojkat_pascala[i, i] = 1
  }
  
  return(trojkat_pascala)
}
N = 10
wynik = f_trojkat_pascala(N)
wynik

#ZADANIE 5

f_palindrom= function(slowo) {
  dlugosc= nchar(slowo)
  palindrom= TRUE
  
  for (i in 1:(dlugosc/2)) {
    if (substr(slowo, i, i) != substr(slowo, dlugosc - i + 1, dlugosc - i + 1)) {
      palindrom <- FALSE
      break
    }
  }
  
  if (palindrom) {
    return(paste(slowo, "jest palindromem"))
  } else {
    return(paste(slowo, "nie jest palindromem"))
  }
}

f_palindrom("RADAR")
f_palindrom("kajak")
f_palindrom("ciasto")
f_palindrom("owocowo")
f_palindrom("lato")
#=============LAB 4===============

#ZADANIE 1

#Zrobiłem funkcją w LAB3

#ZADANIE 2

#a
silnia = function(n) {
  if (n < 0) {
    return("Silnia jest zdefiniowana dla liczb Naturalnych!")
  }
  wynik = 1
  for (i in 1:n) {
    wynik = wynik * i
  }
  return(wynik)
}
wynik_silnia = silnia(4)
paste("4! =", wynik_silnia)
#b
a=c(1:10)
wyniki_silnia=sapply(a,silnia)
wyniki_silnia
#c
newton = function(n, k) {
  symbol_newtona = silnia(n) / (silnia(k) * silnia(n - k))
  return(symbol_newtona)
}
n=5
k=2
wynik_newton = newton(n, k)
paste("Symbol Newtona (", n, "nad", k, ") =", wynik_newton)

#ZADANIE 3
#!is.integer(n) nie działa nie mam pojęcia dlaczego, ale działa z n%%1!=0

potega = function(A, n) {
  if (!is.matrix(A) || n < 0 || n %% 1 != 0) {
    return("Macierz musi być kwadratowa, a potęga musi być liczbą naturalną!")
  }
  
  rozmiar = dim(A)
  if (rozmiar[1] != rozmiar[2]) {
    return("Macierz nie jest kwadratowa!")
  }
  
  wynik = A
  for (i in 2:n) {
    wynik = wynik %*% A
  }
  
  return(wynik)
}
A = matrix(c(1, 2, 3, 4), nrow = 2)
n = 3
wynik_potega = potega(A, n)
wynik_potega

#ZADANIE 4

#a

ciagi = function(N) {
  a= numeric(N)
  b= numeric(N)
  
  a[1]= 1
  b[1]= 2
  
  for (n in 1:(N-1)) {
    a[n + 1]= a[n] + (b[n]^2)
    b[n + 1]= sqrt(a[n] * b[n])
  }
  
  wyniki= list(an = a, bn = b)
  return(wyniki)
}
ciagi(10)$an[10]
ciagi(10)$bn[10]

#b i c
x_an= 1:20
y_an=ciagi(20)$an
plot(x_an, y_an, type = "l", col = "red", xlab = "X", ylab = "Y", main = "Ciąg (an)")
y_bn=ciagi(20)$bn
plot(x_an, y_bn, type = "l", col = "blue", xlab = "X", ylab = "Y", main = "Ciąg (bn)")

#ZADANIE 5

czy_pierwsza=function(n) {
    if(n %% 1 != 0 || n<0){
      return("Podana liczba nie jest naturalna!")
    }
    pierwsza=n
    g=n
    for(g in 2:sqrt(g))
    {
      if(pierwsza%%g==0){
        pierwsza=pierwsza-1
      }
    }
    if(pierwsza==n){
      return("Podana liczba jest pierwsza!")
    }    
    if(pierwsza!=n){
      return("Podana liczba nie jest pierwsza!")
    }
  
}

czy_pierwsza(2^2^1+1)
czy_pierwsza(2^2^2+1)
czy_pierwsza(2^2^3+1)
czy_pierwsza(2^2^4+1)
czy_pierwsza(2^2^5+1)
