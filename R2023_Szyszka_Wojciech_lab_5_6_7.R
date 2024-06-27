#------------------------------------------
# Laboratorium 5 - sekcja 3.2, zadania 1-4
#------------------------------------------

#Zadanie 1 - profil pionowy wiatru

par(mfrow=c(1,1))
wysokosc1=c(80.6,60.9,40.90)
srednia1=c(mean(dane_w2$CH4Avg),mean(dane_w2$CH5Avg),mean(dane_w2$CH6Avg))
plot(wysokosc1,srednia1)
plot(dane_w2$CH4Avg,type = 'l', col="salmon")
lines(dane_w2$CH1Avg, type='l',col='green4')
plot(dane_w2$CH4Avg, type='l', col='aquamarine')
lines(dane_w2$CH5Avg, type='l', col='navyblue')
lines(dane_w2$CH6Avg, type='l', col='green4')

#Zadanie 2a - Wzór/wykładnik Hellmana-Suttona

#pierwsze

h4=80.60
h5=60.90
h6=40.90
help("log")
omega=log(mean(dane_w2$CH4Avg)/mean(dane_w2$CH6Avg), base=h4/h6)
omega

#drugie

teo_sr_pred_CH5=mean(dane_w2$CH4Avg)*(h5/h4)^(omega)
teo_sr_pred_CH5
mean(dane_w2$CH4Avg)

#trzecie

teo_sr_pred_130m=mean(dane_w2$CH4Avg)*(130/h4)^(omega)
teo_sr_pred_130m

#czwarte i piąte

f_pred_wiatr=function(h){
  teo_sr_pred=mean(dane_w2$CH4Avg)*(h/h4)^(omega)
  return(teo_sr_pred)
}
wysokosci=c(10:130)
plot(sapply(wysokosci,f_pred_wiatr),type='l',col='tomato4')
points(h4,mean(dane_w2$CH4Avg),col='navyblue',pch=11)
points(h5,mean(dane_w2$CH5Avg),col='green4',pch=11)
points(h6,mean(dane_w2$CH6Avg),col='salmon3',pch=11)

#Zadanie 2b - prawo logarytmiczne profilu wiatru

z0=1.3
#drugie
f_log_pred_wiatr=function(h){
  teo_log_sr_pred=mean(dane_w2$CH4Avg)*log(h/z0,exp(1))/log(h4/z0,exp(1))
  return(teo_log_sr_pred)
}
plot(sapply(wysokosci,f_log_pred_wiatr),type='l',col='tomato4')
points(h4,mean(dane_w2$CH4Avg),col='navyblue',pch=11)
points(h5,mean(dane_w2$CH5Avg),col='green4',pch=11)
points(h6,mean(dane_w2$CH6Avg),col='salmon3',pch=11)
lines(sapply(wysokosci,f_pred_wiatr),type='l',col='seagreen4')

#Zadanie 3 - różnica wskazań vane’ów

vane1h=78.30
vane2h=38.60
#pierwsze
plot(dane_w2$CH7Avg,type='l',col='tomato4')
lines(dane_w2$CH8Avg,type='l',col='green4')
#drugie
roznica=dane_w2$CH7Avg-dane_w2$CH8Avg
plot(roznica,type='l',col='tomato4')
#trzecie
a=dane_w2$CH7Avg[dane_w2$CH6Avg>3.0]
b=dane_w2$CH8Avg[dane_w2$CH6Avg>3.0]
c=abs(a-b)
srednia_roznica=(mean(c[c<=180])+mean(c[c>180]-180))/2
srednia_roznica

#Zadanie 4 - histogram i rozkład prawdopodobieństwa
par(mfrow=c(1,2))
summary(dane_w2$CH4Avg)
help("hist")
hist(dane_w2$CH4Avg,breaks=seq(from=0.2,to=15.7,by=0.5))
install.packages("EnvStats")
library("EnvStats")
help("eweibull")
eweibull(dane_w2$CH4Avg)
curve(dweibull(x, shape=2.379476, scale = 6.468442), from=0, to=16)
install.packages("mixdist")
library("mixdist")
weibullparinv(shape=2.379476, scale=6.468442, loc = 0)
mean(dane_w2$CH4Avg)
# Postawianie do kalkulatorka i uzyskiwanie różnych wyników
# których chyba nie trzeba tu przepisywać?

#-----------------------------------------
#Laboratorium 6 - sekcja 6, zadanie 1 i 2
#-----------------------------------------
library(lattice)
# Zadanie 1
#a
sector = NULL
sector[dane_w2$CH7Avg<=45 | dane_w2$CH7Avg>315]='N'
sector[dane_w2$CH7Avg>45 & dane_w2$CH7Avg<135]='E'
sector[dane_w2$CH7Avg>=135 & dane_w2$CH7Avg<225]='S'
sector[dane_w2$CH7Avg>=225 & dane_w2$CH7Avg<=315]='W'
sector
#b
any(is.na(sector))
dane = data.frame(date = dane_w2$`Date & Time Stamp`,anemo_80m = dane_w2$CH4Avg, anemo_60m = dane_w2$CH5Avg,anemo_40m = dane_w2$CH6Avg, kierunek_80m = dane_w2$CH7Avg, sektor= sector)
head(dane)

# Zadanie 2
#a

xyplot(anemo_80m+anemo_60m+anemo_40m~date,
       data=dane, type = 'l', auto.key = TRUE,
       xlab = 'data', ylab = 'Średnia', main ="Wykres 1")

#b

xyplot(anemo_80m~date|sector, data = dane,xlab = 'Data',ylab = 'Średnia')

#c

xyplot(anemo_80m+anemo_60m+anemo_40m~date|sector, data = dane,xlab = 'Data',ylab = 'Średnia')

#d

xyplot(anemo_80m+anemo_60m+anemo_40m~date, groups = sector, data = dane,auto.key = TRUE)

#e

bwplot(dane$anemo_80m)

#f

bwplot(anemo_80m~sector, data = dane)

#g

pairs(dane[2:4])
cor(dane[2:4])

#h

set.seed(1)
qqnorm(dane$anemo_80)

#i
library(qqmath)
qqmath(dane$ anemo_80, distribution = function(p) qweibull(p, shape = 2.379476, scale = 6.468442))


#-------------------------------------------
#Laboratorium 7 - sekcja 5, zadanie 1,2 i 3
#-------------------------------------------

library("ggplot2")

#Zadanie 5.1
#---------------utworzenie dwóch funkcji--------------------
#---------------powinny być znane z poprzedniego laboratorium
#---------------można przekopiować, żeby móc działać
v_hell=function(h)
{4.786089*(h/40.9)^0.267734
}
v_log = function(h)
{4.786089*log(h/1.356629)/log(40.9/1.356629)
}

#------------dane do wykresów
wysokosci_pomiar=c(40.9,60.9,80.9)
predkosci_pomiar=c(4.786089,5.296304,5.739292)

h=c(10:130)
ramka_wzorki=data.frame(wysokosci=h,Vhell=v_hell(h),Vlog=v_log(h))
ramka_pomiar=data.frame(wysokosci=wysokosci_pomiar,predkosci=predkosci_pomiar)
#------------ wykres podstawowy
ggplot()+
  geom_line(data=ramka_wzorki,aes(x=wysokosci,y=Vhell ,colour="Vhell"),linewidth=1 )+
  geom_line(data=ramka_wzorki,aes(x=wysokosci,y=Vlog,colour="Vlog"),linewidth=1) +
  geom_point(data=ramka_pomiar,aes(x=wysokosci,y=predkosci,colour="dane z pomiaru"),size=5,pch="x")+
  scale_color_manual(name = "Legenda", values = c("Vhell" = "darkblue","Vlog" = "red","dane z pomiaru"="darkgreen"))+
  labs(
    title="Profil pionowy wiatru",
    subtitle="Wykres dla prawa Hellmana i prawa logarytmicznego",
    caption = "(na podstawie danych pomiarowych dane_w2.xlsx)",
    x="wysokość [m]",
    y="prędkość [m/s]"
  )+
  scale_x_continuous(limits=c(10,130),breaks=c(10,30,50,70,90,110,130))+
  scale_y_continuous(limits = c(0,7))+
  theme(
    plot.title = element_text(face="bold", colour = 'olivedrab',size=(18)),
    plot.subtitle = element_text(colour = 'olivedrab3'),
    legend.title = element_text(colour = 'green4'),
    axis.title = element_text(colour = 'seagreen4'),
    axis.text = element_text(colour = 'seagreen3'),
    panel.background = element_rect(fill='wheat'),
    panel.grid.major = element_line(color = 'wheat4'),
    panel.grid.minor = element_line(color = 'wheat3')
  )
#Zadanie 5.2

x<-seq(-3.1,5.1, by =.01)
y1=dnorm(x)
y2=dnorm(x,sd=2)
y3=dnorm(x,sd=1/2)
y4=dnorm(x,mean=2)

xx=as.data.frame(cbind(x,y1, y2, y3, y4))

ggplot(xx)+
  geom_line(aes(x,y1,color="N(0,1)"),size=2)+
  geom_line(aes(x,y2,color="N(0,4)"),size=2)+
  geom_line(aes(x,y3,color="N(0,1/4)"),size=2)+
  geom_line(aes(x,y4,color="N(2,1)"),size=2)+
  scale_color_manual(name="opis",
                     breaks=c('N(0,1)', 'N(0,4)', 'N(0,1/4)', 'N(2,1)'),
                     values=c('N(0,1)'='blue', 'N(0,4)'='red', 'N(0,1/4)'='green', 'N(2,1)'='orange'))+ 
  theme(
    axis.text.x = element_text(size = 18),
    axis.text.y.left = element_text(size = 18),
    axis.text.y = element_text(size = 18, angle = 90, hjust = 1.0, vjust = 0.0),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
    panel.background = element_rect(fill = "white", color = "black", size = 1),
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_line(color = "lightgray"),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 12),   
    legend.title.align = 0.5,  
    legend.position = c(0.8, 0.75), 
    legend.box.background = element_rect(color = "purple", fill = "lightblue", size = 2),
    legend.key = element_rect(fill = "cyan"),
    legend.background = element_rect(fill = "lightblue")  
  )

#Zadanie 5.3

library(readxl)
dane_w2 <- read_excel("C:/Users/wojti/Desktop/Pakiet R Laby/dane_w2.xlsx", 
                      col_types = c("date", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric"))
sektor=NULL
sektor[which(dane_w2$CH7Avg<=45 | dane_w2$CH7Avg>315)]="N"
sektor[which(dane_w2$CH7Avg>45 & dane_w2$CH7Avg<=135)]="E"
sektor[which(dane_w2$CH7Avg>135 & dane_w2$CH7Avg<=225)]="S"
sektor[which(dane_w2$CH7Avg>225 & dane_w2$CH7Avg<=315)]="W"
wiatry=data.frame(date=dane_w2$`Date & Time Stamp`,anemo_80m=dane_w2$CH4Avg,anemo_60m=dane_w2$CH5Avg,anemo_40m=dane_w2$CH6Avg,kierunek_80m=dane_w2$CH7Avg,sector=sektor)
head(wiatry)

ggplot(data=wiatry, aes(x=anemo_80m, y=anemo_60m,color=sector))+
  geom_point(size=0.5)+
  facet_grid(~sector)+
  scale_color_manual(values=c("#125333", "#E69F00", "#56B4E9","#877214"))+ 
  stat_ellipse(color="black", level=0.5)+
  theme(
    legend.position = 'none'
  )
