# zadanie 10.1 

f <- function(x){
  4.26*(exp(-x) - 4*exp(-2*x) + 3*exp(-3*x))
}

n<- 100
bledy <- rnorm(n, 0, 0.1 )
x <- sort(runif(n,0, 3.5))
y <- f(x) + bledy 
y_prawdziwe <- f(x)
plot(x,y)
lines(x, y_prawdziwe)

#kubiczny tzn wielomiany 3 stopnia
# spline: wazne jest ze w wezlach chcemy to gladko sklejac
#przyblzamy na kawalkach wielomianami 

# a)

#jadrowo 

est.jadrowy <- ksmooth(x,y)
lines(est.jadrowy, col = "red")

# lokolmnie wilomianowy 
est.wiel <- loess((y  ~ x ))
lines(x, est.wiel$fitted, col="blue", lwd = 2 )

#smooth spline 

est.smooth <- smooth.spline(x,y, df = 3)
lines(est.smooth, col = "green", lwd = 2)

#inaczej
library(splines)
est.spline <- lm(y~bs(x,knots=c(0.5,1.5,2.5))) #bs nak³ada spline na x?
#tutaj budujemy model liniowy i nak³adamy na t¹ funkcjê jak¹œ zaleznosc
#i to jest wlasnie ten spline i mamy mozliwosc dodawania wezlow
#bs -domyslnie ma degree=3 czyli kubiczny
lines(x,est.spline$fitted.values, col="yellow",lwd=2)

#b³ad z podpunktu b: liczymy miedzy f_^(xi) a y, ten blad to kwadrat czegos tam
ISE.jadrowy <- (n^(-1))*sum((y_prawdziwe-est.jadrowy$y)^2)
ISE.wiel <- n^(-1)*sum((y_prawdziwe-est.wiel$fitted)^2)
ISE.kub <- n^(-1)*sum((y_prawdziwe-est.spline$fitted)^2)

#c
est.wiel.01 <- loess(y~x,span=0.1)
plot(x,y)
lines(x,est.wiel.01$fitted,col="yellow",lwd=2)

#d

#10.2.

#chcemy zbudowac model podobny do liniowego,ale x obkladamy funkcja ale nadal prosta

#MARS polega na tym ze zaczynamy od pustego modelu(maj¹c tylko sta³¹)
#dodajemy kolejne hm-y 
#kolejno (x,(x1-x1i)+,(x1i-x)+) to daje kolejne modele
#nie chcemy dodawaæ kolejnych (x2-x2i)+ tylko patrzymy na iloczyny
#robimy cos takiego: nowa*jakis hm ktory ju¿ jest w modelu poprzednim jakims
#najczesniej wybierzemy za duzo podzialow
#dopasowujemy za du¿y model i stosujemy selekcje wstecz(BIC,Akkaike) moze byc cv
#powstaja kszta³ty nieliniowe

#z X1
install.packages("earth")
library(earth)
n <- 1000
p <- 50 #u mnie p=1
x <- rnorm(n,0,1)
x <- sort(x)
eps <- rnorm(n,0,0.1)
y <- ifelse(x>0,x,0) +ifelse(x>1,x,0) +eps
plot(x,y)

model.mars <- earth(y~x,degree=2,trace=3)
#degree po to zeby nie miec podzialu po kolejnej zmiennej? 
#degree po to zeby nie mnozyc np. (x4-x4i)+ (x2-x2i)+ jeszcze z (x1-x1i)+
#trace po to zeby zobaczyc po kolei te metody podiza³u?
summary(model.mars)
x <- sort(x)
y <- ifelse(x>0,x,0) +ifelse(x>1,x,0) +eps
plot(x,y)
#wykres sie nie zmienil, tylko zmieniona kolejnosc
lines(x,model.mars$fitted.values,col="red")
#nie potrzebnie tak duo¿o podzia³ow? bo od zera jest liniowo

#mozna tu tez stosowac tak¹ uogólnion¹ kroswalidacjê 
#patrzymy na model Mn, odejmujemy po kolei zmienne i patrzymy ktory z modeli najmniej 
#GCV(lambda) = sum(y-f_^9xi))^2/(1-M(lambda)/N)^2