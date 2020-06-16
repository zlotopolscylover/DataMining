#kmeans(,liczba_srodkow,liczba_iteracji)
dane <- dget("kwadraty.txt")
ksrednich <- kmeans(dane,4,100)
skupienia <- ksrednich$cluster
centra <- ksrednich$centers #wspolrzedne srodkow skupienia
plot(dane)
plot(dane,col=skupienia)#jak algorytm wybra³ te skupienia
#wydaje siê, ¿e w miare okej
#wierzymy, ¿e moze tak byæ

#mozemy jeszcze zobaczyc gdzie s¹ punkty centralne
points(centra,pch=16)

#to s¹ sumy kwadratów odleg³oœci wewn¹trz skupieñ
ksrednich$withinss

#jak chcemy mieæ ogóln¹ odleg³oœæ, czyli posumowaæ po wszystkich skupieniah 
#suma po wszystkich
ksrednich$tot.withinss
#to samo co
sum(ksrednich$withinss)

#mozemy zobaczy ile jest elementów w poszczególnych skupieniach
ksrednich$size
#iektore obserwacje nam przeskoczyly do innego skupienia

#metoda hierarchiczna:
hierarchia <-  hclust(dist(dane),method="single")
plot(hierarchia)
skupienia <- cutree(hierarchia,k=4)
plot(dane,col=skupienia)

#method ="single", "average"
