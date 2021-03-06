#kmeans(,liczba_srodkow,liczba_iteracji)
dane <- dget("kwadraty.txt")
ksrednich <- kmeans(dane,4,100)
skupienia <- ksrednich$cluster
centra <- ksrednich$centers #wspolrzedne srodkow skupienia
plot(dane)
plot(dane,col=skupienia)#jak algorytm wybra� te skupienia
#wydaje si�, �e w miare okej
#wierzymy, �e moze tak by�

#mozemy jeszcze zobaczyc gdzie s� punkty centralne
points(centra,pch=16)

#to s� sumy kwadrat�w odleg�o�ci wewn�trz skupie�
ksrednich$withinss

#jak chcemy mie� og�ln� odleg�o��, czyli posumowa� po wszystkich skupieniah 
#suma po wszystkich
ksrednich$tot.withinss
#to samo co
sum(ksrednich$withinss)

#mozemy zobaczy ile jest element�w w poszczeg�lnych skupieniach
ksrednich$size
#iektore obserwacje nam przeskoczyly do innego skupienia

#metoda hierarchiczna:
hierarchia <-  hclust(dist(dane),method="single")
plot(hierarchia)
skupienia <- cutree(hierarchia,k=4)
plot(dane,col=skupienia)

#method ="single", "average"
