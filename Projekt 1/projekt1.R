library(survival)
library(KMsurv)
library(survMisc)
library(foreign)
library(rms)
library(dplyr)

setwd("C:\\Users\\AS\\Desktop\\SMAD sem 2\\biostata")

df <- read.dta("gbcs_short.dta")

head(df)
summary(df)

# ===== KTORE ZE ZMIENNYCH MAJA WPLYW NA CZAS PRZEZYCIA BEZ NAWROTU CHOROBY? =====

### 1 ###
# WYKRESY/STATYSTYKI OPISOWE 
# wplyw zmiennych na czas przezycia

km <-survfit(Surv(df$rectime, df$censrec) ~ 1)
plot(km) #to jest funkcja przezycia 
plot(km, mark="+") #to jest funkcja przezycia z zaznaczonymi danymi cenzurowanymi

plot(km, fun="event") #to jest' estymator KM
plot(km, fun="cumhaz") #to jest funkcja skumulowanego hazardu

summary(km)
mediana <- min(km$time[1-km$surv>=0.5])

#dzielimy sobie na grupy teraz wzgledem zmiennych skategoryzowanych

#menopauza
#1 - nie, 2 - tak
km_meno <-survfit(Surv(rectime, censrec) ~ meno, data = df)
plot(km_meno, col=1:2)
abline(v=150)
#czarna - 1 - nie
#czerwona - 2 - tak
#Przecinaja sie - trzeba formalny test zeby to stwierdzic


#uzycie terapii hormonalnej
#1 - nie, 2 - tak
km_horm <-survfit(Surv(rectime, censrec) ~ horm, data = df)
plot(km_horm, col=1:2)
abline(v=150)
#czarna - 1 - nie
#czerowna - 2 - tak
#WNIOSEK: mozna tak wstepnie powiedziec, ze uzycie terapii hormonalnej ma istotny wplyw na przezycie
#przy - 2 tak - uzywaniu terapii, pstwo przezycia jest wieksze 


# przeklejone ze strony https://www.labtestsonline.pl/test/receptory-estrogenowe-i-progesteronowe
# Receptory estrogenowe (ER) i receptory progesteronowe (PR) to specyficzne białka 
# obecne w komórkach określonych tkanek organizmu. Funkcją receptorów jest wiązanie 
# estrogenów i progesteronu, żeńskich hormonów płciowych obecnych we krwi, które 
# stymulują wzrost komórek i ich podziały.
# 
# W różnych rodzajach nowotworów sutka stwierdza się w komórkach obecność receptorów 
# estrogenowych i/lub progesteronowych, często o wysokim stężeniu (liczbie). 
# Są to tzw. nowotwory hormono-zależne, których rozrost jest uwarunkowany stężeniem 
# estrogenów i/lub progesteronu we krwi. Tkankę sutka bada się na obecność (ER+, PR+) 
# lub nieobecność tych receptorów (ER-, PR-).

# wskaznik progesteronu
#0 - ujemny, 1 - dodatni
km_prog <-survfit(Surv(rectime, censrec) ~ prog, data = df)
plot(km_prog, col=1:2)
abline(v=150)
#czarna - 0 - ujemny
#czerwona - 1 - dodatni
#WNIOSEK: mozna tak wstepnie powiedziec, ze progesteron ma istotny wplyw na przezycie
#przy - 1 dodatnim wskazniku progesteronu - pstwo przezycia jest wieksze

#wskaznik estrogenu
#0 - ujemny, 1 - dodatni
km_estr <-survfit(Surv(rectime, censrec) ~ estr, data = df)
plot(km_estr, col=1:2)
abline(v=150)
#czarna - 0 - ujemny
#czerwona - 1 - dodatni
#WNIOSEK: tak samo, widzimy istotne roznice pomiedzy tymi oboma grupami
# dodatni wskaznik receptorow estrogenu - wieksze pstwo przezycia

#stopien zroznicowania komorek nowotworu 
# 1 - niski, 2 - sredni, 3-wysoki

# z wikipedii https://pl.wikipedia.org/wiki/Z%C5%82o%C5%9Bliwo%C5%9B%C4%87_histologiczna
# G1 – rak wysokozróżnicowany (niski stopień złośliwości)
# G2 – rak średniozróżnicowany (pośredni stopień złośliwości)
# G3 – rak niskozróżnicowany (wysoki stopień złośliwości)

#UWAGA: grade chyba oznacza jednak stopien zlosliwosci, a nie zroznicowania (?)

km_grade <-survfit(Surv(rectime, censrec) ~ grade, data = df)
plot(km_grade, col=1:3)
abline(v=100)
#WNIOSEK: bez zaskoczenia, wystepuja roznice, oczywiscie im wiekszy stopien zroznicowania komorek nowotworowych
#tym mniejsze prawdopodobienstwo przezycia



#UWAGA: wszelkie przecinania sie tych wykresow trzeba rozgraniczyc na np. wczesna faze i pozna, bo moze sie to w czasie zmieniac
#Tu napisalam tylko takie rzeczy bez przeciec.
#Mozna dodac ze poczatki nie roznia sie za wiele - pierwsze 150dni - te zachowanie w grupach jest takie samo, moze poza ostatnim wykresem, tam bardziej 
#pierwsze 100dni.


#UWAGA: moze byc tak, ze funkcje sie nie krzyzuja, ale sa na tyle blisko siebie, ze moze wystepowac tak naprawde brak istotnych roznic
#dlatego nalezy zastosowac testy statystyczne zeby formalnie sprawdzic czy jakies roznice tam wystepuja czy nie 

### 2 ###
# testy - ocena hipotez dotyczacych roznic w funkcjach przezycia

#test porownania dwoch grup


mod <- comp(ten(Surv(rectime, censrec) ~ meno, data=df))
comp(ten(Surv(rectime, censrec) ~ meno, data=df))
# p-wartosci
#maxAbsZ        Var      Q  pSupBr
#1          9.5143e+00 7.2350e+01 1.1185 0.52508
#n          5.7590e+03 1.6340e+07 1.4247 0.30846
#sqrtN      2.3388e+02 3.2258e+04 1.3022 0.38553
#S1         8.7347e+00 4.3141e+01 1.3299 0.36700
#S2         8.7203e+00 4.2932e+01 1.3309 0.36633
#FH_p=1_q=1 2.5883e+00 2.2613e+00 1.7212 0.17043
#te dwie funkcje sie przecinaly ale p-value testu jest duze wszedzie, a na pewno > 0.05
#wiec stastystycznie widocznie nie jest taka istota roznica pomiedzy tym czy mamy menopauze czy nie mamy


survdiff(Surv(rectime, censrec) ~ meno, data=df)



mod2 <- Surv(df$rectime, df$censrec) ~ df$horm
survdiff(mod2) #zgodnie z oczekiwaniami p-value malutkie
#wystepuja istotne roznice

mod3 <- Surv(df$rectime, df$censrec) ~ df$prog
survdiff(mod3) #zgodnie z oczekiwaniami p-value malutkie
#wystepuja istotne roznice

mod4 <- Surv(df$rectime, df$censrec) ~ df$estr
survdiff(mod4) #zgodnie z oczekiwaniami p-value malutkie
#wystepuja istotne roznice

mod5 <- Surv(df$rectime, df$censrec) ~ df$grade
survdiff(mod5) #zgodnie z oczekiwaniami p-value malutkie
#wystepuja istotne roznice

# WNIOSKI: formalnie za pomoca testow statystycznych zweryfikowalismy to, ze w 
# grupach w czynnikach takie jak horm, prog, estr i grade - wystepuja istotne 
# roznice w funkcjach przezycia
# nie wystepuja dla zmiennej menopauza


### TEST W GRUPACH

fit.km2<-survfit(Surv(rectime, censrec) ~ horm+strata(prog), data=df)

plot(fit.km2,col=1:4)


# progesteron
df.nprog<-df[df$prog==0,]
df.pprog<-df[df$prog==1,]

fit.km2.nprog<-survfit(Surv(rectime, censrec) ~ horm, data=df.nprog)
fit.km2.pprog<-survfit(Surv(rectime, censrec) ~ horm, data=df.pprog)

X11()
par(mfrow=c(1,2))

plot(fit.km2.nprog,col=c(1,2),lwd=2, main="negative prog")
legend("topright",c("non horm","horm"),col=c(1,2),lty=1,lwd=2)

plot(fit.km2.pprog,col=c(1,2),lwd=2, main="positive prog")
legend("topright",c("non horm","horm"),col=c(1,2),lty=1,lwd=2)

# test warstwowy
survdiff(Surv(rectime, censrec) ~ horm+strata(prog), data=df)
# p= 0.005


# oddzielne testy w obu grupach
survdiff(Surv(rectime, censrec) ~ horm, data=df.nprog)
# p= 0.6 
survdiff(Surv(rectime, censrec) ~ horm, data=df.pprog)
# p= 5e-04 
# w grupie kobiet z dodatnim wskaznikiem receptorow progesteronu, terapia hormonalna ma
# pozytywny wplyw na prawdopodobienstwo przezycia, natomiast w grupie z ujemnym 
# wskaznikiem receptorow progesteronu nie ma istotnych roznic w przypadku leczenia
# hormonalnego

# estrogen
df.nestr<-df[df$estr==0,]
df.pestr<-df[df$estr==1,]

fit.km2.nestr<-survfit(Surv(rectime, censrec) ~ horm, data=df.nestr)
fit.km2.pestr<-survfit(Surv(rectime, censrec) ~ horm, data=df.pestr)

X11()
par(mfrow=c(1,2))

plot(fit.km2.nestr,col=c(1,2),lwd=2, main="negative estr")
legend("topright",c("non horm","horm"),col=c(1,2),lty=1,lwd=2)

plot(fit.km2.pestr,col=c(1,2),lwd=2, main="positive estr")
legend("topright",c("non horm","horm"),col=c(1,2),lty=1,lwd=2)

#oddzielne testy w obu grupach
survdiff(Surv(rectime, censrec) ~ horm, data=df.nestr)
# p= 0.2 
survdiff(Surv(rectime, censrec) ~ horm, data=df.pestr)
# p= 0.02 


### TESTY DLA TRENDU ###

km_grade <-survfit(Surv(rectime, censrec) ~ grade, data = df)
plot(km_grade, col=1:3)
# funkcje przezycia ladnie rozdzielone, nie krzyzuja sie
# funkcje przezycia dla 1 najwieksza, dla 3 najmniejsza
# jezeli funkcje przezycia ukladaja sie od najwiekszej do najmniejszej
# to funkjce hazardu ustawiaja sie odwrotnie i wlasnie to mierzy ten test
# porzadek jest tutaj bardzo istotny!!!


grade_test <- survdiff(Surv(rectime, censrec) ~ grade, data = df)

l <- sum((grade_test$obs-grade_test$exp)*(1:3))
m <- sqrt(sum(outer(1:3,1:3,"*")*grade_test$var))

Z<-l/m # test statistic for testing trend, based on log-rank 
# Z = 4.47

pval <- 2*(1-pnorm(Z))
# 7.904387e-06
# odrzucamy H0 na rzecz H1 o tym, ze widoczny trend (im wyzszy stopien zlosliwosci,
# tym mniejsza funkcja przezycia) jest statystycznie  istotny




### 3 ###
#czy testy powinny brac pod uwage wplyw innych czynnikow niz leczenie. 
#czyli czy musimy rozpatrywac:
# 1) size - wielkosc guza 
# 2) nodes - liczbe wezlow chlonnych z przerzutami nowotworu 

min(df$nodes)
max(df$nodes)


quantile(df$nodes, 0.25) #1
quantile(df$nodes, 0.5) #3
quantile(df$nodes,0.75) #7
boxplot(df$nodes)

# grupujemy po liczbie węzłów chłonnych 

df$nodes_gr2<- cut(df$nodes, 
                     breaks = c(0, 3, 51),
                     labels = c(1, 2),
                     include.lowest = TRUE)
df$nodes_gr2 <- as.numeric(df$nodes_gr2)

# sprawdzamy, czy podzial jest ok
df %>%   group_by(nodes_gr2) %>%   summarize(liczba_przypadkow = sum(censrec, na.rm = TRUE))
#  nodes_gr2 liczba_przypadkow
#1         1               119
#2         2               180



krzywa_przezycia_nodes1 <- survfit(Surv(rectime, censrec) ~ 1, 
                                  data=df,
                                  subset = nodes_gr2 == 1,
                                  se.fit = FALSE)

krzywa_przezycia_nodes2 <- survfit(Surv(rectime, censrec) ~ 1, 
                                  data=df,
                                  subset = nodes_gr2 == 2,
                                  se.fit = FALSE)

plot(krzywa_przezycia_nodes1,col="red", xlab = "czas (dni)", ylab = "prawdopodobieństwo przeżycia", 
     main = "Krzywa przeżycia")
lines(krzywa_przezycia_nodes2,col="green")
legend("topright", legend = c("nodes < 3", "nodes > 3"),  
       fill = c("red", "green"))

# testujemy
test_nodes <- survdiff(Surv(rectime, censrec) ~ nodes_gr2, data = df, rho = 0)
print(test_nodes) # p= <2e-16  < 0.05 => odrzucamy H0 => krzywe roznia sie istotnie

# test trendu 
trend_nodes <- ten(Surv(rectime, censrec) ~ nodes_gr2, data = df)
print(trend_nodes)
comp(trend_nodes) # gdzie jest p-value???


#size

min(df$size) #3
max(df$size) #120

quantile(df$size, 0.25) #20
quantile(df$size, 0.5) #25
quantile(df$size,0.75) #35
boxplot(df$size)
abline(h=35)
abline(h=25)
abline(h=20)

df$size_gr2<- cut(df$size, 
                   breaks = c(0, 20, 25, 35, 120),
                   labels = c(1, 2, 3, 4),
                   include.lowest = TRUE)

df$size_gr2<- as.numeric(df$size_gr2)
df %>%   group_by(size_gr2) %>%   summarize(liczba_przypadkow = sum(censrec, na.rm = TRUE))

#  size_gr2 liczba_przypadkow
#1        1                65
#2        2                76
#3        3                83
#4        4                75

krzywa_przezycia_size1 <- survfit(Surv(rectime, censrec) ~ 1, 
                                   data=df,
                                   subset = size_gr2 == 1,
                                   se.fit = FALSE)

krzywa_przezycia_size2 <- survfit(Surv(rectime, censrec) ~ 1, 
                                   data=df,
                                   subset = size_gr2 == 2,
                                   se.fit = FALSE)
krzywa_przezycia_size3 <- survfit(Surv(rectime, censrec) ~ 1, 
                                  data=df,
                                  subset = size_gr2 == 3,
                                  se.fit = FALSE)
krzywa_przezycia_size4 <- survfit(Surv(rectime, censrec) ~ 1, 
                                  data=df,
                                  subset = size_gr2 == 4,
                                  se.fit = FALSE)



plot(krzywa_przezycia_size1,col="red", xlab = "czas (dni)", ylab = "prawdopodobieństwo przeżycia", 
     main = "Krzywa przeżycia")
lines(krzywa_przezycia_size2,col="green")
lines(krzywa_przezycia_size3,col="blue")
lines(krzywa_przezycia_size4,col="yellow")
legend("topright", legend = c("size < 20", "size 20-25", "size 25-35", "size>35"),  
       fill = c("red", "green", "blue", "yellow"))


# testujemy
test_size <- survdiff(Surv(rectime, censrec) ~ size_gr2, data = df, rho = 0)
print(test_size) # p= p= 0.009   < 0.05 => odrzucamy H0 => krzywe roznia sie istotnie

# test trendu 
trend_size <- ten(Surv(rectime, censrec) ~ size_gr2, data = df)
print(trend_size)
comp(trend_size) # gdzie jest p-value???
