library(foreign)
library(survival)
library(KMsurv)
library(survMisc)


df <- read.dta("gbcs_short.dta")

# ===== KTORE ZE ZMIENNYCH MAJA WPLYW NA CZAS PRZEZYCIA BEZ NAWROTU CHOROBY? =====

### 1 ###
# wYKRESY/STATYSTYKI OPISOWE 
# wplyw zmiennych na czas przezycia

km <-survfit(Surv(df$rectime, df$censrec) ~ 1)
plot(km) #to jest funkcja przezycia 
plot(km, mark="+") #to jest funkcja przezycia z zaznaczonymi danymi cenzurowanymi

plot(km, fun="event") #to jest estymator KM
plot(km, fun="cumhaz") #to jest funkcja skumulowanego hazardu

summary(km)

#dzielimy sobie na grupy teraz wzgledem zmiennych kategorycznych

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

# wskaznik progesteronu
#0 ??? ujemny, 1 ??? dodatni
km_prog <-survfit(Surv(rectime, censrec) ~ prog, data = df)
plot(km_prog, col=1:2)
abline(v=150)
#czarna - 0 - ujemny
#czerwona - 1 - dodatni
#WNIOSEK: mozna tak wstepnie powiedziec, ze progesteron istotny wplyw na przezycie
#przy - 1 dodatnim wskazniku progesteronu - pstwo przezycia jest wieksze

#wskaznik estrogenu
#0 ??? ujemny, 1 ??? dodatni
km_estr <-survfit(Surv(rectime, censrec) ~ estr, data = df)
plot(km_estr, col=1:2)
abline(v=150)
#czarna - 0 - ujemny
#czerwona - 1 - dodatni
#WNIOSEK: tak samo, widzimy istotne roznice pomiedzy tymi oboma grupami
# dodatni wskaznik receptorow estrogenu - wieksze pstwo przezycia

#stopien zroznicowania komorek nowotworu 
#1???niski, 2?????redni, 3-wysoki
km_grade <-survfit(Surv(rectime, censrec) ~ grade, data = df)
plot(km_grade, col=1:3)
abline(v=100)
#WNIOSEK: bez zaskoczenia, wystepuja roznice, oczywiscie im wiekszy stopien zroznicowania komorek nowotworowych
#tym mniejsze prawdopodobienstwo przezycia

#UWAGA: wszelkie przecinania si?? tych wykresow trzeba rozgraniczyc na np. wczesna faze i pozna, bo moze si eto w czasie zmieniac
#Tu napisalam tylko takie rzeczy bez przeciec.
#Mozna dodac ze poczatki nie roznia sie za wiele - pierwsze 150dni - te zachowanie w grupach jest takie samo, mo??e poza ostatnim wykresem, tam bardziej 
#pierwsze 100dni.


#UWAGA: moze byc tak, ze funkcje sie nie krzyzuja, ale sa na tyle blisko siebie, ze moze wystepowac tak naprawde brak istotnych roznic
#dlatego nalezy zastosowac testy statystyczne zeby formalnie sprawdzic czy jakies rozncie tam wystepuja czy nie 

### 2 ###
# testy - ocena hipotez dotyczacych roznic w funkcjach przezycia

#test porownania dwoch grup


mod <- comp(ten(Surv(rectime, censrec) ~ meno, data=df))
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

#WNIOSKI: formalnie za pomoca testow statystycznych zweryfikowalismy to, ze w grupach w czynnikach takie jak 
#horm, prog, estr i grade - wystepuja istotne roznice w funkcjach przezycia
# nie wystepuja dla zmiennej menopauza


### 3 ###
#czy testy powinny brac pod uwage wplyw innych czynnikow niz leczenie. 
#czyli czy musimy rozpatrywac:
# 1) size - wielkosc guza 
# 2) nodes - liczbe wezlow chlonnych z przerzutami nowotworu 

