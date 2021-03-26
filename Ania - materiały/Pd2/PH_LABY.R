library(foreign); library(survival); library(rms) 


nsclc <- read.dta("C:/Users/Anna/Desktop/Biostatystyka/pd1/nsclc_eng.dta") 
head(nsclc) 
View(nsclc)

nsclc.PH.P53e <- coxph(Surv(survtime, survind) ~ expression, data=nsclc) 

#wynik testu Walda i testu ilorazu wiarygodnosci 
print(nsclc.PH.P53e) 


# wynik testu score
nsclc.PH.P53e$score 
1-pchisq(nsclc.PH.P53e$score,1) 

#Funkcja przezycia oszacowana metoda PH
nsclc.pKM.P53e <- survfit(nsclc.PH.P53e, newdata=nsclc)

#Funkcja przyzycia oszacowana metoda KM
nsclc.KM.P53e <- survfit(Surv(survtime, survind) ~ expression, data=nsclc) 

#Porównanie funkcji przezycia
plot(nsclc.KM.P53e,conf.int=FALSE) 
lines(nsclc.pKM.P53e,col=c("red"),mark.time=FALSE)

nsclc.PH.P53em <- coxph(Surv(survtime, survind) ~ mutation + expression, data=nsclc) 
print(nsclc.PH.P53em) 

#ocena zalozenia za pomoca transformacji krzywych przezycia
plot(nsclc.KM.P53e, col=c("red","blue"),fun=function(x) log(-log(x)), log="x", firstx=1) 

#test Schoenfelda
nsclc.PHfit.P53e <- cox.zph(nsclc.PH.P53e, transform="identity") 
print(nsclc.PHfit.P53e) 

#wykres skalowanych reszt Schoenfelda
plot(nsclc.PHfit.P53e, df=4, nsmo=10, se=TRUE) 
abline(nsclc.PH.P53e$coeff[1], 0, lty=3) 

#wykres skalowanych reszt Schoenfelda z kwadratowa funkcja czasu
nsclc.PHfit1.P53e <- cox.zph(nsclc.PH.P53e, transform=function(x) x^2) 
plot(nsclc.PHfit1.P53e, df=4, nsmo=10, se=TRUE)
abline(0.786, 0, lty=3) 
#test 
print(nsclc.PHfit1.P53e) 

#ocena dopasowania modelu za pomoca rez dewiancji

#rezydua
nsclc.devres.P53em <- residuals(nsclc.PH.P53em,type="deviance") 

#liniowa kombinacja wspolczynnikow
nsclc.fitval.P53em <- predict(nsclc.PH.P53em,type="lp")

#wykres
plot(nsclc.fitval.P53em,nsclc.devres.P53em)

#krzywa KM
nsclc.KM.P53em <- survfit(Surv(survtime, survind) ~ mutation, data=nsclc) 
plot(nsclc.KM.P53em, col=c("red","blue"), xlab="months",ylab="survival probability") #przeciecie !
#transformacja
plot(nsclc.KM.P53em, col=c("red","blue"),fun=function(x) log(-log(x)), log="x", firstx=1) #przeciecie!
#test Schoenfelda
nsclc.PH.P53em <- coxph(Surv(survtime, survind) ~ expression + mutation , data=nsclc) 
nsclc.PHfit.P53em <- cox.zph(nsclc.PH.P53em, transform="identity") 
print(nsclc.PHfit.P53em) 
#wykres skal. reszt Schoenfelda
plot(nsclc.PHfit.P53em, df=4, nsmo=10, se=TRUE, var=1) 
abline(nsclc.PH.P53em$coeff[1], 0, lty=3) 
plot(nsclc.PHfit.P53em, df=4, nsmo=10, se=TRUE, var=2) #moze lepiej srobic przeksztalcenie log czasu
abline(nsclc.PH.P53em$coeff[2], 0, lty=3) 

#Warstwowanie mutacja - efekt ekspresji znika
nsclc.strPH.P53em <- coxph(Surv(survtime, survind) ~ expression + strata(mutation), data=nsclc)
print(nsclc.strPH.P53em) 

#ocena dopasowania modelu za pomoca rez dewiancji
nsclc.devres1.P53em <- residuals(nsclc.strPH.P53em,type="deviance") 
nsclc.fitval1.P53em <- nsclc.strPH.P53em$linear.predictors 
plot(nsclc.fitval1.P53em,nsclc.devres1.P53em) 

#postac funkcjonalna zmiennej f(Z)

#pusty model
ovar.PH <-  coxph(Surv(futime,fustat)~1, data=ovarian)
#reszty
mart <- resid(ovar.PH)
#wykres 
plot(ovarian$age,mart) 
#wygladzony wykres - sugeruje uzycie funkcji liniowej
lines(lowess(ovarian$age,mart,iter=0,f=0.6)) 

?score.items
