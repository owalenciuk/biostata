library(foreign);library(survival);library(rms) 
df <- read.csv("C://Users//PC//Desktop//Studia//BIO//PD1//follic_short_cr.csv")
#----------postaci funkcyjne zmiennych ilosciowych - reszty martyngalowe----
chloniak.PH <- coxph(Surv(dftime,dfstat)~1, data=df) #model pusty
mart <- resid(chloniak.PH)
#wiek - funkcja liniowa
plot(df$age , mart, main="Wykres reszt martyngalowych dla age ",
     xlab="wiek", ylab="Reszty martyngalowe", cex=0.5, cex.axis=0.5, cex.lab=0.7, cex.main=0.8)
lines(lowess(df$age , mart, iter=0, f=0.6), col="red")
par(mfrow = c(1,2))
# hgb
plot(df$hgb , mart, main="Wykres reszt martyngalowych dla hgb ",
     xlab="hgb", ylab="Reszty martyngalowe", cex=0.5, cex.axis=0.5, cex.lab=0.7, cex.main=0.8)
lines(lowess(df$hgb , mart, iter=0, f=0.6), col="red")
#widac obserwacje odstajaca...

# hgb bez obserwacji odstajacej - funkcja liniowa/nawet stala
df2<-df[df$hgb>40, ]
chloniak2.PH <- coxph(Surv(dftime,dfstat)~1, data=df2)
mart2 <- resid(chloniak2.PH)
plot(df2$hgb , mart2, main="Wykres reszt martyngalowych dla hgb ",
     xlab="hgb", ylab="Reszty martyngalowe", cex=0.5, cex.axis=0.5, cex.lab=0.7, cex.main=0.8)
lines(lowess(df2$hgb , mart2, iter=0, f=0.6), col="red")

#------------------dopasowanie pelnego modelu-----
chloniak.PH.pelny <- coxph(Surv(dftime,dfstat)~age+hgb+clinstg+chemo, data=df) 
print(chloniak.PH.pelny)
chloniak2.PH.pelny <- coxph(Surv(dftime,dfstat)~age+hgb+clinstg+chemo, data=df2)
print(chloniak2.PH.pelny)

#------------------sprawdzanie zalozen--------------
#krzywa KM - chemioterapia
chloniak.KM.chemo <- survfit(Surv(dftime,dfstat) ~ chemo, data=df) 
plot(chloniak.KM.chemo, col=c("red","blue"), xlab="months",ylab="survival probability")
#transformacja
plot(chloniak.KM.chemo, col=c("red","blue"),fun=function(x) log(-log(x)), log="x", firstx=1)

#krzywa KM - st.zaawansowania
chloniak.KM.clinstg <- survfit(Surv(dftime,dfstat) ~ clinstg, data=df) 
plot(chloniak.KM.clinstg, col=c("red","blue"), xlab="months",ylab="survival probability") #przeciecie !
#transformacja
plot(chloniak.KM.clinstg, col=c("red","blue"),fun=function(x) log(-log(x)), log="x", firstx=1) #przeciecie!

#test Schoenfelda
chloniak.PHfit.pelny <- cox.zph(chloniak.PH.pelny, transform="identity") 
print(chloniak.PHfit.pelny) 

#wykres skalowanych reszt Schoenfelda
par(mfrow=c(2,2))
plot(chloniak.PHfit.pelny, df=4, nsmo=10, se=TRUE, var=1) 
abline(chloniak.PH.pelny$coeff[1], 0, lty=3)
plot(chloniak.PHfit.pelny, df=4, nsmo=10, se=TRUE, var=2) 
abline(chloniak.PH.pelny$coeff[2], 0, lty=3)
plot(chloniak.PHfit.pelny, df=4, nsmo=10, se=TRUE, var=3) 
abline(chloniak.PH.pelny$coeff[3], 0, lty=3)
plot(chloniak.PHfit.pelny, df=4, nsmo=10, se=TRUE, var=4) 
abline(chloniak.PH.pelny$coeff[4], 0, lty=3)

#WARSTWY ze wzgledu na wiek
df$age2<- cut(df$age,breaks = c(0, 40, 60, 70, 90),labels = c(1, 2, 3, 4), include.lowest = TRUE)
df$age2 <- as.numeric(df$age2)
chloniak.strPH.wiek <- coxph(Surv(dftime,dfstat)~hgb+clinstg+chemo + strata(age2), data=df)
print(chloniak.strPH.wiek)

#------------------sprawdzanie zalozen--------------

#test Schoenfelda
chloniak.strPHfit.wiek <- cox.zph(chloniak.strPH.wiek, transform="identity") 
print(chloniak.strPHfit.wiek) 
#zalozenia sa spelnione

#wykres skalowanych reszt Schoenfelda
plot(chloniak.strPHfit.wiek, df=4, nsmo=10, se=TRUE, var=1) 
abline(chloniak.strPH.wiek$coeff[1], 0, lty=3) 
plot(chloniak.strPHfit.wiek, df=4, nsmo=10, se=TRUE, var=2) 
abline(chloniak.strPH.wiek$coeff[2], 0, lty=3)
plot(chloniak.strPHfit.wiek, df=4, nsmo=10, se=TRUE, var=3) 
abline(chloniak.strPH.wiek$coeff[3], 0, lty=3)
#-----------------ocena dopasowania modelu warstwowego za pomoca dewiancji
chloniak.devres.strPH.wiek <- residuals(chloniak.strPH.wiek,type="deviance") 
chloniak.fitval.strPH.wiek <- predict(chloniak.strPH.wiek,type="lp")
#wykres
par(mfrow=c(1,2))
plot(chloniak.fitval.strPH.wiek,chloniak.devres.strPH.wiek)
# ---------------------------ocena modelu pe³nego za pomoca dewiancji
chloniak.pelny.devres <- residuals(chloniak.PH.pelny,type="deviance") 
chloniak.pelny.fitval <- predict(chloniak.PH.pelny,type="lp")
#wykres
plot(chloniak.pelny.fitval,chloniak.pelny.devres)

