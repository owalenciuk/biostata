install.packages("foreign")
install.packages("survival")
install.packages("survMisc")
library(survMisc)
library(foreign)
library(survival)
library(haven)

#Choroba lokomocyjna
seasick <-
  read_sas("C:\\Users\\olgaw\\Desktop\\MINI_mgr\\sem2\\Biostatystyka\\projekt1\\Biostata - 2021\\Ania - materia³y\\Pd1\\seasick_eng_data.sas7bdat")

head(seasick, 10)
?survfit
?Surv

sea1.KM <- survfit(Surv(time, vomit) ~ 1, data=seasick, subset=intens==1,
                   conf.type="none") #krzywa i statystyki

summary(sea1.KM)
plot(sea1.KM)
plot(sea1.KM,col=c("red"),lty=c(2),xlab="minutes",ylab="survival probability")

sea2.KM <- survfit(Surv(time, vomit) ~ intens, data=seasick,
                   conf.type="none")
plot(sea2.KM,col=c("red","blue"),lty=c(1,2),xlab="minutes",ylab="survival probability")

sea.test <- survdiff(Surv(time, vomit) ~ intens, data=seasick, rho=0)
print(sea.test)

#Ekspresja bialka

nsclc <- read_sas("C:\\Users\\olgaw\\Desktop\\MINI_mgr\\sem2\\Biostatystyka\\projekt1\\Biostata - 2021\\Ania - materia³y\\Pd1\\nsclc_eng.sas7bdat")

head(nsclc,10)

nsclc.KM <- survfit(Surv(survtime, survind) ~ expression, data=nsclc,
                    conf.type="none")

plot(nsclc.KM,col=c("red","blue"),lty=c(1,2))

nsclc.logrank <- survdiff(Surv(survtime, survind) ~ expression, data=nsclc)
print(nsclc.logrank)

nsclc.strat <- survdiff(Surv(survtime, survind) ~ expression + strata(tnm),
                        data=nsclc)
print(nsclc.strat)

nsclc.trend <- ten(Surv(survtime, survind) ~ tnm, data=nsclc)
nsclc.trend 

comp(nsclc.trend)
attr(nsclc.trend,"tft")

#Cwiczenia
#1
help(survfit.formula)
help(summary.survfit)
help(plot.survfit)

#2
sea1.KM.l <- survfit(Surv(time, vomit) ~ 1, data=seasick, 
                     subset=intens==1,cof.type="log-log",conf.int=0.99)
summary(sea1.KM.l)
plot(sea1.KM.l)

#3
help(survdiff)

#4
nsclc.tnm <- survdiff(Surv(survtime, survind) ~ tnm,
                        data=nsclc)
print(nsclc.tnm)

nsclc.strat.PPP <- survdiff(Surv(survtime, survind) ~ expression + strata(tnm),
                        data=nsclc,rho=1)
print(nsclc.strat.PPP)

