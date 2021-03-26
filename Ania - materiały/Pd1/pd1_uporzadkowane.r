df <- read.csv("C:\\Users\\olgaw\\Desktop\\MINI_mgr\\sem2\\Biostatystyka\\projekt1\\Biostata - 2021\\Ania - materia³y\\Pd1\\follic_short_cr.csv")

install.packages("rms")
library(survMisc)
library(survival)
library(foreign)
library(rms)
library(dplyr) 

?survfit.formula

#----------------------------------- ogolna krzywa przezycia i mediana
par(mfrow = c(1,1))
krzywa_przezycia <- survfit(Surv(dftime, dfstat) ~ 1, # jedna krzywa przezycia
                  data = df, # obiekty z df
                  conf.type = "log-log") # przedial ufnosci

plot(krzywa_przezycia, col = c("red"), lty = c(2), xlab = "czas (lata)", ylab = "prawdopodobieñstwo prze¿ycia", 
     main = "Krzywa prze¿ycia")
abline(0.5, 0)

mediana <- krzywa_przezycia$time[which(krzywa_przezycia$surv<0.5)[1]]

max(df$dftime) # ostatnia osoba wycofala sie z badaniach po 31.10198 latach


#----------------------------- st. zaawansowania choroby

krzywa_przezycia_clinstg1 <- survfit(Surv(dftime, dfstat) ~ 1, # jedna krzywa przezycia
                  data = df, # obiekty z df
                  subset = clinstg == 1, #st. zaawansowania choroby = 1
                  se.fit = FALSE) # bez przedzialu ufnosci

krzywa_przezycia_clinstg2 <- survfit(Surv(dftime, dfstat) ~ 1, # jedna krzywa przezycia
                  data = df, # obiekty z df
                  subset = clinstg == 2, #st. zaawansowania choroby = 2
                  se.fit = FALSE) # bez przedialu ufnosci


plot(krzywa_przezycia_clinstg2,col="red", xlab = "czas (lata)", ylab = "prawdopodobieñstwo prze¿ycia", 
          main = "Krzywa prze¿ycia") 
lines(krzywa_przezycia_clinstg1,col="green")
legend("topright", legend = c("clinstg = 2", "clinstg = 1"),  fill = c("red", "green"))


# Test, czy te dwie krzywe roznia sie istotnie
# test logrank dwuprobkowy dla st. zaawansowania choroby 
test_clinstg <- survdiff(Surv(dftime, dfstat) ~ clinstg, data = df, rho = 0)
print(test_clinstg) # p = 0.2 => krzywe nie roznia sie istotnie


#-------------------------------------------- chemioterapia

krzywa_przezycia_chemio0 <- survfit(Surv(dftime, dfstat) ~ 1, # jedna krzywa przezycia
                                     data = df, # obiekty z df
                                     subset = chemo == 0, #chemioterapia = 0
                                     se.fit = FALSE) # bez przedialu ufnosci

krzywa_przezycia_chemio1 <- survfit(Surv(dftime, dfstat) ~ 1, # jedna krzywa przezycia
                                     data = df, # obiekty z df
                                     subset = chemo == 1, #chemioterapia = 1
                                     se.fit = FALSE) # bez przedialu ufnosci

plot(krzywa_przezycia_chemio0,col="red",xlab = "czas (lata)", ylab = "prawdopodobieñstwo prze¿ycia", 
     main = "Krzywa prze¿ycia") 
lines(krzywa_przezycia_chemio1,col="green")
legend("topright", legend = c("chemo = 0", "chemo = 1"),  fill = c("red", "green"))

# test, czy te krzywe roznia sie istotnie
test_chemio <- survdiff(Surv(dftime, dfstat) ~ chemo, data = df, rho = 0)
print(test_chemio) # p = 0.002 => krzywe roznia sie
# ludzie z chemioterapia przezywaja dluzej


# -------------------------------------- hemoglobina
# tworzymy grupy dla hemoglobiny, podzial ze wzgledu na kwartyle:

df$hgb2<- cut(df$hgb, 
              breaks = c(0, 130, 140, 150, 100000),
              labels = c('<=130', '131-140', '141-150', '>=151'),
              include.lowest = TRUE)

# sprawdzamy, czy podzial jest ok
# liczba jedynek
df %>%   group_by(hgb2) %>%   summarize(liczba_przypadkow = sum(dfstat, na.rm = TRUE))
# podzial taki, zeby w kazdej grupie byla ta sama (mniej wiecej) liczba smierci


krzywa_przezycia_hgb130 <- survfit(Surv(dftime, dfstat) ~ 1, 
                                  data=df,
                                  subset = hgb2 == '<=130',
                                  se.fit = FALSE)
krzywa_przezycia_hgb140 <- survfit(Surv(dftime, dfstat) ~ 1, 
                                   data=df,
                                   subset = hgb2 == '131-140',
                                   se.fit = FALSE)
krzywa_przezycia_hgb150 <- survfit(Surv(dftime, dfstat) ~ 1, 
                                   data=df,
                                   subset = hgb2 == '141-150',
                                   se.fit = FALSE)
krzywa_przezycia_hgb151 <- survfit(Surv(dftime, dfstat) ~ 1, 
                                   data=df,
                                   subset = hgb2 == '>=151',
                                   se.fit = FALSE)

plot(krzywa_przezycia_hgb130, col="red", xlab = "czas (lata)", ylab = "prawdopodobieñstwo prze¿ycia", 
     main = "Krzywa prze¿ycia") 
lines(krzywa_przezycia_hgb140,col="green")
lines(krzywa_przezycia_hgb150,col="blue")
lines(krzywa_przezycia_hgb151,col="yellow")
legend("topright", legend = c("hgb <= 130", "hgb 131-140", "hgb 141-150", "hgb >= 151"),  
       fill = c("red", "green", "blue", "yellow"))

test_hgb <- survdiff(Surv(dftime, dfstat) ~ hgb2, data = df, rho = 0)
print(test_hgb) # p = 0.9
# nie odrzucamy H0, nie ma istotnej roznicy miedzy krzywymi

# -------------------------------------- wiek
# grupujemy wiek:

df$age_groups2<- cut(df$age, 
                     breaks = c(0, 40, 50, 60, 70, 90),
                     labels = c(1, 2, 3, 4, 5),
                     include.lowest = TRUE)
df$age_groups2 <- as.numeric(df$age_groups2)

# sprawdzamy, czy podzial jest ok
df %>%   group_by(age_groups2) %>%   summarize(liczba_przypadkow = sum(dfstat, na.rm = TRUE))

krzywa_przezycia_age40 <- survfit(Surv(dftime, dfstat) ~ 1, 
                          data=df,
                          subset = age_groups2 == 1,
                          se.fit = FALSE)

krzywa_przezycia_age50 <- survfit(Surv(dftime, dfstat) ~ 1, 
                          data=df,
                          subset = age_groups2 == 2,
                          se.fit = FALSE)

krzywa_przezycia_age60 <- survfit(Surv(dftime, dfstat) ~ 1, 
                          data=df,
                          subset = age_groups2 == 3,
                          se.fit = FALSE)

krzywa_przezycia_age70 <- survfit(Surv(dftime, dfstat) ~ 1, 
                          data=df,
                          subset = age_groups2 == 4,
                          se.fit = FALSE)

krzywa_przezycia_age71 <- survfit(Surv(dftime, dfstat) ~ 1, 
                          data=df,
                          subset = age_groups2 == 5,
                          se.fit = FALSE)

plot(krzywa_przezycia_age40,col="red", xlab = "czas (lata)", ylab = "prawdopodobieñstwo prze¿ycia", 
     main = "Krzywa prze¿ycia")
lines(krzywa_przezycia_age50,col="green")
lines(krzywa_przezycia_age60,col="blue")
lines(krzywa_przezycia_age70,col="yellow")
lines(krzywa_przezycia_age71)
legend("topright", legend = c("age < 40", "age 40-50", "age 50-60", "age 60-70", "age > 70"),  
       fill = c("red", "green", "blue", "yellow", "black"))

# testujemy
test_age <- survdiff(Surv(dftime, dfstat) ~ age_groups2, data = df, rho = 0)
print(test_age) # p= 1e-07 < 0.05 => odrzucamy H0 => krzywe roznia sie istotnie

# test trendu dla wieku
trend_age <- ten(Surv(dftime, dfstat) ~ age_groups2, data = df)
print(trend_age)
comp(trend_age) # p-value = 0.022954 < 0.05 => odrzucamy H0 => jest trend


# ------------------------------------ testy z warstwami
# Robimy warstwy dla tych zmiennych, co maja znaczenie.
#------------------------  chemioterapia

# chemioterapia i st. zaawansowania choroby
test_chemio_clinstg <- survdiff(Surv(dftime, dfstat) ~ chemo + strata(clinstg), data = df)
print(test_chemio_clinstg)  # p = 8e-04, p-value sie zmniejszylo

#chemioterapia i hemoglobina
test_chemio_hgb <- survdiff(Surv(dftime, dfstat) ~ chemo  + strata(hgb2), data = df)
print(test_chemio_hgb)  #  p= 0.002, p-value sie zwiekszylo

# chemioterapia i wiek
test_chemio_age <- survdiff(Surv(dftime, dfstat) ~ chemo  + strata(age_groups2), data = df)
print(test_chemio_age) # p = 0.009

krzywa_przezycia_chemio_age40 <- survfit(Surv(dftime, dfstat) ~ chemo,
                                         data = df,
                                         subset = age_groups2 == 1,
                                         se.fit = FALSE)
krzywa_przezycia_chemio_age50 <- survfit(Surv(dftime, dfstat) ~ chemo,
                                         data = df,
                                         subset = age_groups2 == 2,
                                         se.fit = FALSE)
krzywa_przezycia_chemio_age60 <- survfit(Surv(dftime, dfstat) ~ chemo,
                                         data = df,
                                         subset = age_groups2 == 3,
                                         se.fit = FALSE)
krzywa_przezycia_chemio_age70 <- survfit(Surv(dftime, dfstat) ~ chemo,
                                         data = df,
                                         subset = age_groups2 == 4,
                                         se.fit = FALSE)
krzywa_przezycia_chemio_age90 <- survfit(Surv(dftime, dfstat) ~ chemo,
                                         data = df,
                                         subset = age_groups2 == 5,
                                         se.fit = FALSE)
par(mfrow = c(2,3))
plot(krzywa_przezycia_chemio_age40,col="red", xlab = "wiek (lata)", ylab = "prawdopodobieñstwo prze¿ycia", 
     main = "Krzywa prze¿ycia age <40")
plot(krzywa_przezycia_chemio_age50,col="green", xlab = "wiek (lata)", ylab = "prawdopodobieñstwo prze¿ycia", 
     main = "Krzywa prze¿ycia age 40-50")
plot(krzywa_przezycia_chemio_age60,col="blue", xlab = "wiek (lata)", ylab = "prawdopodobieñstwo prze¿ycia", 
     main = "Krzywa prze¿ycia age 50-60")
plot(krzywa_przezycia_chemio_age70,col="black", xlab = "wiek (lata)", ylab = "prawdopodobieñstwo prze¿ycia", 
     main = "Krzywa prze¿ycia age 60-70")
plot(krzywa_przezycia_chemio_age90,col="magenta", xlab = "wiek (lata)", ylab = "prawdopodobieñstwo prze¿ycia", 
     main = "Krzywa prze¿ycia age >70")

# ------------------------------- testy z warstwami dla wieku

test_age_clinstg <- survdiff(Surv(dftime, dfstat) ~ age_groups2 + strata(clinstg), data = df)
print(test_age_clinstg)  # p = 8e-09 

test_age_hgb <- survdiff(Surv(dftime, dfstat) ~ age_groups2  + strata(hgb2), data = df)
print(test_age_hgb)  #  p = 8e-08

test_age_chemio <- survdiff(Surv(dftime, dfstat) ~ age_groups2  + strata(chemo), data = df)
print(test_age_chemio) # p = 7e-07

# sprawdzamy czy wiek i chemioterapia sa jakos powiazane?
mean(df[df$age_groups2=="<40",]$chemo)   # 0.278481
mean(df[df$age_groups2=="40-50",]$chemo) # 0.1686747 => 16% ma chemioterapie
mean(df[df$age_groups2=="50-60",]$chemo) # 0.2116788
mean(df[df$age_groups2=="60-70",]$chemo) # 0.2121212
mean(df[df$age_groups2==">70",]$chemo)   # 0.1162791
# nie wyglada na to