#TD 1
#Exo 1
#Question 1

a = 10
b = 5

#Question 2

resultat = a * b
print(resultat)

#Question 3

A = 7.2
B = 10.1
print("R prend en compte les majuscules/minuscules, car a et A sont deux objets différents")

#Question 4

resultat = A + B
print("Le resultat supprime l'ancienne valeur et fait ressortir la nouvelle")

#Question 5

rm(A,B,a,b,resultat)

#Exo 2.1
#Question 1.a

vecteur = c(1,2,3,4,5)
class(vecteur)
print(vecteur[3])

#Question 1.b

v1 = 1:5

#Question 2

v2 = v1 + 3

#Question 3

v3 = 1:6

#Question 4

v4 = v3**2

#Question 5

v5 = v4/2

#Question 6 

semaine = c("lundi","mardi","mercredi","jeudi"
            ,"vendredi","samedi","dimanche")
class(semaine)
semaine[c(2,7)]

#Question 7

boole = c(TRUE,FALSE,TRUE,FALSE)
class(boole)

#Question 8

decim = c(1.52,8.47,5.69,3.76,12.38)
class(decim)
print(decim[-3])

#Question 9

mois = c("janvier","fevrier","mars"
         ,"avril","mai","juin","juillet"
         ,"aout","septembre","octobre","novembre","decembre")
class(mois)
mois[c(1,2,3)]

#Question 10

num = c(-5,-3,-12,-18)
class(num)
num[c(1,length(num))]

#Question 11

fruits = c("tomates","poire","mangue","peche","kiwi")
class(fruits)
fruits[c(-1,-2)]

#Question 12

valeurs_manquantes = c(1,2,NA,4,NA,7)
print(class(valeurs_manquantes))

#Exo 2.2
#Question 1

nombres = seq(1,10)
length(nombres)

#Question 2

pair = seq(2,20,2)
length(pair)

#Question 3

decroissante = seq(0,-5,-1)
length(decroissante)

#Question 4

nombres = seq(5,50,5)
length(nombres)

#Question 5

nombres = seq(10,1,-1)
length(nombres)

#Question 6

nombres = seq(0,1,0.1)
length(nombres)

#Question 7

nombres = seq(5,-5,-1)
length(nombres)

#Question 8

nombres = seq(1,10,2)
length(nombres)

#Exo 2.3
#Question 1

vecteur = rep(3,5)

#Question 2

vecteur = rep(c("A","B","C"),3)

#Question 3

vecteur = rep(c(1,2,3),3)

#Question 4

vecteur = rep(c(TRUE,FALSE),4)

#Question 5

rm(vecteur)

#Exo 3.1
#Question 1

vecteur = runif(5,0,1)
mean(vecteur)
median(vecteur)
min(vecteur)
max(vecteur)

#Question 2

vecteur = runif(10,-5,5)
mean(vecteur)
median(vecteur)
min(vecteur)
max(vecteur)

#Question 3

vecteur = runif(100,10,20)
mean(vecteur)
median(vecteur)
min(vecteur)
max(vecteur)

#Question 4

vecteur = runif(15,50,100)
mean(vecteur)
median(vecteur)
min(vecteur)
max(vecteur)

#Exo 3.2
#Question 1

echantillon = rnorm(20,-2,3)
moyenne = mean(echantillon)
ecart_type = sd(echantillon)
print(paste("Moyenne :" , moyenne))
print(paste("Ecart-type :" , ecart_type))
hist(echantillon)

#Question 2 

echantillon = rnorm(2000,-2,3)
moyenne = mean(echantillon)
ecart_type = sd(echantillon)
print(paste("Moyenne :" , moyenne))
print(paste("Ecart-type :" , ecart_type))
hist(echantillon)

#Question 3 

echantillon = rnorm(2000,0,1)
moyenne = mean(echantillon)
ecart_type = sd(echantillon)
print(paste("Moyenne :" , moyenne))
print(paste("Ecart-type :" , ecart_type))
hist(echantillon)

#Question 4

quantile(echantillon, probs = c(0.25))
quantile(echantillon, probs = c(0.50))
quantile(echantillon, probs = c(0.75))

#Question 5

quantile(echantillon, probs = seq(0,1,0.1))

#Question 6

quantile(echantillon, probs = seq(0,1,0.01))


#Exo 3.2
#Question 1

vecteur = seq(3000,2400,300)
mean(vecteur)
sd(vecteur)

#Question 2

vecteur = round(vecteur,2)

#Question 3

masse_salariale = sum(vecteur)

#Question 4

med = median(vecteur)

#Question 5

cent = quantile(vecteur, probs = 0,99)

#Question 6

cent = quantile(vecteur, probs = 0,20)

#Exo 3.3
#Question 1

sample(x = c(1,2,3,4,5,6), 1)

#Question 2

simu = sample(x = c(1,2,3,4,5,6), 12, TRUE)

#Question 3 

unique(simu)

#Question 4

sort(table(simu),TRUE)

#Question 5

prop.table(simu)

#Question 6

simulation = sample(x = c(1,2,3,4,5,6), 100000, TRUE)
print(simulation)

#Question 7

sort(table(simulation),TRUE)
hist(simulation)
