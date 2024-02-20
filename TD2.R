bodies_karts = read.csv(file = "bodies_karts.csv", header = TRUE, sep = ";", dec = ",")
tires = read.csv(file = "tires.csv", header = TRUE, sep = "\t", dec = ",")
gliders = read.csv(file = "gliders.csv", header = TRUE, sep = "|", dec = ".")
drivers = read.csv(file = "drivers.csv", header = TRUE, sep = ";", dec = ",")

#5

dim(bodies_karts)
dim(drivers)
dim(gliders)
dim(tires)

#Exo 2
#1

summary(bodies_karts)
summary(drivers)
summary(gliders)
summary(tires)

#2

plot(drivers$Weight,drivers$Acceleration,main = "Drivers : Weight/Acceleration")
#Certains drivers ont les mêmes statistiques donc certains points sont cachés

#3

cor(drivers$Weight,drivers$Acceleration)

#4

a = cov(x= drivers$Weight,y= drivers$Acceleration)
sX = sd(drivers$Weight)
sY = sd(drivers$Acceleration)
print(a/(sX*sY))

#5

coefCorr = cor(x = drivers$Weight,
               y = drivers$Acceleration)
coefDeter = coefCorr^2
print(coefDeter)

#6

matriceCor = cor(drivers[ , - 1])
matriceCor = round(matriceCor , 2)
View(matriceCor)
#Toutes les variables semblent fortement corrélées entre elles.

#7

install.packages("corrplot")

#8

library(corrplot) #je charge mon package pour pouvoir utiliser ses fonctionalités
corrplot(matriceCor, method="circle")

#9

matriceCor = round(cor(tires[ , - 1]),1)
corrplot(matriceCor, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
         )

matriceCor = round(cor(bodies_karts[ , - 1]),1)
corrplot(matriceCor, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
         )

matriceCor = round(cor(gliders[ , - 1]),1)
corrplot(matriceCor, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
         )

#Ex 3
#1

resultat = drivers[,c("Driver","Weight")]
View(resultat)

#2

resultat = drivers[1:10,c("Driver","Weight")]
View(resultat)

#3

resultat = drivers[, c(-5,-7,-9)]
View(resultat)

#4

resultat = drivers[ , -c(2,3)]
View(resultat)

#5

resultat = drivers[ , c("Driver","Acceleration","Weight")]
View(resultat)
#Colonnes dans l'ordre annoncé

#6

resultat = drivers[c(3,12,32) , ]
View(resultat)

#7

resultat = drivers[c(32,3,12) , ]
View(resultat)

#8

rang = order(drivers$Weight)
resultat = drivers[ rang  , c("Driver", "Weight") ]
View(resultat)

#9

rang = order(drivers$Acceleration, decreasing = TRUE)
resultat = drivers[ rang  , c("Driver", "Acceleration") ]
View(resultat)

#10

rang = order(drivers$Acceleration, drivers$Weight, decreasing = c(TRUE,FALSE))
resultat = drivers[ rang  , c("Driver", "Acceleration","Weight") ]
View(resultat)


#Ex 4
#1

topDriver = subset(x = drivers,
                   subset = Acceleration == max(Acceleration), 
                   select = c("Driver","Acceleration"))
View(topDriver)

#2

topGlider = subset(x = gliders,
                   subset = Acceleration == max(Acceleration), 
                   select = c("Glider","Acceleration"))
View(topGlider)
topTires = subset(x = tires,
                  subset = Acceleration == max(Acceleration), 
                  select = c("Tire","Acceleration"))
View(topTires)
topBody = subset(x = bodies_karts,
                 subset = Acceleration == max(Acceleration), 
                 select = c("Body","Acceleration"))
View(topBody)

