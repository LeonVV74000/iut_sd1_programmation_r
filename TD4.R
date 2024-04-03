#Exo 1
#1

df = read.csv("velov.csv",header = TRUE, sep = ";", dec = ",")

#2

summary(df)

#3

df$status = as.factor(df$status)
df$CodePostal = as.factor(df$CodePostal)

#4

df$bornes = ifelse(df$capacity != (df$bikes + df$stands), "KO", "OK")

#Exo 2
#1

hist(x = df$capacity,main = "Distribution des capacités")

#2

hist(x = df$capacity,main = "Distribution des capacités", breaks = 6)

#3

hist(x = df$capacity,main = "Distribution des capacités", breaks = 6, col = "red")

#4

hist(x = df$capacity,main = "Distribution des capacités", breaks = 6, xlab = "Capacity",col = "red")

#5

abline(h = 100, col = "blue")

#6

hist(x = df$capacity, col = "red" , probability = TRUE, xlab = "Capacité")

#7

lines(density(df$capacity), col = "blue",lty = 2, lwd = 4)

#8

hist(x = df$capacity, col = "red" , probability = TRUE,
     xlab = "Capacité", ylim = c(0.00,0.08))

lines(density(df$capacity), col = "blue", lty = 2, lwd = 4)

#Exo 3

#1

boxplot(x = df$capacity, main = "Coucou")

#2

boxplot(x = df$capacity, main = "Coucou", horizontal =TRUE)

#3

boxplot(x = df$capacity, main = "Coucou", outline = FALSE)

#4

points(mean(df$capacity),col = "red", pch = 15)

#5

par(mfrow = c(1,2))

df7 = subset(df, df$CodePostal == "69007")
boxplot(x = df7$bikes, main = "Coucou", ylim = c(0,40))

df8 = subset(df, df$CodePostal == "69008")
boxplot(x = df8$bikes, main = "Nolann", ylim = c(0,40))

#6

par(mfrow = c(1,1))
boxplot(df$bikes ~ df$bonus, main = "En FoNcTiOn Du BoNuS",ylim = c(0,40))

#7

moy = tapply(X = df$bikes, 
             INDEX = df$bonus, 
             FUN = function(X) mean(X))
print(moy)

points(moy, col = "red", pch = 19)

#Exo 4

#1

effectif = table(df$bonus)
barplot(effectif, main = "QR CODE")

#2

effectif = table(df$bonus)
barplot(effectif, main = "QR CODE", horiz = TRUE)

#3

frequence = prop.table(effectif)
barplot(frequence, main = "QR CODE", horiz = TRUE)

#4

effectif = table(df$banking, df$bonus)
print(effectif)
barplot(height = effectif,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?"
        col = c("green","red"))

#Préparer les labels
legend_labels <- colnames(frequence)
#Ajouter une légende
legend(x = "topright", 
       legend = legend_labels, 
       fill  = c("red","green"))

#5

frequence = prop.table(x = effectif, margin = 2)
barplot(height = frequence,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?",
        col = c("red","green"))


legend_labels <- colnames(frequence)
legend(x = "topright", 
       legend = legend_labels, 
       fill  = c("red","green"))

#7

frequence = prop.table(x = effectif, margin = 2)
barplot(height = frequence,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?",
        col = c("red","green"),
        beside = TRUE)


legend_labels <- colnames(frequence)
legend(x = "topright", 
       legend = legend_labels, 
       fill  = c("red","green"))

#8

pie(x = effectif,
    main = "Répartition du nombre \n de station bonus",
    col = c("yellow","green"))

#9

etiquette = paste(rownames(effectif),"\n",effectif)
pie(x = effectif,
    main = "Répartition du nombre \n de station bonus",
    col = c("yellow","green"),
    labels = etiquette)

#10

effectif = table(df$CodePostal)
top10 = sort(effectif, decreasing = TRUE)[1:10]
barplot(height = top10,
        main = "cc",
        col = palette(),
        las = 2)
print(palette())

#11

barplot(height = top10,
        main = "cc" ,
        col = colors(),
        las = 2)

print(colors())

#12

dev.print(device = png, file = "export.png", width = 600)

#Exo 5

#1

plot(x = df$stands, y = df$capacity,
     main = "Place disponible vs Capacité")

#2

plot(x = df$stands, y = df$capacity,
     main = "Place disponible vs Capacité",
     xlim = c(0,60),
     ylim = c(0,60),
     pch=19)

#3

df$bornes = as.factor(df$bornes)
plot(x = df$stands, y = df$capacity,
     main = "Place disponible vs Capacité",
     xlim = c(0,60),
     ylim = c(0,60),
     col = df$bornes,
     pch=19)

# Ajouter une légende
legend("topright", legend = levels(df$bornes),
       col = palette(), pch = 19)

#4

myColors <- c("red", "blue", "green")  
# Ajoutez plus de couleurs si nécessaire avec le code HTML des couleurs à la place des noms

# Tracer le graphique
plot(x = df$stands, y = df$capacity,
     main = "Place disponible vs Capacité",
     xlim = c(0, 60),
     ylim = c(0, 60),
     col = myColors[df$bornes],
     pch = 19)

# Ajouter une légende
legend("topright", legend = levels(df$bornes),
       col = myColors, pch = 19)

#5

moy_stands = mean(df$stands)
moy_capacity = mean(df$capacity)
points(x = moy_stands,y = moy_capacity, 
       pch = 15,
       col = myColors[3],
       cex = 2)

#Exo 6

#1

# Librairies nécessaires
library(leaflet)
library(dplyr)
library(ggplot2)

# Créer une carte Leaflet
maCarte <- leaflet(df) %>% 
  addTiles() %>% 
  addMarkers(~position_longitude, 
             ~position_latitude, 
             popup = ~address)

# Afficher la carte
maCarte


