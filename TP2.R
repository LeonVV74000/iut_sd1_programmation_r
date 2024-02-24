#Exo 1
#Question 1

df = read.csv("L:/BUT/SD/Promo 2023/lvincentvacle/r/TP 2/fao.csv", header = TRUE , dec = ",", sep = ";")

#Question 2

nrow(df)

#Question 3

summary(df)

#Exercice 2
#Question 1

disp_moy = mean(df$Dispo_alim)

#Question 2

nb_habitants = sum(df$Population, na.rm = TRUE)

#Question 3

ecart_type = sd(c(df$Import_viande,df$Export_viande),na.rm =TRUE)

#Question 4

mediane = median(df$Prod_viande, na.rm = TRUE)

#Question 5

quartiles = quantile(df$Dispo_alim, probs = seq(0.25,0.75,0.25))

#Question 6

centiles = quantiles(df$Import_viande, probs = seq(0.01,0.99,0.01))

#Exercice 3
#Question 1

tri = df[order(df$Population,decreasing = TRUE),  ]
top5 = head(tri[,c("Nom","Population")],5)
print(top5)

#Question 2

tri = df[order(df$Population),  ]
top5 = head(tri[,c("Nom","Population")],5)
print(top5)

#Question 3

tri = df[order(df$Prod_viande,decreasing = TRUE),  ]
top5 = head(tri[,c("Nom","Prod_viande")],5)
print(top5)

#Question 4

tri = df[order(df$Import_viande,decreasing = TRUE),  ]
top5 = head(tri[,c("Nom","Import_viande")],5)
print(top5)

#Question 5

tri = subset(df, df$Dispo_alim >= 2300)
top = tri[,c("Nom","Dispo_alim")]
print(top)
nrow(top)

#Question 6

tri = subset(df, df$Dispo_alim >= 3500 & df$Import_viande >= 1000)
top = tri[,c("Nom","Dispo_alim","Import_viande")]
print(top)
nrow(top)

#Question 7

tri = subset(df, df$Nom == c("France","Belgique"))
View(tri)

#Exercice 4
#Question 1

df$part_export = df$Export_viande/df$Prod_viande
View(df)

#Question 2

df$Dispo_alim_pays = df$Dispo_alim/df$Population
View(df)

#Question 3

write.table()

 



