#Exo 1
#1

# Créer une toile de fond vide pour le graphique
par(mfrow = c(1,1))
plot(NA, xlim=c(-5, 5), ylim=c(0, 1), xlab="X", 
     ylab="Densité de probabilité", 
     main="Densités de probabilité \n de lois normales")
#2

# Tracer la densité de probabilité pour chaque simulation
moyennes <- c(0, 0, 0, -2)
sigmas <- c(0.45, 1, 2.25, 0.7)
colors <- c("red", "blue", "green", "orange")
legend_labels <- c()
for (i in 1:length(moyennes)) {
  serie = rnorm(n = 1000, 
                mean = moyennes[i], 
                sd = sigmas[i])
  lines(density(serie), col = colors[i])
  legend_labels <- c(legend_labels, paste("m =", moyennes[i], ",", "s =", sigmas[i]))
}

# Ajouter une légende
legend("topright", legend=legend_labels, col=colors, lwd=2, cex=0.8)

#3

serie = rnorm(n = 10000, mean = 0, sd = 1)

#4

hist(serie, main = "Histogramme de distribution", 
     probability = TRUE)
lines(density(serie))

#5

median(serie)

#6

quantile(serie)

#7

quantile(serie, probs = seq(0,1,0.01))
quantile(serie, probs = 0.95)

#8

qnorm(0.95,0,1)
pnorm(1.644854,0,1)

#9

qnorm(0.975,0,1)

#10

1-pnorm(1.96,0,1)

#Exo 2
#1

indices_lignes = seq(from = 0, to = 3.9, by = 0.1)

#on crée un vecteur vide pour ajouter les probas au fur et à mesure
all_probas = c()
#On parcourt les indices lignes
for (i in indices_lignes){
  proba = pnorm(q = i, mean = 0, sd = 1)
  #on ajoute la nouvelle proba au vecteur existant
  all_probas = c(all_probas,proba)
  all_probas = round(all_probas,digits = 4) }

#2

indices_colones = seq(from = 0.00, to = 0.09, by = 0.01)
indices_lignes = seq(from = 0, to = 3.9, by = 0.1)

#On crée un objet résultat vide.
resultat = NULL
#On parcourt les indices colonnes
for (j in indices_colones) {
  #on crée un vecteur vide pour ajouter les probas au fur et à mesure
  all_probas = c()
  #On parcourt les indices lignes
  for (i in indices_lignes){
    quantile = i + j
    proba = pnorm(q = quantile, mean = 0, sd = 1)
    #on ajoute la nouvelle proba au vecteur existant
    all_probas = c(all_probas,proba)
    all_probas = round(all_probas,digits = 4)
  }
  #On ajoute une colonne au resultat
  resultat = cbind(resultat,all_probas)
}

#3

class(resultat)
table = data.frame(resultat)
colnames(table) = indices_colones
rownames(table) = indices_lignes
View(table)

#Exo 3
#1

moyenne_pop<-171
sd_pop<-9
population<-rnorm(n = 1e7, 
                  mean=moyenne_pop, 
                  sd=sd_pop)

#2

mean(population)
sd(population)

#3

hist(population)
#on observe une courbe en cloche

#4

#observé
pop190 = population[population < 190]
length(pop190)
length(pop190) / length(population)

#en théorie
pnorm(q = 190, mean=moyenne_pop, sd=sd_pop)*1e7

#5

#observé
pop200 = population[population >= 200]
length(pop200)
length(pop200) / length(population)

#en théorie
#proba de P( X < 200cm)
proba_inf_200 = pnorm(q = 200, mean=moyenne_pop, sd=sd_pop)
#proba de P( X >= 200cm)
1 - proba_inf_200

#Exo 4

#1

taille_ech<-100
echantillon<-sample(x = population, 
                    size = taille_ech, 
                    replace = TRUE)
mean(echantillon)
sd(echantillon)

#2

largeur<-qnorm(p = 0.975,mean=0,sd=1)*sd_pop/sqrt(taille_ech)
borne_inf<-moyenne_pop-largeur
borne_sup <-moyenne_pop+largeur

#3

taille_ech<-100
nb_replicat<-1000
echantillons<-replicate(n = nb_replicat,
                        expr =  sample(population,
                                       taille_ech, 
                                       replace = TRUE))

moyennes<-apply(X = echantillons,
                MARGIN = 2,
                FUN = function(x) mean(x))
ecart_types<-apply(echantillons,
                   MARGIN = 2,
                   FUN = function(x) sd(x))

#4

hist(moyennes)

#5

mean(moyennes)
sd(moyennes)

#6

#observé
moy172 = moyennes[moyennes > 172]
length(moy172)
length(moy172) / length(moyennes)

#en théorie
#proba de P( X < 172cm)
proba_inf_172 = pnorm(q = 172, 
                      mean=moyenne_pop, 
                      sd=sd_pop/sqrt(taille_ech))
#proba de P( X >= 172cm)
1 - proba_inf_172

#7

largeur<-apply(X = echantillons,
               MARGIN = 2,
               FUN = function(x) pnorm(0.975)*sd(x)/taille_ech)

borne_inf_IC<-moyennes-largeur
borne_sup_IC<-moyennes+largeur

#8

resultat = data.frame(largeur,borne_inf_IC,borne_sup_IC) 
View(resultat)

#Exo 5
#1

moyenne_echantillon<-function(V,n) {
  return(mean(sample(x = V,size = n, replace=TRUE)))
}

#2

moyennes_20<-replicate(n = nb_replicat, 
                       expr = moyenne_echantillon(V = population,
                                                  n = 20))
moyennes_30<-replicate(n = nb_replicat, 
                       expr = moyenne_echantillon(V = population,
                                                  n = 30))
moyennes_50<-replicate(n = nb_replicat, 
                       expr = moyenne_echantillon(V = population,
                                                  n = 50))
moyennes_100<-replicate(n = nb_replicat, 
                        expr = moyenne_echantillon(V = population,
                                                   n = 100))
moyennes_500<-replicate(n = nb_replicat, 
                        expr = moyenne_echantillon(V = population,
                                                   n = 500))

#3

par(mfrow=c(2,3))
hist(moyennes_20, xlim=c(161,181), main="20")
hist(moyennes_30, xlim=c(161,181), main="30")
hist(moyennes_50, xlim=c(161,181), main="50")
hist(moyennes_100, xlim=c(161,181), main="100")
hist(moyennes_500, xlim=c(161,181), main="500")

#4

population<-runif(n = 1e7, min = 0, max = 1)
moyennes_20<-replicate(nb_replicat, moyenne_echantillon(population,20))
moyennes_30<-replicate(nb_replicat, moyenne_echantillon(population,30))
moyennes_50<-replicate(nb_replicat, moyenne_echantillon(population,50))
moyennes_100<-replicate(nb_replicat, moyenne_echantillon(population,100))
moyennes_500<-replicate(nb_replicat, moyenne_echantillon(population,500))
par(mfrow=c(2,3))
hist(moyennes_20, xlim=c(0,1), main="20")
hist(moyennes_30, xlim=c(0,1), main="30")
hist(moyennes_50, xlim=c(0,1), main="50")
hist(moyennes_100, xlim=c(0,1), main="100")
hist(moyennes_500, xlim=c(0,1), main="500")



  