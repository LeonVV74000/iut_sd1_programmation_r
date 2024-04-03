#Exo 1
#1

salaire_net_cadre = function(salaire_brut_cadre)
{salaire_net_avant_impot = salaire_brut_cadre * 0.75
return(salaire_net_avant_impot)}

salaire_net_cadre(salaire_brut_cadre = 3000)

#2

salaire_net_cadre = function(salaire_brut_cadre = 2500)
{salaire_net_avant_impot = salaire_brut_cadre * 0.75
return(salaire_net_avant_impot)}

salaire_net_cadre()

#3

salaire_net_cadre = function(salaire_brut_cadre = 2500, temps_travail = 1)
{salaire_net_avant_impot = salaire_brut_cadre * temps_travail
return(salaire_net_avant_impot)}

salaire_net_cadre(salaire_brut_cadre = 3000, temps_travail = 0.8)

#4

salaire_net_cadre = function(salaire_brut_cadre, temps_travail)
{if (!is.numeric(salaire_brut_cadre)) return("Erreur")
  
  salaire_net_avant_impot = salaire_brut_cadre * temps_travail
  return(salaire_net_avant_impot)}

salaire_net_cadre(salaire_brut_cadre = "300€", temps_travail = 0.8)

#5

salaire_net_cadre = function(salaire_brut = 2500,temps_travail = 1) {
  
  if (!is.numeric(salaire_brut)) {
    return("Erreur :  le salaire brut doit être une valeur numérique")
  }
  
  if (is.numeric(temps_travail) & (temps_travail >= 0) & (temps_travail <= 1)) {
    return("Erreur :  le temps de travail doit être une valeur numérique entre 0 et 1")
  }
  
  salaire_net_avant_impot = salaire_brut * temps_travail
  return(salaire_net_avant_impot) 
}
#Test de la fonction
salaire_net_cadre(salaire_brut = 2000, temps_travail = "100%")
salaire_net_cadre(salaire_brut = 2000, temps_travail = 0.8)
salaire_net_cadre(salaire_brut = 2000, temps_travail = 100)

#6

salaire_net = function(salaire_brut, temps_travail, statut) {
  
  if (!is.numeric(salaire_brut)) {
    return("Erreur :  le salaire brut doit être une valeur numérique")
  }
  
  if (!is.numeric(temps_travail) & (temps_travail <= 0) | (temps_travail >= 1)) {
    return("Erreur :  le temps de travail doit être une valeur numérique entre 0 et 1")
  }
  
  if (!statut %in% c("cadre","non cadre")) {
    return("Erreur")
  }
  
  if (statut == "cadre") {
    salaire_net_avant_impot = salaire_brut * temps_travail * 0.75
  } else { 
    salaire_net_avant_impot = salaire_brut * temps_travail * 0.78
  }
  
  return(salaire_net_avant_impot) 
}

salaire_net(salaire_brut = 2000,temps_travail = 0.8, statut = "cadre")
salaire_net(salaire_brut = 2000,temps_travail = 10, statut = "non cadre")
salaire_net(salaire_brut = 2000,temps_travail = 0.1, statut = "technicien")

#7

salaire_net = function(salaire_brut, temps_travail, statut) {
  
  if (!is.numeric(salaire_brut)) {
    return("Erreur :  le salaire brut doit être une valeur numérique")
  }
  
  if (!is.numeric(temps_travail) & (temps_travail <= 0) | (temps_travail >= 1)) {
    return("Erreur :  le temps de travail doit être une valeur numérique entre 0 et 1")
  }
  
  if (!statut %in% c("cadre","non cadre")) {
    return("Erreur")
  }
  
  if (statut == "cadre") {
    salaire_net_avant_impot = salaire_brut * temps_travail * 0.75
  } else { 
    salaire_net_avant_impot = salaire_brut * temps_travail * 0.78
  }
  
  if (salaire_net_avant_impot <= 1591) {
    salaire_net_apres_impot = salaire_net_avant_impot
  } else if (salaire_net_avant_impot <= 2006) {
    salaire_net_apres_impot = salaire_net_avant_impot * (1 - 0.029)
  } else if (salaire_net_avant_impot <= 3476) {
    salaire_net_apres_impot = salaire_net_avant_impot * (1 - 0.099)
  } else if (salaire_net_avant_impot <= 8557) {
    salaire_net_apres_impot = salaire_net_avant_impot * (1 - 0.20)
  } else {
    salaire_net_apres_impot = salaire_net_avant_impot * (1 - 0.43)
  }
  
  return(salaire_net_apres_impot) 
}

salaire_net(salaire_brut = 5000,temps_travail = 0.8, statut = "cadre")
salaire_net(salaire_brut = 2000,temps_travail = 10, statut = "non cadre")
salaire_net(salaire_brut = 2000,temps_travail = 0.1, statut = "technicien")

#8

shifumi = function() {
  
choix_uti = readline(prompt = "Choisir entre Pierre, Feuille et Ciseaux")

if (choix_uti %in% c("Pierre","Feuille","Ciseaux")) {
  
  choix_ordi = sample(c("Pierre","Feuille","Ciseaux"),1)
  
  cat("Votre choix :", choix_uti, "\n")
  cat("Choix de l'ordinateur :", choix_ordi, "\n")
  
  if (choix_uti == choix_ordi) {
    return("Egalité")}
  else if ((choix_uti == "pierre" & choix_ordi == "ciseaux") |
           (choix_uti == "papier" & choix_ordi == "pierre") |
           (choix_uti == "ciseaux" & choix_ordi == "papier"))
  {return("Vous avez gagné !")}
  else {
    return("Vous avez perdu...")
  }
    
  }
  else {"Vous avez pas choisi la bonne valeur"}
}
shifumi()

#Exo 2

#1
a = 0
for (i in c(1,2,3,4,5)) {
  a = a + i}
 print(paste("le resultat est :",a ))
 
#2
 
a = 0
b = 1
while (a <= 50) {
  a = a + b
print(paste("Le résultat est :", a))
print(paste("Le programme s'est arrêté à la valeur :", b))
b = b+ 1
}

#3

for (colonne in colnames(iris)) {
  type_colonne = class(iris[,colonne]) 
print(paste("Le type de la colonne ",colonne," est : ", type_colonne))
}

#4

indice_colonne <- 1

while (indice_colonne <= ncol(iris)) {

  nom_colonne <- colnames(iris)[indice_colonne]
  
  type_colonne <- class(iris[, nom_colonne])
  
  print(paste("la colonne ", nom_colonne, " est : ", type_colonne))
  
  indice_colonne <- indice_colonne + 1
  
#Exo 3

#1
  
for (i in (1:5)) {
  choix_uti = readline(prompt = "Choisissez un chiffre")
  choix_uti = as.numeric(choix_uti)
  resultat = choix_uti ^ 2
  print(paste("Le carré de votre nombre est :",resultat))
}

#2

  # Chemin du dossier à explorer
  dossier <- "chemin/vers/le/dossier"
  
  # Liste les fichiers dans le dossier spécifié
  fichiers <- list.files(dossier, full.names = TRUE)
  
  # Affiche la taille de chaque fichier
  for (fichier in fichiers) {
    info <- file.info(fichier)
    taille <- info$size
    cat("Le fichier", basename(fichier), "a une taille de", taille, "octets.\n")
  }

#3
  
  # Parcourir toutes les colonnes du dataframe iris
  for (colonne in colnames(iris)) {
    # Vérifier si la colonne est de type numeric
    if (is.numeric(iris[,colonne])) {
      # Si c'est le cas, construire un boxplot avec un titre
      boxplot(iris[,colonne], main = paste("Boxplot de", colonne))
    } else {
      # Sinon, construire un barplot avec un titre
      barplot(table(iris[,colonne]), main = paste("Barplot de", colonne))
    }
  }
  
#4
  
  # Boucle while pour jouer au shifumi jusqu'à ce que l'utilisateur décide d'arrêter
  continuer <- TRUE
  while (continuer) {
    # Appeler la fonction shifumi et afficher le résultat
    resultat <- shifumi()
    cat("Résultat du jeu :", resultat, "\n")
    
    # Demander à l'utilisateur s'il souhaite continuer
    reponse <- readline(prompt = "Voulez-vous continuer à jouer ? (oui/non) : ")
    
    # Vérifier la réponse de l'utilisateur
    if (tolower(reponse) == "non") {
      print("Arrêt du jeu.")
      continuer <- FALSE  # Mettre fin à la boucle
    }
  }
  
#5
  
  # Fonction pour le jeu du juste prix
  juste_prix <- function() {
    # Génération d'un nombre aléatoire entre 1 et 100
    nombre_a_deviner <- sample(1:100, 1)
    
    # Initialisation de la réponse de l'utilisateur
    reponse <- -1
    
    # Boucle tant que l'utilisateur n'a pas trouvé le bon nombre
    while (reponse != nombre_a_deviner) {
      # Demande à l'utilisateur de saisir un nombre
      reponse <- as.integer(readline(prompt = "Devinez le nombre : "))
      
      # Vérifie si le nombre est trop grand, trop petit ou correct
      if (reponse < nombre_a_deviner) {
        cat("C'est plus !\n")
      } else if (reponse > nombre_a_deviner) {
        cat("C'est moins !\n")
      } else {
        cat("Bravo, vous avez trouvé le juste prix !\n")
      }
    }
  }
  
  # Appel de la fonction juste_prix pour commencer le jeu
  juste_prix()
