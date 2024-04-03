# Exercice 1
#1

setwd("C:/nba")
getwd()

#2

fichiers <- list.files(path = getwd(), pattern = ".csv$", full.names = TRUE)

#3

nom_fichier = basename(path = fichiers[1])
nom_fichier_sans_extension = file_path_sans_ext(nom_fichier)

#4

assign(nom_fichier_sans_extension, read.csv(fichiers[1], sep = ',', dec = '.'))

#5

for (fichier in fichiers) {
  nom_objet <- file_path_sans_ext(basename(fichier))
  
  start_time <- Sys.time()
  assign(nom_objet, read.csv(fichier, sep = ",",dec = "."))
  end_time <- Sys.time()
  execution_time <- end_time - start_time
  cat("Importation : ",nom_objet, "=" , execution_time , "\n")
}

# Exercice 2
#1

dfx = subset(team, city == "Los Angeles", select = c("id", "city"))
dfy = subset(game, select = c("game_id", "team_id_home"))
dfxy = merge(x = dfx, y = dfy, by.x = "id", by.y = "team_id_home", all.x = TRUE)

#2

dfx = dfxy
dfy = subset(game_info, select = c("game_id", "attendance"))
dfxy = merge(dfx, dfy, by.x = "game_id", by.y = "game_id", all.x = TRUE)
mean(dfxy$attendance, na.rm = TRUE)

#3

dfx = subset(game_summary, season == 2020, select = c("game_id", "season"))
dfxy = merge(dfx, officials, by.x = "game_id", by.y = "game_id", all.x = TRUE)
length(dfxy$official_id)

#4

dfy = subset(officials, first_name == "Dick", select = c("game_id", "official_id", "first_name", "last_name"))
dfxy = merge(dfx, dfy, by.x = "game_id", by.y = "game_id", all.y = TRUE)
length(dfxy$official_id)

# Exercice 3
#1

