#Exo 1

#Question 1

install.packages("readxl")
library(readxl)
pokemon = read_excel(path = "L:/BUT/SD/Promo 2023/lvincentvacle/r/TD3/pokemon.xlsx",sheet = "pokemon")

#Question 2 

dim(pokemon)

#Question 3

summary(pokemon)

#Question 4

pokemon$is_legendary = as.factor(pokemon$is_legendary)
pokemon$generation = as.factor(pokemon$generation)
pokemon$type = as.factor(pokemon$type)

#Question 5

summary(pokemon)

#Exo 2

#Question 1

med = median(pokemon$attack)
pokemon$attack_group = ifelse(pokemon$attack >= med, "attack+","attack-")
pokemon$attack_group = as.factor(pokemon$attack_group)
summary(pokemon)

#Question 2

pokemon$water_fire = ifelse(pokemon$type %in% c("water","fire"),"yes","no")
pokemon$water_fire = as.factor(pokemon$water_fire)
summary(pokemon)

#Question 3

q3_attack = quantile(pokemon$attack,probs = 0.75)
q3_defense = quantile(pokemon$defense,probs = 0.75)
q3_speed = quantile(pokemon$speed,probs = 0.75)
pokemon$best = ifelse(pokemon$attack > q3_attack &
                      pokemon$defense > q3_defense &
                      pokemon$speed > q3_speed,"yes","no")
pokemon$best = as.factor(pokemon$best)
summary(pokemon)

#Question 1

requete = subset(pokemon,is.na(weight_kg))
View(requete)

#Question 2

requete = subset(pokemon,!is.na(weight_kg))
View(requete)

#Question 3

med = median(pokemon$weight_kg, na.rm = TRUE)
weight_kgNa = ifelse(is.na(pokemon$weight_kg),med,pokemon$weight_kg)
med_height = median(pokemon$height_m, na.rm = TRUE)
height_mNA = ifelse(is.na(pokemon$height_m),med_height,pokemon$height_m)
View(pokemon)

#Question 4

pokemon$weight_group= cut(pokemon$weight_kgNa, breaks = 3,labels = c("leger","moyen","lourd"))
summary(pokemon$weight_group)

#Question 5

pokemon$height_m_group = cut(pokemon$height_mNA, breaks = c(0,1,2,3,max(pokemon$height_mNA)))

#Question 6

min = min(pokemon$defense)
max = max(pokemon$defense)
q1 = quantile(pokemon$defense, probs = 0.25)
q2 = quantile(pokemon$defense, probs = 0.50)
q3 = quantile(pokemon$defense, probs = 0.75)
pokemon$defense_group = cut(pokemon$defense, breaks = c(min,q1,q2,q3,max), include.lowest = TRUE)
summary(pokemon$defense_group)

#Exo 3

#Question 1

aggregate(x = attack ~ type, data = pokemon, FUN = function(x) mean(x))

#Question 2

aggregate(x = attack ~ type + generation, data = pokemon, FUN = function(x) median(x))

#Question 3

aggregate(x = pokedex_number ~ type, data = pokemon, FUN = function(x) length(x))

#Question 4

aggregate(speed ~ generation + type, data = pokemon, FUN = function(x) c(mean(x),median(x),length(x)))

          