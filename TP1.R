#Exo 1
#Question 1

iris
class(iris)

#Question 2

View(iris)

#Question 3

nrow(iris)

#Question 4

ncol(iris)

#Question 5

colnames(iris)

#Question 6

summary(iris)

#Question 7

iris[c("Sepal.Length","Species")]

#Question 8

iris[c(100,103,105),]

#Question 9

iris[c(50,100),]

#Question 10

mean(iris$"Sepal.Length")

#Question 11

median(iris$"Sepal.Width")

#Question 12

sd(iris$"Petal.Length")

#Question 13

quantile(iris$"Petal.Width", probs = seq(0,0.9,0.1))

#Exercice 2
#Question 1

dfManga = read.csv("C:/Users/lvincentvacle/Downloads/manga.csv",sep = ",", dec = "." )
dfAnime = read.csv("C:/Users/lvincentvacle/Downloads/anime.csv",sep = ",", dec = "." )
class(dfAnime)
class(dfManga)

#Question 2

View(dfManga)
View(dfAnime)

#Question 3

dim(dfAnime)
dim(dfManga)

#Question 4

mean(dfAnime$Score)
mean(dfManga$Score)

#Question 5

sum(dfAnime$Vote)
sum(dfManga$Vote)

#Question 6

sd(dfAnime$Score)
sd(dfManga$Score)

#Question 7

quantile(dfManga$Score,probs = seq(0,0.9,0.1))
quantile(dfAnime$Score,probs = seq(0,0.9,0.1))

#Question 8

nbmangas = subset(dfManga,Score >9)
nrow(nbmangas)

#Question 9

nbmangas = subset(dfManga,Vote > 200000)
nrow(nbmangas)

#Question 10

nbmangas = subset(dfManga,Vote > 200000 & Score >8 )
nrow(nbmangas)

#Question 11

nbmangas = subset(dfManga,Score > 7 & Score < 8 )
nrow(nbmangas)

#Question 12

nbmangas = subset(dfManga,Score > 7 & Score < 8 )
nrow(nbmangas)

#Question 13

effectif = table(dfAnime$Rating)
print(effectif)
length(effectif)
prop.table(effectif)

#Question 14

nbanime = subset(dfAnime,Rating = "R - 17+ (violence & profanity)" )
nrow(nbanime)

#Question 15

nbanime = subset(dfAnime,Rating == "R - 17+ (violence & profanity)" &  Score > 8 )
nrow(nbanime)

#Question 16

nbanime = subset(dfAnime,Rating != "R - 17+ (violence & profanity)" )
nrow(nbanime)

#Question 17

nbanime = subset(dfAnime,Rating %in% c("PG - Children", "G - All Ages") )
nrow(nbanime)

#Question 18

nbanime = subset(dfAnime,!Rating %in% c("PG - Children", "G - All Ages") )
nrow(nbanime)

#Question 19

nbanime = subset(dfAnime ,Score > 9 | Vote > 400000 )
nrow(nbanime)

#Question 20

dfAnime = dfAnime[,c("Title",'Score',"Vote","Ranked")]
dfManga = dfManga[,c("Title",'Score',"Vote","Ranked")]

#Question 21

dfAnime$Type = "Anime"
dfManga$Type = "Manga"

#Question 22

dfConcat = rbind(dfAnime,dfManga)
View(dfConcat)

#Question 23

write.table(dfConcat,"C:/Users/lvincentvacle/Downloads/ExportTp1.csv",sep = ";",FALSE)

