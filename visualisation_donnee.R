#install.packages("ggplot2")
library(ggplot2)
#install.packages("stringr")
#install.packages("FactoMineR")
# webshot::install_phantomjs()


# representation graphique : nb accidents en fonction des conditions atmosphériques

x<-data.frame(table(data$descr_athmo))
graph <- ggplot(data=x, aes(x=Var1, y=Freq)) +  geom_bar(stat="identity", fill="steelblue") + theme_minimal() + geom_text(aes(label=Freq), vjust=-0.3, size=3.5) + labs(title="Nombre d'accidents en fonction des conditions atmosphériques (voir type_atmosphere.txt pour la correspondance)",
             x="Conditions atmosphériques",
             y="Nombre d'accidents")
graph
ggsave("conditions_atmospheriques.png", graph, bg="white", scale=3)

# representation graphique : nb accidents en fonction de la description de la surface

x<-data.frame(table(data$descr_etat_surf))
graph <- ggplot(data=x, aes(x=Var1, y=Freq)) +  geom_bar(stat="identity", fill="steelblue") + theme_minimal() + geom_text(aes(label=Freq), vjust=-0.3, size=3.5) + labs(title="Nombre d'accidents en fonction de la description de la surface (voir etat_surface.txt pour la correspondance)",
             x="Surface",
             y="Nombre d'accidents")
graph
ggsave("description_surface.png", graph, bg="white", scale=2)

# representation graphique : nb accidents selon la gravité

x<-data.frame(table(data$descr_grav))
graph <- ggplot(data=x, aes(x=Var1, y=Freq)) +  geom_bar(stat="identity", fill="steelblue") + theme_minimal() + geom_text(aes(label=Freq), vjust=-0.3, size=3.5) + labs(title="Nombre d'accidents selon la gravité (voir gravite_accident.txt pour correspondance)",
             x="Gravité",
             y="Nombre d'accidents")
graph
ggsave("gravite.png", graph, bg="white", scale=2)

# representation graphique : nb accidents par tranches d'heure

library(stringr)
# recuperation de l'heure avec minute et seconde
date_decoupe <- str_split(data$date, " ")
heure = c()
for(i in 1:length(date_decoupe)){
  print(date_decoupe[[i]][2])
  heure <- append(heure, date_decoupe[[i]][2])
}
print(heure)
# recuperation de l'heure uniquement
heure_decoupe <- str_split(heure, ":")
heure2 = c()
for(i in 1:length(heure_decoupe)){
  print(heure_decoupe[[i]][1])
  heure2 <- append(heure2, heure_decoupe[[i]][1])
}
print(heure2)

x<-data.frame(table(heure2))
x
graph <- ggplot(data=x, aes(x=heure2, y=Freq)) +  geom_bar(stat="identity", fill="steelblue") + theme_minimal() + geom_text(aes(label=Freq), vjust=-0.3, size=3.5) + labs(title="Nombre d'accidents par tranches d'heure",
             x="Heure",
             y="Nombre d'accidents")
graph
ggsave("heure.png", graph, bg="white", scale=2)

# representation graphique : nb accidents par departement (2 premiers chiffres code insee)
x<-data.frame(table(floor(data$id_code_insee/1000)))
graph <- ggplot(data=x, aes(x=Var1, y=Freq)) +  geom_bar(stat="identity", fill="steelblue") + theme_minimal() + geom_text(aes(label=Freq), vjust=-0.3, size=3.5) + labs(title="Nombre d'accidents par département",
             x="Département",
             y="Nombre d'accidents")
graph
ggsave("accidents_departements.png", graph, bg="white", scale=4)

#Construire des séries chronologiques sur l’évolution du nombre d’accidents par mois et semaines sur l’ensemble de la période

# l'année est uniquement 2009 donc on peut passer par cette méthode

# recuperation des mois, jours et année
date_decoupe <- str_split(data$date, " ")
mois = c()
for(i in 1:length(date_decoupe)){
  print(date_decoupe[[i]][1])
  mois <- append(mois, date_decoupe[[i]][1])
}
print(mois)
# recuperation du mois uniquement
mois_decoupe <- str_split(mois, "-")
mois2 = c()
for(i in 1:length(mois_decoupe)){
  print(mois_decoupe[[i]][2])
  mois2 <- append(mois2, mois_decoupe[[i]][2])
}
print(mois2)

x<-data.frame(table(mois2))
x
graph <- ggplot(data=x, aes(x=mois2, y=Freq, group=1)) +   geom_line(linewidth=1, color="darkseagreen") + geom_point() + theme_minimal() + geom_text(aes(label=Freq), vjust=-0.3, size=3.5, color="blue") + labs(title="Evolution du nombre d'accidents par mois",
                                                                                                                                                                                                                 x="Mois",
                                                                                                                                                                                                                 y="Nombre d'accidents")
graph
ggsave("mois.png", graph, bg="white", scale=2)

# recuperation de la semaine
date <- mois
week <- strftime(mois, "%W")
#Week number of the year (Monday as the first day of the week) as a decimal number. All days in a new year preceding the first Monday are considered to be in week 0.
print(week)

x<-data.frame(table(week))
x
graph <- ggplot(data=x, aes(x=week, y=Freq, group=1)) +   geom_line(linewidth=1, color="darkseagreen") + geom_point() + theme_minimal() + geom_text(aes(label=Freq), vjust=-0.3, size=3.5, color="blue") + labs(title="Evolution du nombre d'accidents par semaine",
                                                                                                                                                                                                                x="Semaine",
                                                                                                                                                                                                                y="Nombre d'accidents")
graph
ggsave("semaine.png", graph, bg="white", scale=3)



# Histogrammes des accidents en fonction de l'âge
hist(data$age, xlab="Age du conducteur", ylab="Nombre d'accidents", main="Quantité d'accidents en fonction de l'âge")
hist(data$age, xlab="Age du conducteur", ylab="Nombre d'accidents", main="Quantité d'accidents en fonction de l'âge", breaks = c(0, 20, 40, 60, 80, 100, 120, 140))
hist(data$age, xlab="Age du conducteur", ylab="Nombre d'accidents", main="Quantité d'accidents en fonction de l'âge", breaks = c(10, 30, 50, 70, 90, 110, 130))
hist(data$age, xlab="Age du conducteur", ylab="Nombre d'accidents", main="Quantité d'accidents en fonction de l'âge", breaks = c(10,20,30,40,50,60,70,80,90,100,110,120,130))

# Histogramme des accidents en fonction des mois de l'année
date_reformat<-as.Date(data$date)
hist(date_reformat, xlab="Mois", ylab="Fréquence d'accidents", main="Moyenne mensuelle des accidents", breaks = c(as.Date("2009-01-01"), as.Date("2009-02-01"), as.Date("2009-03-01"), as.Date("2009-04-01"), as.Date("2009-05-01"), as.Date("2009-06-01"), as.Date("2009-07-01"), as.Date("2009-08-01"), as.Date("2009-09-01"), as.Date("2009-10-01"), as.Date("2009-11-01"), as.Date("2009-12-01"), as.Date("2009-12-31")))

library("raster")

### Création des cartes par départements et exportation en png
code_insee_char<-as.character(data$id_code_insee)
for (i in 10:99){
  # Contours administratifs
  filename<-paste("map_departement/map", i, sep="")
  filename<-paste(filename, "png", sep=".")
  png(file = filename)
  adm_fr <- getData('GADM', country='FRA', level=1) 
  plot(adm_fr)
  
  x <- data$longitude[startsWith(code_insee_char, as.character(i))]; y <- data$latitude[startsWith(code_insee_char, as.character(i))]
  points(x,y,col="red",cex=0.5,pch=16)
  dev.off()
}

#### Création des cartes par régions et exportation en png

#### REGION AUVERGNE RHONES ALPES
auvergne_rhone_alpes<-c(15, 26, 38, 42, 43, 63, 69, 73, 74)
png(file = "map_region/map_auvergne_rhone_alpes.png")
adm_fr <- getData('GADM', country='FRA', level=1) 
plot(adm_fr)
for (i in auvergne_rhone_alpes){
  x <- data$longitude[startsWith(code_insee_char, as.character(i))]; y <- data$latitude[startsWith(code_insee_char, as.character(i))]
  points(x,y,col="red",cex=0.5,pch=16)
}
dev.off()

#### REGION BOURGOGNE FRANCHE COMTE
bourgogne_franche_comte<-c(21, 25, 39, 58, 70, 71, 89, 90)
png(file = "map_region/map_bourgogne_franche_comte.png")
adm_fr <- getData('GADM', country='FRA', level=1) 
plot(adm_fr)
for (i in bourgogne_franche_comte){
  x <- data$longitude[startsWith(code_insee_char, as.character(i))]; y <- data$latitude[startsWith(code_insee_char, as.character(i))]
  points(x,y,col="red",cex=0.5,pch=16)
}
dev.off()

#### REGION BRETAGNE
bretagne<-c(22, 29, 35, 56)
png(file = "map_region/map_bretagne.png")
adm_fr <- getData('GADM', country='FRA', level=1) 
plot(adm_fr)
for (i in bretagne){
  x <- data$longitude[startsWith(code_insee_char, as.character(i))]; y <- data$latitude[startsWith(code_insee_char, as.character(i))]
  points(x,y,col="red",cex=0.5,pch=16)
}
dev.off()

#### REGION CENTRE VAL DE LOIRE
centre_val_loire<-c(18, 28, 36, 37, 41, 45)
png(file = "map_region/map_centre_val_loire.png")
adm_fr <- getData('GADM', country='FRA', level=1) 
plot(adm_fr)
for (i in centre_val_loire){
  x <- data$longitude[startsWith(code_insee_char, as.character(i))]; y <- data$latitude[startsWith(code_insee_char, as.character(i))]
  points(x,y,col="red",cex=0.5,pch=16)
}
dev.off()

#### REGION CORSE
corse<-c(20)
png(file = "map_region/map_corse.png")
adm_fr <- getData('GADM', country='FRA', level=1) 
plot(adm_fr)
for (i in corse){
  x <- data$longitude[startsWith(code_insee_char, as.character(i))]; y <- data$latitude[startsWith(code_insee_char, as.character(i))]
  points(x,y,col="red",cex=0.5,pch=16)
}
dev.off()

#### REGION GRAND EST
grand_est<-c(10, 51, 52, 54, 55, 57, 67, 68, 88)
png(file = "map_region/map_grand_est.png")
adm_fr <- getData('GADM', country='FRA', level=1) 
plot(adm_fr)
for (i in grand_est){
  x <- data$longitude[startsWith(code_insee_char, as.character(i))]; y <- data$latitude[startsWith(code_insee_char, as.character(i))]
  points(x,y,col="red",cex=0.5,pch=16)
}
dev.off()


#### REGION HAUTS DE RRANCE
haut_france<-c(59, 60, 62, 80)
png(file = "map_region/map_haut_france.png")
adm_fr <- getData('GADM', country='FRA', level=1) 
plot(adm_fr)
for (i in haut_france){
  x <- data$longitude[startsWith(code_insee_char, as.character(i))]; y <- data$latitude[startsWith(code_insee_char, as.character(i))]
  points(x,y,col="red",cex=0.5,pch=16)
}
dev.off()


#### REGION ILE DE FRANCE
ile_france<-c(75, 77, 78, 91, 92, 93, 94, 95)
png(file = "map_region/map_ile_de_france.png")
adm_fr <- getData('GADM', country='FRA', level=1) 
plot(adm_fr)
for (i in ile_france){
  x <- data$longitude[startsWith(code_insee_char, as.character(i))]; y <- data$latitude[startsWith(code_insee_char, as.character(i))]
  points(x,y,col="red",cex=0.5,pch=16)
}
dev.off()

#### REGION NORMANDIE
normandie<-c(14, 27, 50, 61, 76)
png(file = "map_region/map_normandie.png")
adm_fr <- getData('GADM', country='FRA', level=1) 
plot(adm_fr)
for (i in normandie){
  x <- data$longitude[startsWith(code_insee_char, as.character(i))]; y <- data$latitude[startsWith(code_insee_char, as.character(i))]
  points(x,y,col="red",cex=0.5,pch=16)
}
dev.off()


#### REGION NOUVELLE AQUITAINE
nouvelle_aquitaine<-c(16, 17, 19, 23, 24, 33, 40, 47, 64, 79, 86, 87)
png(file = "map_region/map_nouvelle_aquitaine.png")
adm_fr <- getData('GADM', country='FRA', level=1) 
plot(adm_fr)
for (i in nouvelle_aquitaine){
  x <- data$longitude[startsWith(code_insee_char, as.character(i))]; y <- data$latitude[startsWith(code_insee_char, as.character(i))]
  points(x,y,col="red",cex=0.5,pch=16)
}
dev.off()

#### REGION OCCITANIE
occitanie<-c(11, 12, 30, 31, 32, 34, 46, 48, 65, 66, 81, 82)
png(file = "map_region/map_occitanie.png")
adm_fr <- getData('GADM', country='FRA', level=1) 
plot(adm_fr)
for (i in occitanie){
  x <- data$longitude[startsWith(code_insee_char, as.character(i))]; y <- data$latitude[startsWith(code_insee_char, as.character(i))]
  points(x,y,col="red",cex=0.5,pch=16)
}
dev.off()


#### REGION PAYS DE LA LOIRE
pdl<-c(44, 49, 53, 72, 85)
png(file = "map_region/map_pays_de_la_loire.png")
adm_fr <- getData('GADM', country='FRA', level=1) 
plot(adm_fr)
for (i in pdl){
  x <- data$longitude[startsWith(code_insee_char, as.character(i))]; y <- data$latitude[startsWith(code_insee_char, as.character(i))]
  points(x,y,col="red",cex=0.5,pch=16)
}
dev.off()

#### REGION PROVENCE ALPES COTE D'AZUR
paca<-c(13, 83, 84)
png(file = "map_region/map_provence_alpes_cote_azur.png")
adm_fr <- getData('GADM', country='FRA', level=1) 
plot(adm_fr)
for (i in paca){
  x <- data$longitude[startsWith(code_insee_char, as.character(i))]; y <- data$latitude[startsWith(code_insee_char, as.character(i))]
  points(x,y,col="red",cex=0.5,pch=16)
}
dev.off()

#CARTE DE CHALEUR

library(mapview)
library(leaflet)
library(leaflet.extras)
library(FactoMineR)
map<-leaflet(data) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView(lng = 2.2, lat = 46.2, zoom = 5) %>%
  addHeatmap(
    lng = ~longitude, lat = ~latitude,
    blur = 20, max = 0.05, radius = 15
  )
map
mapshot(map, file="heatmap.png", type="png")

#GRAPH PCA



png(file="PCA_graph.png")
PCA(data[,6:18], graph = TRUE)
dev.off()