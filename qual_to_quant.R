cat("\014")  
# Importation des données
data=read.csv("stat_acc_V3.csv", header=TRUE, sep=";")
data$date <- as.Date(data$date)

#### DESCR_AGGLO
agglo=levels(factor(data$descr_agglo))

count=1
output="CORRESPONDANCE AGGLOMERATION"
for (agglo_type in agglo) {
  data$descr_agglo[data$descr_agglo %in% c(agglo_type)]<-count
  output1<-paste(agglo_type, count, sep=" = ")
  output<-paste(output, output1, sep="\n")
  count<-count+1
}
sink("agglomeration.txt")
cat(output)
sink()

#### DESCR_CAT_VEH
veh=levels(factor(data$descr_cat_veh))

count=1
output="CORRESPONDANCE CATEGORIE VEHICULE"
for (veh_type in veh) {
  data$descr_cat_veh[data$descr_cat_veh %in% c(veh_type)]<-count
  output1<-paste(veh_type, count, sep=" = ")
  output<-paste(output, output1, sep="\n")
  count<-count+1
}
sink("categorie_vehicule.txt")
cat(output)
sink()

#### DESCR_ATHMO
athmo=levels(factor(data$descr_athmo))

count=1
output="CORREPONDANCE ATHMOSPHERE"
for (athmo_type in athmo) {
  data$descr_athmo[data$descr_athmo %in% c(athmo_type)]<-count
  output1<-paste(athmo_type, count, sep=" = ")
  output<-paste(output, output1, sep="\n")
  count<-count+1
}
sink("type_athmosphere.txt")
cat(output)
sink()

#### DESCR_LUM
lum=levels(factor(data$descr_lum))

count=1
output="CORRESPONDANCE LUMIERE"
for (lum_type in lum) {
  data$descr_lum[data$descr_lum %in% c(lum_type)]<-count
  output1<-paste(lum_type, count, sep=" = ")
  output<-paste(output, output1, sep="\n")
  count<-count+1
}
sink("type_luminosite.txt")
cat(output)
sink()

#### DESCR_ETAT_SURF
surf=levels(factor(data$descr_etat_surf))

count=1
output="CORRESPONDANCE SURFACE"
for (surf_type in surf) {
  data$descr_etat_surf[data$descr_etat_surf %in% c(surf_type)]<-count
  output1<-paste(surf_type, count, sep=" = ")
  output<-paste(output, output1, sep="\n")
  count<-count+1
}
sink("etat_surface.txt")
cat(output)
sink()

#### DESCRIPTION_INTERSECTION
inter=levels(factor(data$description_intersection))

count=1
output="CORRESPONDANCE INTERSECTION"
for (inter_type in inter) {
  data$description_intersection[data$description_intersection %in% c(inter_type)]<-count
  output1<-paste(inter_type, count, sep=" = ")
  output<-paste(output, output1, sep="\n")
  count<-count+1
}
sink("type_intersection.txt")
cat(output)
sink()

#### DESCR_DISPO_SECU
secu=levels(factor(data$descr_dispo_secu))

count=1
output="CORRESPONDANCE SECURITE"
for (secu_type in secu) {
  data$descr_dispo_secu[data$descr_dispo_secu %in% c(secu_type)]<-count
  output1<-paste(secu_type, count, sep=" = ")
  output<-paste(output, output1, sep="\n")
  count<-count+1
}
sink("disponibilite_securite.txt")
cat(output)
sink()

#### DESCR_GRAV
grav=levels(factor(data$descr_grav))

count=1
output="CORRESPONDANCE GRAVITE"
for (grav_type in grav) {
  data$descr_grave[data$descr_grav %in% c(grav_type)]<-count
  output1<-paste(grav_type, count, sep=" = ")
  output<-paste(output, output1, sep="\n")
  count<-count+1
}
sink("gravite_accident.txt")
cat(output)
sink()

#### DESCR_MOTIF_TRAJ
motif=levels(factor(data$descr_motif_traj))

count=1
output="CORRESPONDANCE MOTIF TRAJET"
for (motif_type in motif) {
  data$descr_motif_traj[data$descr_motif_traj %in% c(motif_type)]<-count
  output1<-paste(motif_type, count, sep=" = ")
  output<-paste(output, output1, sep="\n")
  count<-count+1
}
sink("motif_trajet.txt")
cat(output)
sink()

#### DESCR_TYPE_COL
col=levels(factor(data$descr_type_col))

count=1
output="CORRESPONDANCE COLLISION"
for (col_type in col) {
  data$descr_type_col[data$descr_type_col %in% c(col_type)]<-count
  output1<-paste(col_type, count, sep=" = ")
  output<-paste(output, output1, sep="\n")
  count<-count+1
}
sink("type_collision.txt")
cat(output)
sink()

data

#### Histogrammes des accidents en fonction de l'âge
hist(data$age, xlab="Age du conducteur", ylab="Nombre d'accidents", main="Quantité d'accidents en fonction de l'âge")
hist(data$age, xlab="Age du conducteur", ylab="Nombre d'accidents", main="Quantité d'accidents en fonction de l'âge", breaks = c(0, 20, 40, 60, 80, 100, 120, 140))
hist(data$age, xlab="Age du conducteur", ylab="Nombre d'accidents", main="Quantité d'accidents en fonction de l'âge", breaks = c(10, 30, 50, 70, 90, 110, 130))
hist(data$age, xlab="Age du conducteur", ylab="Nombre d'accidents", main="Quantité d'accidents en fonction de l'âge", breaks = c(10,20,30,40,50,60,70,80,90,100,110,120,130))

### Histogramme des accidents en fonction des mois de l'année
hist(data$date, xlab="Mois", ylab="Fréquence d'accidents", main="Moyenne mensuelle des accidents", breaks = c(as.Date("2009-01-01"), as.Date("2009-02-01"), as.Date("2009-03-01"), as.Date("2009-04-01"), as.Date("2009-05-01"), as.Date("2009-06-01"), as.Date("2009-07-01"), as.Date("2009-08-01"), as.Date("2009-09-01"), as.Date("2009-10-01"), as.Date("2009-11-01"), as.Date("2009-12-01"), as.Date("2009-12-31")))

### Création des cartes par départements et exportation en png
for (i in 10:99){
  # Contours administratifs
  filename<-paste("map_departement/map", i, sep="")
  filename<-paste(filename, "png", sep=".")
  png(file = filename)
  adm_fr <- getData('GADM', country='FRA', level=1) 
  plot(adm_fr)
  
  # Simulons les points ! (rnorm sert à simuler des valeurs : pas d'inquiétude !)
  x <- data$longitude[startsWith(data$id_code_insee, as.character(i))]; y <- data$latitude[startsWith(data$id_code_insee, as.character(i))]
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
  x <- data$longitude[startsWith(data$id_code_insee, as.character(i))]; y <- data$latitude[startsWith(data$id_code_insee, as.character(i))]
  points(x,y,col="red",cex=0.5,pch=16)
}
dev.off()

#### REGION BOURGOGNE FRANCHE COMTE
bourgogne_franche_comte<-c(21, 25, 39, 58, 70, 71, 89, 90)
png(file = "map_region/map_bourgogne_franche_comte.png")
adm_fr <- getData('GADM', country='FRA', level=1) 
plot(adm_fr)
for (i in bourgogne_franche_comte){
  x <- data$longitude[startsWith(data$id_code_insee, as.character(i))]; y <- data$latitude[startsWith(data$id_code_insee, as.character(i))]
  points(x,y,col="red",cex=0.5,pch=16)
}
dev.off()

#### REGION BRETAGNE
bretagne<-c(22, 29, 35, 56)
png(file = "map_region/map_bretagne.png")
adm_fr <- getData('GADM', country='FRA', level=1) 
plot(adm_fr)
for (i in bretagne){
  x <- data$longitude[startsWith(data$id_code_insee, as.character(i))]; y <- data$latitude[startsWith(data$id_code_insee, as.character(i))]
  points(x,y,col="red",cex=0.5,pch=16)
}
dev.off()

#### REGION CENTRE VAL DE LOIRE
centre_val_loire<-c(18, 28, 36, 37, 41, 45)
png(file = "map_region/map_centre_val_loire.png")
adm_fr <- getData('GADM', country='FRA', level=1) 
plot(adm_fr)
for (i in centre_val_loire){
  x <- data$longitude[startsWith(data$id_code_insee, as.character(i))]; y <- data$latitude[startsWith(data$id_code_insee, as.character(i))]
  points(x,y,col="red",cex=0.5,pch=16)
}
dev.off()

#### REGION CORSE
corse<-c(99)
png(file = "map_region/map_corse.png")
adm_fr <- getData('GADM', country='FRA', level=1) 
plot(adm_fr)
for (i in corse){
  x <- data$longitude[startsWith(data$id_code_insee, as.character(i))]; y <- data$latitude[startsWith(data$id_code_insee, as.character(i))]
  points(x,y,col="red",cex=0.5,pch=16)
}
dev.off()

#### REGION GRAND EST
grand_est<-c(10, 51, 52, 54, 55, 57, 67, 68, 88)
png(file = "map_region/map_grand_est.png")
adm_fr <- getData('GADM', country='FRA', level=1) 
plot(adm_fr)
for (i in grand_est){
  x <- data$longitude[startsWith(data$id_code_insee, as.character(i))]; y <- data$latitude[startsWith(data$id_code_insee, as.character(i))]
  points(x,y,col="red",cex=0.5,pch=16)
}
dev.off()


#### REGION HAUTS DE RRANCE
haut_france<-c(59, 60, 62, 80)
png(file = "map_region/map_haut_france.png")
adm_fr <- getData('GADM', country='FRA', level=1) 
plot(adm_fr)
for (i in haut_france){
  x <- data$longitude[startsWith(data$id_code_insee, as.character(i))]; y <- data$latitude[startsWith(data$id_code_insee, as.character(i))]
  points(x,y,col="red",cex=0.5,pch=16)
}
dev.off()


#### REGION ILE DE FRANCE
ile_france<-c(75, 77, 78, 91, 92, 93, 94, 95)
png(file = "map_region/map_ile_de_france.png")
adm_fr <- getData('GADM', country='FRA', level=1) 
plot(adm_fr)
for (i in ile_france){
  x <- data$longitude[startsWith(data$id_code_insee, as.character(i))]; y <- data$latitude[startsWith(data$id_code_insee, as.character(i))]
  points(x,y,col="red",cex=0.5,pch=16)
}
dev.off()

#### REGION NORMANDIE
normandie<-c(14, 27, 50, 61, 76)
png(file = "map_region/map_normandie.png")
adm_fr <- getData('GADM', country='FRA', level=1) 
plot(adm_fr)
for (i in normandie){
  x <- data$longitude[startsWith(data$id_code_insee, as.character(i))]; y <- data$latitude[startsWith(data$id_code_insee, as.character(i))]
  points(x,y,col="red",cex=0.5,pch=16)
}
dev.off()


#### REGION NOUVELLE AQUITAINE
nouvelle_aquitaine<-c(16, 17, 19, 23, 24, 33, 40, 47, 64, 79, 86, 87)
png(file = "map_region/map_nouvelle_aquitaine.png")
adm_fr <- getData('GADM', country='FRA', level=1) 
plot(adm_fr)
for (i in nouvelle_aquitaine){
  x <- data$longitude[startsWith(data$id_code_insee, as.character(i))]; y <- data$latitude[startsWith(data$id_code_insee, as.character(i))]
  points(x,y,col="red",cex=0.5,pch=16)
}
dev.off()

#### REGION OCCITANIE
occitanie<-c(11, 12, 30, 31, 32, 34, 46, 48, 65, 66, 81, 82)
png(file = "map_region/map_occitanie.png")
adm_fr <- getData('GADM', country='FRA', level=1) 
plot(adm_fr)
for (i in occitanie){
  x <- data$longitude[startsWith(data$id_code_insee, as.character(i))]; y <- data$latitude[startsWith(data$id_code_insee, as.character(i))]
  points(x,y,col="red",cex=0.5,pch=16)
}
dev.off()


#### REGION PAYS DE LA LOIRE
pdl<-c(44, 49, 53, 72, 85)
png(file = "map_region/map_pays_de_la_loire.png")
adm_fr <- getData('GADM', country='FRA', level=1) 
plot(adm_fr)
for (i in pdl){
  x <- data$longitude[startsWith(data$id_code_insee, as.character(i))]; y <- data$latitude[startsWith(data$id_code_insee, as.character(i))]
  points(x,y,col="red",cex=0.5,pch=16)
}
dev.off()

#### REGION PROVENCE ALPES COTE D'AZUR
paca<-c(13, 83, 84)
png(file = "map_region/map_provence_alpes_cote_azur.png")
adm_fr <- getData('GADM', country='FRA', level=1) 
plot(adm_fr)
for (i in paca){
  x <- data$longitude[startsWith(data$id_code_insee, as.character(i))]; y <- data$latitude[startsWith(data$id_code_insee, as.character(i))]
  points(x,y,col="red",cex=0.5,pch=16)
}
dev.off()


library(mapview)
library(leaflet)
library(leaflet.extras)
map<-leaflet(data) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView(lng = 2.2, lat = 46.2, zoom = 5) %>%
  addHeatmap(
    lng = ~longitude, lat = ~latitude,
    blur = 20, max = 0.05, radius = 15
  )
map
mapshot(map, file="heatmap.png", type="png")
