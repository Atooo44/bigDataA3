data <- read.csv("stat_acc_V3.csv", header=TRUE, sep=";", na.strings = "NULL")

# dates deja formatees dans le csv
data$date <- as.POSIXlt(data$date)

data$id_code_insee <- as.integer(data$id_code_insee)
# le warning message : some strings don't represent numbers. Those are correctly changed to NA

# Change les valeurs nulles par 0
data$id_code_insee[is.na(data$id_code_insee)] <- 20000

data$an_nais <- as.integer(data$an_nais)

data$age <- as.integer(data$age)

# Change les valeurs nulles par la médiane des valeurs de la colonne
data$age[is.na(data$age)] <- median(data$age, na.rm = TRUE)

data$place <- as.integer(data$place)

data$place[is.na(data$place)] <- median(data$place, na.rm = TRUE)

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
  data$descr_grav[data$descr_grav %in% c(grav_type)]<-count
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
