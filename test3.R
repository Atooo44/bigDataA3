# Visualisation

install.packages("ggplot2")
library(ggplot2)

acc <- read.csv("stat_acc_V3.csv", header=TRUE, sep=";")

# representation graphique : nb accidents en fonction des conditions atmosphériques

x<-data.frame(table(acc$descr_athmo))
graph <- ggplot(data=x, aes(x=Var1, y=Freq)) +  geom_bar(stat="identity", fill="steelblue") + theme_minimal() + geom_text(aes(label=Freq), vjust=-0.3, size=3.5) + labs(title="Nombre d'accidents en fonction des conditions atmosphériques",
             x="Conditions atmosphériques",
             y="Nombre d'accidents")
graph
ggsave("conditions_atmospheriques.png", graph, bg="white", scale=3)


# representation graphique : nb accidents en fonction de la description de la surface

x<-data.frame(table(acc$descr_etat_surf))
graph <- ggplot(data=x, aes(x=Var1, y=Freq)) +  geom_bar(stat="identity", fill="steelblue") + theme_minimal() + geom_text(aes(label=Freq), vjust=-0.3, size=3.5) + labs(title="Nombre d'accidents en fonction de la description de la surface",
             x="Surface",
             y="Nombre d'accidents")
graph
ggsave("description_surface.png", graph, bg="white", scale=2)


# representation graphique : nb accidents selon la gravité

x<-data.frame(table(acc$descr_grav))
graph <- ggplot(data=x, aes(x=Var1, y=Freq)) +  geom_bar(stat="identity", fill="steelblue") + theme_minimal() + geom_text(aes(label=Freq), vjust=-0.3, size=3.5) + labs(title="Nombre d'accidents selon la gravité",
             x="Gravité",
             y="Nombre d'accidents")
graph
ggsave("gravite.png", graph, bg="white", scale=2)


# representation graphique : nb accidents par tranches d'heure
install.packages("stringr")

library(stringr)
# recuperation de l'heure avec minute et seconde
date_decoupe <- str_split(acc$date, " ")
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
x<-data.frame(table(as.integer(acc$id_code_insee/1000)))
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
