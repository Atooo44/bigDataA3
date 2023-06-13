# Visualisation

install.packages("ggplot2")
library(ggplot2)

acc <- read.csv("stat_acc_V3.csv", header=TRUE, sep=";")

# representation graphique : nb accidents en fonction des conditions atmosphériques

x<-data.frame(table(acc$descr_athmo))
graph <- ggplot(data=x, aes(x=Var1, y=Freq)) +  geom_bar(stat="identity", fill="steelblue") + theme_minimal() + geom_text(aes(label=Freq), vjust=-0.3, size=3.5)
graph + labs(title="Nombre d'accidents en fonction des conditions atmosphériques",
             x="Conditions atmosphériques",
             y="Nombre d'accidents")

# representation graphique : nb accidents en fonction de la description de la surface

x<-data.frame(table(acc$descr_etat_surf))
graph <- ggplot(data=x, aes(x=Var1, y=Freq)) +  geom_bar(stat="identity", fill="steelblue") + theme_minimal() + geom_text(aes(label=Freq), vjust=-0.3, size=3.5)
graph + labs(title="Nombre d'accidents en fonction de la description de la surface",
             x="Surface",
             y="Nombre d'accidents")

# representation graphique : nb accidents selon la gravité

x<-data.frame(table(acc$descr_grav))
graph <- ggplot(data=x, aes(x=Var1, y=Freq)) +  geom_bar(stat="identity", fill="steelblue") + theme_minimal() + geom_text(aes(label=Freq), vjust=-0.3, size=3.5)
graph + labs(title="Nombre d'accidents selon la gravité",
             x="Gravité",
             y="Nombre d'accidents")

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
graph <- ggplot(data=x, aes(x=heure2, y=Freq)) +  geom_bar(stat="identity", fill="steelblue") + theme_minimal() + geom_text(aes(label=Freq), vjust=-0.3, size=3.5)
graph + labs(title="Nombre d'accidents par tranches d'heure",
             x="Heure",
             y="Nombre d'accidents")

# representation graphique : nb accidents par departement (2 premiers chiffres code insee)
x<-data.frame(table(as.integer(acc$id_code_insee/1000)))
graph <- ggplot(data=x, aes(x=Var1, y=Freq)) +  geom_bar(stat="identity", fill="steelblue") + theme_minimal() + geom_text(aes(label=Freq), vjust=-0.3, size=3.5) + labs(title="Nombre d'accidents par département",
             x="Département",
             y="Nombre d'accidents")
ggsave("accidents_departements.png", graph, bg="white", scale=4)

#pour save
#ggsave(filename, graph)