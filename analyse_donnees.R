# https://adjectif.net/spip.php?article275
# https://odr.inrae.fr/intranet/carto/cartowiki/index.php/Regression_lin%C3%A9aire_avec_R

# IMPORTANT : exécuter visualisation_donnee.R avant

# apres execution de visualisation des données, on a la variable mois2 qui stocke tous les mois

# REGRESSION LINEAIRE ACCIDENTS MOIS
donnee <- data.frame(table(mois2))
str(donnee)
# les mois sont en format Factor : on les convertit en numeric
donnee$mois2<-as.numeric(levels(donnee$mois2))[donnee$mois2]
str(donnee)
model<-lm(donnee$Freq~donnee$mois2)
png("reg_mois.png")
plot(donnee$mois2,donnee$Freq, main="Accidents en fonction des mois", xlab="Mois", ylab="Accidents", col="blue")
abline(model, col="red", lwd = 3)
legend("bottomright", lwd=3, legend="Droite de régression linéaire", col="red")
dev.off()
summary(model)
anova(model)

# REGRESSION LINEAIRE ACCIDENTS CUMULES MOIS
# accidents cumulés : premier point = janvier, deuxième point = janvier+février...
donnee2<-as.data.frame(cumsum(donnee$Freq))
donnee2$mois<-c(1:12)
colnames(donnee2)<-c("Freq", "mois")
model2<-lm(donnee2$Freq~donnee2$mois)
png("reg_mois_cumule.png")
plot(donnee2$mois,donnee2$Freq, main="Accidents cumulés en fonction des mois", xlab="Mois", ylab="Accidents cumulés", col="blue", sub="Exemple: 2=janvier+février, 3=janvier+février+mars")
abline(model2, col="red", lwd = 3)
legend("bottomright", lwd=3, legend="Droite de régression linéaire", col="red")
dev.off()
summary(model2)
anova(model2)

#REGRESSION LINEAIRE ACCIDENT SEMAINE
# apres execution de visualisation des données, on a la variable week qui stocke toutes les semaines
donnee<-data.frame(table(week))
str(donnee)
# les semaines sont en format Factor : on les convertit en numeric
donnee$week<-as.numeric(levels(donnee$week))[donnee$week]
str(donnee)
model<-lm(donnee$Freq~donnee$week)
png("reg_semaine.png")
plot(donnee$week,donnee$Freq, main="Accidents en fonction des semaines", xlab="Semaines", ylab="Accidents", col="blue")
abline(model, col="red", lwd=3)
dev.off()
summary(model)
anova(model)

#REGRESSION LINEAIRE ACCIDENT CUMULES SEMAINE
# accidents cumulés : premier point = janvier, deuxième point = janvier+février...
donnee2<-as.data.frame(cumsum(donnee$Freq))
donnee2$week<-c(0:52)
colnames(donnee2)<-c("Freq", "week")
donnee2
model2<-lm(donnee2$Freq~donnee2$week)
png("reg_semaine_cumule.png")
plot(donnee2$week,donnee2$Freq, main="Accidents cumulés en fonction des semaines", xlab="Semaines", ylab="Accidents cumulés", col="blue", sub="Exemple: 1=semaine0+semaine1, 2=semaine0+semaine1+semaine2")
abline(model2, col="red", lwd = 3)
legend("bottomright", lwd=3, legend="Droite de régression linéaire", col="red")
dev.off()
summary(model2)
anova(model2)