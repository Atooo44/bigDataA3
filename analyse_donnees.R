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
coef(model2) # extrait les coefficients estimés
confint(model2) # affiche l'intervalle de confiance à 95% pour les coefficients estimés
fitted(model2) # extrait les valeurs prédites
resid(model2) # extrait les résidus (valeur prédite - valeur réelle)
# residus en ordonnee
res<-resid(model2)
plot(res,main="Résidus")
abline(h=0,col="red")
# residus vs. valeurs prédites
plot(fitted(model2),res,main="Residuals vs. fitted")
abline(h=0,col="red")


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
coef(model2) # extrait les coefficients estimés
confint(model2) # affiche l'intervalle de confiance à 95% pour les coefficients estimés
fitted(model2) # extrait les valeurs prédites
resid(model2) # extrait les résidus (valeur prédite - valeur réelle)
# residus en ordonnee
res<-resid(model2)
plot(res,main="Résidus")
abline(h=0,col="red")
# residus vs. valeurs prédites
plot(fitted(model2),res,main="Residuals vs. fitted")
abline(h=0,col="red")

#TABLEAUX CROISES ET TESTS D'INDEPENDANCE DU CHI2

#DESRIPTION GRAVITE + DESCRIPTION MOTIF TRAJET
t<-table(data$descr_grav,data$descr_motif_traj)
addmargins(t) # pour avoir la somme
prop.table(t) # frequence relative
prop.table(t)*100 # frequence absolue (pourcentage)
png("mosaicplot_gravite_motif_trajet.png")
mosaicplot(t,
           main="Gravité et motif trajet",
           xlab="Gravité",
           ylab="Motif trajet",
           las=1, # labels horizontaux
           shade=TRUE, # standardized residuals
           off=10) # espacement entre les blocs
dev.off()
result <- chisq.test(t)
result
# valeur du khideux : X-squared
# ddl : df

result$observed # tableau d'origine
result$expected # tableau d'indépendance ou tableau des effectifs théoriques --> tableau théorique
result$residuals # tableau des résidus
result$residuals^2 # tableau des écarts à l'indépendance

#pvalue < 0.05 donc on rejette l'hypothèse d'indépendance --> les deux événements sont liés

result <- chisq.test(t, simulate.p.value=TRUE)
result

#DESRIPTION GRAVITE + DESCRIPTION DISPO SECU
t<-table(data$descr_grav,data$descr_dispo_secu)
addmargins(t) # pour avoir la somme
prop.table(t) # frequence relative
prop.table(t)*100 # frequence absolue (pourcentage)
png("mosaicplot_gravite_dispo_secu.png", width=1051, height=1051)
mosaicplot(t,
           main="Gravité et sécurité",
           xlab="Gravité",
           ylab="Sécurité",
           las=1, # labels horizontaux
           shade=TRUE, # standardized residuals
           off=10) # espacement entre les blocs
dev.off()
result <- chisq.test(t)
result
# valeur du khideux : X-squared
# ddl : df

result$observed # tableau d'origine
result$expected # tableau d'indépendance ou tableau des effectifs théoriques --> tableau théorique
result$residuals # tableau des résidus
result$residuals^2 # tableau des écarts à l'indépendance

#p-value < 0.05 donc on rejette l'hypothèse d'indépendance --> les deux événements sont liés

result <- chisq.test(t, simulate.p.value=TRUE)
result

#NUMERO VEHICULE + VILLE
t<-table(data$num_veh,data$ville)
addmargins(t) # pour avoir la somme
prop.table(t) # frequence relative
prop.table(t)*100 # frequence absolue (pourcentage)
result <- chisq.test(t)
result
# valeur du khideux : X-squared
# ddl : df

result$observed # tableau d'origine
result$expected # tableau d'indépendance ou tableau des effectifs théoriques --> tableau théorique
result$residuals # tableau des résidus
result$residuals^2 # tableau des écarts à l'indépendance

# p-value = 1 > 0.05, on conserve l'hypothèse d'indépendance --> les deux événements sont indépendants

result <- chisq.test(t, simulate.p.value=TRUE)
result