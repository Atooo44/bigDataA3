# analyse

# lecture
acc <- read.csv("stat_acc_V3.csv", header=TRUE, sep=";")

# condition de validité du khideux
# Les valeurs de toutes les cases du tableau des effectifs doivent être supérieur à 5
# Les pourcentages ne sont pas trop proche de zéro ou de 100%

# tableau croisé : Etude des relations entre variables qualitatives

#DESRIPTION GRAVITE + DESCRIPTION MOTIF TRAJET
t<-table(acc$descr_grav,acc$descr_motif_traj)
addmargins(t) # pour avoir la somme
prop.table(t) # frequence relative
prop.table(t)*100 # frequence absolue (pourcentage)
mosaicplot(t,
           main="Gravité et usage",
           xlab="Gravité",
           ylab="Usage",
           las=1, # labels horizontaux
           shade=TRUE, # standardized residuals
           off=10) # espacement entre les blocs
result <- chisq.test(t)
result
# valeur du khideux : X-squared
# ddl : df

result$observed # tableau d'origine
result$expected # tableau d'indépendance ou tableau des effectifs théoriques --> tableau théorique
result$residuals # tableau des résidus
result$residuals^2 # tableau des écarts à l'indépendance

#pvalue < 0.05 donc on rejette l'hypothèse d'indépendance


#http://mehdikhaneboubi.free.fr/stat/co/khi_deux_r.html
#https://rstudio-pubs-static.s3.amazonaws.com/224337_f0de438bd82e4a769e55e039e33b6a0a.html#manipulation-du-dataframe