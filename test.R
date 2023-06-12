print("Hello")
print("test")

acc <- read.csv("stat_acc_V3.csv", header=TRUE, sep=";")
# retourne les valeurs de la colonne
levels(factor(acc$descr_etat_surf))
