acc <- read.csv("stat_acc_V3.csv", header=TRUE, sep=";")

# dates deja formatees dans le csv
acc$date <- as.POSIXlt(acc$date)

# obtenir le type d'une colonne
class(acc$date)

acc$date

# type de chaque colonne
str(acc)
