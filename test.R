acc <- read.csv("stat_acc_V3.csv", header=TRUE, sep=";")

# dates deja formatees dans le csv
acc$date <- as.POSIXlt(acc$date)

acc$id_code_insee <- as.integer(acc$id_code_insee)
# le warning message : some strings don't represent numbers. Those are correctly changed to NA

acc$an_nais <- as.integer(acc$an_nais)

acc$age <- as.integer(acc$age)

acc$place <- as.integer(acc$place)

# type de chaque colonne
str(acc)
