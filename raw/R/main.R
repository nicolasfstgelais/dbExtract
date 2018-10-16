rm(list=ls(all=TRUE))

devtools::install("../dbExtract")
library("dbExtract")
dbExtract_init()
dbExtract()
DBnormStations()
dataPrep()
sitesClassification()


