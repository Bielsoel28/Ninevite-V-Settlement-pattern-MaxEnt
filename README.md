# Ninevite-V
Biel Soriano-Elias, Francesc Xavier Garcia-Ramis, Miquel Molist,
Maximum entropy and GIS: An approach to assessing the settlement pattern of the Ninevite 5 culture (3000–2500 BCE) in the Upper Great Zab region (Erbil Province, Kurdistan region of Iraq), Journal of Archaeological Science: Reports, Volume 65, 2025, https://doi.org/10.1016/j.jasrep.2025.105231.

This repository contains all the data and scripts required to fully reproduce all analyses presented in the paper.

To get started:

Set the wd in a folder with all the data

&

Activate the following packages:

Code for activating Java in computer (May need to be adapted)

Sys.setenv(JAVA_HOME = "C:/Program Files/OpenJDK/jdk-23.0.1")

List of nescessary packages:

packages <- c("terra","sf","dplyr","devtools","viewscape","corrplot","dismo","rJava")

for (package in packages) {
  install.packages(package, character.only = TRUE)
}

for (package in packages) {
  library(package, character.only = TRUE)
}
