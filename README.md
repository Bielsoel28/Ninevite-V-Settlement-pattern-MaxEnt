# Ninevite-V
Garcia-Ramis, X.; Soriano-Elias, B. &amp; Molist Montañà, M. (In prep.) Maximum entropy and GIS: An approach to Assessing the Settlement Pattern of Ninevite 5 Culture in Northern Mesopotamia (3000-2500 BCE).

This repository contains all the data and scripts required to fully reproduce all analyses presented in the paper.

To get started:

Set the wd in a folder with all the data

&

Activate the following packages:

Code for activating Java in computer (May need to be adapted)

Sys.setenv(JAVA_HOME = "C:/Program Files/OpenJDK/jdk-23.0.1")

List of nescessary packages:

packages <- c("terra","sf","dplyr","devtools","viewscape","spatstat","MASS","corrplot","dismo","rJava")

for (package in packages) {
  install.packages(paquete, character.only = TRUE)
}

for (package in packages) {
  library(paquete, character.only = TRUE)
}
