
install.packages("readxl")
install.packages("tidyverse")
install.packages("likert")
install.packages("xtable")

library(readxl)
library(tidyverse)
library(likert)
library(dplyr)
library(xtable)

data_2=read.table("clipboard",header=TRUE,dec=",",sep="\t")
data_2=read.table("clipboard",header=TRUE,dec=",",sep="\t", quote="")###pour supprimer les chaines de caractères
names(data_2)
attach(data_2)
data_2

renv::init()
renv::restore()  

#Load data
#survey1
data_1 <- read_excel("data/Agroeco_pratiques_perception.xlsx", sheet=1)
str(data_1)

#Data preprocessing

# Transforme les valeurs des variables en "Oui" si 1 et en "Non" si 0
data_1 <- data_1 %>%
  dplyr::mutate(
    across(
      where(is.numeric),
      ~ifelse(.x==1, "Oui", "Non") %>% factor(levels = c("Non", "Oui"))
    ) 
  ) %>% dplyr::select(-ID)

# Likert scale

az <- data_1 %>%
  as.data.frame()


survey_p1 <- plot(likert(az),
                  ordered = T) +
  ggtitle("Opinion des producteurs sur les pratiques agroécologiques")
survey_p1

#survey2 
data_2 <- read_excel("data/Agroeco_pratiques_perception.xlsx", sheet=2)


data_2 <- data_2 %>%
  dplyr::mutate(
    across(
      where(is.character),
      ~as.factor(.x) %>%
        factor(levels = c("Completement insatisfait", "Quelque peu insatisfait.", "Neutre", "Plutôt satisfait.", 
                          "Complètement satisfait."))
    )
  ) %>% dplyr::select(-ID)

#factor_levels <- c("Completement insatisfait", "Quelque peu insatisfait.", "Neutre", "Plutôt satisfait.", 
                   #"Complètement satisfait.")

data2_updated <- data_2 %>%
  as.data.frame() 

survey_p2 <- plot(likert(data2_updated),
                  ordered = T) +
ggtitle("Satisfaction des producteurs laitiers sur des aspects spécifiques de leur vie")
survey_p2
