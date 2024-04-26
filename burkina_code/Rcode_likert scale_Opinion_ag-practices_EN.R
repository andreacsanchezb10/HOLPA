
installed.packages()

install.packages("readxl")
install.packages("tidyverse")
install.packages("likert")
install.packages("readr")
install.packages("renv")
install.packages("renv")

library(readxl)
library(tidyverse)
library(dplyr)
library(likert)
library(xtable)
library(renv)

#data_1<-read.table("clipboard",header=TRUE,dec=",",sep="\t")
#attach(data_1)
#names(data_1)
#data_1
#Load data
#survey1
data_1 <- read_excel("data/Agroeco_pratiques_perception.xlsx", sheet=1)
str(data_1)

renv::init()
renv::restore() 


#Data preprocessing

# Transforme les valeurs des variables en "Yes" si 1 et en "No" si 0
data_1 <- data_1 %>%
  dplyr::mutate(
    across(
      where(is.numeric),
      ~ifelse(.x==1, "yes", "No") %>% factor(levels = c("No", "yes"))
    ) 
  ) %>% dplyr::select(-ID)

# Likert scale

az <- data %>%
  as.data.frame()


survey_p1 <- plot(likert(az),
                  ordered = T) +
  ggtitle("Agricultural practices implemented on farms")


#survey2 
data_2 <- read_excel("data/Agroeco_pratiques_perception.xlsx", sheet=2)


data_2 <- data_2 %>%
  dplyr::mutate(
    across(
      where(is.character),
      ~as.factor(.x) %>%
        factor(levels = c("Completely disagree", "Somewhat disagree", "Neutral", "Somewhat agree", 
                          "Completely agree"))
    )
  ) %>% dplyr::select(-ID)

#factor_levels <- c("Completely disagree", "Somewhat disagree", "Neutral", "Somewhat agree", 
                   #"Completely agree")

data2_updated <- data_2 %>%
  as.data.frame() 

survey_p2 <- plot(likert(data2_updated),
                  ordered = T) +
ggtitle("Farmers' understanding of the meaning of agroecology")

