
#install.packages("readxl")
#install.packages("tidyverse")
#install.packages("likert")
#install.packages("xtable")

#library(readxl)
library(tidyverse)
library(dplyr)
library(likert)
library(xtable)

data_1=read.table("clipboard",header=TRUE,dec=",",sep="\t")
names(data_1)
attach(data_1)
data_1

renv::init()
renv::restore()  

data_1 <- data_1 %>%
  dplyr::mutate(
    across(
      where(is.character),
      ~as.factor(.x) %>%
        factor(levels = c("Step 1", "Step 2", "Step 3", "Step 4", 
                          "Step 5"))
    )
  ) %>% dplyr::select(-ID)

#factor_levels <- c("Step 1", "Step 2", "Step 3", "Step 4", 
                   #"Step 5")

data1_updated <- data_1 %>%
  as.data.frame() 

survey_p1 <- plot(likert(data1_updated),
                  ordered = T) +
ggtitle("Power and freedom to make important decisions in the food system")
survey_p1
