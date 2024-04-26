data=read.table("clipboard",header=TRUE,dec=",",sep="\t")
names(data)
attach(data)
#install.packages("prettyR")
library(prettyR)
describe(data)
summary(data)
str(data)
library(agricolae)
#[1] "Clust"                  "nb_fos_fum"             "qte_fourqualite_kgMS"  
#[4] "qte_fourgrossier_kgMS"  "nb_bov_UBT"      "nb_Silos"              
#[7] "nb_Fenil"               "nb_hangars_foin"        "sens_regl_vivre_ens"   
#[10] "suivi_sanitaire"        "sup_ha"                 "sup_proprio_inteste_ha"

###ANOVA
#####nb_fos_fum
shapiro.test(nb_fos_fum)
mod1=aov(nb_fos_fum~Clust)
summary.aov(mod1)
tapply(nb_fos_fum,Clust,mean)
tapply(nb_fos_fum,Clust,sd)
A1<-SNK.test(mod1,"Clust");A1


#####qte_fourqualite_kgMS
shapiro.test(qte_fourqualite_kgMS)
mod2=aov(qte_fourqualite_kgMS~Clust)
summary.aov(mod2)
tapply(qte_fourqualite_kgMS,Clust,mean)
tapply(qte_fourqualite_kgMS,Clust,sd)
A2<-SNK.test(mod2,"Clust");A2


#####qte_fourgrossier_kgMS
shapiro.test(qte_fourgrossier_kgMS)
mod3=aov(qte_fourgrossier_kgMS~Clust)
summary.aov(mod3)
tapply(qte_fourgrossier_kgMS,Clust,mean)
tapply(qte_fourgrossier_kgMS,Clust,sd)
A3<-SNK.test(mod3,"Clust");A3


#####nb_bov_UBT
shapiro.test(nb_bov_UBT)
mod4=aov(nb_bov_UBT~Clust)
summary.aov(mod4)
tapply(nb_bov_UBT,Clust,mean)
tapply(nb_bov_UBT,Clust,sd)
A4<-SNK.test(mod4,"Clust");A4


#####nb_Silos
shapiro.test(nb_Silos)
mod5=aov(nb_Silos~Clust)
summary.aov(mod5)
tapply(nb_Silos,Clust,mean)
tapply(nb_Silos,Clust,sd)
A5<-SNK.test(mod5,"Clust");A5

#####nb_Fenil
shapiro.test(nb_Fenil)
mod6=aov(nb_Fenil~Clust)
summary.aov(mod6)
tapply(nb_Fenil,Clust,mean)
tapply(nb_Fenil,Clust,sd)
A6<-SNK.test(mod6,"Clust");A6


#####nb_hangars_foin
shapiro.test(nb_hangars_foin)
mod7=aov(nb_hangars_foin~Clust)
summary.aov(mod7)
tapply(nb_hangars_foin,Clust,mean)
tapply(nb_hangars_foin,Clust,sd)
A7<-SNK.test(mod7,"Clust");A7

#####sens_regl_vivre_ens
shapiro.test(sens_regl_vivre_ens)
mod8=aov(sens_regl_vivre_ens~Clust)
summary.aov(mod8)
tapply(sens_regl_vivre_ens,Clust,mean)
tapply(sens_regl_vivre_ens,Clust,sd)
A8<-SNK.test(mod8,"Clust");A8


#####suivi_sanitaire
shapiro.test(suivi_sanitaire)
mod9=aov(suivi_sanitaire~Clust)
summary.aov(mod9)
tapply(suivi_sanitaire,Clust,mean)
tapply(suivi_sanitaire,Clust,sd)
A9<-SNK.test(mod9,"Clust");A9


#####sup_ha
shapiro.test(sup_ha)
mod10=aov(sup_ha~Clust)
summary.aov(mod10)
tapply(sup_ha,Clust,mean)
tapply(sup_ha,Clust,sd)
A10<-SNK.test(mod10,"Clust");A10


#####sup_proprio_inteste_ha
shapiro.test(sup_proprio_inteste_ha)
mod11=aov(sup_proprio_inteste_ha~Clust)
summary.aov(mod11)
tapply(sup_proprio_inteste_ha,Clust,mean)
tapply(sup_proprio_inteste_ha,Clust,sd)
A11<-SNK.test(mod11,"Clust");A11

#####qte_fourqualite_kgMS_UBT
shapiro.test(qte_fourqualite_kgMS_UBT)
mod2=aov(qte_fourqualite_kgMS_UBT~Clust)
summary.aov(mod2)
tapply(qte_fourqualite_kgMS_UBT,Clust,mean)
tapply(qte_fourqualite_kgMS_UBT,Clust,sd)
A2<-SNK.test(mod2,"Clust");A2


#####qte_fourgrossier_kgMS_UBT
shapiro.test(qte_fourgrossier_kgMS_UBT)
mod3=aov(qte_fourgrossier_kgMS_UBT~Clust)
summary.aov(mod3)
tapply(qte_fourgrossier_kgMS_UBT,Clust,mean)
tapply(qte_fourgrossier_kgMS_UBT,Clust,sd)
A3<-SNK.test(mod3,"Clust");A3
