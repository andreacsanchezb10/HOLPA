data1=read.table("clipboard",header=TRUE,dec=",",sep="\t")
data1=read.table("clipboard",header=TRUE,dec=",",sep="\t", quote="")###pour supprimer les chaines de caractères
names(data1)
attach(data1)
data1
install.packages("FactoMineR")
install.packages("factoextra")

data1<-data1[,-1]
print(summary(data1))
#  correlation entre les variables en image 
pairs(data1)
library("FactoMineR")
library(ggplot2)
library("factoextra")
res.pca <- PCA(data1, graph = FALSE)
res.pca <- PCA(data1, graph = TRUE)
print(res.pca)
eig.val <- get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
var <- get_pca_var(res.pca)
res.pca
# Coordonn?es
head(var$coord)
# Cos2: qualit? de r?presentation
head(var$cos2)
# Contributions aux composantes principales
head(var$contrib)
my.cont.var <- rnorm (30)
#install.packages("corrplot")
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)
# Cos2 total des variables sur Dim.1 et Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)
# Colorer en fonction du cos2: qualit? de repr?sentation
fviz_pca_var(res.pca, col.var = "cos2",arrowsize = 1, labelsize = 5,
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # ?vite le chevauchement de texte
)
fviz_pca_ind(res.pca, col.var = "cos2",arrowsize = 1, labelsize = 5,
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # ?vite le chevauchement de texte
)
fviz_pca_var(res.pca, col.var = "cos2",arrowsize = 1, axes = 2:3,labelsize = 5,
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # ?vite le chevauchement de texte
)
fviz_pca_ind(res.pca, col.var = "cos2",arrowsize = 1,axes = 3:4, labelsize = 5,
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # ?vite le chevauchement de texte
)
# modification de la taille des points 
fviz_pca_ind (res.pca, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE #  le chevauchement de texte
)
fviz_pca_ind (res.pca, pointsize = "cos2",
              pointshape = 21, fill = "#E7B800",
              repel = TRUE # ?vite le chevauchement de texte
)
fviz_pca_ind (res.pca, pointsize = "cos2",
              pointshape = 21, fill = "#E7B800",
              repel = FALSE # ?vite le chevauchement de texte
)
# Colorer en fonction du cos2: qualit? de repr?sentation
fviz_pca_var(res.pca, col.var = "cos2",arrowsize = 1, labelsize = 5,
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # ?vite le chevauchement de texte
)           

fviz_pca_var(res.pca, col.var = "cos2",arrowsize = 1, labelsize = 5,
             repel = TRUE,palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             # Couleur des individues
             addEllipses = TRUE, 
             # ?vite le chevauchement de texte
)           


fviz_pca_ind(res.pca, col.var = "cos2",axes = c(1, 2),arrowsize = 1, labelsize = 5,
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # ?vite le chevauchement de texte
)

fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # ?vite le chevauchement de texte
)
# Changer la transparence en fonction du cos2
fviz_pca_var(res.pca, alpha.var = "cos2")
head(var$contrib, 13)
# Contributions des variables ? PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions des variables ? PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
#La contribution totale ? PC1 et PC2 est obtenue avec le code R suivant:
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
#Pour cr?er un biplot simple des individus et des variables, tapez ceci:
fviz_pca_biplot(res.pca, repel = TRUE,axes = c(1, 2),arrowsize = 1, labelsize = 5,
                col.var = "black", # Couleur des variables
                palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                geom.ind = "point",
                col.ind =Treatment,  # Couleur des individues
                addEllipses = TRUE, # Ellipses de concentration
)           

fviz_pca_biplot(res.pca, repel = TRUE,axes = c(1,2),arrowsize = 1, labelsize = 4,
                col.var = "red", # Couleur des variables
                palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                coord.ind = "point",
                col.ind =Treatment,  # Couleur des individues
                addEllipses = TRUE, # Ellipses de concentration
                legend.title = "Groups"
) 
fviz_pca_ind(res.pca, col.var = "cos2",axes = c(1, 2),arrowsize = 1, labelsize = 5,
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # ?vite le chevauchement de texte     
             legend.title = "Groups"
)
fviz_pca_biplot(res.pca, repel = TRUE,axes = c(1,2),arrowsize = 1, labelsize = 4,
                col.var = "red", # Couleur des variables
                palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                geom.ind = "point",
                col.ind =Treatment,  # Couleur des individues
                addEllipses = TRUE, # Ellipses de concentration
                legend.title = "Groups"
)
fviz_pca_biplot(res.pca, repel = TRUE,axes = c(1,2),arrowsize = 1, labelsize = 5,
                col.var = "blue", # Couleur des variables
                geom.ind = "text",
                palette = "jco",
                col.ind = Treatment,  # Couleur des individues
                addEllipses = TRUE, # Ellipses de concentration
                legend.title = "Groups", ellipse.level=0.95
)
par(  # Modificcation de la taille de la police
  cex.main=2, cex.lab=1.7, cex.sub=1.2
)
fviz_pca_biplot(res.pca, repel = TRUE,axes = c(1, 2),arrowsize = 1, labelsize = 4,
                col.var = "red", # Couleur des variables
                geom.ind = "point",
                palette = "jco",
                col.ind =,  # Couleur des individues
                addEllipses = TRUE, # Ellipses de concentration
                legend.title = "Groups", ellipse.level=0.95
)
fviz_pca_biplot(res.pca, repel = TRUE,axes = c(1, 2),arrowsize = 1, labelsize = 4,
                col.var = "red", # Couleur des variables
                coord.ind = "point",
                palette = "jco",
                col.ind = Nature,  # Couleur des individues
                addEllipses = TRUE, # Ellipses de concentration
                legend.title = "Groups", ellipse.level=0.95
                
)
fviz_pca_biplot(res.pca, repel = TRUE,axes = c(1, 2),arrowsize = 1, labelsize = 4,
                col.var = "red", # Coleur des variables
                geom.ind = "point",
                palette = "jco",
                col.ind = Treatment,  # Coleur des individues
                addEllipses = TRUE, # Ellipses de concentration
                legend.title = "Groups", ellipse.level=0.95
)
fviz_pca_biplot(res.pca, repel = TRUE,axes = c(1, 2),arrowsize = 1, labelsize = 4,
                col.var = "red", # Couleur des variables
                geom.ind = "point",
                palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                col.ind = Nature,  # Couleur des individues
                addEllipses = TRUE, # Ellipses de concentration
                legend.title = "Groups", ellipse.level=0.95
)

fviz_pca_biplot(res.pca, repel = TRUE,axes = c(3, 4),arrowsize = 1, labelsize = 4,
                col.var = "red", # Couleur des variables
                geom.ind = "point",
                palette = "simpson",
                col.ind = Produits,  # Couleur des individues
                addEllipses = TRUE, # Ellipses de concentration
                legend.title = "Groups", ellipse.level=0.95
)
fviz_pca_biplot(res.pca, repel = TRUE,axes = c(1, 2),arrowsize = 1, labelsize = 5,
                col.var = "red", # Couleur des variables
                geom.ind = "text",
                palette = "simpson",
                col.ind =Produits,  # Couleur des individues
                addEllipses = TRUE, # Ellipses de concentration
                legend.title = "Groups", ellipse.level=0.95
)
fviz_pca_biplot(res.pca, repel = TRUE,axes = c(3, 4),arrowsize = 1, labelsize = 5,
                col.var = "black", # Couleur des variables
                coord.ind = "individus",
                palette = "simpson",
                col.ind =Nature,  # Couleur des individues
                addEllipses = TRUE, # Ellipses de concentration
                legend.title = "Groups", ellipse.level=0.95
)
# Cr?er une variable al?atoire continue de longueur 10
set.seed (123)
my.cont.var <- rnorm (10)
# Colorer les variables en fonction de la variable continue
fviz_pca_var(res.pca, col.var = my.cont.var,
             gradient.cols = c("blue", "yellow", "red"),
             legend.title = "Cont.Var")
# Colorer par groupes
# Cr?ez une variable de regroupement en utilisant kmeans
# Cr?ez 3 groupes de variables (centers = 4)
set.seed(123)
res.km <- kmeans(var$coord, centers = 4, nstart = 25)
grp <- as.factor(res.km$cluster)


# Colorer les variables par groupes
fviz_pca_biplot(res.pca, col.var = Location, 
                palette = "jco",
                legend.title = "Cluster")
fviz_pca_ind(res.pca, col.var = Location, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")
fviz_pca_ind(res.pca, col.var = Location, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")
res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description de la dimension 1
res.desc$Dim.1
ind <- get_pca_ind(res.pca)
# Pour acc?der aux diff?rents ?l?ments, utilisez ceci:
# Coordonn?es des individus
head(ind$coord)
# Qualit? des individus
head(ind$cos2)
# Modification de la taille, de la forme 
# et de la couleur de remplissage des points
# Modifier la taille du texte
fviz_pca_ind (res.pca,geom.ind = "text",
              pointsize = 3, pointshape = 21, fill = "lightblue",
              labelsize = 5, repel = TRUE)
#Ellipses
# Add confidence ellipses
fviz_pca_ind(data1.pca, geom.ind = "point", 
             col.ind = Code, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence",
             legend.title = "Groups",repel = TRUE 
)


#CLASSIFICATION HIERARCHIQUE ASCENDANTE

#centrage réduction des données




###Analyse Factorielle des Donn?es Mixtes (AFDM ou FAMD pour Factor Analysis of Mixed Data en anglais)###

data<-data[,-1]

####MICHEL'S SCRIPT ####
data<-read.table("clipboard",header=TRUE,dec=",",sep="\t")
attach(data)
names(data)
library(FactoMineR)
library(ggplot2)
library(factoextra)

str(data)
FAMD(data, ncp=5, sup.var=16:17,ind=NULL, graph = TRUE)
res.famd <- FAMD(data, graph = TRUE)
print(res.famd)
eig.val <- get_eigenvalue(res.famd)
head(eig.val)
fviz_screeplot(res.famd)
var <- get_famd_var (res.famd)
var
head(var$coord)
head(var$cos2)
head(var$contrib)

###Graphique des variables
fviz_famd_var (res.famd, repel = TRUE)
###Contribution ? la premi?re dimension
fviz_contrib (res.famd, "var", axes = 1)
###Contribution ? la deuxi?me dimension
fviz_contrib (res.famd, "var", axes = 2)
###Extraction variables quantitatives
quanti.var <- get_famd_var (res.famd, "quanti.var")
quanti.var


fviz_famd_var(res.famd, "quanti.var", repel = TRUE,
              col.var = "black")

fviz_famd_var(res.famd, "quanti.var", col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

# Couleur par valeurs cos2: qualit? sur le plan des facteurs
fviz_famd_var(res.famd, "quanti.var", col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)

###Graphique des variables qualitatives
quali.var <- get_famd_var(res.famd, "quali.var")
quali.var 

fviz_famd_var(res.famd, "quali.var", col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
             )

###Graphique des individus
ind <- get_famd_ind(res.famd)
ind

fviz_famd_ind(res.famd, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)


fviz_mfa_ind(res.famd, 
             habillage = "Label", # color by groups 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence", 
             repel = TRUE # Avoid text overlapping
             ) 

fviz_ellipses(res.famd, c("Label", "Soil"), repel = TRUE)

fviz_ellipses(res.famd, 1:2, geom = "point")



res<-PCA(data1)# lancer l'analyse PCA
summary(res)
res.hcpc<-HCPC(res)# lancer l'analyse de la classification
res.hcpc<-HCPC(res,consol=T)# consolider les groupes obtenus 
names(res.hcpc)# donne des fonctions utiles qu'on peut utiliser
res.hcpc$data.clust# d?gager les individus de chaque groupe
res.hcpc$desc.axes
res.hcpc$desc.var
res$eig

write.csv(res.hcpc$data.clust,'Cluster_typo.csv')
fviz_cluster(res.hcpc, data[,], ellipse.type = "norm")##### Pour déssiner les ellipes
fviz_ellipses(res.hcpc, repel = TRUE) +theme_bw()



