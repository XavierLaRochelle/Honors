# Niveau institutiionnel --------------------------------------------------

#1. Importer les données ####
library(readxl)
df <- read_excel("pinel.xlsx")
View(df)

#2. Structure des données #### 
library(dplyr)
library(labelled)

##2.1. Période financière (index) ####
frq(df,per)      # OK

##2.2. Dates de débuts et de fin des périodes financières ####
class(df$datedeb) # "POSIXct" "POSIXt"
class(df$datefin) # "POSIXct" "POSIXt"
df$datedeb <- as.Date(df$datedeb, "%Y-%M-%D")
df$datefin <- as.Date(df$datefin, "%Y-%M-%D")
class(df$datedeb) # Date
class(df$datefin) # Date

##2.3. Variable « période COVID » ####

df$cov <- ifelse(df$datefin > as.Date("2020-03-13"),1,0) 
df$cov <- factor(df$cov, levels = c(0,1),labels = c("Pré-COVID","Post-COVID"))
library(sjmisc)
frq(df,cov) # OK

# Vérification
df %>%
  select(per,datefin,cov) %>%
  View() # OK

##2.4. Durée de la période financière ####
frq(df,dureeper) # OK
df %>%
  ggplot(aes(datefin,dureeper)) +
  coord_cartesian(ylim = c(0,max(df$dureeper))) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line() + 
  theme_classic()
# Toutes les periodes n'ont pas la même durée. 
# Pour les 6 périodes indexées 12 et 13, la durée diffère de 28 jours. 

##2.5. Lits dressés ####
frq(df,litsdres) # OK
df %>%
  ggplot(aes(datefin,litsdres)) +
  coord_cartesian(ylim = c(0,max(df$litsdres))) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line() + 
  theme_classic()
# Le nombre total de lits dressé passe de 267 à 268 
# entre l'année financière 2019-2020 et l'année financière 2020-2021.

#3. Visualisation des données ####
library(sjmisc)
library(ggplot2)

##3.1. Admissions ####
frq(df,adm)      # OK
# Le nombre d'admission dépend de la durée de la période.
# Le nombre moyens d'admission est une meilleur mesure du nombre d'admission.
df$madm <- df$adm/df$dureeper
df %>%
  ggplot(aes(datefin,madm)) +
  coord_cartesian(ylim = c(0,max(df$madm) + max(df$madm)/4)) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line() + 
  theme_classic() 
# On voit une légère tendance à la baisse qui coincide 
df %>%
  ggplot(aes(madm,col = cov)) +
  geom_density() + 
  facet_wrap(vars(cov)) +
  theme_classic()

##3.2. Départs ####
frq(df,dep)      # OK
# Le nombre de départ dépend de la durée de la période.
# Le nombre moyens de départ est une meilleur mesure du nombre de départs.
df$mdep <- df$dep/df$dureeper
df %>%
  ggplot(aes(datefin,mdep)) +
  coord_cartesian(ylim = c(0,max(df$mdep) + max(df$mdep)/4)) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line() + 
  theme_classic() 
# On voit une légère tendance à la baisse.
df %>%
  ggplot(aes(mdep)) +
  geom_density() + 
  facet_wrap(vars(cov)) +
  theme_classic()

##3.3. Transferts ####
frq(df,trans)    
# Le nombre moyens de transferts est une meilleur mesure du nombre de transferts.
df$mtrans <- df$trans/df$dureeper
df %>%
  ggplot(aes(datefin,mtrans)) +
  coord_cartesian(ylim = c(0,max(df$mtrans) + max(df$mtrans)/4)) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line() + 
  theme_classic() 
# On voit une importante augmentation à partir de la période
df %>%
  ggplot(aes(mtrans)) +
  geom_density() + 
  facet_wrap(vars(cov)) +
  theme_classic()

##3.4. Taux d'occupation ####

###3.4.1. Jours-présences ####
frq(df,jrpres)   # OK
# Le nombre jours de présence dépend de la durée de la période.
df$mjrpres <- df$jrpres/df$dureeper
df %>%
  ggplot(aes(datefin,mjrpres)) +
  coord_cartesian(ylim = c(0,max(df$mjrpres) + max(df$mjrpres)/4)) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line() + 
  theme_classic() 
# Calculer le nombre de jours-présence moyen par jours règle le problème.  
df %>%
  ggplot(aes(mjrpres)) +
  geom_density() + 
  facet_wrap(vars(cov)) +
  theme_classic()

###3.4.2. Moyenne de patients par jours ####
# Même chose que le nombre Moyens de Jours-Présence.
# moypat = jrpres/dureeper = mjrpres
frq(df,moypat)
df %>%
  ggplot(aes(datefin,moypat)) +
  coord_cartesian(ylim = c(0,max(df$moypat) + max(df$moypat)/4)) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line() + 
  theme_classic() #OK
df %>%
  ggplot(aes(datefin,mjrpres)) +
  coord_cartesian(ylim = c(0,max(df$mjrpres) + max(df$mjrpres)/4)) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line() + 
  theme_classic() #Pareil
# En comparant à la variable calculée au point 3.7 (mjrpres), on constate qu'on obtient le même résultat.
df %>%
  ggplot(aes(moypat)) +
  geom_density() + 
  facet_wrap(vars(cov)) +
  theme_classic()

###3.4.3 Taux d'occupation ####
# Le taux d'occupation correspond au coefficient du nombre de jours-présence observés (jrpres) 
# divisé par le nombre maximum de jours-présence possibles durant la période (jrpresmax = litsdres*dureeper).
df$txoccr <- (df$jrpres*100)/(df$litsdres*df$dureeper)
frq(df,txocc)   
df %>%
  ggplot(aes(datefin,txoccr)) +
  coord_cartesian(ylim = c(0,max(df$txoccr) + max(df$txoccr)/4)) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line() + 
  theme_classic()
df %>%
  ggplot(aes(datefin,txocc)) +
  coord_cartesian(ylim = c(0,max(df$txocc) + max(df$txocc)/4)) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line() + 
  theme_classic() # pas OK.
# Une erreur dans le document excel d'origine à la periode 4 de l'année financière 2021-2022.
# La durée de la période a été notée 25 au lieu de 28. 
df %>%
  ggplot(aes(txoccr)) +
  geom_density() + 
  facet_wrap(vars(cov)) +
  theme_classic()

##3.5. Séjour moyen 

###3.5.1. Jours d'hospitalisation ####
# Correspond à la somme des Jours-présence de tous les départs. 
df$jrhosr <- ifelse(df$dep == 0,NA,df$jrhos)
frq(df,jrhos)
df %>%
  ggplot(aes(datefin,jrhos)) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line() + 
  theme_classic() # OK
df %>%
  ggplot(aes(datefin,jrhosr)) + 
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line() + 
  theme_classic() # Pareil
df %>%
  ggplot(aes(jrhos)) +
  geom_density() + 
  facet_wrap(vars(cov)) +
  theme_classic()

##3.11. Séjour moyen ####
# Correspond au nombre de départ diviser par le nombre de jours d'hospitalisation.
df$sejmoyr <- df$jrhos/df$dep
frq(df,sejmoy)
df %>%
  ggplot(aes(datefin,sejmoy)) + 
  coord_cartesian(ylim = c(0,max(df$sejmoy) + max(df$sejmoy)/4)) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line() + 
  theme_classic() # OK
df %>%
  ggplot(aes(datefin,sejmoyr)) + 
  coord_cartesian(ylim = c(0,max(df$sejmoy) + max(df$sejmoy)/4)) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line() + 
  theme_classic() # OK

# Il semble y avoir un outlier durant la période post-COVID.
df %>%
  ggplot(aes(sejmoy)) + 
  geom_density() + 
  facet_wrap(vars(cov)) + 
  theme_classic() 
df$zsejmoy<-scale(df$sejmoy)
df$zsejmoy <-as.numeric(unlist(df$zsejmoy))
frq(df,zsejmoy,sejmoy)
df %>%
  select(per,datefin,cov,sejmoy) %>%
  View()
# sejmoy = 1196 (zscore = 5,89) durant la période Post-COVID.

# Niveau unité ------------------------------------------------------------

#1. Importer les données ####
library(readxl)
dfu <- read_excel("unite.xlsx")
View(dfu) # OK

#2. Créer une variable « Période COVID » ####
library(dplyr)
library(labelled)

# Convertir les dates d'un format excel au format R
class(dfu$datedeb) # "POSIXct" "POSIXt"
class(dfu$datefin) # "POSIXct" "POSIXt"
dfu$datedeb <- as.Date(dfu$datedeb, "%Y-%M-%D")
dfu$datefin <- as.Date(dfu$datefin, "%Y-%M-%D")
class(dfu$datedeb) # Date
class(dfu$datefin) # Date

# Créer la variable dichotomique
dfu$cov <- ifelse(dfu$datefin > as.Date("2020-03-13"),1,0) 
dfu$cov <- factor(dfu$cov, levels = c(0,1),labels = c("Pré-COVID","Post-COVID"))
frq(dfu,cov) # OK

# Vérification
dfu %>%
  select(per,datefin,cov) %>%
  View() # OK

#3. Vérification des données ####

##3.1.a Période financières ####
# 15 unités x 4 années pour les périodes 1 à 5 
# 15 unités x 3 années pour les périodes 6 à 13.
frq(dfu,per)      # OK

#3.1.b Dates de début et de fin
# 15 unités = 15 dates/périodes 
frq(dfu,datedeb)  # OK
frq(dfu,datefin)  # OK

##3.2. Durée de la période financière ####
frq(dfu,dureeper) # OK
dfu %>%
  ggplot(aes(datefin,dureeper,col = unite)) +
  coord_cartesian(ylim = c(0,max(dfu$dureeper)+max(dfu$dureeper)/4)) +
  geom_line(show.legend = F) + 
  facet_wrap(vars(unite)) +
  theme_classic() # OK

##3.3. Lits dressés ####
frq(dfu,litsdres) # OK
dfu %>%
  ggplot(aes(datefin,litsdres,col = unite)) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line(show.legend = F) + 
  facet_wrap(vars(unite)) +
  theme_classic() #OK
#Pas de lits dressé dans l'unité h2.
#Augmentation de 1 lits dressés dans l'unité c1 en 2020. 
#Échange de trois lits dressés entre l'unité b1 et f1 en 2019.

##3.4. Admissions ####
frq(dfu,adm)      # OK
# Le nombre moyens d'admission par jour est une meilleur mesure du nombre d'admission.
dfu$madm <- dfu$adm/dfu$dureeper
dfu %>%
  ggplot(aes(datefin,madm,col = unite)) +
  facet_wrap(vars(unite)) + 
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line(show.legend = F) + 
  theme_classic()  
# Avant la pandémie, la majorité des admissions semblaient avoir lieu dans les unités h3, h4 et c1.
# Après le début de la pandémie, les admissions ont largement transitionnées vers l'unité h2.
summary(dfu$madm)
# min = 0; max = 1,04
dfu %>%
  ggplot(aes(madm,col = unite)) +
  coord_cartesian(xlim = c(0,1.05)) +
  facet_wrap(vars(unite)) +
  geom_density(show.legend = F) + 
  theme_classic()
dfu %>%
  filter(cov == "Pré-COVID") %>%
  ggplot(aes(madm,col = unite)) +
  coord_cartesian(xlim = c(0,1.05)) +
  facet_wrap(vars(unite)) +
  geom_density(show.legend = F) + 
  theme_classic()
dfu %>%
  filter(cov == "Post-COVID") %>%
  ggplot(aes(madm,col = unite)) +
  coord_cartesian(xlim = c(0,1.05)) +
  facet_wrap(vars(unite)) +
  geom_density(show.legend = F) + 
  theme_classic()

##3.5. Départs ####
frq(dfu,dep)      # OK
# Le nombre moyens de départ par jour est une meilleur mesure du nombre de départs.
dfu$mdep <- dfu$dep/dfu$dureeper
dfu %>%
  ggplot(aes(datefin,mdep,col = unite)) +
  facet_wrap(vars(unite)) + 
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line(show.legend = F) + 
  theme_classic()  
# Peut de différences avant et après la pandémie. 
# Les départs semblent avoir diminué dans les unités h3 et h4.
# Il est intéressant de constater qu'il y a des départs de l'unité h2. 
summary(dfu$mdep)
# min = 0; max = 1,04
dfu %>%
  filter(cov == "Pré-COVID") %>%
  ggplot(aes(mdep,col = unite)) +
  coord_cartesian(xlim = c(0,1.05)) +
  facet_wrap(vars(unite)) +
  geom_density(show.legend = F) + 
  theme_classic()
dfu %>%
  filter(cov == "Post-COVID") %>%
  ggplot(aes(mdep,col = unite)) +
  coord_cartesian(xlim = c(0,1.05)) +
  facet_wrap(vars(unite)) +
  geom_density(show.legend = F) + 
  theme_classic()

##3.6. Transferts ####
frq(dfu,trans) # OK 
# Le nombre moyens de transferts est une meilleur mesure du nombre de transferts.
dfu$mtrans <- dfu$trans/dfu$dureeper
dfu %>%
  ggplot(aes(datefin,mtrans,col = unite)) +
  facet_wrap(vars(unite)) + 
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line(show.legend = F) + 
  theme_classic() 
# Les transferts semblent avoir augmentés après le début de la pandémie dans les unités c1, h3 et h4. 
summary(dfu$mtrans)
# min = 0; max = 0,96
dfu %>%
  filter(cov == "Pré-COVID") %>%
  ggplot(aes(mtrans,col = unite)) +
  coord_cartesian(xlim = c(0,1)) +
  facet_wrap(vars(unite)) +
  geom_density(show.legend = F) + 
  theme_classic()
dfu %>%
  filter(cov == "Post-COVID") %>%
  ggplot(aes(mtrans,col = unite)) +
  coord_cartesian(xlim = c(0,1)) +
  facet_wrap(vars(unite)) +
  geom_density(show.legend = F) + 
  theme_classic()

##3.7. Jours-présence ####
frq(dfu,jrpres)   # OK
# Le nombre jours de présence dépend de la durée de la période.
dfu$mjrpres <- dfu$jrpres/dfu$dureeper
dfu %>%
  ggplot(aes(datefin,mjrpres,col = unite)) +
  facet_wrap(vars(unite)) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line(show.legend = F) + 
  theme_classic() 
# On voit une diminution importante des jours-présence dans les unités h3 et h4 au début de la pandémie.
# Toutefois, cette diminution semble éphémère. 
summary(dfu$mjrpres)
# min = 1,13; max = 21,29

dfu %>%
  filter(cov == "Pré-COVID") %>%
  ggplot(aes(mjrpres,col = unite)) +
  coord_cartesian(xlim = c(0,22)) +
  facet_wrap(vars(unite)) +
  geom_density(show.legend = F) + 
  theme_classic()
dfu %>%
  filter(cov == "Post-COVID") %>%
  ggplot(aes(mjrpres,col = unite)) +
  coord_cartesian(xlim = c(0,22)) +
  facet_wrap(vars(unite)) +
  geom_density(show.legend = F) + 
  theme_classic()

##3.8. Nombre moyen de patients par jours ####
frq(dfu,moypat)
dfu %>%
  ggplot(aes(datefin,moypat,col = unite)) +
  facet_wrap(vars(unite)) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line(show.legend = F) + 
  theme_classic() # OK
dfu %>%
  ggplot(aes(datefin,mjrpres,col = unite)) +
  facet_wrap(vars(unite)) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) +
  geom_line(show.legend = F) + 
  theme_classic() # Pareil (avec une légère marge d'erreur d'approximation)

##3.9. Taux d'occupation ####
frq(dfu,txocc)
dfu %>%
  ggplot(aes(datefin,txocc,col = unite)) +
  facet_wrap(vars(unite)) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line(show.legend = F) + 
  theme_classic() # OK
# On voit une diminution importante du taux d'occupation dans les unités h3 et h4 au début de la pandémie.
# Toutefois, cette diminution semble éphémère. 

##3.10 Jours d'hospitalisation ####
dfu$jrhost <- dfu$sejmoy*dfu$dep
dfu$jrhosr <- ifelse(dfu$dep == 0,NA,dfu$jrhos)
dfu %>%
  select(dep,jrhos,jrhosr,sejmoy) %>%
  View(.)

# Après vérification des documents excels, certaine cellule n'ont pas été codé correctement pour la variable « Durée moyenne de séjour ».
# La variable « jrhosr » est correctement codé et représente donc une meilleur représentation des données.
summary(dfu$jrhosr)
dfu %>%
  ggplot(aes(jrhosr,col = unite)) +
  geom_density(show.legend = F) + 
  theme_classic() + 
  facet_wrap(vars(unite))

##3.11. Séjour moyen ####
frq(dfu,sejmoy)
dfu %>%
  ggplot(aes(datefin,sejmoy, col = unite)) +
  facet_wrap(vars(unite)) + 
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line(show.legend = F) +
  theme_classic()
dfu %>%
  ggplot(aes(sejmoy, col = unite)) +
  geom_density(show.legend = F) + 
  facet_wrap(vars(unite)) +
  theme_classic()
