# Niveau unité ------------------------------------------------------------

#1. Importer les données ####
library(readxl)
dfu <- read_excel("df.unite.xlsx")
View(dfu) # OK

#2. Structure des données ####
library(dplyr)
library(labelled)
library(sjmisc)

##2.1. Période financière (index) ####
frq(dfu,per)      # OK

##2.2. Dates de débuts et de fin des périodes financières ####
class(dfu$datedeb) # "POSIXct" "POSIXt"
class(dfu$datefin) # "POSIXct" "POSIXt"
dfu$datedeb <- as.Date(dfu$datedeb, "%Y-%M-%D")
dfu$datefin <- as.Date(dfu$datefin, "%Y-%M-%D")
class(dfu$datedeb) # Date
class(dfu$datefin) # Date

##2.3. Variable « période COVID » ####
dfu$cov <- ifelse(dfu$datefin > as.Date("2020-03-13"),1,0) 
dfu$cov <- factor(dfu$cov, levels = c(0,1),labels = c("Pré-COVID","Post-COVID"))
frq(dfu,cov) # OK

# Vérification
dfu %>%
  select(per,datefin,cov) %>%
  View() # OK

##2.4. Durée de la période financière ####
library(ggplot2)
frq(df,dureeper) # OK
df %>%
  ggplot(aes(datefin,dureeper)) +
  coord_cartesian(ylim = c(0,max(df$dureeper))) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_point() + 
  theme_classic()
# Toutes les periodes n'ont pas la même durée. 
# Pour les 6 périodes indexées 12 et 13, la durée diffère de 28 jours. 

##2.5. Lits dressés ####
frq(df,litsdres) # OK
df %>%
  ggplot(aes(datefin,litsdres)) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_point() + 
  theme_classic()
# Le nombre total de lits dressé passe de 267 à 268 
# entre l'année financière 2019-2020 et l'année financière 2020-2021.efin,cov) %>%
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
